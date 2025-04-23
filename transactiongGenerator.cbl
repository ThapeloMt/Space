      *Transaction Generator

       IDENTIFICATION DIVISION.
       PROGRAM-ID. TRANSACTIONS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT TRANSACTION-FILE ASSIGN TO 'projectTRANSACTIONFile.dat'
         ORGANISATION IS INDEXED
          ACCESS MODE IS DYNAMIC
           RECORD KEY IS TRANS-ID
            FILE STATUS IS FILE-STATUS.

       SELECT CUSTOMERSDATA ASSIGN TO 'CUSTOMER.txt'
         ORGANIZATION IS LINE SEQUENTIAL
          ACCESS MODE IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD TRANSACTION-FILE.
       COPY projectTranDataBook.
       FD  CUSTOMERSDATA.
       COPY projectCusDataBook.

       WORKING-STORAGE SECTION.
       01 FILE-STATUS      PIC  XX.
       COPY pojCusRecordBook.
       COPY CUS-TRANSACTIONS.
       01 END-OF-FILE                PIC X VALUE "N".
       01 CURRENT-DATES.
         05 CURRENT-YEAR         PIC 9(4).
         05 CURRENT-MONTH        PIC 9(2).
         05 CURRENT-DAY          PIC 9(2).
       01  Random-Number    PIC 9(1)V9(9) VALUE 0.
       01  Scaled-Random-Number PIC 9(3) VALUE 0.
       01  Scaled-Random-AMOUNT PIC S9(7)V9(2).
       01  WS-CUSTOMER-COUNTER   PIC 9(2) VALUE 1.
       01  WS-HASH-TOTALS        PIC 9(2) VALUE 1.

       PROCEDURE DIVISION.

           PERFORM TRANSACTION-FILE-CHECK.
           ACCEPT CURRENT-DATES FROM DATE  YYYYMMDD.
           PERFORM TRANSACTION-MACHINE.
           DISPLAY " ". 
           DISPLAY " ". 
           PERFORM TRANSACTION-DISPLAY.

           STOP RUN.

       TRANSACTION-FILE-CHECK.
           
        OPEN I-O TRANSACTION-FILE

         IF FILE-STATUS = "00"
           DISPLAY "TRANSACTION-FILE exists and opened successfully."
             
         ELSE IF FILE-STATUS = "35"
           DISPLAY "TRANSACTION-FILE   "
                       ": doesn't exist. File being created..."
               
               OPEN OUTPUT TRANSACTION-FILE
                   CONTINUE
               CLOSE TRANSACTION-FILE

          ELSE
           DISPLAY "Error opening file. FILE STATUS: " FILE-STATUS
             
          END-IF.

        CLOSE TRANSACTION-FILE.

        EXIT.

       TRANSACTION-MACHINE.
         
         OPEN OUTPUT TRANSACTION-FILE
             CONTINUE
         CLOSE TRANSACTION-FILE

           
         OPEN I-O TRANSACTION-FILE

           OPEN INPUT CUSTOMERSDATA
           
            PERFORM UNTIL END-OF-FILE = 'Y'

              READ CUSTOMERSDATA INTO CUS-RECORD
                AT END
                 MOVE "Y" TO END-OF-FILE
                NOT AT END
                 COMPUTE Random-Number = FUNCTION RANDOM
                 COMPUTE Scaled-Random-Number = Random-Number * 2

                  EVALUATE Scaled-Random-Number
                   WHEN 0 
                     CONTINUE
                   WHEN 1
                     MOVE CURRENT-DATES TO TRANSACTION-DATE
                     MOVE WS-CUSTOMER-COUNTER TO TRANS-ID
                     MOVE CUS-ACCOUNT-NUM TO TRANS-ACCOUNT-NUM
                     MOVE CUS-ACCOUNT-TYPE TO TRANS-ACCOUNT-TYPE
                       
                      COMPUTE Random-Number = FUNCTION RANDOM
                      COMPUTE Scaled-Random-Number = Random-Number * 2 
                       EVALUATE Scaled-Random-Number
                        WHEN 0 
                         MOVE "DEP" TO TRANS-TYPE 
                        WHEN 1
                         MOVE "WIT" TO TRANS-TYPE 
                       END-EVALUATE

                     COMPUTE Random-Number = FUNCTION RANDOM
                    COMPUTE Scaled-Random-AMOUNT = Random-Number *100000
                       IF TRANS-TYPE = "WIT"
                           MULTIPLY -1 BY Scaled-Random-AMOUNT
                       END-IF
                       
                     MOVE Scaled-Random-AMOUNT TO TRANS-AMOUNT
                     MOVE WS-HASH-TOTALS TO HASH-TOTALS 
                     ADD 1 TO WS-CUSTOMER-COUNTER
                     ADD 1 TO WS-HASH-TOTALS
                      WRITE TRANSACTION-RECORD
                  END-EVALUATE
              END-READ
            END-PERFORM
           CLOSE CUSTOMERSDATA

         CLOSE TRANSACTION-FILE

       EXIT.

       TRANSACTION-DISPLAY.
         MOVE "N" TO END-OF-FILE  
         OPEN INPUT TRANSACTION-FILE
           
            PERFORM UNTIL END-OF-FILE = 'Y'

              READ TRANSACTION-FILE INTO WS-TRANSACTION-RECORD
                AT END
                 MOVE "Y" TO END-OF-FILE
                NOT AT END
                  DISPLAY WS-TRANSACTION-DATE " " WS-TRANS-ID  " "
                   WS-TRANS-ACCOUNT-NUM " " WS-TRANS-ACCOUNT-TYPE " "
                   WS-TRANS-TYPE " " WS-TRANS-AMOUNT " " WS-HASH-TOTAL
              END-READ
            END-PERFORM

         CLOSE TRANSACTION-FILE

       EXIT.
           