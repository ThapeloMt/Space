      *The Main Program

       IDENTIFICATION DIVISION.
       PROGRAM-ID. projMainProg.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT CUSTOMER-FILE ASSIGN TO 'projectCusFile.dat'
           ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC
               RECORD KEY IS  ACCOUNT-NUM 
               FILE STATUS IS FILE-STATUS.

       SELECT TRANSACTION-FILE ASSIGN TO 'projectTRANSACTIONFile.dat'
           ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC
               RECORD KEY IS  TRANS-ID 
              FILE STATUS IS FILE-STATUS1.

       SELECT HISTORY-FILE ASSIGN TO 'projectHistoryFile.dat'
           ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC
               RECORD KEY IS  HIS-ID 
              FILE STATUS IS FILE-STATUS2.
       
       SELECT CUSTOMERSDATA ASSIGN TO 'CUSTOMER.txt'
           ORGANIZATION IS LINE SEQUENTIAL
             ACCESS MODE IS SEQUENTIAL.
       
       SELECT KEYS ASSIGN TO 'KEEPING.dat'
           ORGANIZATION IS INDEXED
            ACCESS MODE IS DYNAMIC
             RECORD KEY IS KEYS-ID
              FILE STATUS IS FILE-STATUS3.

       DATA DIVISION.

       FILE SECTION.
       FD CUSTOMER-FILE.
       COPY projectCusDataBook.
      
       FD TRANSACTION-FILE.
       COPY projectTranDataBook.
       
       FD HISTORY-FILE.
       COPY projectHistDataBook.
       
       FD KEYS.
       01 KEYS-RECORD.
        05 KEYS-ID        PIC 9(5).
        05 KEYS-VALUE     PIC 9(5).

       FD CUSTOMERSDATA.
       COPY CusDataBook.

       WORKING-STORAGE SECTION.
       COPY pojCusRecordBook.
       COPY CUS-TRANSACTIONS.
       COPY projHISTdateDataBook.
       COPY TransactionHIST.
       01  FILE-STATUS           PIC XX. 
       01  FILE-STATUS1          PIC XX. 
       01  FILE-STATUS2          PIC XX. 
       01  FILE-STATUS3          PIC XX. 
       01 END-OF-FILE            PIC X VALUE "N".
       01 END-OF-FILE2           PIC X VALUE "N".
       01 WS-HASH-TOTAL-TEMP     PIC 9(10) VALUE 0.
       01 WS-DATERANGE           PIC 9(10) VALUE 0.
       01 WS-HIST-COUNTER        PIC 9(5)  VALUE 1.
       01 WS-HIST-COUNTER2       PIC 9(5)  VALUE 1.
       01 WS-HIS-SUM             PIC 9(10) VALUE 0.

       01 WS-KEYS-RECORD.
        05 WS-KEYS-ID        PIC 9(5).
        05 WS-KEYS-VALUE     PIC 9(5).

       01 WS-KEY-SUM         PIC 9(5).

       01 LABELS1.
        05 USER-ACC              PIC A(11) VALUE "ACCOUNT".
        05 USER-NAME             PIC A(51) VALUE "CUSTOMER NAMES".
        05 USER-ACCOUNT-TYPE     PIC A(7) VALUE "TYPE".
        05 USER-ACC-BALANCE      PIC A(10) VALUE "BALANCE".
       
       01 LABELS2.
        05 TRANSACT-DATE         PIC A(9) VALUE "DATE".
        05 TRANSACT-ID           PIC A(6) VALUE "ID".
        05 TRANSACT-ACCOUNT      PIC A(11) VALUE "ACOUNT".
        05 TRANSACT-ACC-TYPE     PIC A(7) VALUE "TYPE".
        05 TRANSACT-TYPE         PIC X(4) VALUE "D/W".
        05 TRANSACT-AMOUNT       PIC A(11) VALUE "AMOUNT".
        05 TRANSACT-VALIDATION   PIC A(10) VALUE "HASH".

       01 LABELS3.
        05 HIST-ACCOUNT          PIC A(11) VALUE "ACCOUNT".
        05 HISTORY-DATE          PIC A(9)  VALUE "DATE".
        05 HIST-ACC-TYPE         PIC A(7)  VALUE "TYPE".
        05 HIST-TRANS-AMOUNT     PIC A(7)  VALUE "AMOUNT".
       
       
       PROCEDURE DIVISION.

           call 'print'.
           PERFORM SPACING.
           PERFORM CUSTOMER-FILE-CHECK.
           PERFORM DISPLAY-CUSTOMERS.

           PERFORM SPACING.
           OPEN I-O TRANSACTION-FILE.
            PERFORM TRANSACTION-FILE-CHECK
           CLOSE TRANSACTION-FILE.
       
           PERFORM DISPLAY-TRANSACTION.

           PERFORM SPACING.
           PERFORM DISPLAY-CUSTOMERS.
           PERFORM SPACING.
           
           PERFORM KEYS-FILE-CHECK.
           PERFORM HISTORY-FILE-CHECK.
           

           PERFORM SPACING.
           PERFORM WRITE-TRANSACTION-HISTORY.
           PERFORM DISPLAY-TRANSACTION-HISTORY.
          

          STOP RUN.

       CUSTOMER-FILE-CHECK.
           
        OPEN I-O CUSTOMER-FILE.

         IF FILE-STATUS = "00"
            DISPLAY "CUSTOMER-FILE exists and opened successfully."
               CLOSE CUSTOMER-FILE

         ELSE IF FILE-STATUS = "35"
          DISPLAY "CUSTOMER-FILE      "
                       ": Not found. Loading the file from archive..."
              
              OPEN OUTPUT CUSTOMER-FILE    
               OPEN INPUT CUSTOMERSDATA
                   PERFORM UNTIL END-OF-FILE = 'Y'
                     
                   READ CUSTOMERSDATA INTO CUS-RECORD
                     AT END 
                      MOVE 'Y' TO END-OF-FILE
                     
                     NOT AT END 
                     MOVE CUS-RECORD TO CUSTOMER-RECORD
                      WRITE CUSTOMER-RECORD
                       INVALID KEY
                         DISPLAY "Duplicate Key. Record not written."
                       NOT INVALID KEY
                          CONTINUE
                      END-WRITE
                    END-READ
                   END-PERFORM
               CLOSE CUSTOMER-FILE
              CLOSE CUSTOMER-FILE          
          ELSE
          DISPLAY "Error opening file. FILE STATUS: " FILE-STATUS
             
           END-IF

         CLOSE CUSTOMER-FILE.
        EXIT.

       HISTORY-FILE-CHECK.
         OPEN I-O HISTORY-FILE. 
         IF FILE-STATUS2 = "00"
           DISPLAY "HISTORY-FILE exists and opened successfully."

           ACCEPT INPUT-DATE FROM DATE  YYYYMMDD
           call 'CalculateEpochDays'   USING YEAR-IN MONTH-IN DAY-IN 
                 TOTALRETURN1

            MOVE 'N' TO END-OF-FILE
            PERFORM UNTIL END-OF-FILE = 'Y'
                 
            READ HISTORY-FILE INTO WS-HISTORY-RECORD
             AT END 
              MOVE 'Y' TO END-OF-FILE
             
             NOT AT END 
             MOVE WS-HIS-ID TO HIS-ID

             MOVE WS-HIS-TRANSACTION-DATE TO INPUT-DATE
             call 'CalculateEpochDays'   USING YEAR-IN MONTH-IN DAY-IN
             TOTALRETURN2

            END-READ
           
           COMPUTE WS-DATERANGE = TOTALRETURN1 - TOTALRETURN2

            IF WS-DATERANGE >= 100 AND WS-HIS-TRANSACTION-DATE >= 1
             DELETE HISTORY-FILE RECORD
              INVALID KEY 
                DISPLAY "Invalid Key"
              NOT INVALID KEY 
                DISPLAY "Record Deleted"
             END-DELETE
            END-IF
            
           END-PERFORM

           ELSE IF FILE-STATUS2 = "35"
           DISPLAY "HISTORY-FILE       "
                       ": doesn't exist.Please Load the file..."
               OPEN OUTPUT HISTORY-FILE
                   CONTINUE
               CLOSE HISTORY-FILE
             
             ELSE
            DISPLAY "Error opening file. FILE STATUS: " FILE-STATUS2
             
            END-IF
           CLOSE HISTORY-FILE.
           EXIT.

       TRANSACTION-FILE-CHECK.

         IF FILE-STATUS1 = "00"
           DISPLAY "TRANSACTION-FILE exists and opened successfully."
           
           OPEN I-O CUSTOMER-FILE
           PERFORM UNTIL END-OF-FILE2 = 'Y'

           READ TRANSACTION-FILE INTO WS-TRANSACTION-RECORD
            AT END
             MOVE "Y" TO END-OF-FILE2
            NOT AT END
             MOVE WS-TRANS-ACCOUNT-NUM TO ACCOUNT-NUM

              READ CUSTOMER-FILE INTO CUS-RECORD KEY IS ACCOUNT-NUM
               INVALID KEY 
                DISPLAY "Customer record not found for account: "
               NOT INVALID KEY
                 ADD WS-TRANS-AMOUNT TO CUS-ACCOUNT-BALANCE
  
                 MOVE CUS-ACCOUNT-BALANCE TO ACCOUNT-BALANCE
                 REWRITE CUSTOMER-RECORD
                 ADD 1 TO WS-HASH-TOTAL-TEMP                
              END-READ
           END-READ
           END-PERFORM
           DISPLAY "FILE VALIDATION"
           IF WS-HASH-TOTAL = WS-HASH-TOTAL-TEMP
              DISPLAY "The HASH FILES Match"

           ELSE
             DISPLAY "ABEND"
               STOP RUN 
           END-IF
           CLOSE CUSTOMER-FILE
         
           ELSE IF FILE-STATUS1 = "35"
           DISPLAY "TRANSACTION-FILE   "
                       ": doesn't exist. Please Load the file..."     
          ELSE
           DISPLAY "Error opening file. FILE STATUS: " FILE-STATUS1
             
          END-IF
        EXIT.

        SPACING.
           PERFORM 2 TIMES
               DISPLAY " "
           END-PERFORM
        EXIT.

       WRITE-TRANSACTION-HISTORY.

          OPEN I-O HISTORY-FILE.
          MOVE 'N' TO END-OF-FILE
          OPEN INPUT TRANSACTION-FILE
           IF FILE-STATUS1 = "00"
            DISPLAY " "

           OPEN INPUT KEYS 
            READ KEYS INTO WS-KEYS-RECORD
               AT END
                CONTINUE
               NOT AT END
                 IF WS-KEYS-VALUE > 10
                   MOVE WS-KEYS-VALUE TO WS-HIST-COUNTER
                 END-IF

            END-READ
           CLOSE KEYS
          

           PERFORM UNTIL END-OF-FILE = 'Y'    
              READ TRANSACTION-FILE INTO WS-TRANSACTION-RECORD
                AT END
                 MOVE "Y" TO END-OF-FILE
                NOT AT END
                 MOVE WS-HIST-COUNTER       TO HIS-ID 
                 MOVE WS-TRANSACTION-DATE   TO HIS-TRANSACTION-DATE
                 MOVE WS-TRANS-ACCOUNT-NUM  TO HIS-ACCOUNT-NUM
                 MOVE WS-TRANS-ACCOUNT-TYPE TO HIS-ACCOUNT-TYPE
                 MOVE WS-TRANS-AMOUNT       TO HIS-TRANS-AMOUNT 
                 WRITE HISTORY-RECORD
                ADD 1 TO WS-HIST-COUNTER 
                ADD 1 TO WS-HIST-COUNTER2 
              END-READ

           END-PERFORM
           
           OPEN I-O KEYS

              MOVE 1 TO KEYS-ID

              READ KEYS INTO WS-KEYS-RECORD
                 KEY IS KEYS-ID
                 INVALID KEY 
                   STOP RUN
                 NOT INVALID KEY
                   MOVE WS-KEYS-VALUE TO WS-KEY-SUM
              END-READ

              ADD WS-HIST-COUNTER TO WS-KEY-SUM
              ADD 1               TO WS-KEY-SUM
              MOVE WS-KEY-SUM TO KEYS-VALUE
              REWRITE KEYS-RECORD
              END-REWRITE
               
            CLOSE KEYS

           END-IF
          CLOSE TRANSACTION-FILE
          CLOSE HISTORY-FILE.
       EXIT.


       DISPLAY-CUSTOMERS.

          MOVE 'N' TO END-OF-FILE
          OPEN INPUT CUSTOMER-FILE.
           IF FILE-STATUS = "00"
           DISPLAY "The Customers"
           DISPLAY " "
           DISPLAY LABELS1
           PERFORM UNTIL END-OF-FILE = 'Y'
                     
            READ CUSTOMER-FILE INTO CUS-RECORD
             AT END 
              MOVE 'Y' TO END-OF-FILE
             
             NOT AT END 
             DISPLAY CUS-ACCOUNT-NUM  " " CUS-ACCOUNT-NAMES " " 
                    CUS-ACCOUNT-TYPE " " CUS-ACCOUNT-BALANCE

            END-READ
           END-PERFORM
           END-IF
          CLOSE CUSTOMER-FILE.
       EXIT.
          

       DISPLAY-TRANSACTION.

          MOVE 'N' TO END-OF-FILE
          OPEN INPUT TRANSACTION-FILE.
           IF FILE-STATUS1 = "00"
            DISPLAY " "
           DISPLAY "The Transactions"
           DISPLAY LABELS2
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
           END-IF
          CLOSE TRANSACTION-FILE.
      
       EXIT.
       
       DISPLAY-TRANSACTION-HISTORY.

          MOVE 'N' TO END-OF-FILE
          OPEN INPUT HISTORY-FILE.
           IF FILE-STATUS2 = "00"
            DISPLAY " "
           DISPLAY "The Transactions History"
           DISPLAY " "
           DISPLAY LABELS3
           PERFORM UNTIL END-OF-FILE = 'Y'    
              READ HISTORY-FILE INTO WS-HISTORY-RECORD
                AT END
                 MOVE 'Y' TO END-OF-FILE
                NOT AT END
                 DISPLAY WS-HIS-ACCOUNT-NUM  " " WS-HIS-TRANSACTION-DATE
                   " " WS-HIS-ACCOUNT-TYPE " " WS-HIS-TRANS-AMOUNT          
              END-READ
           END-PERFORM
           END-IF
          CLOSE HISTORY-FILE.  
       EXIT.
       
       KEYS-FILE-CHECK.
           
        OPEN I-O KEYS.

         IF FILE-STATUS3 = "00"
             CONTINUE

         ELSE IF FILE-STATUS3 = "35"           
              OPEN OUTPUT KEYS    
                MOVE   1   TO KEYS-ID
                MOVE   0   TO KEYS-VALUE
                WRITE KEYS-RECORD
              CLOSE KEYS         
          ELSE
          DISPLAY "Error opening file. FILE STATUS: " FILE-STATUS3
             
          END-IF

         CLOSE KEYS.
        EXIT.

