       IDENTIFICATION DIVISION.
       PROGRAM-ID. CREATE-CUSTOMERSDATA.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTOMERSDATA ASSIGN TO 'CUSTOMER.txt'
               ORGANIZATION IS LINE SEQUENTIAL
                ACCESS MODE IS SEQUENTIAL
                 FILE STATUS IS FILE-STATUS.
           
           SELECT ACCOUNT ASSIGN TO 'projAccounts.txt'
            ORGANISATION IS LINE SEQUENTIAL
             ACCESS MODE IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  CUSTOMERSDATA.
       COPY projectCusDataBook.
       
       FD ACCOUNT.
       01 ACCOUNT-NUMBERS            PIC 9(10).

       WORKING-STORAGE SECTION.
       COPY pojCusRecordBook.
       01 FILE-STATUS                PIC XX. 
       01 END-OF-FILE                PIC X VALUE "N".
       01 TEMP-ACCOUNT-NUMBERS       PIC 9(10).
       01 ALIGN                      PIC X(2) VALUE " ".
       01  WS-CUSTOMER-COUNTER   PIC 9(2) VALUE 1.
       01  WS-CUS-NAMES          PIC X(50) VALUE SPACES.
       01  WS-ACCOUNT-TYPES      PIC X(6) VALUE SPACES.
       01  Random-Number    PIC 9(1)V9(9) VALUE 0.
       01  Scaled-Random-Number PIC 9(3) VALUE 0.
       01  Scaled-Random-AMOUNT PIC S9(9)V9(2).

       PROCEDURE DIVISION.
    
       OPEN INPUT CUSTOMERSDATA.
       IF FILE-STATUS = "00"
        DISPLAY "CUSTOMERSDATA exists and opened successfully."
         
       ELSE IF FILE-STATUS = "35"
      
        OPEN OUTPUT CUSTOMERSDATA
        OPEN INPUT ACCOUNT
        PERFORM UNTIL END-OF-FILE = 'Y'
         
           READ ACCOUNT INTO TEMP-ACCOUNT-NUMBERS
             
             AT END 
               MOVE 'Y' TO END-OF-FILE
             
             NOT AT END 
               MOVE TEMP-ACCOUNT-NUMBERS TO ACCOUNT-NUM
           END-READ
       
         EVALUATE WS-CUSTOMER-COUNTER
         WHEN 1 MOVE 'John Doe                         ' TO WS-CUS-NAMES
         WHEN 2 MOVE 'Jane Smith                       ' TO WS-CUS-NAMES
         WHEN 3 MOVE 'Alice Johnson                    ' TO WS-CUS-NAMES
         WHEN 4 MOVE 'Bob Brown                        ' TO WS-CUS-NAMES
         WHEN 5 MOVE 'Charlie Davis                    ' TO WS-CUS-NAMES
         WHEN 6 MOVE 'Diana Clark                      ' TO WS-CUS-NAMES
         WHEN 7 MOVE 'Ethan Lewis                      ' TO WS-CUS-NAMES
         WHEN 8 MOVE 'Fiona Adams                      ' TO WS-CUS-NAMES
         WHEN 9 MOVE 'George Young                     ' TO WS-CUS-NAMES
         WHEN 10 MOVE 'Hannah White                    ' TO WS-CUS-NAMES
         WHEN 11 MOVE 'Ian Scott                       ' TO WS-CUS-NAMES
         WHEN 12 MOVE 'Julia Green                     ' TO WS-CUS-NAMES
         WHEN 13 MOVE 'Kevin Hill                      ' TO WS-CUS-NAMES
         WHEN 14 MOVE 'Lily Cooper                     ' TO WS-CUS-NAMES
         WHEN 15 MOVE 'Mason Carter                    ' TO WS-CUS-NAMES
         WHEN 16 MOVE 'Nora Evans                      ' TO WS-CUS-NAMES
         WHEN 17 MOVE 'Oliver Foster                   ' TO WS-CUS-NAMES
         WHEN 18 MOVE 'Paula Baker                     ' TO WS-CUS-NAMES
         WHEN 19 MOVE 'Quincy Turner                   ' TO WS-CUS-NAMES
         WHEN 20 MOVE 'Ruby Hall                       ' TO WS-CUS-NAMES
        WHEN OTHER MOVE 'Unknown                       ' TO WS-CUS-NAMES
               
        END-EVALUATE
        MOVE WS-CUS-NAMES TO ACCOUNT-NAMES
        COMPUTE ACCOUNT-BALANCE = 0
        
        COMPUTE Random-Number = FUNCTION RANDOM
        COMPUTE Scaled-Random-Number = Random-Number * 2
        
        EVALUATE Scaled-Random-Number
           WHEN 0 MOVE 'DEBIT ' TO ACCOUNT-TYPE
           WHEN 1 MOVE 'CREDIT' TO ACCOUNT-TYPE
        END-EVALUATE

         WRITE CUSTOMER-RECORD
        ADD 1 TO WS-CUSTOMER-COUNTER

          IF WS-CUSTOMER-COUNTER > 20
               MOVE 'Y' TO END-OF-FILE
          END-IF
        
         END-PERFORM

          CLOSE ACCOUNT
          CLOSE CUSTOMERSDATA         
        
        ELSE
          DISPLAY "Error opening file. FILE STATUS: " FILE-STATUS
         
         END-IF.

       CLOSE CUSTOMERSDATA.

           OPEN I-O CUSTOMERSDATA.

           IF FILE-STATUS = "00"
           PERFORM UNTIL END-OF-FILE = "Y"
           
           READ CUSTOMERSDATA INTO CUS-RECORD 
            AT END 
             MOVE "Y" TO END-OF-FILE
            NOT AT END
             COMPUTE Random-Number = FUNCTION RANDOM
             COMPUTE Scaled-Random-AMOUNT = Random-Number * 10000000
             
                IF CUS-ACCOUNT-TYPE = "CREDIT"
                   MULTIPLY -1 BY Scaled-Random-AMOUNT
                END-IF
             MOVE Scaled-Random-AMOUNT TO  CUS-ACCOUNT-BALANCE
             MOVE CUS-ACCOUNT-BALANCE TO ACCOUNT-BALANCE
             REWRITE CUSTOMER-RECORD
           
           END-PERFORM

          CLOSE CUSTOMERSDATA

           OPEN INPUT CUSTOMERSDATA.

           MOVE "N" TO END-OF-FILE.
           PERFORM UNTIL END-OF-FILE = "Y"
           
           READ CUSTOMERSDATA INTO CUS-RECORD 
            AT END 
             MOVE "Y" TO END-OF-FILE
            NOT AT END
                  DISPLAY CUS-RECORD
           
           END-PERFORM
           CLOSE CUSTOMERSDATA.
         
           STOP RUN.
         