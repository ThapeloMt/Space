
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RANDOM-NUMBER-SIMULATION.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT ACCOUNT ASSIGN TO 'projAccounts.txt'
         ORGANISATION IS LINE SEQUENTIAL
          ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS FILE-STATUS.
       
       DATA DIVISION.
       
       FILE SECTION.
       FD ACCOUNT.
       01 ACCOUNT-NUMBERS       PIC 9(10).

       WORKING-STORAGE SECTION.
       01 FILE-STATUS            PIC XX. 
       01 END-OF-FILE            PIC X VALUE "N".
       01 TEMP-ACCOUNT-NUMBERS       PIC 9(10).
       01 CURRENT-TIME-VALUE.
          05 HOURS              PIC 99.
          05 MINUTES            PIC 99.
          05 SECONDS            PIC 99.
          05 HUNDREDTH-SECONDS  PIC 99.
       01 RANDOM-NUMBER         PIC 9(2).
       01 MULOFTWO              PIC 9(3).
       01 ANSWER                PIC 9(3).
       01 COUNTER                PIC 9(3).
       01 COUNTER2               PIC 9(3).
       01 TEMPORARY              PIC 9(10).

       01 ACCOUNTS.
        05 ACCOUNT-NUMBER OCCURS 20 TIMES.
         10 NUMBERSONCARD OCCURS 5 TIMES.
          15 VALUESWRITTEN PIC 9(2) VALUE 0.
       
       PROCEDURE DIVISION.
           
          PERFORM VARYING COUNTER2 FROM 1 BY 1 UNTIL COUNTER2 > 20 
           PERFORM GENERATOR
          END-PERFORM.

          PERFORM VARYING COUNTER2 FROM 1 BY 1 UNTIL COUNTER2 > 20 
            MOVE ACCOUNT-NUMBER(COUNTER2) TO TEMPORARY
            COMPUTE TEMPORARY = TEMPORARY * COUNTER2 * HUNDREDTH-SECONDS
            MOVE TEMPORARY TO ACCOUNT-NUMBER(COUNTER2)
          END-PERFORM.
           
           OPEN INPUT ACCOUNT.
           PERFORM ACOUNT-FILE-CHECK
           CLOSE ACCOUNT.

           PERFORM ACOUNT-DISPLAY.

          STOP RUN.
       GENERATOR.

         PERFORM VARYING COUNTER FROM 1 BY 1 UNTIL COUNTER > 5 
           ACCEPT CURRENT-TIME-VALUE FROM TIME
           MULTIPLY COUNTER BY SECONDS HUNDREDTH-SECONDS 
           MULTIPLY SECONDS BY HUNDREDTH-SECONDS GIVING MULOFTWO 
           DIVIDE MULOFTWO BY 100 GIVING ANSWER REMAINDER RANDOM-NUMBER
           MOVE RANDOM-NUMBER TO NUMBERSONCARD(COUNTER2,COUNTER)
         END-PERFORM
       EXIT.

       ACOUNT-FILE-CHECK.

         IF FILE-STATUS = "00"
            DISPLAY "ACCOUNT-FILE exists and opened successfully."
             
         ELSE IF FILE-STATUS = "35"
          DISPLAY "ACCOUNT-FILE      "
                       ": Not found. Loading the file from archive..."
               PERFORM ACOUNT-FILLER
        
           ELSE
           DISPLAY "Error opening file. FILE STATUS: " FILE-STATUS
             
           END-IF
         EXIT.

       ACOUNT-FILLER.
         OPEN OUTPUT ACCOUNT   
           PERFORM VARYING COUNTER2 FROM 1 BY 1 UNTIL COUNTER2 > 20 
            MOVE ACCOUNT-NUMBER(COUNTER2) TO TEMPORARY
            COMPUTE TEMPORARY = TEMPORARY * COUNTER2 * HUNDREDTH-SECONDS
            MOVE TEMPORARY TO ACCOUNT-NUMBER(COUNTER2)

              
                MOVE ACCOUNT-NUMBER(COUNTER2) TO ACCOUNT-NUMBERS
                WRITE ACCOUNT-NUMBERS
                END-WRITE
                
          END-PERFORM
         CLOSE ACCOUNT.
       EXIT.

       ACOUNT-DISPLAY.
           OPEN INPUT ACCOUNT.

           PERFORM UNTIL END-OF-FILE = 'Y'
            READ ACCOUNT INTO TEMP-ACCOUNT-NUMBERS
             
             AT END 
               MOVE 'Y' TO END-OF-FILE
             
             NOT AT END 
               DISPLAY TEMP-ACCOUNT-NUMBERS
            
            END-READ
           END-PERFORM

           CLOSE ACCOUNT.
       EXIT.



