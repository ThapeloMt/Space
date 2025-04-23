       IDENTIFICATION DIVISION.
       PROGRAM-ID. CalculateEpochDays.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 EPOCH-YEAR        PIC 9(4) VALUE 1970.
       01 EPOCH-DAY         PIC 9(9) VALUE 0.
       01 TOTAL-DAYS        PIC 9(9) VALUE 0.
       01 DAYS-IN-MONTH     PIC 9(2) OCCURS 12.
       01 LEAP-YEARS        PIC 9(9) VALUE 0.
       01 NON-LEAP-YEARS    PIC 9(9) VALUE 0.
       01 IS-LEAP-YEAR      PIC X VALUE "N".
       01 QUOTIENT          PIC 9(9).
       01 R1                PIC 9.
       01 R2                PIC 9.
       01 R3                PIC 9.
       01 I                 PIC 9(4) VALUE 0.
      
       LINKAGE SECTION.
       01 YEAR-IN       PIC 9(4).
       01 MONTH-IN      PIC 99.
        01 DAY-IN        PIC 99. 
       01 TOTALRETURN    PIC 9(9).


       PROCEDURE DIVISION USING YEAR-IN ,MONTH-IN ,DAY-IN, TOTALRETURN.
       
           PERFORM INITIALIZE-ARRAY.
           INITIALIZE TOTAL-DAYS.

           PERFORM CALCULATE-DAYS-BEFORE-YEAR.
           PERFORM CALCULATE-DAYS-IN-CURRENT-YEAR.

           COMPUTE TOTAL-DAYS = TOTAL-DAYS + DAY-IN - 1.

           MOVE TOTAL-DAYS TO TOTALRETURN

           GOBACK.

       INITIALIZE-ARRAY.
           MOVE 31 TO DAYS-IN-MONTH(1)
           MOVE 28 TO DAYS-IN-MONTH(2)
           MOVE 31 TO DAYS-IN-MONTH(3)
           MOVE 30 TO DAYS-IN-MONTH(4)
           MOVE 31 TO DAYS-IN-MONTH(5)
           MOVE 30 TO DAYS-IN-MONTH(6)
           MOVE 31 TO DAYS-IN-MONTH(7)
           MOVE 31 TO DAYS-IN-MONTH(8)
           MOVE 30 TO DAYS-IN-MONTH(9)
           MOVE 31 TO DAYS-IN-MONTH(10)
           MOVE 30 TO DAYS-IN-MONTH(11)
           MOVE 31 TO DAYS-IN-MONTH(12)
         EXIT.

       CALCULATE-DAYS-BEFORE-YEAR.
           IF YEAR-IN > EPOCH-YEAR
               PERFORM VARYING I FROM EPOCH-YEAR BY 1 UNTIL I = YEAR-IN
                   PERFORM CHECK-LEAP-YEAR
                   IF IS-LEAP-YEAR = "Y"
                       ADD 366 TO TOTAL-DAYS
                   ELSE
                       ADD 365 TO TOTAL-DAYS
                   END-IF
               END-PERFORM
           ELSE
               PERFORM VARYING I FROM YEAR-IN BY 1 UNTIL I = EPOCH-YEAR
                   PERFORM CHECK-LEAP-YEAR
                   IF IS-LEAP-YEAR = "Y"
                       SUBTRACT 366 FROM TOTAL-DAYS
                   ELSE
                       SUBTRACT 365 FROM TOTAL-DAYS
                   END-IF
               END-PERFORM
           END-IF
           EXIT.

       CALCULATE-DAYS-IN-CURRENT-YEAR.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I = MONTH-IN
               ADD DAYS-IN-MONTH(I) TO TOTAL-DAYS
               IF I = 2
                   PERFORM CHECK-LEAP-YEAR
                   IF IS-LEAP-YEAR = "Y"
                       ADD 1 TO TOTAL-DAYS
                   END-IF
               END-IF
           END-PERFORM
           EXIT.

       CHECK-LEAP-YEAR.
           DIVIDE I BY 4 GIVING QUOTIENT REMAINDER R1.
           IF R1 = 0
               DIVIDE I BY 100 GIVING QUOTIENT REMAINDER R2
               IF R2 NOT = 0
                   MOVE "Y" TO IS-LEAP-YEAR
               ELSE
                   DIVIDE I BY 400 GIVING QUOTIENT REMAINDER R3
                   IF R3 = 0
                       MOVE "Y" TO IS-LEAP-YEAR
                   ELSE
                       MOVE "N" TO IS-LEAP-YEAR
                   END-IF
               END-IF
           ELSE
               MOVE "N" TO IS-LEAP-YEAR
           END-IF
           EXIT.
