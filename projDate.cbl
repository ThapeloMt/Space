      **The Program
      
       IDENTIFICATION DIVISION.
       PROGRAM-ID. projDate.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       
       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.

       01 UNFORM-DATE    PIC 9(8) VALUE 0.
       01 INPUT-DATE.
        05 YEAR-IN       PIC 9(4).
        05 MONTH-IN      PIC 99.
        05 DAY-IN        PIC 99.
       01 TOTALRETURN1    PIC 9(9) VALUE 0.
       01 TOTALRETURN2    PIC 9(9) VALUE 0.

       PROCEDURE DIVISION.
           
           ACCEPT INPUT-DATE FROM DATE  YYYYMMDD.
           call 'CalculateEpochDays'   USING YEAR-IN MONTH-IN DAY-IN 
                 TOTALRETURN1.
           display " ".
           DISPLAY TOTALRETURN1.
           
           MOVE 20240930 TO INPUT-DATE.
           call 'CalculateEpochDays'   USING YEAR-IN MONTH-IN DAY-IN
                 TOTALRETURN2.
           display " ".
           DISPLAY TOTALRETURN2.

           STOP RUN.
           