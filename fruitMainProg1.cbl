      *MAINPROGRAM

       IDENTIFICATION DIVISION.
       PROGRAM-ID. fruitMainProg1.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY fruitProgDatabook.

       PROCEDURE DIVISION.
           
           CALL 'fruitSubProg1' 
               USING FRUIT-TABLE,CATEGORIZE,NAMING.

           STOP RUN.
           