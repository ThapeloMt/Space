      *FILING SUB-PROG

       IDENTIFICATION DIVISION.
       PROGRAM-ID. bankSubProg1.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT KEYS ASSIGN TO 'KEEPING.dat'
           ORGANIZATION IS INDEXED
            ACCESS MODE IS DYNAMIC
             RECORD KEY IS KEYS-ID
              FILE STATUS IS FILE-STATUS3.
       DATA DIVISION.
       FILE SECTION.

       FD KEYS.
       01 KEYS-RECORD.
        05 KEYS-ID        PIC 9(5).
        05 KEYS-VALUE     PIC 9(5).

       WORKING-STORAGE SECTION.
        01 WS-KEYS-RECORD.
        05 WS-KEYS-ID        PIC 9(5).
        05 WS-KEYS-VALUE     PIC 9(5).
       01  FILE-STATUS3          PIC XX.
       01 END-OF-FILE            PIC X VALUE "N". 

       PROCEDURE DIVISION.
       
           OPEN INPUT KEYS.
           
            READ KEYS INTO WS-KEYS-RECORD
               AT END
                MOVE 'Y' TO END-OF-FILE
               NOT AT END
               DISPLAY WS-KEYS-RECORD
            END-READ
         
           CLOSE KEYS.     
         STOP RUN.
           