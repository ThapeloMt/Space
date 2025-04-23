      *LOGO

       IDENTIFICATION DIVISION.
       PROGRAM-ID. print.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 LOGO.                         
        05 ROW1 PIC X(100)
         VALUE "                            .==:==.                   ".                                                            
        05 ROW2 PIC X(100)
         VALUE "                      -================-              ".                                                            
        05 ROW3 PIC X(100)
         VALUE "                   =====              =====           ".                                                            
        05 ROW4 PIC X(100)
         VALUE "                :===                     .===:        ".                                                            
        05 ROW5 PIC X(100)
         VALUE "              -===                          ===-      ".                                                            
        05 ROW6 PIC X(100)
         VALUE "             ===                              ===     ".                                                            
        05 ROW7 PIC X(100)
         VALUE "            ===                                ===    ".                                                            
        05 ROW8 PIC X(100)
         VALUE "           -=-                                  :+-   ".                                                            
        05 ROW9 PIC X(100)
         VALUE "                       ==                             ".                                                            
        05 ROW10 PIC X(100)
         VALUE "         :==           ==                         ==. ".                                                            
        05 ROW11 PIC X(100)
         VALUE "         ==:   =====   =====-    =====   .=====   :== ".                                                            
        05 ROW12 PIC X(100)
         VALUE "         ==        ==  ==   ==. ==            ==   == ".                                                            
        05 ROW13 PIC X(100)
         VALUE "         ==    ======  ==    ==  ======  -======   == ".                                                            
        05 ROW14 PIC X(100)
         VALUE "         ==   ==   ==  ==   ==:      ==  ==   ==   == ".                                                            
        05 ROW15 PIC X(100)
         VALUE "         ==:   -=====  ======    =====    =====   :=+ ".                                                            
        05 ROW16 PIC X(100)
         VALUE "         -==                                      ==." .                                                            
        05 ROW17 PIC X(100)
         VALUE "                                                      ".                                                            
        05 ROW18 PIC X(100)
         VALUE "          .=:                                  .=:    ".                                                            
        05 ROW19 PIC X(100)
         VALUE "           ==-                                -==     ".                                                            
        05 ROW20 PIC X(100)
         VALUE "            ===                              ===      ".                                                            
        05 ROW21 PIC X(100)
         VALUE "             ====                          ===-       ".                                                            
        05 ROW22 PIC X(100)
         VALUE "               -===                      ===-         ".                                                            
        05 ROW23 PIC X(100)
         VALUE "                  ====-              -====            ".                                                            
        05 ROW24 PIC X(100)
         VALUE "                     ==================               ".                                                             
        05 ROW25 PIC X(100)
         VALUE "                           .==:==.                    ".                                                             

       PROCEDURE DIVISION.

           PERFORM SPACING
           DISPLAY "                 WELCOME TO ABSA BANKING MONITOR"
           DISPLAY " "
           DISPLAY ROW1
           DISPLAY ROW2
           DISPLAY ROW3
           DISPLAY ROW4
           DISPLAY ROW5
           DISPLAY ROW6
           DISPLAY ROW7
           DISPLAY ROW8
           DISPLAY ROW9
           DISPLAY ROW10
           DISPLAY ROW11
           DISPLAY ROW12
           DISPLAY ROW13
           DISPLAY ROW14
           DISPLAY ROW15
           DISPLAY ROW16
           DISPLAY ROW17
           DISPLAY ROW18
           DISPLAY ROW19
           DISPLAY ROW20
           DISPLAY ROW21
           DISPLAY ROW22
           DISPLAY ROW23
           DISPLAY ROW24
           DISPLAY ROW25
           PERFORM SPACING

           GOBACK.

       SPACING.
           
           PERFORM 3 TIMES
               DISPLAY " "
           END-PERFORM
       EXIT.

           