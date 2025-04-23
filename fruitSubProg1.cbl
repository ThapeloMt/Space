      *SUBPROGRAM
       IDENTIFICATION DIVISION.
       PROGRAM-ID. fruitSubProg1.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       LINKAGE SECTION.
       COPY fruitProgDatabook REPLACING FRUIT-TABLE BY BUSKET
                                        FRUITS      BY ITEMS                               
                                        CATEGORY    BY ITEM-CATEG                               
                                        FRUIT-NAME  BY ITEM-NAME.

       PROCEDURE DIVISION USING BUSKET,CATEGORIZE,NAMING.
           
              UNSTRING CATEGORIZE DELIMITED BY SPACE
                       INTO ITEM-CATEG(1)
                            ITEM-CATEG(2)
                            ITEM-CATEG(3)
              END-UNSTRING

              UNSTRING NAMING     DELIMITED BY SPACES
                       INTO ITEM-NAME(1)
                            ITEM-NAME(2)
                            ITEM-NAME(3)
              END-UNSTRING

               SET IDX TO 1
                PERFORM SPACING
                 PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 3 
                   DISPLAY ITEMS(IDX)
                 END-PERFORM
                PERFORM SPACING
           GOBACK.
           
       SPACING.
        DISPLAY " "
       EXIT.
