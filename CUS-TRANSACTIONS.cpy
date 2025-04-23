       
       01 WS-TRANSACTION-RECORD.
        05 WS-TRANSACTION-DATE.
         10 WS-TRANS-YEAR         PIC 9(4).
         10 WS-TRANS-MONTH        PIC 9(2).
         10 WS-TRANS-DAY          PIC 9(2).
        05 WS-TRANS-ID            PIC 9(5).
        05 WS-TRANS-ACCOUNT-NUM   PIC 9(10).
        05 WS-TRANS-ACCOUNT-TYPE  PIC X(6).
        05 WS-TRANS-TYPE          PIC X(3).
        05 WS-TRANS-AMOUNT        PIC S9(6)V99.
        05 WS-HASH-TOTAL          PIC 9(10).
           