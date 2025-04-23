      *Transaction Databook
       01 TRANSACTION-RECORD.
        05 TRANSACTION-DATE.
         10 TRANS-YEAR         PIC 9(4).
         10 TRANS-MONTH        PIC 9(2).
         10 TRANS-DAY          PIC 9(2).
        05 TRANS-ID            PIC 9(5).
        05 TRANS-ACCOUNT-NUM   PIC 9(10).
        05 TRANS-ACCOUNT-TYPE  PIC X(6).
        05 TRANS-TYPE          PIC X(3).
        05 TRANS-AMOUNT        PIC S9(6)V99.
        05 HASH-TOTALS         PIC 9(10).
           