
       01 WS-HISTORY-RECORD.
        05 WS-HIS-ID                  PIC 9(10). 
        05 WS-HIS-ACCOUNT-NUM         PIC 9(10). 
        05 WS-HIS-TRANSACTION-DATE.
         10 WS-HIS-TRANS-YEAR         PIC 9(4).
         10 WS-HIS-TRANS-MONTH        PIC 9(2).
         10 WS-HIS-TRANS-DAY          PIC 9(2).
        05 WS-HIS-ACCOUNT-TYPE        PIC X(6).
        05 WS-HIS-TRANS-AMOUNT        PIC S9(6)V99.
           