       
      *History Databook      
       01 HISTORY-RECORD.
        05 HIS-ID                  PIC 9(10). 
        05 HIS-ACCOUNT-NUM         PIC 9(10). 
        05 HIS-TRANSACTION-DATE.
         10 HIS-TRANS-YEAR         PIC 9(4).
         10 HIS-TRANS-MONTH        PIC 9(2).
         10 HIS-TRANS-DAY          PIC 9(2).
        05 HIS-ACCOUNT-TYPE        PIC X(6).
        05 HIS-TRANS-AMOUNT        PIC S9(6)V99.
           