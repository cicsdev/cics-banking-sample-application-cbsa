      ******************************************************************
      *                                                                *
      *  Copyright IBM Corp. 2022                                      *
      *                                                                *
      *                                                                *
      ******************************************************************
          03 NUMBER-OF-ACCOUNTS        PIC S9(8) BINARY.
          03 CUSTOMER-NUMBER           PIC 9(10).
          03 COMM-SUCCESS              PIC X.
          03 COMM-FAIL-CODE            PIC X.
          03 CUSTOMER-FOUND            PIC X.
          03 COMM-PCB-POINTER          PIC X(4).
          03 ACCOUNT-DETAILS OCCURS 1 TO 20 DEPENDING ON
              NUMBER-OF-ACCOUNTS.
            05 COMM-EYE                  PIC X(4).
            05 COMM-CUSTNO               PIC X(10).
            05 COMM-SCODE                PIC X(6).
            05 COMM-ACCNO                PIC 9(8).
            05 COMM-ACC-TYPE             PIC X(8).
            05 COMM-INT-RATE             PIC 9(4)V99.
            05 COMM-OPENED               PIC 9(8).
            05 COMM-OPENED-GROUP REDEFINES COMM-OPENED.
              07 COMM-OPENED-DAY PIC 99.
              07 COMM-OPENED-MONTH PIC 99.
              07 COMM-OPENED-YEAR PIC 9999.
            05 COMM-OVERDRAFT            PIC 9(8).
            05 COMM-LAST-STMT-DT         PIC 9(8).
            05 COMM-LAST-STMT-GROUP REDEFINES COMM-LAST-STMT-DT.
              07 COMM-LAST-STMT-DAY PIC 99.
              07 COMM-LAST-STMT-MONTH PIC 99.
              07 COMM-LAST-STMT-YEAR PIC 9999.
            05 COMM-NEXT-STMT-DT         PIC 9(8).
            05 COMM-NEXT-STMT-GROUP REDEFINES COMM-NEXT-STMT-DT.
              07 COMM-NEXT-STMT-DAY PIC 99.
              07 COMM-NEXT-STMT-MONTH PIC 99.
              07 COMM-NEXT-STMT-YEAR PIC 9999.
            05 COMM-AVAIL-BAL            PIC S9(10)V99.
            05 COMM-ACTUAL-BAL           PIC S9(10)V99.
