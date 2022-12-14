      ******************************************************************
      *                                                                *
      *  Copyright IBM Corp. 2022                                      *
      *                                                                *
      *                                                                *
      ******************************************************************
          03 COMM-EYE                  PIC X(4).
          03 COMM-CUSTNO               PIC X(10).
          03 COMM-SCODE                PIC X(6).
          03 COMM-ACCNO                PIC 9(8).
          03 COMM-ACC-TYPE             PIC X(8).
          03 COMM-INT-RATE             PIC 9(4)V99.
          03 COMM-OPENED               PIC 9(8).
          03 COMM-OPENED-GROUP REDEFINES COMM-OPENED.
             05 COMM-OPENED-DAY              PIC 99.
             05 COMM-OPENED-MONTH            PIC 99.
             05 COMM-OPENED-YEAR             PIC 9999.
          03 COMM-OVERDRAFT            PIC 9(8).
          03 COMM-LAST-STMT-DT         PIC 9(8).
          03 COMM-LAST-STMNT-GROUP REDEFINES COMM-LAST-STMT-DT.
             05 COMM-LASTST-DAY               PIC 99.
             05 COMM-LASTST-MONTH             PIC 99.
             05 COMM-LASTST-YEAR              PIC 9999.
          03 COMM-NEXT-STMT-DT         PIC 9(8).
          03 COMM-NEXT-STMNT-GROUP REDEFINES COMM-NEXT-STMT-DT.
             05 COMM-NEXTST-DAY               PIC 99.
             05 COMM-NEXTST-MONTH             PIC 99.
             05 COMM-NEXTST-YEAR              PIC 9999.
          03 COMM-AVAIL-BAL            PIC S9(10)V99.
          03 COMM-ACTUAL-BAL           PIC S9(10)V99.
          03 COMM-SUCCESS              PIC X.
