      ******************************************************************
      *                                                                *
      *  Copyright IBM Corp. 2022                                      *
      *                                                                *
      *                                                                *
      ******************************************************************
        01 DELACC-COMMAREA.
          03 DELACC-EYE                  PIC X(4).
          03 DELACC-CUSTNO               PIC X(10).
          03 DELACC-SCODE                PIC X(6).
          03 DELACC-ACCNO                PIC 9(8).
          03 DELACC-ACC-TYPE             PIC X(8).
          03 DELACC-INT-RATE             PIC 9(4)V99.
          03 DELACC-OPENED               PIC 9(8).
          03 DELACC-OPENED-GROUP REDEFINES DELACC-OPENED.
            05 DELACC-OPENED-DAY   PIC 99.
            05 DELACC-OPENED-MONTH PIC 99.
            05 DELACC-OPENED-YEAR  PIC 9999.
          03 DELACC-OVERDRAFT            PIC 9(8).
          03 DELACC-LAST-STMT-DT         PIC 9(8).
          03 DELACC-LAST-STMT-GROUP REDEFINES DELACC-LAST-STMT-DT.
            05 DELACC-LAST-STMT-DAY   PIC 99.
            05 DELACC-LAST-STMT-MONTH PIC 99.
            05 DELACC-LAST-STMT-YEAR  PIC 9999.
          03 DELACC-NEXT-STMT-DT         PIC 9(8).
          03 DELACC-NEXT-STMT-GROUP REDEFINES DELACC-NEXT-STMT-DT.
            05 DELACC-NEXT-STMT-DAY   PIC 99.
            05 DELACC-NEXT-STMT-MONTH PIC 99.
            05 DELACC-NEXT-STMT-YEAR  PIC 9999.
          03 DELACC-AVAIL-BAL            PIC S9(10)V99.
          03 DELACC-ACTUAL-BAL           PIC S9(10)V99.
          03 DELACC-SUCCESS              PIC X.
          03 DELACC-FAIL-CD              PIC X.
          03 DELACC-DEL-SUCCESS          PIC X.
          03 DELACC-DEL-FAIL-CD          PIC X.
          03 DELACC-DEL-APPLID           PIC X(8).
          03 DELACC-DEL-PCB1             POINTER.
          03 DELACC-DEL-PCB2             POINTER.
          03 DELACC-DEL-PCB3             POINTER.
