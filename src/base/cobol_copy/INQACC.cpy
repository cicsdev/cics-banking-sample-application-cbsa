      ******************************************************************
      *                                                                *
      *  Copyright IBM Corp. 2022                                      *
      *                                                                *
      *                                                                *
      ******************************************************************
       01 INQACC-COMMAREA.
          03 INQACC-EYE                  PIC X(4).
          03 INQACC-CUSTNO               PIC 9(10).
          03 INQACC-SCODE                PIC 9(6).
          03 INQACC-ACCNO                PIC 9(8).
          03 INQACC-ACC-TYPE             PIC X(8).
          03 INQACC-INT-RATE             PIC 9(4)V99.
          03 INQACC-OPENED               PIC 9(8).
          03 INQACC-OPENED-GROUP REDEFINES INQACC-OPENED.
            05 INQACC-OPENED-DAY         PIC 99.
            05 INQACC-OPENED-MONTH         PIC 99.
            05 INQACC-OPENED-YEAR         PIC 9999.
          03 INQACC-OVERDRAFT            PIC 9(8).
          03 INQACC-LAST-STMT-DT         PIC 9(8).
          03 INQACC-LAST-STMT-GROUP REDEFINES INQACC-LAST-STMT-DT.
            05 INQACC-LAST-STMT-DAY         PIC 99.
            05 INQACC-LAST-STMT-MONTH         PIC 99.
            05 INQACC-LAST-STMT-YEAR         PIC 9999.
          03 INQACC-NEXT-STMT-DT         PIC 9(8).
          03 INQACC-NEXT-STMT-GROUP REDEFINES INQACC-NEXT-STMT-DT.
            05 INQACC-NEXT-STMT-DAY         PIC 99.
            05 INQACC-NEXT-STMT-MONTH         PIC 99.
            05 INQACC-NEXT-STMT-YEAR         PIC 9999.
          03 INQACC-AVAIL-BAL            PIC S9(10)V99.
          03 INQACC-ACTUAL-BAL           PIC S9(10)V99.
          03 INQACC-SUCCESS              PIC X.
          03 INQACC-PCB1-POINTER         POINTER.
