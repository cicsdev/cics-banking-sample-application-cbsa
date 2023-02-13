      ******************************************************************
      *                                                                *
      *  Copyright IBM Corp. 2023                                      *
      *                                                                *
      *                                                                *
      ******************************************************************
          03 COMM-EYE                  PIC X(4).
          03 COMM-SCODE                PIC X(6).
          03 COMM-CUSTNO               PIC X(10).
          03 COMM-NAME                 PIC X(60).
          03 COMM-ADDR                 PIC X(160).
          03 COMM-DOB                  PIC 9(8).
          03 COMM-DOB-GROUP REDEFINES COMM-DOB.
             05 COMM-BIRTH-DAY               PIC 99.
             05 COMM-BIRTH-MONTH             PIC 99.
             05 COMM-BIRTH-YEAR              PIC 9999.
          03 COMM-CREDIT-SCORE         PIC 9(3).
          03 COMM-CS-REVIEW-DATE       PIC 9(8).
          03 COMM-CS-GROUP REDEFINES COMM-CS-REVIEW-DATE.
             05 COMM-CS-DAY                  PIC 99.
             05 COMM-CS-MONTH                PIC 99.
             05 COMM-CS-YEAR                 PIC 9999.
          03 COMM-UPD-SUCCESS          PIC X.
          03 COMM-UPD-FAIL-CD          PIC X.
