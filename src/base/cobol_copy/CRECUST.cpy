      ******************************************************************
      *                                                                *
      *  Copyright contributors to the CICS Banking Sample Application *
      * (CBSA) project                                                 *
      *                                                                *
      ******************************************************************
          03 COMM-EYECATCHER                 PIC X(4).
          03 COMM-KEY.
             05 COMM-SORTCODE                PIC 9(6) DISPLAY.
             05 COMM-NUMBER                  PIC 9(10) DISPLAY.
          03 COMM-NAME                       PIC X(60).
          03 COMM-ADDRESS                    PIC X(160).
          03 COMM-DATE-OF-BIRTH              PIC 9(8).
          03 COMM-DOB-GROUP REDEFINES COMM-DATE-OF-BIRTH.
             05 COMM-BIRTH-DAY               PIC 99.
             05 COMM-BIRTH-MONTH             PIC 99.
             05 COMM-BIRTH-YEAR              PIC 9999.
          03 COMM-CREDIT-SCORE               PIC 999.
          03 COMM-CS-REVIEW-DATE             PIC 9(8).
          03 COMM-CS-REVIEW-DATE-GROUP REDEFINES COMM-CS-REVIEW-DATE.
             05 COMM-CS-REVIEW-DD            PIC 99.
             05 COMM-CS-REVIEW-MM            PIC 99.
             05 COMM-CS-REVIEW-YYYY          PIC 9999.
          03 COMM-SUCCESS                    PIC X.
          03 COMM-FAIL-CODE                  PIC X.
