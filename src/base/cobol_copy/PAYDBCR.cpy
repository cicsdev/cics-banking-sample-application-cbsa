      ******************************************************************
      *                                                                *
      *  Copyright IBM Corp. 2023                                      *
      *                                                                *
      *                                                                *
      ******************************************************************
          03 COMM-ACCNO               PIC X(8).
          03 COMM-AMT                 PIC S9(10)V99.
          03 COMM-SORTC               PIC 9(6).
          03 COMM-AV-BAL              PIC S9(10)V99.
          03 COMM-ACT-BAL             PIC S9(10)V99.
          03 COMM-ORIGIN.
               05 COMM-APPLID           PIC X(8).
               05 COMM-USERID           PIC X(8).
               05 COMM-FACILITY-NAME    PIC X(8).
               05 COMM-NETWRK-ID        PIC X(8).
               05 COMM-FACILTYPE        PIC S9(8) COMP.
               05 FILLER                PIC X(4).
          03 COMM-SUCCESS             PIC X.
          03 COMM-FAIL-CODE           PIC X.