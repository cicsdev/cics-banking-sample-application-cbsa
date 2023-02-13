      ******************************************************************
      *                                                                *
      *  Copyright IBM Corp. 2023                                      *
      *                                                                *
      *                                                                *
      ******************************************************************
       01 PROCISRT-COMMAREA.
           03 PROCISRT-FUNCTION PIC X.
           88 PROCISRT-DEBIT VALUE '1'.
           88 PROCISRT-CREDIT VALUE '2'.
           88 PROCISRT-XFR-LOCAL VALUE '3'.
           88 PROCISRT-DELETE-CUSTOMER VALUE '4'.
           88 PROCISRT-CREATE-CUSTOMER VALUE '5'.
           88 PROCISRT-DELETE-ACCOUNT VALUE '6'.
           88 PROCISRT-CREATE-ACCOUNT VALUE '7'.
           03 PROCISRT-PCB-POINTER POINTER.
           03 PROCISRT-DEBIT-STRUCT.
             05 PROCISRT-DEBIT-ACCNO PIC 9(8).
             05 PROCISRT-DEBIT-SORTCODE       PIC 9(6).
             05 PROCISRT-DEBIT-AMOUNT         PIC S9(10)V99.
             05 FILLER PIC X(100).
           03 PROCISRT-CREDIT-STRUCT
             REDEFINES PROCISRT-DEBIT-STRUCT.
             05 PROCISRT-CREDIT-ACCNO PIC 9(8).
             05 PROCISRT-CREDIT-SORTCODE       PIC 9(6).
             05 PROCISRT-CREDIT-AMOUNT         PIC S9(10)V99.
           03 PROCISRT-XFR-LOCAL-STRUCT
             REDEFINES PROCISRT-DEBIT-STRUCT.
             05 PROCISRT-XFR-L-ACCNO PIC 9(8).
             05 PROCISRT-XFR-L-SORTCODE       PIC 9(6).
             05 PROCISRT-XFR-L-AMOUNT         PIC S9(10)V99.
             05 PROCISRT-XFR-L-TARGET-ACCNO PIC 9(8).
           03 PROCISRT-DELETE-CUST-STRUCT
             REDEFINES PROCISRT-DEBIT-STRUCT.
             05 PROCISRT-DELETE-CUST-ACCNO PIC 9(8).
             05 PROCISRT-DELETE-CUST-SORTCODE       PIC 9(6).
             05 PROCISRT-DELETE-CUST-BALANCE        PIC S9(10)V99.
             05 PROCISRT-DELETE-CUST-DOB.
               07 PROCISRT-DELETE-CUST-DOB-YYYY     PIC 9999.
               07 PROCISRT-DELETE-CUST-DOB-MM       PIC 99.
               07 PROCISRT-DELETE-CUST-DOB-DD       PIC 99.
             05 PROCISRT-DELETE-CUST-NAME           PIC X(60).
             05 PROCISRT-DELETE-CUST-NUMBER         PIC 9(10).
           03 PROCISRT-CREATE-CUST-STRUCT
             REDEFINES PROCISRT-DEBIT-STRUCT.
             05 PROCISRT-CREATE-CUST-ACCNO PIC 9(8).
             05 PROCISRT-CREATE-CUST-SORTCODE       PIC 9(6).
             05 PROCISRT-CREATE-CUST-BALANCE        PIC S9(10)V99.
             05 PROCISRT-CREATE-CUST-DOB.
               07 PROCISRT-CREATE-CUST-DOB-YYYY     PIC 9999.
               07 PROCISRT-CREATE-CUST-DOB-MM       PIC 99.
               07 PROCISRT-CREATE-CUST-DOB-DD       PIC 99.
             05 PROCISRT-CREATE-CUST-NAME           PIC X(60).
             05 PROCISRT-CREATE-CUST-NUMBER         PIC 9(10).
           03 PROCISRT-DELACC-STRUCT
             REDEFINES PROCISRT-DEBIT-STRUCT.
             05 PROCISRT-DELACC-ACCNO PIC 9(8).
             05 PROCISRT-DELACC-SORTCODE       PIC 9(6).
             05 PROCISRT-DELACC-BALANCE        PIC S9(10)V99.
             05 PROCISRT-DELACC-L-STMNT.
               07 PROCISRT-DELACC-L-STMNT-YYYY     PIC 9999.
               07 PROCISRT-DELACC-L-STMNT-MM       PIC 99.
               07 PROCISRT-DELACC-L-STMNT-DD       PIC 99.
             05 PROCISRT-DELACC-N-STMNT.
               07 PROCISRT-DELACC-N-STMNT-YYYY     PIC 9999.
               07 PROCISRT-DELACC-N-STMNT-MM       PIC 99.
               07 PROCISRT-DELACC-N-STMNT-DD       PIC 99.
             05 PROCISRT-DELACC-TYPE           PIC X(8).
             05 PROCISRT-DELACC-CUSTNO         PIC 9(10).
           03 PROCISRT-CREACC-STRUCT
             REDEFINES PROCISRT-DEBIT-STRUCT.
             05 PROCISRT-CREACC-ACCNO PIC 9(8).
             05 PROCISRT-CREACC-SORTCODE       PIC 9(6).
             05 PROCISRT-CREACC-BALANCE        PIC S9(10)V99.
             05 PROCISRT-CREACC-L-STMNT.
               07 PROCISRT-CREACC-L-STMNT-YYYY     PIC 9999.
               07 PROCISRT-CREACC-L-STMNT-MM       PIC 99.
               07 PROCISRT-CREACC-L-STMNT-DD       PIC 99.
             05 PROCISRT-CREAET-ACCT-N-STMNT.
               07 PROCISRT-CREACC-N-STMNT-YYYY     PIC 9999.
               07 PROCISRT-CREACC-N-STMNT-MM       PIC 99.
               07 PROCISRT-CREACC-N-STMNT-DD       PIC 99.
             05 PROCISRT-CREACC-TYPE           PIC X(8).
             05 PROCISRT-CREACC-CUSTNO         PIC 9(10).
