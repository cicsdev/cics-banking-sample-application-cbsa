       CBL CICS('SP,EDF')
       CBL SQL
      ******************************************************************
      *                                                                *
      *  Copyright IBM Corp. 2023                                      *
      *                                                                *
      ******************************************************************

      ******************************************************************
      * This program takes an incoming customer number & retrieves
      * the associated accounts for it and stores these in an
      * array.
      *
      * Then it deletes the accounts one at a time and writes a
      * PROCTRAN delete account record out for each deleted account.
      *
      * When all accounts have been deleted, it deletes the customer
      * record (and writes out a customer delete record to PROCTRAN).
      *
      * If there is a failure at any time after we have started to
      * delete things then abend (or else the records will be out
      * of step). The only failure excluded from this, is where we
      * go to delete an account and it had already been deleted (if
      * this happens then we can just continue).
      *
      ******************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. DELCUS.
       AUTHOR. Jon Collett.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      * SOURCE-COMPUTER.   IBM-370 WITH DEBUGGING MODE.
       SOURCE-COMPUTER.  IBM-370.
       OBJECT-COMPUTER.  IBM-370.
       INPUT-OUTPUT SECTION.

       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.

       COPY SORTCODE.



       01 SYSIDERR-RETRY               PIC 999.
       01 FILE-RETRY                   PIC 999.
       01 WS-EXIT-RETRY-LOOP           PIC X VALUE ' '.


      * PROCTRAN DB2 copybook
           EXEC SQL
              INCLUDE PROCDB2
           END-EXEC.

      * PROCTRAN host variables for DB2
       01 HOST-PROCTRAN-ROW.
           03 HV-PROCTRAN-EYECATCHER   PIC X(4).
           03 HV-PROCTRAN-SORT-CODE    PIC X(6).
           03 HV-PROCTRAN-ACC-NUMBER   PIC X(8).
           03 HV-PROCTRAN-DATE         PIC X(10).
           03 HV-PROCTRAN-TIME         PIC X(6).
           03 HV-PROCTRAN-REF          PIC X(12).
           03 HV-PROCTRAN-TYPE         PIC X(3).
           03 HV-PROCTRAN-DESC         PIC X(40).
           03 HV-PROCTRAN-AMOUNT       PIC S9(10)V99 COMP-3.

      * Pull in the SQL COMMAREA
        EXEC SQL
          INCLUDE SQLCA
        END-EXEC.

       01 WS-CICS-WORK-AREA.
           05 WS-CICS-RESP             PIC S9(8) COMP.
           05 WS-CICS-RESP2            PIC S9(8) COMP.

       01 EXIT-BROWSE-LOOP             PIC X VALUE 'N'.

       01 OUTPUT-DATA.
           COPY ACCOUNT.

       01 OUTPUT-CUST-DATA.
           COPY CUSTOMER.

       01 PROCTRAN-AREA.
           COPY PROCTRAN.

       01 PROCTRAN-RIDFLD                 PIC S9(8) COMP.
       77 PROCTRAN-RETRY                  PIC 999.

       01 ACCOUNT-ACT-BAL-STORE           PIC S9(10)V99 VALUE 0.

       01 RETURNED-DATA.
           03 RETURNED-EYE-CATCHER        PIC X(4).
           03 RETURNED-CUST-NO            PIC 9(10).
           03 RETURNED-KEY.
              05 RETURNED-SORT-CODE       PIC 9(6).
              05 RETURNED-NUMBER          PIC 9(8).
           03 RETURNED-TYPE               PIC X(8).
           03 RETURNED-INTEREST-RATE      PIC 9(4)V99.
           03 RETURNED-OPENED             PIC 9(8).
           03 RETURNED-OVERDRAFT-LIMIT    PIC 9(8).
           03 RETURNED-LAST-STMT-DATE     PIC 9(8).
           03 RETURNED-NEXT-STMT-DATE     PIC 9(8).
           03 RETURNED-AVAILABLE-BALANCE  PIC S9(10)V99.
           03 RETURNED-ACTUAL-BALANCE     PIC S9(10)V99.

       01 ACCTCUST-DESIRED-KEY PIC 9(10) BINARY.

       01 DB2-DATE-REFORMAT.
          03 DB2-DATE-REF-YR           PIC 9(4).
          03 FILLER                    PIC X.
          03 DB2-DATE-REF-MNTH         PIC 99.
          03 FILLER                    PIC X.
          03 DB2-DATE-REF-DAY          PIC 99.

       01 DB2-EXIT-LOOP                PIC X.
       01 FETCH-DATA-CNT               PIC 9(4) COMP.
       01 WS-CUST-ALT-KEY-LEN          PIC S9(4) COMP VALUE +10.

       01 WS-EIBTASKN12                PIC 9(12) VALUE 0.

       01 WS-CNT                       PIC S9(4) COMP VALUE 0.

       01 CUSTOMER-KY.
          03 REQUIRED-SORT-CODE        PIC 9(6)  VALUE 0.
      *   03 REQUIRED-ACC-NUM          PIC 9(8)  VALUE 0.



       01 WS-ACC-KEY-LEN               PIC S9(4) COMP VALUE +14.
       01 WS-ACC-NUM                   PIC S9(4) COMP VALUE 0.

       01 WS-CUST-KEY-LEN              PIC S9(4) COMP VALUE +16.
       01 WS-CUST-NUM                  PIC S9(4) COMP VALUE 0.

       01 DESIRED-KEY-ACCTCUST.
           03 DESIRED-KEY-CUSTOMER-ACCTCUST   PIC 9(10).
           03 DESIRED-KEY-SORTCODE-ACCTCUST   PIC 9(6).

       01 DESIRED-KEY.
           03 DESIRED-KEY-SORTCODE     PIC 9(6).
           03 DESIRED-KEY-CUSTOMER     PIC 9(10).

       01 WS-U-TIME                    PIC S9(15) COMP-3.
       01 WS-ORIG-DATE                 PIC X(10).
       01 WS-ORIG-DATE-GRP REDEFINES WS-ORIG-DATE.
          03 WS-ORIG-DATE-DD              PIC 99.
          03 FILLER                       PIC X.
          03 WS-ORIG-DATE-MM              PIC 99.
          03 FILLER                       PIC X.
          03 WS-ORIG-DATE-YYYY            PIC 9999.

       01 WS-ORIG-DATE-GRP-X.
          03 WS-ORIG-DATE-DD-X          PIC XX.
          03 FILLER                     PIC X VALUE '.'.
          03 WS-ORIG-DATE-MM-X          PIC XX.
          03 FILLER                     PIC X VALUE '.'.
          03 WS-ORIG-DATE-YYYY-X        PIC X(4).

       01 REMIX-STMT-DATE              PIC 9(8).

       01 WS-APPLID                    PIC X(8).

       01 VAR-REMIX.
          03 REMIX-SCODE               PIC 9(6).
          03 REMIX-CHAR REDEFINES REMIX-SCODE.
             05 REMIX-SCODE-CHAR           PIC X(6).

      *
      * The CUSTOMER storage area
      *
       01 WS-STOREDC-CUSTOMER.
          03 WS-STOREDC-EYECATCHER           PIC X(4).
          03 WS-STOREDC-SORTCODE             PIC 9(6).
          03 WS-STOREDC-NUMBER               PIC 9(10).
          03 WS-STOREDC-NAME                 PIC X(60).
          03 WS-STOREDC-ADDRESS              PIC X(160).
          03 WS-STOREDC-DATE-OF-BIRTH        PIC X(10).
          03 WS-STOREDC-CREDIT-SCORE         PIC 9(3).
          03 WS-STOREDC-CS-REVIEW-DATE       PIC X(10).



       01 WS-NONE-LEFT                 PIC X VALUE 'N'.

       01 WS-EXIT-FETCH                PIC X VALUE 'N'.

       01 SQLCODE-DISPLAY              PIC S9(8) DISPLAY
                                         SIGN LEADING SEPARATE.

       01 DELACC-COMMAREA.
          03 DELACC-COMM-EYE           PIC X(4).
          03 DELACC-COMM-CUSTNO        PIC X(10).
          03 DELACC-COMM-SCODE         PIC X(6).
          03 DELACC-COMM-ACCNO         PIC 9(8).
          03 DELACC-COMM-ACC-TYPE      PIC X(8).
          03 DELACC-COMM-INT-RATE      PIC 9(4)V99.
          03 DELACC-COMM-OPENED        PIC 9(8).
          03 DELACC-COMM-OVERDRAFT     PIC 9(8).
          03 DELACC-COMM-LAST-STMT-DT  PIC 9(8).
          03 DELACC-COMM-NEXT-STMT-DT  PIC 9(8).
          03 DELACC-COMM-AVAIL-BAL     PIC S9(10)V99.
          03 DELACC-COMM-ACTUAL-BAL    PIC S9(10)V99.
          03 DELACC-COMM-SUCCESS       PIC X.
          03 DELACC-COMM-FAIL-CD       PIC X.
          03 DELACC-COMM-DEL-SUCCESS   PIC X.
          03 DELACC-COMM-DEL-FAIL-CD   PIC X.
          03 DELACC-COMM-APPLID        PIC X(8).
          03 DELACC-COMM-PCB1          POINTER.
          03 DELACC-COMM-PCB2          POINTER.


       01 WS-TOKEN                     PIC S9(8) BINARY.
       01 WS-INDEX                     PIC S9(8) BINARY.

       01 INQACCCU-PROGRAM         PIC X(8) VALUE 'INQACCCU'.
       01 INQACCCU-COMMAREA.
          COPY INQACCCU.

       01 STORM-DRAIN-CONDITION       PIC X(20).
       01 INQCUST-PROGRAM          PIC X(8) VALUE 'INQCUST '.
       01 INQCUST-COMMAREA.
          COPY INQCUST.

       01 WS-TIME-DATA.
           03 WS-TIME-NOW                  PIC 9(6).
           03 WS-TIME-NOW-GRP REDEFINES WS-TIME-NOW.
              05 WS-TIME-NOW-GRP-HH     PIC 99.
              05 WS-TIME-NOW-GRP-MM     PIC 99.
              05 WS-TIME-NOW-GRP-SS     PIC 99.

       01 WS-ABEND-PGM                  PIC X(8) VALUE 'ABNDPROC'.

       01 ABNDINFO-REC.
           COPY ABNDINFO.

       LINKAGE SECTION.
       01 DFHCOMMAREA.
           COPY DELCUS.


       PROCEDURE DIVISION USING DFHCOMMAREA.
       PREMIERE SECTION.
       A010.

           MOVE SORTCODE TO REQUIRED-SORT-CODE
                            REQUIRED-SORT-CODE OF CUSTOMER-KY
                            DESIRED-KEY-SORTCODE.

           MOVE COMM-CUSTNO OF DFHCOMMAREA
             TO DESIRED-KEY-CUSTOMER.

           INITIALIZE INQCUST-COMMAREA.
           MOVE COMM-CUSTNO OF DFHCOMMAREA TO
              INQCUST-CUSTNO.

           EXEC CICS LINK PROGRAM(INQCUST-PROGRAM)
                     COMMAREA(INQCUST-COMMAREA)
           END-EXEC.

           IF INQCUST-INQ-SUCCESS = 'N'
             MOVE 'N' TO COMM-DEL-SUCCESS
             MOVE INQCUST-INQ-FAIL-CD TO COMM-DEL-FAIL-CD
             EXEC CICS RETURN
             END-EXEC
           END-IF.

           PERFORM GET-ACCOUNTS
      *
      *          If there are related accounts found then delete
      *          them.
      *
           IF NUMBER-OF-ACCOUNTS > 0
             PERFORM DELETE-ACCOUNTS
           END-IF

      *
      *    Having deleted the accounts and written the
      *    details to the PROCTRAN datastore, if we haven't abended
      *    then we must go on to delete the CUSTOMER record
      *

           PERFORM DEL-CUST-VSAM


           MOVE 'Y' TO COMM-DEL-SUCCESS.
           MOVE ' ' TO COMM-DEL-FAIL-CD.

           PERFORM GET-ME-OUT-OF-HERE.

       A999.
           EXIT.


       DELETE-ACCOUNTS SECTION.
       DA010.

      *
      *    Go through the entries (accounts) in the array,
      *    and for each one link to DELACC to delete that
      *    account.
      *
           PERFORM VARYING WS-INDEX FROM 1 BY 1
           UNTIL WS-INDEX > NUMBER-OF-ACCOUNTS
              INITIALIZE DELACC-COMMAREA
              MOVE WS-APPLID TO DELACC-COMM-APPLID
              MOVE COMM-ACCNO(WS-INDEX) TO DELACC-COMM-ACCNO

              EXEC CICS LINK PROGRAM('DELACC  ')
                       COMMAREA(DELACC-COMMAREA)
              END-EXEC

           END-PERFORM.

       DA999.
           EXIT.


       GET-ACCOUNTS SECTION.
       GAC010.
      *
      *    Link to INQACCCU to get all of the accounts for a
      *    given customer number.
      *
           MOVE COMM-CUSTNO OF DFHCOMMAREA
              TO CUSTOMER-NUMBER OF INQACCCU-COMMAREA.
           MOVE 20 TO NUMBER-OF-ACCOUNTS IN INQACCCU-COMMAREA.
           SET COMM-PCB-POINTER OF INQACCCU-COMMAREA
              TO DELACC-COMM-PCB1

           EXEC CICS LINK PROGRAM('INQACCCU')
                     COMMAREA(INQACCCU-COMMAREA)
                     SYNCONRETURN
           END-EXEC.

       GAC999.
           EXIT.


       DEL-CUST-VSAM SECTION.
       DCV010.

      *
      *    Read the CUSTOMER record and store the details
      *    for inclusion on PROCTRAN later, then delete the CUSTOMER
      *    record and write to PROCTRAN.
      *
           INITIALIZE OUTPUT-CUST-DATA.

           EXEC CICS READ FILE('CUSTOMER')
                RIDFLD(DESIRED-KEY)
                INTO(OUTPUT-CUST-DATA)
                UPDATE
                TOKEN(WS-TOKEN)
                RESP(WS-CICS-RESP)
                RESP2(WS-CICS-RESP2)
           END-EXEC.

           IF WS-CICS-RESP = DFHRESP(SYSIDERR)
              PERFORM VARYING SYSIDERR-RETRY FROM 1 BY 1
              UNTIL SYSIDERR-RETRY > 100
              OR WS-CICS-RESP = DFHRESP(NORMAL)
              OR WS-CICS-RESP IS NOT EQUAL TO DFHRESP(SYSIDERR)

                 EXEC CICS DELAY FOR SECONDS(3)
                 END-EXEC

                 EXEC CICS READ FILE('CUSTOMER')
                    RIDFLD(DESIRED-KEY)
                    INTO(OUTPUT-CUST-DATA)
                    UPDATE
                    TOKEN(WS-TOKEN)
                    RESP(WS-CICS-RESP)
                    RESP2(WS-CICS-RESP2)
                 END-EXEC

              END-PERFORM

           END-IF

           IF WS-CICS-RESP = DFHRESP(NOTFND)
      *
      *       Someone else has deleted it
      *
              GO TO DCV999
           END-IF

           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)
      *
      *       Preserve the RESP and RESP2, then set up the
      *       standard ABEND info before getting the applid,
      *       date/time etc. and linking to the Abend Handler
      *       program.
      *
              INITIALIZE ABNDINFO-REC
              MOVE EIBRESP    TO ABND-RESPCODE
              MOVE EIBRESP2   TO ABND-RESP2CODE
      *
      *       Get supplemental information
      *
              EXEC CICS ASSIGN APPLID(ABND-APPLID)
              END-EXEC

              MOVE EIBTASKN   TO ABND-TASKNO-KEY
              MOVE EIBTRNID   TO ABND-TRANID

              PERFORM POPULATE-TIME-DATE

              MOVE WS-ORIG-DATE TO ABND-DATE
              STRING WS-TIME-NOW-GRP-HH DELIMITED BY SIZE,
                    ':' DELIMITED BY SIZE,
                     WS-TIME-NOW-GRP-MM DELIMITED BY SIZE,
                     ':' DELIMITED BY SIZE,
                     WS-TIME-NOW-GRP-MM DELIMITED BY SIZE
                     INTO ABND-TIME
              END-STRING

              MOVE WS-U-TIME   TO ABND-UTIME-KEY
              MOVE 'WPV6'      TO ABND-CODE

              EXEC CICS ASSIGN PROGRAM(ABND-PROGRAM)
              END-EXEC

              MOVE ZEROS    TO ABND-SQLCODE

              STRING 'DCV010 - Unable to READ CUSTOMER VSAM rec '
                    DELIMITED BY SIZE,
                    'for key:' DESIRED-KEY DELIMITED BY SIZE,
                    ' EIBRESP=' DELIMITED BY SIZE,
                    ABND-RESPCODE DELIMITED BY SIZE,
                    ' RESP2=' DELIMITED BY SIZE,
                    ABND-RESP2CODE DELIMITED BY SIZE
                    INTO ABND-FREEFORM
              END-STRING

              EXEC CICS LINK PROGRAM(WS-ABEND-PGM)
                        COMMAREA(ABNDINFO-REC)
              END-EXEC

              DISPLAY 'In DELCUS (DCV010) '
              'UNABLE TO READ CUSTOMER VSAM REC'
              ' RESP CODE=' WS-CICS-RESP, ' RESP2=' WS-CICS-RESP2
              'FOR KEY=' DESIRED-KEY

              EXEC CICS ABEND
                 ABCODE ('WPV6')
              END-EXEC

           END-IF.

           MOVE CUSTOMER-EYECATCHER TO WS-STOREDC-EYECATCHER
                                        COMM-EYE IN DFHCOMMAREA.
           MOVE CUSTOMER-SORTCODE   TO WS-STOREDC-SORTCODE
                                       COMM-SCODE IN DFHCOMMAREA.
           MOVE CUSTOMER-NUMBER OF CUSTOMER-RECORD
              TO WS-STOREDC-NUMBER COMM-CUSTNO IN DFHCOMMAREA.
           MOVE CUSTOMER-NAME       TO WS-STOREDC-NAME
                                       COMM-NAME IN DFHCOMMAREA.
           MOVE CUSTOMER-ADDRESS    TO WS-STOREDC-ADDRESS
                                       COMM-ADDR IN DFHCOMMAREA.
           MOVE CUSTOMER-DATE-OF-BIRTH(1:2)
              TO WS-STOREDC-DATE-OF-BIRTH(1:2)
                 COMM-BIRTH-DAY IN DFHCOMMAREA.
           MOVE '/'                 TO WS-STOREDC-DATE-OF-BIRTH(3:1).
           MOVE CUSTOMER-DATE-OF-BIRTH(3:2)
              TO WS-STOREDC-DATE-OF-BIRTH(4:2)
                 COMM-BIRTH-MONTH IN DFHCOMMAREA.
           MOVE '/'                 TO WS-STOREDC-DATE-OF-BIRTH(6:1).

           MOVE CUSTOMER-DATE-OF-BIRTH(5:4)
              TO WS-STOREDC-DATE-OF-BIRTH(7:4)
                 COMM-BIRTH-YEAR IN DFHCOMMAREA.

           MOVE CUSTOMER-CREDIT-SCORE TO WS-STOREDC-CREDIT-SCORE
                                         COMM-CREDIT-SCORE.
           MOVE CUSTOMER-CS-REVIEW-DATE(1:2)
             TO WS-STOREDC-CS-REVIEW-DATE(1:2)
                COMM-CS-REVIEW-DD IN DFHCOMMAREA.
           MOVE '/'                 TO WS-STOREDC-CS-REVIEW-DATE(3:1).
           MOVE CUSTOMER-CS-REVIEW-DATE(3:2)
             TO WS-STOREDC-CS-REVIEW-DATE(4:2)
                COMM-CS-REVIEW-MM IN DFHCOMMAREA.
           MOVE '/'                 TO WS-STOREDC-CS-REVIEW-DATE(6:1).
           MOVE CUSTOMER-CS-REVIEW-DATE(5:4)
             TO WS-STOREDC-CS-REVIEW-DATE(7:4)
                COMM-CS-REVIEW-YYYY IN DFHCOMMAREA.

           EXEC CICS
              DELETE FILE ('CUSTOMER')
              TOKEN(WS-TOKEN)
              RESP(WS-CICS-RESP)
              RESP2(WS-CICS-RESP2)
           END-EXEC.

           IF WS-CICS-RESP = DFHRESP(SYSIDERR)

              PERFORM VARYING SYSIDERR-RETRY FROM 1 BY 1
              UNTIL SYSIDERR-RETRY > 100
              OR WS-CICS-RESP = DFHRESP(NORMAL)
              OR WS-CICS-RESP IS NOT EQUAL TO DFHRESP(SYSIDERR)

                 EXEC CICS DELAY FOR SECONDS(3)
                 END-EXEC

                 EXEC CICS DELETE FILE ('CUSTOMER')
                    TOKEN(WS-TOKEN)
                    RESP(WS-CICS-RESP)
                    RESP2(WS-CICS-RESP2)
                 END-EXEC
              END-PERFORM

           END-IF

           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)
      *
      *       Preserve the RESP and RESP2, then set up the
      *       standard ABEND info before getting the applid,
      *       date/time etc. and linking to the Abend Handler
      *       program.
      *
              INITIALIZE ABNDINFO-REC
              MOVE EIBRESP    TO ABND-RESPCODE
              MOVE EIBRESP2   TO ABND-RESP2CODE
      *
      *       Get supplemental information
      *
              EXEC CICS ASSIGN APPLID(ABND-APPLID)
              END-EXEC

              MOVE EIBTASKN   TO ABND-TASKNO-KEY
              MOVE EIBTRNID   TO ABND-TRANID

              PERFORM POPULATE-TIME-DATE

              MOVE WS-ORIG-DATE TO ABND-DATE
              STRING WS-TIME-NOW-GRP-HH DELIMITED BY SIZE,
                    ':' DELIMITED BY SIZE,
                     WS-TIME-NOW-GRP-MM DELIMITED BY SIZE,
                     ':' DELIMITED BY SIZE,
                     WS-TIME-NOW-GRP-MM DELIMITED BY SIZE
                     INTO ABND-TIME
              END-STRING

              MOVE WS-U-TIME   TO ABND-UTIME-KEY
              MOVE 'WPV7'      TO ABND-CODE

              EXEC CICS ASSIGN PROGRAM(ABND-PROGRAM)
              END-EXEC

              MOVE ZEROS    TO ABND-SQLCODE

              STRING 'DCV010(2) - Unbale to DELETE CUSTOMER VSAM rec '
                    DELIMITED BY SIZE,
                    'for key:' DESIRED-KEY DELIMITED BY SIZE,
                    ' EIBRESP=' DELIMITED BY SIZE,
                    ABND-RESPCODE DELIMITED BY SIZE,
                    ' RESP2=' DELIMITED BY SIZE,
                    ABND-RESP2CODE DELIMITED BY SIZE
                    INTO ABND-FREEFORM
              END-STRING

              EXEC CICS LINK PROGRAM(WS-ABEND-PGM)
                        COMMAREA(ABNDINFO-REC)
              END-EXEC

              DISPLAY 'In DELCUS (DCV010) '
              'UNABLE TO DELETE CUSTOMER VSAM REC'
              ' RESP CODE=' WS-CICS-RESP, ' RESP2=' WS-CICS-RESP2
              'FOR KEY=' DESIRED-KEY

              EXEC CICS ABEND
                 ABCODE ('WPV7')
              END-EXEC

           END-IF.

           PERFORM WRITE-PROCTRAN-CUST.

       DCV999.
           EXIT.


       WRITE-PROCTRAN-CUST SECTION.
       WPC010.

      *
      *    Record the CUSTOMER deletion on PROCTRAN
      *
              PERFORM WRITE-PROCTRAN-CUST-DB2.
       WPC999.
           EXIT.


       WRITE-PROCTRAN-CUST-DB2 SECTION.
       WPCD010.

           INITIALIZE HOST-PROCTRAN-ROW.
           INITIALIZE WS-EIBTASKN12.

           MOVE 'PRTR' TO HV-PROCTRAN-EYECATCHER.
           MOVE WS-STOREDC-SORTCODE
              TO HV-PROCTRAN-SORT-CODE.
           MOVE ZEROS TO HV-PROCTRAN-ACC-NUMBER.
           MOVE EIBTASKN TO WS-EIBTASKN12.
           MOVE WS-EIBTASKN12 TO HV-PROCTRAN-REF.

      *
      * Populate the time and date
      *
           EXEC CICS ASKTIME
              ABSTIME(WS-U-TIME)
           END-EXEC.

           EXEC CICS FORMATTIME
                     ABSTIME(WS-U-TIME)
                     DDMMYYYY(WS-ORIG-DATE)
                     TIME(HV-PROCTRAN-TIME)
                     DATESEP('.')
           END-EXEC.

           MOVE WS-ORIG-DATE TO WS-ORIG-DATE-GRP-X.
           MOVE WS-ORIG-DATE-GRP-X TO HV-PROCTRAN-DATE.

           MOVE WS-STOREDC-SORTCODE      TO HV-PROCTRAN-DESC(1:6).
           MOVE WS-STOREDC-NUMBER        TO HV-PROCTRAN-DESC(7:10).
           MOVE WS-STOREDC-NAME          TO HV-PROCTRAN-DESC(17:14).
           MOVE WS-STOREDC-DATE-OF-BIRTH TO HV-PROCTRAN-DESC(31:10).

           MOVE 'ODC'         TO HV-PROCTRAN-TYPE.
           MOVE ZEROS         TO HV-PROCTRAN-AMOUNT.


           EXEC SQL
              INSERT INTO PROCTRAN
                     (
                      PROCTRAN_EYECATCHER,
                      PROCTRAN_SORTCODE,
                      PROCTRAN_NUMBER,
                      PROCTRAN_DATE,
                      PROCTRAN_TIME,
                      PROCTRAN_REF,
                      PROCTRAN_TYPE,
                      PROCTRAN_DESC,
                      PROCTRAN_AMOUNT
                     )
              VALUES
                     (
                      :HV-PROCTRAN-EYECATCHER,
                      :HV-PROCTRAN-SORT-CODE,
                      :HV-PROCTRAN-ACC-NUMBER,
                      :HV-PROCTRAN-DATE,
                      :HV-PROCTRAN-TIME,
                      :HV-PROCTRAN-REF,
                      :HV-PROCTRAN-TYPE,
                      :HV-PROCTRAN-DESC,
                      :HV-PROCTRAN-AMOUNT
                     )
           END-EXEC.

      *
      * Check the SQLCODE
      *
           IF SQLCODE NOT = 0
              MOVE SQLCODE TO SQLCODE-DISPLAY
      *
      *       Preserve the RESP and RESP2, then set up the
      *       standard ABEND info before getting the applid,
      *       date/time etc. and linking to the Abend Handler
      *       program.
      *
              INITIALIZE ABNDINFO-REC
              MOVE EIBRESP    TO ABND-RESPCODE
              MOVE EIBRESP2   TO ABND-RESP2CODE
      *
      *       Get supplemental information
      *
              EXEC CICS ASSIGN APPLID(ABND-APPLID)
              END-EXEC

              MOVE EIBTASKN   TO ABND-TASKNO-KEY
              MOVE EIBTRNID   TO ABND-TRANID

              PERFORM POPULATE-TIME-DATE

              MOVE WS-ORIG-DATE TO ABND-DATE
              STRING WS-TIME-NOW-GRP-HH DELIMITED BY SIZE,
                    ':' DELIMITED BY SIZE,
                     WS-TIME-NOW-GRP-MM DELIMITED BY SIZE,
                     ':' DELIMITED BY SIZE,
                     WS-TIME-NOW-GRP-MM DELIMITED BY SIZE
                     INTO ABND-TIME
              END-STRING

              MOVE WS-U-TIME   TO ABND-UTIME-KEY
              MOVE 'HWPT'      TO ABND-CODE

              EXEC CICS ASSIGN PROGRAM(ABND-PROGRAM)
              END-EXEC

              MOVE SQLCODE-DISPLAY   TO ABND-SQLCODE

              STRING 'WPCD010 - Unable to WRITE to PROCTRAN DB2 '
                    DELIMITED BY SIZE,
                    'datastore with the following data:'
                    DELIMITED BY SIZE,
                    HOST-PROCTRAN-ROW DELIMITED BY SIZE,
                    ' EIBRESP=' DELIMITED BY SIZE,
                    ABND-RESPCODE DELIMITED BY SIZE,
                    ' RESP2=' DELIMITED BY SIZE,
                    ABND-RESP2CODE DELIMITED BY SIZE
                    INTO ABND-FREEFORM
              END-STRING

              EXEC CICS LINK PROGRAM(WS-ABEND-PGM)
                        COMMAREA(ABNDINFO-REC)
              END-EXEC

              DISPLAY 'In DELCUS(WPCD010) '
              'UNABLE TO WRITE TO PROCTRAN DB2 DATASTORE'
              ' SQLCODE=' SQLCODE-DISPLAY
              'WITH THE FOLLOWING DATA:' HOST-PROCTRAN-ROW

              EXEC CICS ABEND
                 ABCODE('HWPT')
                 NODUMP
              END-EXEC

           END-IF.

       WPCD999.
           EXIT.


       GET-ME-OUT-OF-HERE SECTION.
       GMOFH010.

           GOBACK.

       GMOFH999.
           EXIT.


       POPULATE-TIME-DATE SECTION.
       PTD010.

           EXEC CICS ASKTIME
              ABSTIME(WS-U-TIME)
           END-EXEC.

           EXEC CICS FORMATTIME
                     ABSTIME(WS-U-TIME)
                     DDMMYYYY(WS-ORIG-DATE)
                     TIME(WS-TIME-NOW)
                     DATESEP
           END-EXEC.

       PTD999.
           EXIT.
