      ******************************************************************
      *                                                                *
      *  Copyright IBM Corp. 2022                                      *
      *                                                                *
      ******************************************************************
       CBL CICS('SP,EDF,DLI')
       CBL SQL
      ******************************************************************
      * This program takes an incoming account number it then accesses
      * the account datastore and retrieves the associated account
      * record matching on the customer number & the account_type
      * and then deletes it.
      *
      * If no matching customer_number and account_type combination can
      * found it will return an error flag and the visual1sation layer
      * should process this accordingly.
      *
      * Should there be any issues, the program will abend.
      *
      * It is assumed that the incoming customer number will be valid.
      *****************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. DELACC.
       AUTHOR. Jon Collett.


       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      *SOURCE-COMPUTER.   IBM-370 WITH DEBUGGING MODE.
       SOURCE-COMPUTER.  IBM-370.
       OBJECT-COMPUTER.  IBM-370.

       INPUT-OUTPUT SECTION.

       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
      * Copyright statement as a literal to go into the load module
       77 FILLER PIC X(24) VALUE 'Copyright IBM Corp. 2022'. 


       COPY SORTCODE.



       01 SYSIDERR-RETRY               PIC 999.
       01 FILE-RETRY                   PIC 999.
       01 WS-EXIT-RETRY-LOOP           PIC X VALUE ' '.

      * Get the ACCOUNT DB2 copybook
           EXEC SQL
              INCLUDE ACCDB2
           END-EXEC.

      * ACCOUNT Host variables for DB2
       01 HOST-ACCOUNT-ROW.
           03 HV-ACCOUNT-EYECATCHER     PIC X(4).
           03 HV-ACCOUNT-CUST-NO        PIC X(10).
           03 HV-ACCOUNT-SORTCODE       PIC X(6).
           03 HV-ACCOUNT-ACC-NO         PIC X(8).
           03 HV-ACCOUNT-ACC-TYPE       PIC X(8).
           03 HV-ACCOUNT-INT-RATE       PIC S9(4)V99 COMP-3.
           03 HV-ACCOUNT-OPENED         PIC X(10).
           03 HV-ACCOUNT-OVERDRAFT-LIM  PIC S9(9) COMP.
           03 HV-ACCOUNT-LAST-STMT      PIC X(10).
           03 HV-ACCOUNT-NEXT-STMT      PIC X(10).
           03 HV-ACCOUNT-AVAIL-BAL      PIC S9(10)V99 COMP-3.
           03 HV-ACCOUNT-ACTUAL-BAL     PIC S9(10)V99 COMP-3.

      * PROCTRAN DB2 copybook
           EXEC SQL
              INCLUDE PROCDB2
           END-EXEC.

      * PROCTRAN host variables for DB2
       01 HOST-PROCTRAN-ROW.
           03 HV-PROCTRAN-EYECATCHER    PIC X(4).
           03 HV-PROCTRAN-SORT-CODE     PIC X(6).
           03 HV-PROCTRAN-ACC-NUMBER    PIC X(8).
           03 HV-PROCTRAN-DATE          PIC X(10).
           03 HV-PROCTRAN-TIME          PIC X(6).
           03 HV-PROCTRAN-REF           PIC X(12).
           03 HV-PROCTRAN-TYPE          PIC X(3).
           03 HV-PROCTRAN-DESC          PIC X(40).
           03 HV-PROCTRAN-AMOUNT        PIC S9(10)V99 COMP-3.

      * Pull in the SQL COMMAREA
           EXEC SQL
              INCLUDE SQLCA
           END-EXEC.

       01 SQLCODE-DISPLAY              PIC S9(8) DISPLAY
           SIGN LEADING SEPARATE.

       01 WS-CICS-WORK-AREA.
           05 WS-CICS-RESP              PIC S9(8) COMP.
           05 WS-CICS-RESP2             PIC S9(8) COMP.
           05 WS-EIBRESP-DISPLAY        PIC S9(8) DISPLAY
                                   SIGN LEADING SEPARATE.

       01 WS-APPLID                    PIC X(8).

       01 EXIT-BROWSE-LOOP             PIC X VALUE 'N'.

       01 OUTPUT-DATA.
           COPY ACCOUNT.

       01 PROCTRAN-AREA.
          COPY PROCTRAN.

       01 PROCTRAN-RIDFLD              PIC S9(8) COMP.
       77 PROCTRAN-RETRY               PIC 999.

       01 ACCOUNT-ACT-BAL-STORE        PIC S9(10)V99 VALUE 0.

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

       01 DESIRED-KEY                  PIC 9(10) BINARY.


       01 DB2-DATE-REFORMAT.
          03 DB2-DATE-REF-YR           PIC 9(4).
          03 FILLER                    PIC X.
          03 DB2-DATE-REF-MNTH         PIC 99.
          03 FILLER                    PIC X.
          03 DB2-DATE-REF-DAY          PIC 99.

       01 DATA-STORE-TYPE              PIC X.
          88 DATASTORE-TYPE-DB2           VALUE '2'.
          88 DATASTORE-TYPE-VSAM          VALUE 'V'.

       01 DB2-EXIT-LOOP                PIC X.
       01 FETCH-DATA-CNT               PIC 9(4) COMP.
       01 WS-CUST-ALT-KEY-LEN          PIC S9(4) COMP VALUE +10.

       01 WS-EIBTASKN12                PIC 9(12) VALUE 0.

       01 ACCOUNT-KEY-RID.
          03 REQUIRED-SORT-CODE        PIC 9(6)  VALUE 0.
          03 REQUIRED-ACC-NUM          PIC 9(8)  VALUE 0.

       01 MY-TCB                       PIC S9(8) BINARY.

       01 MY-TCB-STRING                PIC X(8).

       01 WS-ACC-KEY-LEN               PIC S9(4) COMP VALUE +14.
       01 WS-ACC-NUM                   PIC S9(4) COMP VALUE 0.

       01 WS-U-TIME                    PIC S9(15) COMP-3.
       01 WS-ORIG-DATE                 PIC X(10).
       01 WS-ORIG-DATE-GRP REDEFINES WS-ORIG-DATE.
          03 WS-ORIG-DATE-DD              PIC 99.
          03 FILLER                       PIC X.
          03 WS-ORIG-DATE-MM              PIC 99.
          03 FILLER                       PIC X.
          03 WS-ORIG-DATE-YYYY            PIC 9999.

       01 WS-ORIG-DATE-GRP-X.
          03 WS-ORIG-DATE-DD-X         PIC XX.
          03 FILLER                    PIC X VALUE '.'.
          03 WS-ORIG-DATE-MM-X         PIC XX.
          03 FILLER                    PIC X VALUE '.'.
          03 WS-ORIG-DATE-YYYY-X       PIC X(4).


       01 WS-TOKEN                     PIC S9(8) BINARY.


       01 STORM-DRAIN-CONDITION        PIC X(20).

       01 ACCOUNT-CONTROL.
           COPY ACCTCTRL.

       01 WS-TIME-DATA.
           03 WS-TIME-NOW              PIC 9(6).
           03 WS-TIME-NOW-GRP REDEFINES WS-TIME-NOW.
              05 WS-TIME-NOW-GRP-HH       PIC 99.
              05 WS-TIME-NOW-GRP-MM       PIC 99.
              05 WS-TIME-NOW-GRP-SS       PIC 99.

       01 WS-ABEND-PGM                    PIC X(8) VALUE 'ABNDPROC'.

       01 ABNDINFO-REC.
           COPY ABNDINFO.


       LINKAGE SECTION.
       COPY DELACC REPLACING DELACC-COMMAREA BY DFHCOMMAREA.


       PROCEDURE DIVISION USING DFHCOMMAREA.
       PREMIERE SECTION.
       A010.

           MOVE SORTCODE TO REQUIRED-SORT-CODE OF ACCOUNT-KEY-RID.

      *
      *          Get the account record
      *
           PERFORM READ-ACCOUNT-DB2.

      *
      *          If a matching account record was successfully
      *          retrieved then delete it
      *
           IF DELACC-DEL-SUCCESS = 'Y'

             PERFORM DEL-ACCOUNT-DB2
             IF DELACC-DEL-SUCCESS = 'Y'
               PERFORM WRITE-PROCTRAN
             END-IF
           END-IF


           PERFORM GET-ME-OUT-OF-HERE.

       A999.
           EXIT.


      *
      * Read the DB2 datastore and process the data
      *
       READ-ACCOUNT-DB2 SECTION.
       RAD010.

      *
      *    Take the Account number from the comm area and retrieve
      *    the account record from the datastore.
      *
           MOVE DELACC-ACCNO
              TO HV-ACCOUNT-ACC-NO.

           MOVE SORTCODE TO HV-ACCOUNT-SORTCODE.

           EXEC SQL
              SELECT ACCOUNT_EYECATCHER,
                     ACCOUNT_CUSTOMER_NUMBER,
                     ACCOUNT_SORTCODE,
                     ACCOUNT_NUMBER,
                     ACCOUNT_TYPE,
                     ACCOUNT_INTEREST_RATE,
                     ACCOUNT_OPENED,
                     ACCOUNT_OVERDRAFT_LIMIT,
                     ACCOUNT_LAST_STATEMENT,
                     ACCOUNT_NEXT_STATEMENT,
                     ACCOUNT_AVAILABLE_BALANCE,
                     ACCOUNT_ACTUAL_BALANCE
              INTO  :HV-ACCOUNT-EYECATCHER,
                    :HV-ACCOUNT-CUST-NO,
                    :HV-ACCOUNT-SORTCODE,
                    :HV-ACCOUNT-ACC-NO,
                    :HV-ACCOUNT-ACC-TYPE,
                    :HV-ACCOUNT-INT-RATE,
                    :HV-ACCOUNT-OPENED,
                    :HV-ACCOUNT-OVERDRAFT-LIM,
                    :HV-ACCOUNT-LAST-STMT,
                    :HV-ACCOUNT-NEXT-STMT,
                    :HV-ACCOUNT-AVAIL-BAL,
                    :HV-ACCOUNT-ACTUAL-BAL
              FROM ACCOUNT
              WHERE   ACCOUNT_NUMBER = :HV-ACCOUNT-ACC-NO AND
                    ACCOUNT_SORTCODE = :HV-ACCOUNT-SORTCODE
           END-EXEC.

      *
      *    If the SQLCODE returned anything other than OK or row not
      *    found then we have a problem (so abend it)
      *
           IF SQLCODE NOT = 0 AND SQLCODE NOT = +100
              MOVE SQLCODE TO SQLCODE-DISPLAY
      *
      *       Preserve the RESP and RESP2, then set up the
      *       standard ABEND info before getting the applid,
      *       date/time etc. and linking to the Abend Handler
      *       program.
      *
              INITIALIZE ABNDINFO-REC
              MOVE ZERO       TO ABND-RESPCODE
              MOVE ZERO       TO ABND-RESP2CODE
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


              MOVE WS-U-TIME  TO ABND-UTIME-KEY
              MOVE 'HRAC'     TO ABND-CODE

              EXEC CICS ASSIGN PROGRAM(ABND-PROGRAM)
              END-EXEC

              MOVE SQLCODE-DISPLAY      TO ABND-SQLCODE

              STRING 'RAD010 - Issue with ACCOUNT row select, for '
                     DELIMITED BY SIZE,
                     'Account ' HV-ACCOUNT-ACC-NO
                     DELIMITED BY SIZE,
                     'and SORTCODE ' HV-ACCOUNT-SORTCODE
                     DELIMITED BY SIZE,
                     ' EIBRESP=' DELIMITED BY SIZE,
                     ABND-RESPCODE DELIMITED BY SIZE,
                     ' RESP2=' DELIMITED BY SIZE
                     INTO ABND-FREEFORM
              END-STRING


              DISPLAY 'Issue with ACCOUNT row select. SQLCODE='
                 SQLCODE-DISPLAY ' . For Account ' HV-ACCOUNT-ACC-NO
                 ' and Sortcode' HV-ACCOUNT-SORTCODE

              EXEC CICS ABEND ABCODE('HRAC')
                   NODUMP
              END-EXEC

           END-IF.

      *
      *    If the account is not found, then we need to finish
      *    processing and return an error (via the COMM AREA) to the
      *    calling program
      *
           IF SQLCODE = +100
              INITIALIZE OUTPUT-DATA
              MOVE SORTCODE TO ACCOUNT-SORT-CODE OF OUTPUT-DATA
              MOVE DELACC-ACCNO TO ACCOUNT-NUMBER OF OUTPUT-DATA
              MOVE 'N' TO DELACC-DEL-SUCCESS
              MOVE '1' TO DELACC-DEL-FAIL-CD
              GO TO RAD999
           END-IF.

      *
      *    If the SQL return code was OK, then set the flags
      *    accordingly.
      *
           IF SQLCODE = 0
              MOVE ' ' TO DELACC-SUCCESS
              MOVE 'Y' TO DELACC-DEL-SUCCESS
              MOVE ' ' TO DELACC-DEL-FAIL-CD
           END-IF.

      *
      *    Return the ACCOUNT data to the COMMAREA
      *
           MOVE HV-ACCOUNT-EYECATCHER TO
              ACCOUNT-EYE-CATCHER OF OUTPUT-DATA.
           MOVE HV-ACCOUNT-CUST-NO TO
              ACCOUNT-CUST-NO OF OUTPUT-DATA.
           MOVE HV-ACCOUNT-SORTCODE TO
              ACCOUNT-SORT-CODE OF OUTPUT-DATA.
           MOVE HV-ACCOUNT-ACC-NO TO
              ACCOUNT-NUMBER OF OUTPUT-DATA.
           MOVE HV-ACCOUNT-ACC-TYPE TO
              ACCOUNT-TYPE OF OUTPUT-DATA.
           MOVE HV-ACCOUNT-INT-RATE TO
              ACCOUNT-INTEREST-RATE OF OUTPUT-DATA.
           MOVE HV-ACCOUNT-OPENED TO DB2-DATE-REFORMAT.
           MOVE DB2-DATE-REF-DAY TO
              ACCOUNT-OPENED-DAY OF OUTPUT-DATA.
           MOVE DB2-DATE-REF-MNTH TO
              ACCOUNT-OPENED-MONTH OF OUTPUT-DATA.
           MOVE DB2-DATE-REF-YR TO
              ACCOUNT-OPENED-YEAR OF OUTPUT-DATA.
           MOVE HV-ACCOUNT-OVERDRAFT-LIM TO
              ACCOUNT-OVERDRAFT-LIMIT OF OUTPUT-DATA.
           MOVE HV-ACCOUNT-LAST-STMT TO DB2-DATE-REFORMAT.
           MOVE DB2-DATE-REF-DAY TO
              ACCOUNT-LAST-STMT-DAY OF OUTPUT-DATA.
           MOVE DB2-DATE-REF-MNTH TO
              ACCOUNT-LAST-STMT-MONTH OF OUTPUT-DATA.
           MOVE DB2-DATE-REF-YR TO
              ACCOUNT-LAST-STMT-YEAR OF OUTPUT-DATA.
           MOVE HV-ACCOUNT-NEXT-STMT TO DB2-DATE-REFORMAT
           MOVE DB2-DATE-REF-DAY TO
              ACCOUNT-NEXT-STMT-DAY OF OUTPUT-DATA.
           MOVE DB2-DATE-REF-MNTH TO
              ACCOUNT-NEXT-STMT-MONTH OF OUTPUT-DATA.
           MOVE DB2-DATE-REF-YR TO
              ACCOUNT-NEXT-STMT-YEAR OF OUTPUT-DATA.
           MOVE HV-ACCOUNT-AVAIL-BAL TO
              ACCOUNT-AVAILABLE-BALANCE OF OUTPUT-DATA.
           MOVE HV-ACCOUNT-ACTUAL-BAL TO
              ACCOUNT-ACTUAL-BALANCE OF OUTPUT-DATA.
           MOVE HV-ACCOUNT-ACTUAL-BAL TO ACCOUNT-ACT-BAL-STORE.

           MOVE ACCOUNT-EYE-CATCHER       TO DELACC-EYE.
           MOVE ACCOUNT-CUST-NO           TO DELACC-CUSTNO.
           MOVE ACCOUNT-SORT-CODE         TO DELACC-SCODE.
           MOVE ACCOUNT-NUMBER            TO DELACC-ACCNO.
           MOVE ACCOUNT-TYPE              TO DELACC-ACC-TYPE.
           MOVE ACCOUNT-INTEREST-RATE     TO DELACC-INT-RATE.
           MOVE ACCOUNT-OPENED            TO DELACC-OPENED.
           MOVE ACCOUNT-OVERDRAFT-LIMIT   TO DELACC-OVERDRAFT.
           MOVE ACCOUNT-LAST-STMT-DATE    TO DELACC-LAST-STMT-DT.
           MOVE ACCOUNT-NEXT-STMT-DATE    TO DELACC-NEXT-STMT-DT.
           MOVE ACCOUNT-AVAILABLE-BALANCE TO DELACC-AVAIL-BAL.
           MOVE ACCOUNT-ACTUAL-BALANCE    TO DELACC-ACTUAL-BAL.
           MOVE ACCOUNT-ACTUAL-BALANCE    TO ACCOUNT-ACT-BAL-STORE.

       RAD999.
           EXIT.


       DEL-ACCOUNT-DB2 SECTION.
       DADB010.

      *
      *    Delete the ACCOUNT row where the SORTCODE and ACCOUNT
      *    NUMBER match.
      *
           EXEC SQL
              DELETE FROM ACCOUNT
              WHERE ACCOUNT_SORTCODE = :HV-ACCOUNT-SORTCODE AND
                    ACCOUNT_NUMBER = :HV-ACCOUNT-ACC-NO
           END-EXEC.

           IF SQLCODE NOT = 0
              MOVE ' ' TO DELACC-SUCCESS
              MOVE 'N' TO DELACC-DEL-SUCCESS
              MOVE '3' TO DELACC-DEL-FAIL-CD
           END-IF.

       DADB999.
           EXIT.


       WRITE-PROCTRAN SECTION.
       WP010.

           PERFORM WRITE-PROCTRAN-DB2.
       WP999.
           EXIT.


       WRITE-PROCTRAN-DB2 SECTION.
       WPD010.

      *
      *    If the DELETE of the account row was successful then record
      *    this on the PROCTRAN datastore.
      *
           INITIALIZE HOST-PROCTRAN-ROW.
           INITIALIZE WS-EIBTASKN12.

           MOVE 'PRTR' TO HV-PROCTRAN-EYECATCHER.
           MOVE ACCOUNT-SORT-CODE  TO HV-PROCTRAN-SORT-CODE.
           MOVE ACCOUNT-NUMBER     TO HV-PROCTRAN-ACC-NUMBER.
           MOVE EIBTASKN           TO WS-EIBTASKN12.
           MOVE WS-EIBTASKN12      TO HV-PROCTRAN-REF.

      *
      *    Populate the time and date
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

           MOVE ACCOUNT-CUST-NO    TO PROC-DESC-DELACC-CUSTOMER
            OF PROCTRAN-AREA
           MOVE ACCOUNT-TYPE       TO PROC-DESC-DELACC-ACCTYPE
            OF PROCTRAN-AREA
           MOVE ACCOUNT-LAST-STMT-DAY
             TO PROC-DESC-DELACC-LAST-DD  OF PROCTRAN-AREA
           MOVE ACCOUNT-LAST-STMT-MONTH
             TO PROC-DESC-DELACC-LAST-MM  OF PROCTRAN-AREA
           MOVE ACCOUNT-LAST-STMT-YEAR
             TO PROC-DESC-DELACC-LAST-YYYY  OF PROCTRAN-AREA
           MOVE ACCOUNT-NEXT-STMT-DAY
             TO PROC-DESC-DELACC-NEXT-DD  OF PROCTRAN-AREA
           MOVE ACCOUNT-NEXT-STMT-MONTH
             TO PROC-DESC-DELACC-NEXT-MM  OF PROCTRAN-AREA
           MOVE ACCOUNT-NEXT-STMT-YEAR
             TO PROC-DESC-DELACC-NEXT-YYYY  OF PROCTRAN-AREA

           SET PROC-DESC-DELACC-FLAG  OF PROCTRAN-AREA TO TRUE.
           SET PROC-TY-BRANCH-DELETE-ACCOUNT  OF PROCTRAN-AREA TO TRUE.

           MOVE PROC-TRAN-DESC  OF PROCTRAN-AREA TO HV-PROCTRAN-DESC


           MOVE PROC-TRAN-TYPE  OF PROCTRAN-AREA TO HV-PROCTRAN-TYPE.
           MOVE ACCOUNT-ACT-BAL-STORE    TO HV-PROCTRAN-AMOUNT.

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
      *    Check the SQLCODE
      *

           IF SQLCODE NOT = 0
              MOVE SQLCODE TO SQLCODE-DISPLAY

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

              MOVE SQLCODE-DISPLAY    TO ABND-SQLCODE

              STRING 'WPD010 - Unable to  WRITE to PROCTRAN row '
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

              DISPLAY 'In DELACC (WPD010) '
              'UNABLE TO WRITE TO PROCTRAN ROW DATASTORE'
              ' SQLCODE=' SQLCODE-DISPLAY
              'WITH THE FOLLOWING DATA:' HOST-PROCTRAN-ROW

              EXEC CICS ABEND
                 ABCODE ('HWPT')
              END-EXEC

           END-IF.

       WPD999.
           EXIT.


      *
      * Finish
      *
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
