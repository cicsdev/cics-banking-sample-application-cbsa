       CBL CICS('SP,EDF,DLI')
       CBL SQL
      ******************************************************************
      *                                                                *
      *  Copyright IBM Corp. 2023                                      *
      *                                                                *
      ******************************************************************
      ******************************************************************
      * This program takes an incoming customer number
      * and determines which accounts it is associated with
      * by accessing the datastore & retrieving
      * the associated account records matching on the customer number
      ******************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. INQACCCU.
       AUTHOR. James O'Grady.


       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      *SOURCE-COMPUTER.   IBM-370 WITH DEBUGGING MODE.
       SOURCE-COMPUTER.  IBM-370.
       OBJECT-COMPUTER.  IBM-370.

       INPUT-OUTPUT SECTION.

       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.

       COPY SORTCODE.



      *
      * Get the ACCOUNT DB2 copybook
      *
           EXEC SQL
              INCLUDE ACCDB2
           END-EXEC.

      * ACCOUNT Host variables for DB2
       01 HOST-ACCOUNT-ROW.
          03 HV-ACCOUNT-EYECATCHER        PIC X(4).
          03 HV-ACCOUNT-CUST-NO           PIC X(10).
          03 HV-ACCOUNT-SORTCODE          PIC X(6).
          03 HV-ACCOUNT-ACC-NO            PIC X(8).
          03 HV-ACCOUNT-ACC-TYPE          PIC X(8).
          03 HV-ACCOUNT-INT-RATE          PIC S9(4)V99 COMP-3.
          03 HV-ACCOUNT-OPENED            PIC X(10).
          03 HV-ACCOUNT-OVERDRAFT-LIM     PIC S9(9) COMP.
          03 HV-ACCOUNT-LAST-STMT         PIC X(10).
          03 HV-ACCOUNT-NEXT-STMT         PIC X(10).
          03 HV-ACCOUNT-AVAIL-BAL         PIC S9(10)V99 COMP-3.
          03 HV-ACCOUNT-ACTUAL-BAL        PIC S9(10)V99 COMP-3.

       01 EIBRCODE-NICE.
          03 EIBRCODE-FIRST               PIC X.
          03 EIBRCODE-SECOND              PIC X.
          03 EIBRCODE-THIRD               PIC X.
          03 EIBRCODE-FOURTH              PIC X.
          03 EIBRCODE-FIFTH               PIC X.
          03 EIBRCODE-SIXTH               PIC X.

      * Pull in the SQL COMMAREA
        EXEC SQL
          INCLUDE SQLCA
        END-EXEC.


      * Declare the CURSOR for ACCOUNT table
           EXEC SQL DECLARE ACC-CURSOR CURSOR FOR
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
                     FROM ACCOUNT
                     WHERE ACCOUNT_CUSTOMER_NUMBER =
                        :HV-ACCOUNT-CUST-NO
                      AND ACCOUNT_SORTCODE =
                      :HV-ACCOUNT-SORTCODE
                     FOR FETCH ONLY
           END-EXEC.


       01 WS-CICS-WORK-AREA.
          03 WS-CICS-RESP                 PIC S9(8) COMP.
          03 WS-CICS-RESP2                PIC S9(8) COMP.
          03 WS-CICS-RESP-DISPLAY         PIC +9(8) DISPLAY.

       01 EXIT-BROWSE-LOOP                PIC X VALUE 'N'.

       01 OUTPUT-DATA.
           COPY ACCOUNT.

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

       01 DESIRED-KEY.
           03 DESIRED-KEY-CUSTOMER        PIC 9(10).
           03 DESIRED-KEY-SORTCODE        PIC 9(6).

       01 DB2-DATE-REFORMAT.
          03 DB2-DATE-REF-YR              PIC 9(4).
          03 FILLER                       PIC X.
          03 DB2-DATE-REF-MNTH            PIC 99.
          03 FILLER                       PIC X.
          03 DB2-DATE-REF-DAY             PIC 99.

       01 DATA-STORE-TYPE                 PIC X.
          88 DATASTORE-TYPE-DB2              VALUE '2'.
          88 DATASTORE-TYPE-VSAM             VALUE 'V'.

       01 DB2-EXIT-LOOP                   PIC X.
       01 FETCH-DATA-CNT                  PIC 9(4) COMP.
       01 WS-CUST-ALT-KEY-LEN             PIC S9(4) COMP VALUE +10.


       01 CUSTOMER-KY.
          03 REQUIRED-SORT-CODE           PIC 9(6)  VALUE 0.
          03 REQUIRED-ACC-NUM             PIC 9(8)  VALUE 0.

       01 SQLCODE-DISPLAY                 PIC S9(8) DISPLAY
                                          SIGN LEADING SEPARATE.

       01 MY-ABEND-CODE                   PIC XXXX.

       01 WS-STORM-DRAIN                  PIC X VALUE 'N'.

       01 STORM-DRAIN-CONDITION           PIC X(20).

       01 CUSTOMER-AREA.
       COPY CUSTOMER.

       01 INQCUST-COMMAREA.
          COPY INQCUST.

       01 WS-U-TIME                       PIC S9(15) COMP-3.
       01 WS-ORIG-DATE                    PIC X(10).
       01 WS-ORIG-DATE-GRP REDEFINES WS-ORIG-DATE.
          03 WS-ORIG-DATE-DD                 PIC 99.
          03 FILLER                          PIC X.
          03 WS-ORIG-DATE-MM                 PIC 99.
          03 FILLER                          PIC X.
          03 WS-ORIG-DATE-YYYY               PIC 9999.

       01 WS-ORIG-DATE-GRP-X.
          03 WS-ORIG-DATE-DD-X            PIC XX.
          03 FILLER                       PIC X VALUE '.'.
          03 WS-ORIG-DATE-MM-X            PIC XX.
          03 FILLER                       PIC X VALUE '.'.
          03 WS-ORIG-DATE-YYYY-X          PIC X(4).


       01 WS-TIME-DATA.
           03 WS-TIME-NOW                 PIC 9(6).
           03 WS-TIME-NOW-GRP REDEFINES WS-TIME-NOW.
              05 WS-TIME-NOW-GRP-HH          PIC 99.
              05 WS-TIME-NOW-GRP-MM          PIC 99.
              05 WS-TIME-NOW-GRP-SS          PIC 99.

       01 WS-ABEND-PGM                       PIC X(8) VALUE 'ABNDPROC'.

       01 ABNDINFO-REC.
           COPY ABNDINFO.

       LINKAGE SECTION.
       01 DFHCOMMAREA.
          COPY INQACCCU.

       PROCEDURE DIVISION USING DFHCOMMAREA.
       PREMIERE SECTION.
       A010.
           MOVE 'N' TO COMM-SUCCESS
           MOVE '0' TO COMM-FAIL-CODE

           EXEC CICS HANDLE ABEND
              LABEL(ABEND-HANDLING)
           END-EXEC.

           MOVE SORTCODE TO REQUIRED-SORT-CODE OF CUSTOMER-KY.

      *
      *    CUSTOMER-CHECK LINKS to program INQCUST to retrieve the
      *    customer information.
      *

           PERFORM CUSTOMER-CHECK.

      *
      *    If a  matching customer was not returned then set fail flags
      *
           IF CUSTOMER-FOUND = 'N'
              MOVE 'N' TO COMM-SUCCESS
              MOVE '1' TO COMM-FAIL-CODE
              PERFORM GET-ME-OUT-OF-HERE
           END-IF


           PERFORM READ-ACCOUNT-DB2
      *
      * Return the ACCOUNT data to the COMMAREA
      *

           PERFORM GET-ME-OUT-OF-HERE.

       A999.
           EXIT.


       READ-ACCOUNT-DB2 SECTION.
       RAD010.
      *
      *    Get accounts from account datastore
      *

      *
      *    Open the DB2 CURSOR
      *

           MOVE CUSTOMER-NUMBER IN DFHCOMMAREA TO HV-ACCOUNT-CUST-NO.
           MOVE  SORTCODE TO HV-ACCOUNT-SORTCODE.

           EXEC SQL OPEN
              ACC-CURSOR
           END-EXEC.

           MOVE SQLCODE TO SQLCODE-DISPLAY.

           IF SQLCODE NOT = 0
              MOVE SQLCODE TO SQLCODE-DISPLAY

      *
      *       Check if SQLCODE indicates that Storm Drain processing
      *       is applicable in a workload if activated
      *
              PERFORM CHECK-FOR-STORM-DRAIN-DB2

              MOVE 'N'  TO COMM-SUCCESS
              MOVE 'N'  TO CUSTOMER-FOUND
              MOVE '2'  TO COMM-FAIL-CODE
              MOVE ZERO TO NUMBER-OF-ACCOUNTS

              EXEC CICS SYNCPOINT ROLLBACK
                RESP(WS-CICS-RESP)
                RESP2(WS-CICS-RESP2)
              END-EXEC

              IF WS-CICS-RESP NOT = DFHRESP(NORMAL)
      *
      *          Preserve the RESP and RESP2, then set up the
      *          standard ABEND info before getting the applid,
      *          date/time etc. and linking to the Abend Handler
      *          program.
      *
                 INITIALIZE ABNDINFO-REC
                 MOVE EIBRESP    TO ABND-RESPCODE
                 MOVE EIBRESP2   TO ABND-RESP2CODE
      *
      *          Get supplemental information
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
                 MOVE 'HROL'      TO ABND-CODE

                 EXEC CICS ASSIGN PROGRAM(ABND-PROGRAM)
                 END-EXEC

                 MOVE ZEROS      TO ABND-SQLCODE

                 STRING 'RAD010 - Unable to perform Synpoint Rollback'
                       DELIMITED BY SIZE,
                       '. Possible Integrity issue following DB2 '
                       DELIMITED BY SIZE,
                       'CURSOR OPEN' DELIMITED BY SIZE,
                       'EIBRESP=' DELIMITED BY SIZE,
                       ABND-RESPCODE DELIMITED BY SIZE,
                       ' RESP2=' DELIMITED BY SIZE,
                       ABND-RESP2CODE DELIMITED BY SIZE
                       INTO ABND-FREEFORM
                 END-STRING

                 EXEC CICS LINK PROGRAM(WS-ABEND-PGM)
                           COMMAREA(ABNDINFO-REC)
                 END-EXEC


                 DISPLAY 'INQACCCU: Unable to perform Synpoint Rollback'
                 '. Possible Integrity issue following DB2 CURSOR OPEN'
                 ' SQLCODE=' SQLCODE-DISPLAY
                 ',RESP=' WS-CICS-RESP
                 ',RESP2=' WS-CICS-RESP2
                 EXEC CICS ABEND
                    ABCODE ('HROL')
                    CANCEL
                 END-EXEC

              END-IF
              GO TO RAD999
           END-IF.

      *
      *    Retrieve each of the accounts associated with this customer
      *
           PERFORM FETCH-DATA.

      *
      *    Close the DB2 CURSOR
      *
           EXEC SQL CLOSE
                          ACC-CURSOR
           END-EXEC.

           IF SQLCODE NOT = 0
              MOVE SQLCODE TO SQLCODE-DISPLAY
              DISPLAY 'Failure when attempting to close the DB2 CURSOR'
                  ' ACC-CURSOR. With SQL code='
                  SQLCODE-DISPLAY

      *
      *       Check if SQLCODE indicates that Storm Drain processing
      *       is applicable in a workload if activated
      *
              PERFORM CHECK-FOR-STORM-DRAIN-DB2

              MOVE 'N' TO COMM-SUCCESS
              MOVE 'N' TO CUSTOMER-FOUND
              MOVE '4' TO COMM-FAIL-CODE

              EXEC CICS SYNCPOINT
                 ROLLBACK
                 RESP(WS-CICS-RESP)
                 RESP2(WS-CICS-RESP2)
              END-EXEC

              IF WS-CICS-RESP NOT = DFHRESP(NORMAL)
      *
      *          Preserve the RESP and RESP2, then set up the
      *          standard ABEND info before getting the applid,
      *          date/time etc. and linking to the Abend Handler
      *          program.
      *
                 INITIALIZE ABNDINFO-REC
                 MOVE EIBRESP    TO ABND-RESPCODE
                 MOVE EIBRESP2   TO ABND-RESP2CODE
      *
      *          Get supplemental information
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
                 MOVE 'HROL'      TO ABND-CODE

                 EXEC CICS ASSIGN PROGRAM(ABND-PROGRAM)
                 END-EXEC

                 MOVE ZEROS      TO ABND-SQLCODE

                 STRING 'RAD010(2)- Unable to perform Syncpoint'
                       DELIMITED BY SIZE,
                       'Rollback. Possible Integrity issue following'
                       DELIMITED BY SIZE,
                       'CURSOR CLOSE' DELIMITED BY SIZE,
                       'EIBRESP=' DELIMITED BY SIZE,
                       ABND-RESPCODE DELIMITED BY SIZE,
                       ' RESP2=' DELIMITED BY SIZE,
                       ABND-RESP2CODE DELIMITED BY SIZE
                       INTO ABND-FREEFORM
                 END-STRING

                 EXEC CICS LINK PROGRAM(WS-ABEND-PGM)
                           COMMAREA(ABNDINFO-REC)
                 END-EXEC

      *          '. Possible Integrity issue following DB2 CURSOR CLOSE'
      *          ' SQLCODE=' SQLCODE-DISPLAY
      *          ',RESP=' WS-CICS-RESP
      *          ',RESP2=' WS-CICS-RESP2

                 DISPLAY 'INQACCCU:Unable to perform Synpoint ROLLBACK'
                         '. Possible Integrity issue following '
                         'DB2 CURSOR OPEN SQLCODE=' SQLCODE-DISPLAY
                         ',RESP=' WS-CICS-RESP
                         ',RESP2=' WS-CICS-RESP2

                 EXEC CICS ABEND
                    ABCODE ('HROL')
                    CANCEL
                 END-EXEC
              END-IF

              GO TO RAD999
           END-IF.

           MOVE 'Y' TO COMM-SUCCESS.

       RAD999.
           EXIT.


       FETCH-DATA SECTION.
       FD010.
      *
      *    Fetch each account in turn, & store data until there are no
      *    more rows to process. (There is a maximum of 20 accounts per
      *    customer).
      *
           MOVE ZERO TO NUMBER-OF-ACCOUNTS.

           PERFORM UNTIL SQLCODE NOT = 0 OR
           NUMBER-OF-ACCOUNTS = 20

              EXEC SQL FETCH FROM ACC-CURSOR
              INTO :HV-ACCOUNT-EYECATCHER,
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
              END-EXEC

      *
      *       If there is no data found at all, then return a
      *       low value record
      *
              IF SQLCODE = +100
                  MOVE 'Y' TO COMM-SUCCESS
                  GO TO FD999
              END-IF

              IF SQLCODE NOT = 0
                 MOVE SQLCODE TO SQLCODE-DISPLAY

                 DISPLAY 'Failure when attempting to FETCH from the'
                    ' DB2 CURSOR ACC-CURSOR. With SQL code='
                    SQLCODE-DISPLAY
      *
      *          Check if SQLCODE indicates that Storm Drain processing
      *          is applicable in a workload if activated
      *
                 PERFORM CHECK-FOR-STORM-DRAIN-DB2

                 MOVE 'N'  TO COMM-SUCCESS
                 MOVE 'N'  TO CUSTOMER-FOUND
                 MOVE ZERO TO NUMBER-OF-ACCOUNTS
                 MOVE '3' TO COMM-FAIL-CODE

                 EXEC CICS SYNCPOINT
                    ROLLBACK
                    RESP(WS-CICS-RESP)
                    RESP2(WS-CICS-RESP2)
                 END-EXEC

                 IF WS-CICS-RESP NOT = DFHRESP(NORMAL)
      *
      *             Preserve the RESP and RESP2, then set up the
      *             standard ABEND info before getting the applid,
      *             date/time etc. and linking to the Abend Handler
      *             program.
      *
                    INITIALIZE ABNDINFO-REC
                    MOVE EIBRESP    TO ABND-RESPCODE
                    MOVE EIBRESP2   TO ABND-RESP2CODE
      *
      *             Get supplemental information
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
                    MOVE 'HROL'      TO ABND-CODE

                    EXEC CICS ASSIGN PROGRAM(ABND-PROGRAM)
                    END-EXEC

                    MOVE ZEROS      TO ABND-SQLCODE

                    STRING 'FD010 - Unable to perform Syncpoint'
                       DELIMITED BY SIZE,
                       'Rollback. Possible Integrity issue following'
                       DELIMITED BY SIZE,
                       'DB2 FETCH' DELIMITED BY SIZE,
                       'EIBRESP=' DELIMITED BY SIZE,
                       ABND-RESPCODE DELIMITED BY SIZE,
                       ' RESP2=' DELIMITED BY SIZE,
                       ABND-RESP2CODE DELIMITED BY SIZE
                       INTO ABND-FREEFORM
                    END-STRING

                    EXEC CICS LINK PROGRAM(WS-ABEND-PGM)
                           COMMAREA(ABNDINFO-REC)
                    END-EXEC

                    DISPLAY 'INQACCCU: Unable to perform Syncpoint '
                       'ROLLBACK. Possible Integrity issue following'
                       ' DB2 FETCH. SQLCODE=' SQLCODE-DISPLAY
                       ',RESP=' WS-CICS-RESP
                       ',RESP2=' WS-CICS-RESP2

                    EXEC CICS ABEND
                       ABCODE ('HROL')
                       CANCEL
                    END-EXEC

                 END-IF
                 GO TO FD999

              END-IF

              ADD 1 TO NUMBER-OF-ACCOUNTS GIVING NUMBER-OF-ACCOUNTS
      *
      *       If we find a matching customer
      *
              MOVE HV-ACCOUNT-EYECATCHER
                 TO COMM-EYE(NUMBER-OF-ACCOUNTS)
              MOVE HV-ACCOUNT-CUST-NO
                  TO COMM-CUSTNO(NUMBER-OF-ACCOUNTS)
              MOVE HV-ACCOUNT-SORTCODE
                  TO COMM-SCODE(NUMBER-OF-ACCOUNTS)
              MOVE HV-ACCOUNT-ACC-NO
                  TO COMM-ACCNO(NUMBER-OF-ACCOUNTS)
              MOVE HV-ACCOUNT-ACC-TYPE
                  TO COMM-ACC-TYPE(NUMBER-OF-ACCOUNTS)
              MOVE HV-ACCOUNT-INT-RATE
                  TO COMM-INT-RATE(NUMBER-OF-ACCOUNTS)
              MOVE HV-ACCOUNT-OPENED TO DB2-DATE-REFORMAT

              STRING DB2-DATE-REF-DAY
                DB2-DATE-REF-MNTH
                DB2-DATE-REF-YR
                DELIMITED BY SIZE
                INTO COMM-OPENED(NUMBER-OF-ACCOUNTS)
              END-STRING

              MOVE HV-ACCOUNT-OVERDRAFT-LIM
                 TO COMM-OVERDRAFT(NUMBER-OF-ACCOUNTS)
              MOVE HV-ACCOUNT-LAST-STMT TO DB2-DATE-REFORMAT

              STRING DB2-DATE-REF-DAY
                 DB2-DATE-REF-MNTH
                 DB2-DATE-REF-YR
                 DELIMITED BY SIZE
                 INTO COMM-LAST-STMT-DT(NUMBER-OF-ACCOUNTS)
              END-STRING

              MOVE HV-ACCOUNT-NEXT-STMT TO DB2-DATE-REFORMAT

              STRING DB2-DATE-REF-DAY
                 DB2-DATE-REF-MNTH
                 DB2-DATE-REF-YR
                 DELIMITED BY SIZE
                 INTO COMM-NEXT-STMT-DT(NUMBER-OF-ACCOUNTS)
              END-STRING

              MOVE HV-ACCOUNT-ACTUAL-BAL
                 TO COMM-ACTUAL-BAL(NUMBER-OF-ACCOUNTS)
              MOVE HV-ACCOUNT-AVAIL-BAL
                 TO COMM-AVAIL-BAL(NUMBER-OF-ACCOUNTS)

           END-PERFORM.

       FD999.
           EXIT.

       GET-ME-OUT-OF-HERE SECTION.
       GMOFH010.
      *
      *    Return control back to CICS
      *
           EXEC CICS RETURN
           END-EXEC.

           GOBACK.

       GMOFH999.
           EXIT.


      *
      * Check for Storm Drain Condition (DB2)
      *
       CHECK-FOR-STORM-DRAIN-DB2 SECTION.
       CFSDD010.

      *
      *    Check if the Sqlcode is one that will
      *    trigger Storm Drain is active in Workload
      *
           EVALUATE SQLCODE

              WHEN 923
                 MOVE 'DB2 Connection lost ' TO STORM-DRAIN-CONDITION

              WHEN OTHER
                 MOVE 'Not Storm Drain     ' TO STORM-DRAIN-CONDITION

           END-EVALUATE.

           IF STORM-DRAIN-CONDITION NOT EQUAL 'Not Storm Drain     '
              DISPLAY 'INQACCCU: Check-For-Storm-Drain-DB2: Storm '
                      'Drain condition (' STORM-DRAIN-CONDITION ') '
                      'has been met (' SQLCODE-DISPLAY ').'
           ELSE
              CONTINUE
           END-IF.


       CFSDD999.
           EXIT.


      *
      * How ABENDs are dealt with
      *
       ABEND-HANDLING SECTION.
       AH010.

           EXEC CICS ASSIGN
              ABCODE(MY-ABEND-CODE)
           END-EXEC.

      *
      *    Evaluate the Abend code that is returned
      *     - For DB2 AD2Z ... provide some diagnostics.
      *     - For VSAM RLS abends: AFCR, AFCS and AFCT record the
      *       Abend as happening but do not abend ... leave this to
      *       CPSM WLM "Storm drain" (Abend probability) to handle
      *     If not a "storm drain" ... take the abend afterwards
      *
           EVALUATE MY-ABEND-CODE

      *
      *       DB2 AD2Z abend
      *
              WHEN 'AD2Z'
                 MOVE SQLCODE TO SQLCODE-DISPLAY
                 DISPLAY 'DB2 DEADLOCK DETECTED IN INQACCCU, SQLCODE='
                    SQLCODE-DISPLAY
                 DISPLAY 'DB2 DEADLOCK FOR ACCOUNT '
                    HV-ACCOUNT-ACC-NO
                 DISPLAY  'SQLSTATE=' SQLSTATE
                          ',SQLERRMC=' sqlerrmc(1:sqlerrmL)
                          ',sqlerrd(1)=' sqlerrd(1)
                          ',sqlerrd(2)=' sqlerrd(2)
                          ',sqlerrd(3)=' sqlerrd(3)
                          ',sqlerrd(4)=' sqlerrd(4)
                          ',sqlerrd(5)=' sqlerrd(5)
                          ',sqlerrd(6)=' sqlerrd(6)

      *
      *          VSAM RLS abends, subject to CPSM WLM Storm Drain check
      *          if handled (as here) and Workload Abend Thresholds are
      *          Set.
      *
              WHEN 'AFCR'

              WHEN 'AFCS'

              WHEN 'AFCT'
                 MOVE 'Y' TO WS-STORM-DRAIN
                 DISPLAY 'INQACCCU: Check-For-Storm-Drain-VSAM: Storm '
                        'Drain condition (Abend ' MY-ABEND-CODE ') '
                        'has been met.'

                 EXEC CICS SYNCPOINT ROLLBACK
                    RESP(WS-CICS-RESP)
                    RESP2(WS-CICS-RESP2)
                 END-EXEC

                 IF WS-CICS-RESP NOT = DFHRESP(NORMAL)
      *
      *             Preserve the RESP and RESP2, then set up the
      *             standard ABEND info before getting the applid,
      *             date/time etc. and linking to the Abend Handler
      *             program.
      *
                    INITIALIZE ABNDINFO-REC
                    MOVE EIBRESP    TO ABND-RESPCODE
                    MOVE EIBRESP2   TO ABND-RESP2CODE
      *
      *             Get supplemental information
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
                    MOVE 'HROL'      TO ABND-CODE

                    EXEC CICS ASSIGN PROGRAM(ABND-PROGRAM)
                    END-EXEC

                    MOVE 0 TO ABND-SQLCODE

                    STRING 'AH010 -Unable to perform SYNCPOINT '
                       DELIMITED BY SIZE,
                       'ROLLBACK' DELIMITED BY SIZE,
                       ' Possible integrity issue following VSAM RLS '
                       DELIMITED BY SIZE,
                       ' abend.' DELIMITED BY SIZE,
                       'EIBRESP=' DELIMITED BY SIZE,
                       ABND-RESPCODE DELIMITED BY SIZE,
                       ' RESP2=' DELIMITED BY SIZE,
                       ABND-RESP2CODE DELIMITED BY SIZE
                       INTO ABND-FREEFORM
                    END-STRING

                    EXEC CICS LINK PROGRAM(WS-ABEND-PGM)
                           COMMAREA(ABNDINFO-REC)
                    END-EXEC

                    DISPLAY 'INQACCC: Unable to perform Syncpoint.'
                       ' Rollback. Possible Integrity issue following '
                       ' VSAM RLS abend RESP CODE=' WS-CICS-RESP
                       ' RESP2 CODE=' WS-CICS-RESP2

                    EXEC CICS ABEND
                       ABCODE ('HROL')
                       CANCEL
                    END-EXEC

                 END-IF

                 MOVE 'N' TO COMM-SUCCESS

                 EXEC CICS RETURN
                 END-EXEC

           END-EVALUATE.

           IF WS-STORM-DRAIN = 'N'
              EXEC CICS ABEND
                 ABCODE( MY-ABEND-CODE)
                 NODUMP
                 CANCEL
              END-EXEC
           END-IF.

       AH999.
           EXIT.


       CUSTOMER-CHECK SECTION.
       CC010.
      *
      *    Retrieve customer information by linking to INQCUST
      *

           IF CUSTOMER-NUMBER IN DFHCOMMAREA = ZERO
              MOVE 'N' TO CUSTOMER-FOUND
              MOVE ZERO TO NUMBER-OF-ACCOUNTS
              GO TO CC999
           END-IF.

           IF CUSTOMER-NUMBER IN DFHCOMMAREA = '9999999999'
              MOVE 'N' TO CUSTOMER-FOUND
              MOVE ZERO TO NUMBER-OF-ACCOUNTS
              GO TO CC999
           END-IF.

           INITIALIZE INQCUST-COMMAREA.

           MOVE CUSTOMER-NUMBER IN DFHCOMMAREA TO INQCUST-CUSTNO.

           EXEC CICS LINK PROGRAM('INQCUST ')
              COMMAREA(INQCUST-COMMAREA)
           END-EXEC.

           IF INQCUST-INQ-SUCCESS = 'Y'
              MOVE 'Y' TO CUSTOMER-FOUND
           ELSE
              MOVE 'N' TO CUSTOMER-FOUND
              MOVE ZERO TO NUMBER-OF-ACCOUNTS
           END-IF.

       CC999.
           EXIT.


       POPULATE-TIME-DATE SECTION.
       PTD010.
      D    DISPLAY 'POPULATE-TIME-DATE SECTION'.

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
