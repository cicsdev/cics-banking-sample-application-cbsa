       CBL CICS('SP,EDF')
       CBL SQL
      ******************************************************************
      *                                                                *
      *  Copyright IBM Corp. 2023                                      *
      *                                                                *
      ******************************************************************
      ******************************************************************
      * This program takes customer information from the BMS
      * application (name, address and DOB) and should get the SORTCODE,
      * determine which datastore to use (VSAM or DB2), then enqueue
      * the counter for CUSTOMER, increment the counter and get
      * that number.
      *
      * It then performs a credit check on multiple
      * credit agencies (using Async API), waits for 3 seconds and
      * aggregates and averages the returned credit scores.
      *
      * If no data is returned, from the credit checks, then
      * set the credit score to 0 and mark the credit
      * score review date as today (so another attempt at credit
      * scoring will be carried out later).
      *
      * Next, attempt to update the CUSTOMER datastore & if that is
      * successful, write a rec to the PROCTRAN datastore.
      *
      * If all of that works, then DEQUEUE the named counter
      * and return the SORTCODE and CUSTOMER number.
      *
      * If for any reason the write to the CUSTOMER or PROCTRAN
      * datatsore is unsuccessful, then we need to decrement the Named
      * Counter (restoring it to the start position) and DEQUEUE the
      * Named Counter.
      *
      ******************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. CRECUST.
       AUTHOR. Jon Collett.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      * SOURCE-COMPUTER.  IBM-370 WITH DEBUGGING MODE.
       SOURCE-COMPUTER.  IBM-370.
       OBJECT-COMPUTER.  IBM-370.

       INPUT-OUTPUT SECTION.

       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.


       COPY SORTCODE.



       01 SYSIDERR-RETRY                PIC 999.

      * PROCTRAN DB2 copybook
          EXEC SQL
             INCLUDE PROCDB2
          END-EXEC.

      * PROCTRAN host variables for DB2
       01 HOST-PROCTRAN-ROW.
          03 HV-PROCTRAN-EYECATCHER     PIC X(4).
          03 HV-PROCTRAN-SORT-CODE      PIC X(6).
          03 HV-PROCTRAN-ACC-NUMBER     PIC X(8).
          03 HV-PROCTRAN-DATE           PIC X(10).
          03 HV-PROCTRAN-TIME           PIC X(6).
          03 HV-PROCTRAN-REF            PIC X(12).
          03 HV-PROCTRAN-TYPE           PIC X(3).
          03 HV-PROCTRAN-DESC           PIC X(40).
          03 HV-PROCTRAN-AMOUNT         PIC S9(10)V99 COMP-3.

      * Pull in the SQL COMMAREA
       EXEC SQL
          INCLUDE SQLCA
       END-EXEC.

       01 PROCTRAN-AREA.
          COPY PROCTRAN.

       01 PROCTRAN-RIDFLD               PIC S9(8) COMP.

       01 WS-CICS-WORK-AREA.
          05 WS-CICS-RESP               PIC S9(8) COMP.
          05 WS-CICS-RESP2              PIC S9(8) COMP.

       01 WS-CUSTOMER-NO-NUM            PIC 9(10).

       01 WS-TIME-DATA.
          03 WS-TIME-NOW                PIC 9(6).
          03 WS-TIME-NOW-GRP REDEFINES WS-TIME-NOW.
             05 WS-TIME-NOW-GRP-HH      PIC 99.
             05 WS-TIME-NOW-GRP-MM      PIC 99.
             05 WS-TIME-NOW-GRP-SS      PIC 99.

       01 WS-ABEND-PGM                  PIC X(8)      VALUE 'ABNDPROC'.
       01 ABNDINFO-REC.
           COPY ABNDINFO.

       LOCAL-STORAGE SECTION.
       01 FILE-RETRY                    PIC 999.
       01 WS-EXIT-RETRY-LOOP            PIC X         VALUE ' '.

       01 OUTPUT-DATA.
           COPY CUSTOMER.

       01 RETURN-DATA.
          03 RETURN-DATA-EYECATCHER     PIC X(4).
          03 RETURN-DATA-NUMBER         PIC 9(10) DISPLAY.
          03 RETURN-DATA-NAME           PIC X(60).
          03 RETURN-DATA-ADDRESS        PIC X(160).
          03 RETURN-DATA-DATE-OF-BIRTH  PIC 9(8).


       01 CUSTOMER-KY.
          03 REQUIRED-SORT-CODE         PIC 9(6)      VALUE 0.
          03 REQUIRED-CUST-NUMBER       PIC 9(10)     VALUE 0.

       01 RANDOM-CUSTOMER               PIC 9(10)     VALUE 0.
       01 HIGHEST-CUST-NUMBER           PIC 9(10)     VALUE 0.

       01 EXIT-VSAM-READ                PIC X         VALUE 'N'.
       01 EXIT-DB2-READ                 PIC X         VALUE 'N'.

       01 WS-V-RETRIED                  PIC X         VALUE 'N'.
       01 WS-D-RETRIED                  PIC X         VALUE 'N'.

       01 SQLCODE-DISPLAY               PIC S9(8) DISPLAY
             SIGN LEADING SEPARATE.


      *
      * CUSTOMER NCS definitions
      *
       01 NCS-CUST-NO-STUFF.
          03 NCS-CUST-NO-NAME.
             05 NCS-CUST-NO-ACT-NAME    PIC X(8)
                                                      VALUE 'CBSACUST'.
             05 NCS-CUST-NO-TEST-SORT   PIC X(6)
                                                      VALUE '      '.
             05 NCS-CUST-NO-FILL        PIC XX
                                                      VALUE '  '.

          03 NCS-CUST-NO-INC            PIC 9(16) COMP
                                                      VALUE 0.
          03 NCS-CUST-NO-VALUE          PIC 9(16) COMP
                                                      VALUE 0.

          03 NCS-CUST-NO-RESP           PIC XX        VALUE '00'.

       01 WS-DISP-CUST-NO-VAL           PIC S9(18) DISPLAY.

       01 WS-CUST-REC-LEN               PIC S9(4) COMP
                                                      VALUE 0.

       01 NCS-UPDATED                   PIC X         VALUE 'N'.

       01 WS-U-TIME                     PIC S9(15) COMP-3.
       01 WS-ORIG-DATE                  PIC X(10).
       01 WS-ORIG-DATE-GRP REDEFINES WS-ORIG-DATE.
          03 WS-ORIG-DATE-DD            PIC 99.
          03 FILLER                     PIC X.
          03 WS-ORIG-DATE-MM            PIC 99.
          03 FILLER                     PIC X.
          03 WS-ORIG-DATE-YYYY          PIC 9999.

       01 WS-ORIG-DATE-GRP-X.
          03 WS-ORIG-DATE-DD-X          PIC XX.
          03 FILLER                     PIC X         VALUE '.'.
          03 WS-ORIG-DATE-MM-X          PIC XX.
          03 FILLER                     PIC X         VALUE '.'.
          03 WS-ORIG-DATE-YYYY-X        PIC X(4).

       01 STORED-SORTCODE               PIC X(6)      VALUE SPACES.
       01 STORED-CUSTNO                 PIC X(10)     VALUE SPACES.
       01 STORED-NAME                   PIC X(60)     VALUE SPACES.
       01 STORED-DOB                    PIC X(10)     VALUE SPACES.

       01 WS-EIBTASKN12                 PIC 9(12)     VALUE 0.
       77 PROCTRAN-RETRY                PIC 999.

       01 CUSTOMER-KY2.
          03 REQUIRED-SORT-CODE2        PIC 9(6)      VALUE 0.
          03 REQUIRED-CUST-NUMBER2      PIC 9(10)     VALUE 0.

       01 CUSTOMER-KY2-BYTES REDEFINES CUSTOMER-KY2
                                        PIC X(16).

       01 HIGHEST-CUST-NUMBER           PIC 9(10)     VALUE 0.
       01 WS-CC-CNT                     PIC 9         VALUE 0.
       01 WS-FINISHED-FETCHING          PIC X         VALUE 'N'.
       01 WS-RETRIEVED-CNT              PIC 9         VALUE 0.
       01 WS-CHANNEL-NAME               PIC X(16)     VALUE SPACES.
       01 WS-CREDIT-CHECK-ERROR         PIC X         VALUE 'N'.
       01 WS-ACTUAL-CS-SCR              PIC 9(6)      VALUE 0.
       01 WS-TOTAL-CS-SCR               PIC 9(6)      VALUE 0.

       01 WS-CHILD-TOKENS.
          03 WS-ANY-CHILD-TKN           PIC X(16).
          03 WS-ANY-CHILD-FETCH-TKN     PIC X(16).

       01 WS-ANY-CHILD-FETCH-CHAN       PIC X(16).
       01 WS-ANY-CHILD-FETCH-ABCODE     PIC X(4)      VALUE SPACES.
       01 WS-CHILD-ISSUED-CNT           PIC 9         VALUE 0.

       01 WS-CHILD-ARRAY.
          03 WS-CHILD-DETAILS OCCURS 9 TIMES.
             05 WS-CHILD-CHAN           PIC X(16).
             05 WS-CHILD-TKN            PIC X(16).

       01 WS-CHILD-RECEIVED-CNT         PIC 9         VALUE 0.
       01 WS-RECEIVE-CHILD-ARRAY.
          03 WS-RECEIVE-CHILD-DTLS OCCURS 9 TIMES.
             05 WS-RECEIVE-CHILD-CHAN   PIC X(16).

       01 WS-CHILD-FETCH-COMPST         PIC S9(8) COMP.

       01 WS-CHILD-DATA.
          03 WS-CHILD-DATA-EYECATCHER   PIC X(4).
          03 WS-CHLD-DATA-KEY.
             05 WS-CHILD-DATA-SORTCODE  PIC 9(6) DISPLAY.
             05 WS-CHILD-DATA-NUMBER    PIC 9(10) DISPLAY.
          03 WS-CHILD-DATA-NAME         PIC X(60).
          03 WS-CHILD-DATA-ADDRESS      PIC X(160).
          03 WS-CHILD-DATA-DATE-OF-BIRTH
                                        PIC 9(8).
          03 WS-CHILD-DATA-DOB-GROUP
                REDEFINES WS-CHILD-DATA-DATE-OF-BIRTH.
             05 WS-CHILD-DATA-BIRTH-DAY PIC 99.
             05 WS-CHILD-DATA-BIRTH-MONTH
                                        PIC 99.
             05 WS-CHILD-DATA-BIRTH-YEAR
                                        PIC 9999.
          03 WS-CHILD-DATA-CREDIT-SCORE PIC 999.
          03 WS-CHILD-DATA-CS-REVIEW-DATE
                                        PIC 9(8).
          03 WS-CHILD-DATA-SUCCESS      PIC X.
          03 WS-CHILD-DATA-FAIL-CODE    PIC X.

       01 WS-CONTAINER-NAME             PIC X(16)     VALUE SPACES.
       01 WS-CHILD-CONTAINER-LEN        PIC S9(8) COMP
                                                      VALUE 0.
       01 WS-RUN-TRANSID                PIC X(4)      VALUE SPACES.

       01 CICSTSLEVEL                   PIC X(6)      VALUE SPACES.

       01 WS-CICSTS-LEVEL-DATA.
          03 WS-CICSTSLEVEL             PIC X(6).
          03 WS-CICSTS-LEVEL-NUM-GRP REDEFINES WS-CICSTSLEVEL.
             05 WS-CICSTS-LEVEL-NUM-VV  PIC 99.
             05 WS-CICSTS-LEVEL-NUM-RR  PIC 99.
             05 WS-CICSTS-LEVEL-NUM-MM  PIC 99.

       01 WS-CURRENT-DATE-DATA.
          03 WS-CURRENT-DATE.
             05 WS-CURRENT-YYYY         PIC 9(4).
             05 WS-CURRENT-MM           PIC 99.
             05 WS-CURRENT-DD           PIC 99.
          03 WS-CURRENT-TIME.
             05 WS-CURRENT-HOURS        PIC 99.
             05 WS-CURRENT-MINS         PIC 99.
             05 WS-CURRENT-SECS         PIC 99.
             05 WS-CURRENT-MILLI        PIC 99.
          03 WS-DIFFERENCE-FROM-GMT     PIC S9(4).

       01 WS-CURRENT-DATE-9             PIC 9(8)      VALUE 0.
       01 WS-TODAY-INT                  PIC 9(8)      VALUE 0.
       01 WS-REVIEW-DATE-ADD            PIC 99        VALUE 0.
       01 WS-NEW-REVIEW-DATE-INT        PIC 9(8)      VALUE 0.
       01 WS-NEW-REVIEW-YYYYMMDD        PIC 9(8)      VALUE 0.
       01 WS-PUT-CONT-NAME              PIC X(16)     VALUE SPACES.
       01 WS-PUT-CONT-LEN               PIC S9(8) COMP
                                                      VALUE 0.

       01 WS-SEED                       PIC S9(15) COMP.

       01 STORM-DRAIN-CONDITION         PIC X(20).

       01 WS-DATE-OF-BIRTH-ERROR        PIC X         VALUE 'N'.
       01 WS-DATE-OF-BIRTH-LILLIAN      PIC S9(9) BINARY.

       01 DATE-OF-BIRTH-FORMAT.
          03 DATE-OF-BIRTH-FORMAT-LENGTH
                                        PIC S9(4) BINARY
                                                      VALUE 10.
          03 DATE-OF-BIRTH-FORMAT-TEXT  PIC X(8)      VALUE 'YYYYMMDD'.

       01 DATE-OF-BIRTH-FOR-CEEDAYS.
          03 DATE-OF-BIRTH-CEEDAYS-LENGTH
                                        PIC S9(4) BINARY
                                                      VALUE 10.
          03 CEEDAYS-YEAR               PIC 9999.
          03 CEEDAYS-MONTH              PIC 99.
          03 CEEDAYS-DAY                PIC 99.

       01 FC.
          02 CONDITION-TOKEN-VALUE.
           COPY  CEEIGZCT.
             03 CASE-1-CONDITION-ID.
                04 SEVERITY             PIC S9(4) BINARY.
                04 MSG-NO               PIC S9(4) BINARY.
             03 CASE-2-CONDITION-ID
                   REDEFINES CASE-1-CONDITION-ID.
                04 CLASS-CODE           PIC S9(4) BINARY.
                04 CAUSE-CODE           PIC S9(4) BINARY.
             03 CASE-SEV-CTL            PIC X.
             03 FACILITY-ID             PIC XXX.
          02 I-S-INFO                   PIC S9(9) BINARY.

       01 WS-TODAY-LILLIAN              PIC S9(9) BINARY.
       01 WS-TODAY-SECONDS COMP-2.
       01 WS-TODAY-GREGORIAN.
          03 WS-TODAY-G-YEAR            PIC 9(4).
          03 WS-TODAY-G-MONTH           PIC 9(2).
          03 WS-TODAY-G-DAY             PIC 9(2).
          03 WS-TODAY-G-HOURS           PIC 9(2).
          03 WS-TODAY-G-MINUTES         PIC 9(2).
          03 WS-TODAY-G-SECONDS         PIC 9(2).
          03 WS-TODAY-G-MILLISECONDS    PIC 999.

       01 CHRDATE.
          02 VSTRING-LENGTH             PIC S9(4) BINARY.
          02 VSTRING-TEXT.
             03 VSTRING-CHAR            PIC X
                   OCCURS 0 TO 256 TIMES
                   DEPENDING ON VSTRING-LENGTH
                   OF CHRDATE.
       01 PICSTR.
          02 VSTRING-LENGTH             PIC S9(4) BINARY.
          02 VSTRING-TEXT.
             03 VSTRING-CHAR            PIC X
                   OCCURS 0 TO 256 TIMES
                   DEPENDING ON VSTRING-LENGTH
                   OF PICSTR.
       01 LILIAN                        PIC S9(9) BINARY.

       01 WS-CUSTOMER-AGE               PIC S9999.

       01 CUSTOMER-CONTROL.
           COPY CUSTCTRL.


       LINKAGE SECTION.
       01 DFHCOMMAREA.
           COPY CRECUST.


       PROCEDURE DIVISION USING DFHCOMMAREA.
       PREMIERE SECTION.
       P010.

           MOVE SORTCODE TO REQUIRED-SORT-CODE.


      *
      *    Derive the date and time
      *

           PERFORM POPULATE-TIME-DATE.

      *
      *    Perform the Asynchronous credit check
      *
           PERFORM CREDIT-CHECK.

           IF WS-CREDIT-CHECK-ERROR = 'Y'
              MOVE 0 TO COMM-CREDIT-SCORE

              STRING WS-ORIG-DATE-DD DELIMITED BY SIZE,
                     WS-ORIG-DATE-MM DELIMITED BY SIZE,
                     WS-ORIG-DATE-YYYY DELIMITED BY SIZE
                     INTO COMM-CS-REVIEW-DATE
              END-STRING

              MOVE 'N' TO COMM-SUCCESS
              MOVE 'G' TO COMM-FAIL-CODE

              DISPLAY 'WS-CREDIT-CHECK-ERROR = Y, '
                       ' RESP='
                       WS-CICS-RESP ' RESP2=' WS-CICS-RESP2
              DISPLAY '   Exiting CRECUST. COMMAREA='
                       DFHCOMMAREA
              PERFORM GET-ME-OUT-OF-HERE

           END-IF.

           PERFORM DATE-OF-BIRTH-CHECK.

           IF WS-DATE-OF-BIRTH-ERROR = 'Y'

              MOVE 'N' TO COMM-SUCCESS
              PERFORM GET-ME-OUT-OF-HERE

           END-IF.

      *
      *    Enqueue the named counter for customer
      *
           PERFORM ENQ-NAMED-COUNTER.

      *
      *    Get the next CUSTOMER number from the CUSTOMER Named Counter
      *
           PERFORM UPD-NCS.

      *
      *    Update the datastore
      *
           PERFORM WRITE-CUSTOMER-VSAM.


           PERFORM GET-ME-OUT-OF-HERE.

       P999.
           EXIT.


       POPULATE-TIME-DATE SECTION.
       PTD010.

           EXEC CICS ASKTIME
              ABSTIME(WS-U-TIME)
           END-EXEC.

           EXEC CICS FORMATTIME
                     ABSTIME(WS-U-TIME)
                     DDMMYYYY(WS-ORIG-DATE)
                     TIME(PROC-TRAN-TIME OF PROCTRAN-AREA )
                     DATESEP
           END-EXEC.

       PTD999.
           EXIT.


       ENQ-NAMED-COUNTER SECTION.
       ENC010.
           MOVE SORTCODE TO
              NCS-CUST-NO-TEST-SORT.

           EXEC CICS ENQ
              RESOURCE(NCS-CUST-NO-NAME)
              LENGTH(16)
              RESP(WS-CICS-RESP)
              RESP2(WS-CICS-RESP2)
           END-EXEC.

           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)
             MOVE 'N' TO COMM-SUCCESS
             MOVE '3' TO COMM-FAIL-CODE
             PERFORM GET-ME-OUT-OF-HERE
           END-IF.

       ENC999.
           EXIT.


       DEQ-NAMED-COUNTER SECTION.
       DNC010.

           MOVE SORTCODE TO
              NCS-CUST-NO-TEST-SORT.

      D    EXEC CICS ASKTIME ABSTIME(START-DEQ) END-EXEC

           EXEC CICS DEQ
              RESOURCE(NCS-CUST-NO-NAME)
              LENGTH(16)
              RESP(WS-CICS-RESP)
              RESP2(WS-CICS-RESP2)
           END-EXEC.

           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)
             MOVE 'N' TO COMM-SUCCESS
             MOVE '5' TO COMM-FAIL-CODE
             PERFORM GET-ME-OUT-OF-HERE
           END-IF.

       DNC999.
           EXIT.


       UPD-NCS SECTION.
       UN010.
      *
      *    Update the Named Counter Server
      *
           MOVE 1 TO NCS-CUST-NO-INC.

           PERFORM GET-LAST-CUSTOMER-VSAM

           MOVE 'Y' TO NCS-UPDATED.

       UN999.
           EXIT.




       CREDIT-CHECK SECTION.
       CC010.
      *
      *    Carry out the Credit Check Asynchronously
      *


      *
      *    Retrieve the table of transaction IDs to use &
      *    initiate each Asynchronous transaction
      *
           MOVE 'CIPCREDCHANN    ' TO WS-CHANNEL-NAME.
           MOVE 0 TO WS-CHILD-ISSUED-CNT.

           COMPUTE WS-PUT-CONT-LEN = LENGTH OF DFHCOMMAREA.

           PERFORM VARYING WS-CC-CNT FROM 1 BY 1
           UNTIL WS-CC-CNT > 5

      *
      *       Use transactions OCR1 - OCR5
      *
              STRING 'OCR' DELIMITED BY SIZE,
                      WS-CC-CNT DELIMITED BY SIZE
                 INTO WS-RUN-TRANSID
              END-STRING

              EVALUATE WS-CC-CNT
                 WHEN 1
                    MOVE 'CIPA            ' TO WS-PUT-CONT-NAME
                 WHEN 2
                    MOVE 'CIPB            ' TO WS-PUT-CONT-NAME
                 WHEN 3
                    MOVE 'CIPC            ' TO WS-PUT-CONT-NAME
                 WHEN 4
                    MOVE 'CIPD            ' TO WS-PUT-CONT-NAME
                 WHEN 5
                    MOVE 'CIPE            ' TO WS-PUT-CONT-NAME
                 WHEN 6
                    MOVE 'CIPF            ' TO WS-PUT-CONT-NAME
                 WHEN 7
                    MOVE 'CIPG            ' TO WS-PUT-CONT-NAME
                 WHEN 8
                    MOVE 'CIPH            ' TO WS-PUT-CONT-NAME
                 WHEN 9
                    MOVE 'CIPI            ' TO WS-PUT-CONT-NAME

              END-EVALUATE

      *
      *       Pass the details of the customer into a container
      *
              EXEC CICS PUT CONTAINER(WS-PUT-CONT-NAME)
                            FROM(DFHCOMMAREA)
                            FLENGTH(WS-PUT-CONT-LEN)
                            CHANNEL(WS-CHANNEL-NAME)
                            RESP(WS-CICS-RESP)
                            RESP2(WS-CICS-RESP2)
              END-EXEC

              IF WS-CICS-RESP NOT = DFHRESP(NORMAL)
                 MOVE 'N' TO COMM-SUCCESS
                 MOVE 'A' TO COMM-FAIL-CODE

                 DISPLAY 'Unsuccessful attempt to PUT CONTAINER. '
                         'CONTAINER=' WS-PUT-CONT-NAME 'CHANNEL='
                         WS-CHANNEL-NAME '. FLENGTH ='
                         WS-PUT-CONT-LEN '.'
                 DISPLAY '    RESP=' WS-CICS-RESP ' RESP2='
                         WS-CICS-RESP2

                 PERFORM GET-ME-OUT-OF-HERE
              END-IF

      *
      *       Issue the ASYNC transaction
      *
              EXEC CICS RUN TRANSID(WS-RUN-TRANSID)
                   CHANNEL(WS-CHANNEL-NAME)
                   CHILD(WS-ANY-CHILD-TKN)
                   RESP(WS-CICS-RESP)
                   RESP2(WS-CICS-RESP2)
              END-EXEC

              IF WS-CICS-RESP NOT = DFHRESP(NORMAL)
                 MOVE 'N' TO COMM-SUCCESS
                 MOVE 'B' TO COMM-FAIL-CODE

                 DISPLAY 'Unsuccessful attempt to RUN TRANSID. '
                         'TRANSID=' WS-RUN-TRANSID 'CHANNEL='
                         WS-CHANNEL-NAME '. TOKEN='
                         WS-ANY-CHILD-TKN
                 DISPLAY '    RESP=' WS-CICS-RESP ' RESP2='
                         WS-CICS-RESP2

                 PERFORM GET-ME-OUT-OF-HERE
              END-IF

      *
      *       Store away the CHANNEL and the TKN into an array
      *       for later use.
      *
              ADD 1 TO WS-CHILD-ISSUED-CNT
              MOVE WS-CHANNEL-NAME TO
                 WS-CHILD-CHAN (WS-CHILD-ISSUED-CNT)
              MOVE WS-ANY-CHILD-TKN TO
                 WS-CHILD-TKN (WS-CHILD-ISSUED-CNT)

      D       DISPLAY 'WS-CHILD-ARRAY=' WS-CHILD-ARRAY


           END-PERFORM.

      *
      *    Having sent all of the ASYNC requests, delay for
      *    3 seconds (to allow the request time to be processed)
      *    and then FETCH any data
      *
           EXEC CICS DELAY
              FOR SECONDS(3)
           END-EXEC.

           MOVE 'N' TO WS-FINISHED-FETCHING.
           MOVE 0 TO WS-RETRIEVED-CNT.
           MOVE 0 TO WS-TOTAL-CS-SCR.

           PERFORM UNTIL WS-FINISHED-FETCHING = 'Y'

              MOVE SPACES TO WS-ANY-CHILD-FETCH-ABCODE

      *
      *       Fetch an available reply immediately (without
      *       waiting).
      *
              EXEC CICS FETCH ANY(WS-ANY-CHILD-FETCH-TKN)
                   CHANNEL(WS-ANY-CHILD-FETCH-CHAN)
                   NOSUSPEND
                   COMPSTATUS(WS-CHILD-FETCH-COMPST)
                   ABCODE(WS-ANY-CHILD-FETCH-ABCODE)
                   RESP(WS-CICS-RESP)
                   RESP2(WS-CICS-RESP2)
              END-EXEC

              IF WS-CICS-RESP NOT = DFHRESP(NORMAL)
      *
      *          Check to see if the response was NOTFINISHED
      *          this means that not all credit agencies replied in
      *          time (for example we asked 5 and only 4 replied in
      *          the required time frame). So there is no more data
      *          to retrieve (the outstanding reply remains
      *          outstanding).
      *
                 IF WS-CICS-RESP = DFHRESP(NOTFINISHED) AND
                 WS-CICS-RESP2 = 52

      *
      *             If we retrieved nothing at all then it is an
      *             error
      *
                    IF WS-RETRIEVED-CNT = 0
                       MOVE 'Y' TO WS-FINISHED-FETCHING
                       MOVE 0 TO COMM-CREDIT-SCORE
                       MOVE 'Y' TO WS-CREDIT-CHECK-ERROR

                       STRING WS-ORIG-DATE-DD DELIMITED BY SIZE,
                              WS-ORIG-DATE-MM DELIMITED BY SIZE,
                              WS-ORIG-DATE-YYYY DELIMITED BY SIZE
                              INTO COMM-CS-REVIEW-DATE
                       END-STRING

                       MOVE 'N' TO COMM-SUCCESS
                       MOVE 'C' TO COMM-FAIL-CODE

                       DISPLAY 'EXEC CICS FETCH ANY failed. RESP='
                          WS-CICS-RESP ' RESP2=' WS-CICS-RESP2
                       DISPLAY '   NOTFINISHED (no data) was returned'
                       DISPLAY '   Exiting CRECUST. COMMAREA='
                          DFHCOMMAREA
                       PERFORM GET-ME-OUT-OF-HERE

                    ELSE
      *
      *                If we have previously retrieved some data from
      *                the credit checking agency/agencies then
      *                calculate the average credit score and a
      *                new random review date (sometime in the
      *                next 21 days)
      *
                       MOVE 'Y' TO WS-FINISHED-FETCHING
                       MOVE 'N' TO WS-CREDIT-CHECK-ERROR
      *
      *                Compute the average credit score from those
      *                credit agencies that responded
      *
                       COMPUTE WS-ACTUAL-CS-SCR = WS-TOTAL-CS-SCR /
                          WS-RETRIEVED-CNT
                       MOVE WS-ACTUAL-CS-SCR TO COMM-CREDIT-SCORE

      *
      *                Get today's date
      *
                       MOVE FUNCTION CURRENT-DATE
                          TO WS-CURRENT-DATE-DATA

                       MOVE WS-CURRENT-DATE-DATA (1:8)
                          TO WS-CURRENT-DATE-9

                      COMPUTE WS-TODAY-INT =
                          FUNCTION INTEGER-OF-DATE (WS-CURRENT-DATE-9)

      *
      *                Set up a random Credit Score review date
      *                within the next 21 days.
      *
                       MOVE EIBTASKN           TO WS-SEED

                       COMPUTE WS-REVIEW-DATE-ADD = ((21 - 1)
                                   * FUNCTION RANDOM(WS-SEED)) + 1

                       COMPUTE WS-NEW-REVIEW-DATE-INT =
                          WS-TODAY-INT + WS-REVIEW-DATE-ADD

      *
      *                Convert the integer date back to YYYYMMDD
      *                format
      *
                       COMPUTE WS-NEW-REVIEW-YYYYMMDD = FUNCTION
                          DATE-OF-INTEGER (WS-NEW-REVIEW-DATE-INT)

                       MOVE WS-NEW-REVIEW-YYYYMMDD(1:4) TO
                          COMM-CS-REVIEW-DATE(5:4)
                       MOVE WS-NEW-REVIEW-YYYYMMDD(5:2) TO
                          COMM-CS-REVIEW-DATE(3:2)
                       MOVE WS-NEW-REVIEW-YYYYMMDD(7:2) TO
                          COMM-CS-REVIEW-DATE(1:2)

                    END-IF
                 END-IF
      *
      *          Check to see if the response was INVREQ
      *          this means that the parent never had any children
      *
                 IF WS-CICS-RESP = DFHRESP(INVREQ) AND
                 WS-CICS-RESP2 = 1

                    MOVE 0 TO COMM-CREDIT-SCORE

                    STRING WS-ORIG-DATE-DD DELIMITED BY SIZE,
                           WS-ORIG-DATE-MM DELIMITED BY SIZE,
                           WS-ORIG-DATE-YYYY DELIMITED BY SIZE
                           INTO COMM-CS-REVIEW-DATE
                    END-STRING

                    MOVE 'N' TO COMM-SUCCESS
                    MOVE 'D' TO COMM-FAIL-CODE

                    DISPLAY 'EXEC CICS FETCH ANY failed. RESP='
                       WS-CICS-RESP ' RESP2=' WS-CICS-RESP2
                    DISPLAY '   INVREQ (no data) was returned'
                    DISPLAY '   Exiting CRECUST. COMMAREA='
                       DFHCOMMAREA
                    PERFORM GET-ME-OUT-OF-HERE

                 END-IF

      *
      *          Check to see if we are finished yet.
      *
                 IF WS-CICS-RESP = DFHRESP(NOTFND) AND
                 WS-CICS-RESP2 = 1

      *
      *             If we retrieved nothing at all then it is an
      *             error
      *
                    IF WS-RETRIEVED-CNT = 0
                       MOVE 'Y' TO WS-FINISHED-FETCHING
                       MOVE 0 TO COMM-CREDIT-SCORE

                       STRING WS-ORIG-DATE-DD DELIMITED BY SIZE,
                              WS-ORIG-DATE-MM DELIMITED BY SIZE,
                              WS-ORIG-DATE-YYYY DELIMITED BY SIZE
                              INTO COMM-CS-REVIEW-DATE
                       END-STRING
                       MOVE 'Y' TO WS-CREDIT-CHECK-ERROR

                    ELSE
      *
      *                If we have previously retrieved data from the
      *                credit checking agency/agencies then calculate
      *                the average credit score and a new random
      *                review date (sometime in the next 21 days)
      *
                       MOVE 'Y' TO WS-FINISHED-FETCHING
                       MOVE 'N' TO WS-CREDIT-CHECK-ERROR
      *
      *                Compute the average credit score from those
      *                credit agencies that responded
      *
                       COMPUTE WS-ACTUAL-CS-SCR = WS-TOTAL-CS-SCR /
                          WS-RETRIEVED-CNT
                       MOVE WS-ACTUAL-CS-SCR TO COMM-CREDIT-SCORE

      *
      *                Get today's date
      *
                       MOVE FUNCTION CURRENT-DATE
                          TO WS-CURRENT-DATE-DATA

                       MOVE WS-CURRENT-DATE-DATA (1:8)
                          TO WS-CURRENT-DATE-9

                      COMPUTE WS-TODAY-INT =
                          FUNCTION INTEGER-OF-DATE (WS-CURRENT-DATE-9)
      *
      *                Set up a random Credit Score review date
      *                within the next 21 days.
      *
                       MOVE EIBTASKN           TO WS-SEED

                       COMPUTE WS-REVIEW-DATE-ADD = ((21 - 1)
                                   * FUNCTION RANDOM(WS-SEED)) + 1

                       COMPUTE WS-NEW-REVIEW-DATE-INT =
                          WS-TODAY-INT + WS-REVIEW-DATE-ADD

      *
      *                Convert the integer date back to YYYYMMDD
      *                format
      *
                       COMPUTE WS-NEW-REVIEW-YYYYMMDD = FUNCTION
                          DATE-OF-INTEGER (WS-NEW-REVIEW-DATE-INT)

                       MOVE WS-NEW-REVIEW-YYYYMMDD(1:4) TO
                          COMM-CS-REVIEW-DATE(5:4)
                       MOVE WS-NEW-REVIEW-YYYYMMDD(5:2) TO
                          COMM-CS-REVIEW-DATE(3:2)
                       MOVE WS-NEW-REVIEW-YYYYMMDD(7:2) TO
                          COMM-CS-REVIEW-DATE(1:2)

                    END-IF
                 END-IF
              ELSE

      *       If it is a NORMAL RESPONSE code we need to check
      *       the COMP-STATUS
      *

                 EVALUATE WS-CHILD-FETCH-COMPST

                    WHEN DFHVALUE(NORMAL)
      *
      *                Set up the correct container name
      *
                       EVALUATE WS-ANY-CHILD-FETCH-TKN

                          WHEN WS-CHILD-TKN(1)
                             MOVE 'CIPA            '
                                TO WS-CONTAINER-NAME
                          WHEN WS-CHILD-TKN(2)
                             MOVE 'CIPB            '
                                TO WS-CONTAINER-NAME
                          WHEN WS-CHILD-TKN(3)
                             MOVE 'CIPC            '
                                TO WS-CONTAINER-NAME
                          WHEN WS-CHILD-TKN(4)
                             MOVE 'CIPD            '
                                TO WS-CONTAINER-NAME
                          WHEN WS-CHILD-TKN(5)
                             MOVE 'CIPE            '
                                TO WS-CONTAINER-NAME
                          WHEN WS-CHILD-TKN(6)
                             MOVE 'CIPF            '
                                TO WS-CONTAINER-NAME
                          WHEN WS-CHILD-TKN(7)
                             MOVE 'CIPG            '
                                TO WS-CONTAINER-NAME
                          WHEN WS-CHILD-TKN(8)
                             MOVE 'CIPH            '
                                TO WS-CONTAINER-NAME
                          WHEN WS-CHILD-TKN(9)
                             MOVE 'CIPI            '
                                TO WS-CONTAINER-NAME
                       END-EVALUATE

                       MOVE 261 TO WS-CHILD-CONTAINER-LEN

                       EXEC CICS GET CONTAINER(WS-CONTAINER-NAME)
                            CHANNEL(WS-ANY-CHILD-FETCH-CHAN)
                            INTO(WS-CHILD-DATA)
                            FLENGTH(WS-CHILD-CONTAINER-LEN)
                            RESP(WS-CICS-RESP)
                            RESP2(WS-CICS-RESP2)
                       END-EXEC

                       IF WS-CICS-RESP NOT = DFHRESP(NORMAL)
                          MOVE 0 TO COMM-CREDIT-SCORE

                          STRING WS-ORIG-DATE-DD DELIMITED BY SIZE,
                                 WS-ORIG-DATE-MM DELIMITED BY SIZE,
                                 WS-ORIG-DATE-YYYY DELIMITED BY SIZE
                                 INTO COMM-CS-REVIEW-DATE
                          END-STRING

                          MOVE 'N' TO COMM-SUCCESS
                          MOVE 'E' TO COMM-FAIL-CODE

                          DISPLAY 'EXEC CICS FETCH ANY worked, '
                             'but GET CONTAINER failed.'
                             ' CONTAINER='  WS-CONTAINER-NAME
                             ' CHANNEL=' WS-ANY-CHILD-FETCH-CHAN
                             ' RESP='
                             WS-CICS-RESP ' RESP2=' WS-CICS-RESP2

                          DISPLAY '   Exiting CRECUST. COMMAREA='
                             DFHCOMMAREA
                          PERFORM GET-ME-OUT-OF-HERE
                       END-IF

      *
      *                If the GET CONTAINER was successful
      *
                       COMPUTE WS-RETRIEVED-CNT =
                          WS-RETRIEVED-CNT + 1
                       COMPUTE WS-TOTAL-CS-SCR =
                          WS-TOTAL-CS-SCR + WS-CHILD-DATA-CREDIT-SCORE

                    WHEN DFHVALUE(ABEND)
      *
      *                You are here because the completion status from
      *                the FETCH ANY is ABEND, so deal with that.
      *
                       MOVE 0 TO COMM-CREDIT-SCORE

                       STRING WS-ORIG-DATE-DD DELIMITED BY SIZE,
                              WS-ORIG-DATE-MM DELIMITED BY SIZE,
                              WS-ORIG-DATE-YYYY DELIMITED BY SIZE
                              INTO COMM-CS-REVIEW-DATE
                       END-STRING

                       MOVE 'N' TO COMM-SUCCESS
                       MOVE 'F' TO COMM-FAIL-CODE

                       PERFORM GET-ME-OUT-OF-HERE


                    WHEN DFHVALUE(SECERROR)
      *
      *                The completion status from the FETCH ANY is
      *                SECERROR
      *
                       MOVE 0 TO COMM-CREDIT-SCORE

                       STRING WS-ORIG-DATE-DD DELIMITED BY SIZE,
                              WS-ORIG-DATE-MM DELIMITED BY SIZE,
                              WS-ORIG-DATE-YYYY DELIMITED BY SIZE
                              INTO COMM-CS-REVIEW-DATE
                       END-STRING

                       MOVE 'N' TO COMM-SUCCESS
                       MOVE 'G' TO COMM-FAIL-CODE

                       DISPLAY 'EXEC CICS FETCH ANY worked, '
                          'but COMP-STATUS = SECERROR.'
                          ' RESP='
                          WS-CICS-RESP ' RESP2=' WS-CICS-RESP2
                       DISPLAY '   Exiting CRECUST. COMMAREA='
                          DFHCOMMAREA
                       PERFORM GET-ME-OUT-OF-HERE


                    WHEN OTHER
      *
      *                This catches any other completion status values
      *                from the FETCH ANY.
      *
                       MOVE 0 TO COMM-CREDIT-SCORE

                       STRING WS-ORIG-DATE-DD DELIMITED BY SIZE,
                              WS-ORIG-DATE-MM DELIMITED BY SIZE,
                              WS-ORIG-DATE-YYYY DELIMITED BY SIZE
                              INTO COMM-CS-REVIEW-DATE
                       END-STRING

                       MOVE 'N' TO COMM-SUCCESS
                       MOVE 'H' TO COMM-FAIL-CODE

                       DISPLAY 'EXEC CICS FETCH ANY worked, '
                          'but COMP-STATUS IS UNKNOWN.'
                          ' RESP='
                          WS-CICS-RESP ' RESP2=' WS-CICS-RESP2
                       DISPLAY '   Exiting CRECUST. COMMAREA='
                          DFHCOMMAREA
                       PERFORM GET-ME-OUT-OF-HERE

                 END-EVALUATE

              END-IF

           END-PERFORM.


       CC999.
           EXIT.


       WRITE-CUSTOMER-VSAM SECTION.
       WCV010.
      *
      *    Write a record to the CUSTOMER VSAM file
      *
           INITIALIZE OUTPUT-DATA.

           MOVE 'CUST'              TO CUSTOMER-EYECATCHER.
           MOVE SORTCODE            TO CUSTOMER-SORTCODE.
           MOVE NCS-CUST-NO-VALUE   TO CUSTOMER-NUMBER.
           MOVE COMM-NAME           TO CUSTOMER-NAME.
           MOVE COMM-ADDRESS        TO CUSTOMER-ADDRESS.
           MOVE COMM-DATE-OF-BIRTH  TO CUSTOMER-DATE-OF-BIRTH.
           MOVE COMM-CREDIT-SCORE   TO CUSTOMER-CREDIT-SCORE.
           MOVE COMM-CS-REVIEW-DATE TO CUSTOMER-CS-REVIEW-DATE.

           COMPUTE WS-CUST-REC-LEN = LENGTH OF OUTPUT-DATA.

           EXEC CICS WRITE
                FILE('CUSTOMER')
                FROM(OUTPUT-DATA)
                RIDFLD(CUSTOMER-KEY)
                LENGTH(WS-CUST-REC-LEN)
                KEYLENGTH(16)
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

                 EXEC CICS WRITE
                    FILE('CUSTOMER')
                    FROM(OUTPUT-DATA)
                    RIDFLD(CUSTOMER-KEY)
                    LENGTH(WS-CUST-REC-LEN)
                    KEYLENGTH(16)
                    RESP(WS-CICS-RESP)
                    RESP2(WS-CICS-RESP2)
                 END-EXEC

              END-PERFORM
           END-IF

      *
      *    Check if the WRITE was unsuccessful and take action.
      *
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)
              MOVE 'N' TO COMM-SUCCESS
              MOVE '1' TO COMM-FAIL-CODE
              PERFORM DEQ-NAMED-COUNTER
              PERFORM GET-ME-OUT-OF-HERE
           END-IF.

           INITIALIZE CUSTOMER-CONTROL
           MOVE ZERO TO CUSTOMER-CONTROL-SORTCODE
           MOVE ALL '9' TO CUSTOMER-CONTROL-NUMBER

           EXEC CICS READ FILE('CUSTOMER')
                RIDFLD(CUSTOMER-CONTROL-KEY)
                INTO(CUSTOMER-CONTROL)
                UPDATE
           END-EXEC

           ADD 1 TO NUMBER-OF-CUSTOMERS IN CUSTOMER-CONTROL-RECORD
           GIVING NUMBER-OF-CUSTOMERS IN CUSTOMER-CONTROL-RECORD
           MOVE CUSTOMER-NUMBER OF CUSTOMER-RECORD TO
           LAST-CUSTOMER-NUMBER IN CUSTOMER-CONTROL-RECORD

           EXEC CICS REWRITE FILE('CUSTOMER')
                FROM(CUSTOMER-CONTROL)
           END-EXEC

      *
      *    If the WRITE was successful then WRITE to PROCTRAN datastore
      *
           MOVE CUSTOMER-SORTCODE OF OUTPUT-DATA TO STORED-SORTCODE.
           MOVE CUSTOMER-NUMBER OF OUTPUT-DATA TO STORED-CUSTNO.
           MOVE CUSTOMER-NAME TO STORED-NAME.
           MOVE CUSTOMER-DATE-OF-BIRTH(1:2) TO STORED-DOB(1:2).
           MOVE '/' TO STORED-DOB(3:1).
           MOVE CUSTOMER-DATE-OF-BIRTH(3:2) TO STORED-DOB(4:2).
           MOVE '/' TO STORED-DOB(6:1).
           MOVE CUSTOMER-DATE-OF-BIRTH(5:4) TO STORED-DOB(7:4).

           PERFORM WRITE-PROCTRAN.

           PERFORM DEQ-NAMED-COUNTER.

      *
      *    Set up the missing data in the COMM AREA ready for return
      *
           MOVE CUSTOMER-SORTCODE OF OUTPUT-DATA
              TO COMM-SORTCODE.
           MOVE CUSTOMER-NUMBER OF OUTPUT-DATA
              TO COMM-NUMBER
           MOVE 'CUST' TO COMM-EYECATCHER.
           MOVE 'Y' TO COMM-SUCCESS.
           MOVE ' ' TO COMM-FAIL-CODE.

       WCV999.
           EXIT.


       WRITE-PROCTRAN SECTION.
       WP010.
              PERFORM WRITE-PROCTRAN-DB2.

       WP999.
           EXIT.


       WRITE-PROCTRAN-DB2 SECTION.
       WPD010.
      *
      *    Record the creation of a new CUSTOMER on PROCTRAN
      *
           INITIALIZE HOST-PROCTRAN-ROW.
           INITIALIZE WS-EIBTASKN12.

           MOVE 'PRTR' TO HV-PROCTRAN-EYECATCHER.
           MOVE SORTCODE TO HV-PROCTRAN-SORT-CODE.
           MOVE ZEROS TO HV-PROCTRAN-ACC-NUMBER.
           MOVE EIBTASKN TO WS-EIBTASKN12.
           MOVE WS-EIBTASKN12 TO HV-PROCTRAN-REF.

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

           MOVE STORED-SORTCODE TO HV-PROCTRAN-DESC(1:6).
           MOVE STORED-CUSTNO TO HV-PROCTRAN-DESC(7:10).
           MOVE STORED-NAME   TO HV-PROCTRAN-DESC(17:14).
           MOVE STORED-DOB    TO HV-PROCTRAN-DESC(31:10).

           MOVE 'OCC'         TO HV-PROCTRAN-TYPE.
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
      *    Check the SQLCODE
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

              PERFORM POPULATE-TIME-DATE2

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

              STRING 'WPD010  - Unable to write to PROCTRAN DB2 '
                    DELIMITED BY SIZE,
                    'datastore with the following data:'
                    DELIMITED BY SIZE,
                    HOST-PROCTRAN-ROW
                    DELIMITED BY SIZE,
                    ' EIBRESP=' DELIMITED BY SIZE,
                    ABND-RESPCODE DELIMITED BY SIZE,
                    ' RESP2=' DELIMITED BY SIZE,
                    ABND-RESP2CODE DELIMITED BY SIZE
                    INTO ABND-FREEFORM
              END-STRING

              EXEC CICS LINK PROGRAM(WS-ABEND-PGM)
                        COMMAREA(ABNDINFO-REC)
              END-EXEC

              DISPLAY 'In CRECUST(WPD010) '
              'UNABLE TO WRITE TO PROCTRAN DB2 DATASTORE'
              ' SQLCODE=' SQLCODE-DISPLAY
              'WITH THE FOLLOWING DATA:' HOST-PROCTRAN-ROW


              PERFORM DEQ-NAMED-COUNTER

              EXEC CICS ABEND
                    ABCODE ('HWPT')
              END-EXEC
           END-IF.

       WPD999.
           EXIT.


       GET-ME-OUT-OF-HERE SECTION.
       GMOFH010.
      *
      *    Finish
      *
           EXEC CICS RETURN
           END-EXEC.

       GMOFH999.
           EXIT.


       GET-LAST-CUSTOMER-VSAM SECTION.
       GLCV010.

           INITIALIZE CUSTOMER-CONTROL
           MOVE ZERO TO CUSTOMER-CONTROL-SORTCODE
           MOVE ALL '9' TO CUSTOMER-CONTROL-NUMBER

           EXEC CICS READ FILE('CUSTOMER')
                     RIDFLD(CUSTOMER-CONTROL-KEY)
                     UPDATE
                     INTO(CUSTOMER-CONTROL)
                     RESP(WS-CICS-RESP)
                     RESP2(WS-CICS-RESP2)
           END-EXEC

           IF WS-CICS-RESP = DFHRESP(SYSIDERR)
             PERFORM VARYING SYSIDERR-RETRY FROM 1 BY 1
             UNTIL SYSIDERR-RETRY > 100
             OR WS-CICS-RESP = DFHRESP(NORMAL)
             OR WS-CICS-RESP IS NOT EQUAL TO DFHRESP(SYSIDERR)
               EXEC CICS DELAY FOR SECONDS(3)
               END-EXEC

               EXEC CICS READ FILE('CUSTOMER')
                         RIDFLD(CUSTOMER-CONTROL-KEY)
                         UPDATE
                         INTO(CUSTOMER-CONTROL)
                         RESP(WS-CICS-RESP)
                         RESP2(WS-CICS-RESP2)
               END-EXEC

             END-PERFORM
           ELSE
             IF WS-CICS-RESP IS NOT = DFHRESP(NORMAL)
               MOVE 'N' TO COMM-SUCCESS
               MOVE '4' TO COMM-FAIL-CODE
               PERFORM DEQ-NAMED-COUNTER
               PERFORM GET-ME-OUT-OF-HERE
             END-IF
           END-IF.
           ADD 1 TO LAST-CUSTOMER-NUMBER IN CUSTOMER-CONTROL
           GIVING LAST-CUSTOMER-NUMBER IN CUSTOMER-CONTROL

           EXEC CICS REWRITE FILE('CUSTOMER')
                FROM(CUSTOMER-CONTROL)
                RESP(WS-CICS-RESP)
                RESP2(WS-CICS-RESP2)
           END-EXEC

           IF WS-CICS-RESP = DFHRESP(SYSIDERR)
              PERFORM VARYING SYSIDERR-RETRY FROM 1 BY 1
              UNTIL SYSIDERR-RETRY > 100
              OR WS-CICS-RESP = DFHRESP(NORMAL)
              OR WS-CICS-RESP IS NOT EQUAL TO DFHRESP(SYSIDERR)
                 EXEC CICS DELAY FOR SECONDS(3)
                 END-EXEC

                 EXEC CICS REWRITE FILE('CUSTOMER')
                    FROM(CUSTOMER-CONTROL)
                    RESP(WS-CICS-RESP)
                    RESP2(WS-CICS-RESP2)
                 END-EXEC
              END-PERFORM
           ELSE
              IF WS-CICS-RESP IS NOT EQUAL TO DFHRESP(NORMAL)
                 MOVE 'N' TO COMM-SUCCESS
                 MOVE '4' TO COMM-FAIL-CODE

                 PERFORM DEQ-NAMED-COUNTER
                 PERFORM GET-ME-OUT-OF-HERE
              END-IF
           END-IF

           MOVE LAST-CUSTOMER-NUMBER OF CUSTOMER-CONTROL  TO
              COMM-NUMBER CUSTOMER-NUMBER REQUIRED-CUST-NUMBER2
              NCS-CUST-NO-VALUE.

       GLCV999.
           EXIT.


       DATE-OF-BIRTH-CHECK SECTION.
       DOBC010.
      *
      *    Ensure that the Date Of Birth is valid
      *
           IF COMM-BIRTH-YEAR < 1601
              MOVE 'Y' TO WS-DATE-OF-BIRTH-ERROR
              MOVE 'O' TO COMM-FAIL-CODE
              GO TO DOBC999
           END-IF.

           MOVE COMM-BIRTH-YEAR TO CEEDAYS-YEAR.
           MOVE COMM-BIRTH-MONTH TO CEEDAYS-MONTH.
           MOVE COMM-BIRTH-DAY TO CEEDAYS-DAY.

           CALL "CEEDAYS" USING DATE-OF-BIRTH-FOR-CEEDAYS
                                DATE-OF-BIRTH-FORMAT,
                                WS-DATE-OF-BIRTH-LILLIAN,
                                FC.

           IF NOT CEE000 OF FC THEN
              MOVE 'Y' TO WS-DATE-OF-BIRTH-ERROR
              MOVE 'Z' TO COMM-FAIL-CODE
              DISPLAY 'CEEDAYS failed, FORMAT LENGTH 10 with msg '
                 MSG-NO OF FC
                 ' for date YYYYMMDD' DATE-OF-BIRTH-FOR-CEEDAYS
              GO TO DOBC999
           END-IF.

           CALL "CEELOCT" USING WS-TODAY-LILLIAN,
                                WS-TODAY-SECONDS,
                                WS-TODAY-GREGORIAN,
                                FC.

           IF NOT CEE000 OF FC THEN
              MOVE 'Y' TO WS-DATE-OF-BIRTH-ERROR
              DISPLAY 'CEEDLOCT failed with msg '
                 MSG-NO OF FC
              GO TO DOBC999
           END-IF.

           SUBTRACT COMM-BIRTH-YEAR FROM WS-TODAY-G-YEAR
              GIVING WS-CUSTOMER-AGE

           IF WS-CUSTOMER-AGE > 150
              MOVE 'Y' TO WS-DATE-OF-BIRTH-ERROR
              MOVE 'O' TO COMM-FAIL-CODE
              GO TO DOBC999
           END-IF.

           IF WS-TODAY-LILLIAN < WS-DATE-OF-BIRTH-LILLIAN
                        MOVE 'Y' TO WS-DATE-OF-BIRTH-ERROR
              MOVE 'Y' TO COMM-FAIL-CODE
           END-IF.

       DOBC999.
           EXIT.


       POPULATE-TIME-DATE2 SECTION.
       PTD2010.
      D    DISPLAY 'POPULATE-TIME-DATE2 SECTION'.

           EXEC CICS ASKTIME
              ABSTIME(WS-U-TIME)
           END-EXEC.

           EXEC CICS FORMATTIME
                     ABSTIME(WS-U-TIME)
                     DDMMYYYY(WS-ORIG-DATE)
                     TIME(WS-TIME-NOW)
                     DATESEP
           END-EXEC.

       PTD2999.
           EXIT.

