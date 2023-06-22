       PROCESS CICS,NODYNAM,NSYMBOL(NATIONAL),TRUNC(STD)
       CBL CICS('SP,EDF')
      ******************************************************************
      *                                                                *
      *  Copyright IBM Corp. 2023                                      *
      *                                                                *
      ******************************************************************


      ******************************************************************
      * This is the Update Account program in the BANKING application
      * BMS suite.
      *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BNK1UAC.
       AUTHOR. Jon Collett.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      * SOURCE-COMPUTER.   IBM-370 WITH DEBUGGING MODE.
       SOURCE-COMPUTER.  IBM-370.
       OBJECT-COMPUTER.  IBM-370.

       INPUT-OUTPUT SECTION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 WS-CICS-WORK-AREA.
          03 WS-CICS-RESP              PIC S9(8) COMP VALUE 0.
          03 WS-CICS-RESP2             PIC S9(8) COMP VALUE 0.

       01 WS-FAIL-INFO.
          03 FILLER                    PIC X(9) VALUE 'BNK1UAC '.
          03 WS-CICS-FAIL-MSG          PIC X(70) VALUE ' '.
          03 FILLER                    PIC X(6)  VALUE ' RESP='.
          03 WS-CICS-RESP-DISP         PIC 9(10) VALUE 0.
          03 FILLER                    PIC X(7)  VALUE ' RESP2='.
          03 WS-CICS-RESP2-DISP        PIC 9(10) VALUE 0.
          03 FILLER                    PIC X(15)
                                          VALUE ' ABENDING TASK.'.

       01 SWITCHES.
           03 VALID-DATA-SW            PIC X VALUE 'Y'.
              88 VALID-DATA               VALUE 'Y'.

       01 FLAGS.
           03 SEND-FLAG                PIC X.
              88 SEND-ERASE               VALUE '1'.
              88 SEND-DATAONLY            VALUE '2'.
              88 SEND-DATAONLY-ALARM      VALUE '3'.

       01 ACTION-ALPHA.
           03 ACTION-NUM               PIC 9.

      *
      * The end of session message
      *
       01 END-OF-SESSION-MESSAGE       PIC X(13) VALUE 'Session Ended'.

       01 RESPONSE-CODE                PIC S9(8) COMP.

       01 COMMUNICATION-AREA           PIC X.

       COPY BNK1UAM.

       COPY DFHAID.


       01 AMTI9                        PIC 9(12).

       01 WS-STUFF1.
           03 WS-COMM-ACT-BAL-UNSIGN   PIC 9(12).

       01 WS-STUFF2 REDEFINES WS-STUFF1.
           03 WS-COMM-ACT-BAL-X        PIC X(12).

       01 WS-STUFF3.
           03 WS-COMM-AMT-UNSIGN          PIC 9(12).
       01 WS-STUFF4 REDEFINES WS-STUFF3.
           03 WS-COMM-AMT-X               PIC X(12).

       01 WS-STUFF5.
            03 WS-COMM-AV-BAL-UNSIGN      PIC 9(12).

       01 WS-STUFF6 REDEFINES WS-STUFF5.
           03 WS-COMM-AV-BAL-X            PIC X(12).

       01 WS-LSTMTDDI                  PIC 99.
       01 WS-LSTMTMMI                  PIC 99.
       01 WS-LSTMTYYI                  PIC 9999.

       01 WS-NSTMTDDI                  PIC 99.
       01 WS-NSTMTMMI                  PIC 99.
       01 WS-NSTMTYYI                  PIC 9999.

       01 WS-DATE-SPLIT.
           03 WS-DATE-SPLIT-DD         PIC 99.
           03 WS-DATE-SPLIT-MM         PIC 99.
           03 WS-DATE-SPLIT-YY         PIC 9999.

       01 WS-CONVERSION.
           03 WS-CONVERT-PICX          PIC X(14).
           03 WS-CONVERT-SPLIT REDEFINES WS-CONVERT-PICX.
              05 WS-CONVERT-SIGN          PIC X.
              05 WS-CONVERT-DEC           PIC 9(10).
              05 WS-CONVERT-POINT         PIC X.
              05 WS-CONVERT-REMAIN        PIC 99.

       01 WS-CONVERTED-VAL1            PIC S9(10)V99 VALUE 0.
       01 WS-CONVERTED-VAL2            PIC S9(10)V99 VALUE 0.
       01 WS-CONVERTED-VAL3            PIC S9(10)V99 VALUE 0.
       01 WS-CONVERTED-VAL4            PIC S9(10)V99 VALUE 0.

       01 INT-RT-CONVERT.
          03 INT-RT-X                  PIC X(6).
          03 INT-RT-GROUP REDEFINES INT-RT-X.
             05 INT-RT-9                 PIC 9(4)V99.

       01 WS-COMM-AREA.
          03 WS-COMM-EYE               PIC X(4).
          03 WS-COMM-CUSTNO            PIC X(10).
          03 WS-COMM-SCODE             PIC X(6).
          03 WS-COMM-ACCNO             PIC 9(8).
          03 WS-COMM-ACC-TYPE          PIC X(8).
          03 WS-COMM-INT-RATE          PIC 9(4)V99.
          03 WS-COMM-OPENED            PIC 9(8).
          03 WS-COMM-OVERDRAFT         PIC 9(8).
          03 WS-COMM-LAST-STMT-DT      PIC 9(8).
          03 WS-COMM-NEXT-STMT-DT      PIC 9(8).
          03 WS-COMM-AVAIL-BAL         PIC S9(10)V99.
          03 WS-COMM-ACTUAL-BAL        PIC S9(10)V99.
          03 WS-COMM-SUCCESS           PIC X.

       01 COMPANY-NAME-FULL            PIC X(32).

       01 INTRTI-COMP-1 USAGE COMP-1.

       01 INTRT-PIC9                   PIC 9999.99.

       01 WS-NUM-COUNT-POINT           PIC S9(8) BINARY.
       01 WS-NUM-COUNT-TOTAL           PIC S9(8) BINARY.

       01 AVAILABLE-BALANCE-DISPLAY    PIC +9(10).99.
       01 ACTUAL-BALANCE-DISPLAY       PIC +9(10).99.

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


       01 WS-TIME-DATA.
           03 WS-TIME-NOW              PIC 9(6).
           03 WS-TIME-NOW-GRP REDEFINES WS-TIME-NOW.
              05 WS-TIME-NOW-GRP-HH       PIC 99.
              05 WS-TIME-NOW-GRP-MM       PIC 99.
              05 WS-TIME-NOW-GRP-SS       PIC 99.

       01 WS-ABEND-PGM                 PIC X(8) VALUE 'ABNDPROC'.

       01 ABNDINFO-REC.
           COPY ABNDINFO.


       LINKAGE SECTION.
       01 DFHCOMMAREA.
          03 COMM-EYE                  PIC X(4).
          03 COMM-CUSTNO               PIC X(10).
          03 COMM-SCODE                PIC X(6).
          03 COMM-ACCNO                PIC 9(8).
          03 COMM-ACC-TYPE             PIC X(8).
          03 COMM-INT-RATE             PIC 9(4)V99.
          03 COMM-OPENED               PIC 9(8).
          03 COMM-OVERDRAFT            PIC 9(8).
          03 COMM-LAST-STMT-DT         PIC 9(8).
          03 COMM-NEXT-STMT-DT         PIC 9(8).
          03 COMM-AVAIL-BAL            PIC S9(10)V99.
          03 COMM-ACTUAL-BAL           PIC S9(10)V99.
          03 COMM-SUCCESS              PIC X.
          03 COMM-PCB1-POINTER         POINTER.


       PROCEDURE DIVISION.
       PREMIERE SECTION.
       A010.

           EVALUATE TRUE
      *
      *       Is it the first time through? If so, send the map
      *       with erased (empty) data fields.
      *
              WHEN EIBCALEN = ZERO
                 MOVE LOW-VALUE TO BNK1UAO
                 MOVE -1 TO ACCNOL
                 SET SEND-ERASE TO TRUE
                 INITIALIZE WS-COMM-AREA
                 PERFORM SEND-MAP

      *
      *       If a PA key is pressed, just carry on
      *
              WHEN EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
                 CONTINUE

      *
      *       When Pf3 is pressed, return to the main menu
      *
              WHEN EIBAID = DFHPF3
                 EXEC CICS RETURN
                    TRANSID('OMEN')
                    IMMEDIATE
                    RESP(WS-CICS-RESP)
                    RESP2(WS-CICS-RESP2)
                 END-EXEC

      *
      *       If the aid or Pf12 is pressed, then send a termination
      *       message.
      *
              WHEN EIBAID = DFHAID OR DFHPF12
                 PERFORM SEND-TERMINATION-MSG
                 EXEC CICS
                    RETURN
                 END-EXEC

      *
      *       When CLEAR is pressed
      *
              WHEN EIBAID = DFHCLEAR
                EXEC CICS SEND CONTROL
                   ERASE
                   FREEKB
                END-EXEC

                EXEC CICS RETURN
                END-EXEC

      *
      *       When enter is pressed then process the content
      *
              WHEN EIBAID = DFHENTER
                 PERFORM PROCESS-MAP

      *
      *       When Pf5 is pressed then process the content
      *
              WHEN EIBAID = DFHPF5
                 PERFORM PROCESS-MAP

      *
      *       When anything else happens, send the invalid key message
      *
              WHEN OTHER
                 MOVE LOW-VALUES TO BNK1UAO
                 MOVE 'Invalid key pressed.' TO MESSAGEO
                 MOVE -1 TO ACCNOL
                 SET SEND-DATAONLY-ALARM TO TRUE
                 PERFORM SEND-MAP

           END-EVALUATE.

      *
      *    If it is not the first time through, set the return
      *    information accordingly.
      *
      *
           IF EIBCALEN NOT = ZERO
              MOVE COMM-EYE            TO WS-COMM-EYE
              MOVE COMM-CUSTNO         TO WS-COMM-CUSTNO
              MOVE COMM-SCODE          TO WS-COMM-SCODE
              MOVE COMM-ACCNO          TO WS-COMM-ACCNO
              MOVE COMM-ACC-TYPE       TO WS-COMM-ACC-TYPE
              MOVE COMM-INT-RATE       TO WS-COMM-INT-RATE
              MOVE COMM-OPENED         TO WS-COMM-OPENED
              MOVE COMM-OVERDRAFT      TO WS-COMM-OVERDRAFT
              MOVE COMM-LAST-STMT-DT   TO WS-COMM-LAST-STMT-DT
              MOVE COMM-NEXT-STMT-DT   TO WS-COMM-NEXT-STMT-DT
              MOVE COMM-AVAIL-BAL      TO WS-COMM-AVAIL-BAL
              MOVE COMM-ACTUAL-BAL     TO WS-COMM-ACTUAL-BAL
           END-IF.

           EXEC CICS
              RETURN TRANSID('OUAC')
              COMMAREA(WS-COMM-AREA)
              LENGTH(99)
              RESP(WS-CICS-RESP)
              RESP2(WS-CICS-RESP2)
           END-EXEC.

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
              MOVE 'HBNK'      TO ABND-CODE

              EXEC CICS ASSIGN PROGRAM(ABND-PROGRAM)
              END-EXEC

              MOVE ZEROS      TO ABND-SQLCODE

              STRING 'A010 - RETURN TRANSID(OUAC) FAIL.'
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

              INITIALIZE WS-FAIL-INFO
              MOVE 'BNK1UAC - A010 - RETURN TRANSID(OUAC) FAIL' TO
                 WS-CICS-FAIL-MSG
              MOVE WS-CICS-RESP  TO WS-CICS-RESP-DISP
              MOVE WS-CICS-RESP2 TO WS-CICS-RESP2-DISP
              PERFORM ABEND-THIS-TASK
           END-IF.

       A999.
           EXIT.


       PROCESS-MAP SECTION.
       PM010.
      *
      *    Retrieve the data from the map
      *
           PERFORM RECEIVE-MAP.
           MOVE 'Y' TO VALID-DATA-SW
      *
      *    If enter was pressed, validate the received data
      *
           IF EIBAID = DFHENTER
              PERFORM EDIT-DATA
      *
      *       If the data passes validation go on to
      *       get the account
      *
              IF VALID-DATA-SW = 'Y'
                 PERFORM INQ-ACC-DATA
              END-IF

           END-IF.

      *
      *    If Pf5 was pressed, validate the received data
      *
           IF EIBAID = DFHPF5
              PERFORM VALIDATE-DATA

      *
      *       If the data passes validation go on to
      *       update the account
      *
              IF VALID-DATA-SW = 'Y'
                 PERFORM UPD-ACC-DATA
              END-IF

           END-IF.

           SET SEND-DATAONLY-ALARM TO TRUE.
      *
      *    Output the data to the screen
      *
           PERFORM SEND-MAP.

       PM999.
           EXIT.


       RECEIVE-MAP SECTION.
       RM010.
      *
      *    Retrieve the data
      *
           EXEC CICS
              RECEIVE MAP('BNK1UA')
              MAPSET('BNK1UAM')
              INTO(BNK1UAI)
              RESP(WS-CICS-RESP)
              RESP2(WS-CICS-RESP2)
           END-EXEC.

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
              MOVE 'HBNK'      TO ABND-CODE

              EXEC CICS ASSIGN PROGRAM(ABND-PROGRAM)
              END-EXEC

              MOVE ZEROS      TO ABND-SQLCODE

              STRING 'RM010 - RECEIVE MAP FAIL.'
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


              INITIALIZE WS-FAIL-INFO
              MOVE 'BNKUAC - RM010 - RECEIVE MAP FAIL ' TO
                 WS-CICS-FAIL-MSG
              MOVE WS-CICS-RESP  TO WS-CICS-RESP-DISP
              MOVE WS-CICS-RESP2 TO WS-CICS-RESP2-DISP
              PERFORM ABEND-THIS-TASK
           END-IF.

       RM999.
           EXIT.


       EDIT-DATA SECTION.
       ED010.
      *
      *    Perform validation on the incoming fields
      *
           IF ACCNOI NOT NUMERIC
              MOVE 'Please enter an account number.   ' TO
                 MESSAGEO
              MOVE 'N' TO VALID-DATA-SW
           END-IF.

       ED999.
           EXIT.


       VALIDATE-DATA SECTION.
       VD010.
      *
      *    Perform more validation
      *
           IF ACTYPEI NOT = 'CURRENT ' AND
           ACTYPEI NOT = 'SAVING  ' AND
           ACTYPEI NOT = 'LOAN    ' AND
           ACTYPEI NOT = 'MORTGAGE' AND
           ACTYPEI NOT = 'ISA     '

              MOVE SPACES TO MESSAGEO
              STRING 'Account Type must be CURRENT, SAVING, LOAN, '
                 'MORTGAGE or ISA' DELIMITED BY SIZE,
                 '. Then press PF5.' DELIMITED BY SIZE
              INTO MESSAGEO
              MOVE -1 to ACTYPEL
              MOVE 'N' TO VALID-DATA-SW
           END-IF.

           IF INTRTL = ZERO
              MOVE SPACES TO MESSAGEO
              STRING 'Please supply a numeric interest rate '
                 'then press PF5.' DELIMITED BY SIZE
                 INTO MESSAGEO
              MOVE 'N' TO VALID-DATA-SW
              MOVE -1 TO INTRTL
              GO TO VD999
           END-IF.

           IF INTRTI(1:INTRTL) IS NOT NUMERIC
              MOVE ZERO TO WS-NUM-COUNT-TOTAL
              INSPECT INTRTI(1:INTRTL) TALLYING
                 WS-NUM-COUNT-TOTAL for ALL '0'
                 WS-NUM-COUNT-TOTAL for ALL '1'
                 WS-NUM-COUNT-TOTAL for ALL '2'
                 WS-NUM-COUNT-TOTAL for ALL '3'
                 WS-NUM-COUNT-TOTAL for ALL '4'
                 WS-NUM-COUNT-TOTAL for ALL '5'
                 WS-NUM-COUNT-TOTAL for ALL '6'
                 WS-NUM-COUNT-TOTAL for ALL '7'
                 WS-NUM-COUNT-TOTAL for ALL '8'
                 WS-NUM-COUNT-TOTAL for ALL '9'
                 WS-NUM-COUNT-TOTAL for ALL '.'
                 WS-NUM-COUNT-TOTAL for ALL '-'
                 WS-NUM-COUNT-TOTAL for ALL '+'
                 WS-NUM-COUNT-TOTAL for ALL ' '

      *
      *       So the idea here is that if there is a
      *       decimal point, the field is not numeric. But if it is
      *       1.1 then it is valid. So first of all we check to see
      *       that only the above chars are OK. We also want to
      *       tolerate the minus and plus signs. We don't support
      *       negative interest though.
      *       We tolerate trailing spaces too.
      *
              IF WS-NUM-COUNT-TOTAL < INTRTL
                 MOVE SPACES TO MESSAGEO
                 STRING 'Please supply a numeric interest rate'
                      DELIMITED BY SIZE,
                 INTO MESSAGEO
                 MOVE 'N' TO VALID-DATA-SW
                 MOVE -1 TO INTRTL
                 GO TO VD999
              END-IF
      *
      *       And let's check to make sure we only have
      *       0 to 1 decimal points.
      *
              MOVE ZERO TO WS-NUM-COUNT-POINT
              INSPECT INTRTI(1:INTRTL) TALLYING
                 WS-NUM-COUNT-POINT FOR ALL '.'

              IF WS-NUM-COUNT-POINT > 1
                 STRING 'Use one decimal point for interest rate '
                    DELIMITED BY SIZE,
                    'only' DELIMITED BY SIZE,
                 INTO MESSAGEO
                 MOVE 'N' TO VALID-DATA-SW
                 MOVE -1 TO INTRTL
                 GO TO VD999
              END-If
      *
      *       Now let's check to see if we have too
      *       many decimals!
      *
              IF WS-NUM-COUNT-POINT = 1
                 MOVE ZERO TO WS-NUM-COUNT-TOTAL
                 INSPECT INTRTI(1:INTRTL) TALLYING
                    WS-NUM-COUNT-TOTAL FOR CHARACTERS AFTER '.'

                 IF WS-NUM-COUNT-TOTAL > 2
      *
      *             There are more than 2 characters after the point
      *
                    MOVE ZERO TO WS-NUM-COUNT-TOTAL
                       WS-NUM-COUNT-POINT
                    INSPECT INTRTI(1:INTRTL) TALLYING
                       WS-NUM-COUNT-POINT FOR CHARACTERS BEFORE '.'

                    ADD 2 TO WS-NUM-COUNT-POINT
                       GIVING WS-NUM-COUNT-POINT

                    INSPECT INTRTI(WS-NUM-COUNT-POINT:INTRTL) TALLYING
                       WS-NUM-COUNT-TOTAL for ALL '0'
                       WS-NUM-COUNT-TOTAL for ALL '1'
                       WS-NUM-COUNT-TOTAL for ALL '2'
                       WS-NUM-COUNT-TOTAL for ALL '3'
                       WS-NUM-COUNT-TOTAL for ALL '4'
                       WS-NUM-COUNT-TOTAL for ALL '5'
                       WS-NUM-COUNT-TOTAL for ALL '6'
                       WS-NUM-COUNT-TOTAL for ALL '7'
                       WS-NUM-COUNT-TOTAL for ALL '8'
                       WS-NUM-COUNT-TOTAL for ALL '9'
                       WS-NUM-COUNT-TOTAL for ALL '-'
                       WS-NUM-COUNT-TOTAL for ALL '+'
                       AFTER '.'

                    IF WS-NUM-COUNT-TOTAL > 2
      *
      *                More than two of them are numeric
      *
                       MOVE SPACES TO MESSAGEO
                       STRING
                          'Only up to two decimal places '
                           DELIMITED BY SIZE,
                          'are supported' DELIMITED BY SIZE,
                       INTO MESSAGEO

                       MOVE 'N' TO VALID-DATA-SW
                       MOVE -1 TO INTRTL
                       GO TO VD999
                    END-IF
                 END-IF
              END-IF
           END-IF.

           COMPUTE INTRTI-COMP-1 = FUNCTION NUMVAL(INTRTI).

           IF INTRTI-COMP-1 < 0
              MOVE SPACES TO MESSAGEO
              STRING 'Please supply a zero or positive interest rate'
                 DELIMITED BY SIZE,
                 INTO MESSAGEO
              MOVE 'N' TO VALID-DATA-SW
              MOVE -1 TO INTRTL
              GO TO VD999
           END-IF.

           IF INTRTI-COMP-1 > 9999.99
              MOVE SPACES TO MESSAGEO
              STRING 'Please supply an interest rate less than '
                     DELIMITED BY SIZE,
                     '9999.99%' DELIMITED BY SIZE
              INTO MESSAGEO
              MOVE 'N' TO VALID-DATA-SW
              MOVE -1 TO INTRTL
              GO TO VD999
           END-IF.

           IF (ACTYPEI = 'LOAN    ' AND INTRTI = '0000.00') OR
           (ACTYPEI = 'MORTGAGE' AND INTRTI = '0000.00')
              MOVE SPACES TO MESSAGEO
              STRING
              'Interest rate cannot be 0 with this account type.'
              ' Correct and press PF5.' delimited by size into
                 MESSAGEO
              MOVE 'N' TO VALID-DATA-SW
              MOVE -1  TO INTRTL
              GO TO VD999
           END-IF.

           IF(OVERDRL = ZERO OR OVERDRI(1:OVERDRL) IS NOT NUMERIC)
               MOVE 'Overdraft must be numeric. Correct and press PF5.'
                  TO MESSAGEO
              MOVE 'N' TO VALID-DATA-SW
              MOVE -1  TO OVERDRL
              GO TO VD999
           END-IF.

           IF LSTMTDDI NOT NUMERIC OR
           LSTMTMMI NOT NUMERIC OR
           LSTMTYYI NOT NUMERIC
              MOVE 'Last statement date must be numeric      ' TO
                 MESSAGEO
              MOVE 'N' TO VALID-DATA-SW
              GO TO VD999
           END-IF.

           IF NSTMTDDI NOT NUMERIC OR
           NSTMTMMI NOT NUMERIC OR
           NSTMTYYI NOT NUMERIC
              MOVE 'Next statement date must be numeric      ' TO
                 MESSAGEO
              MOVE 'N' TO VALID-DATA-SW
              GO TO VD999
           END-IF.

           MOVE LSTMTDDI TO WS-LSTMTDDI.
           MOVE LSTMTMMI TO WS-LSTMTMMI.
           MOVE LSTMTYYI TO WS-LSTMTYYI.

           IF WS-LSTMTDDI > 31 OR
           WS-LSTMTDDI = 0 OR
           WS-LSTMTMMI > 12 OR
           WS-LSTMTMMI = 0

              MOVE 'Incorrect date for LAST STATEMENT.      '
                 TO MESSAGEO
              MOVE 'N' TO VALID-DATA-SW
           END-IF.

           IF (WS-LSTMTDDI = 31 AND WS-LSTMTMMI = 9) OR
           (WS-LSTMTDDI = 31 AND WS-LSTMTMMI = 4) OR
           (WS-LSTMTDDI = 31 AND WS-LSTMTMMI = 6) OR
           (WS-LSTMTDDI = 31 AND WS-LSTMTMMI = 11) OR
           (WS-LSTMTDDI > 29 AND WS-LSTMTMMI = 2)
               MOVE 'Incorrect date for LAST STATEMENT.      '
                  TO MESSAGEO
              MOVE 'N' TO VALID-DATA-SW
           END-IF.

           MOVE NSTMTDDI TO WS-NSTMTDDI.
           MOVE NSTMTMMI TO WS-NSTMTMMI.
           MOVE NSTMTYYI TO WS-NSTMTYYI.

           IF (WS-NSTMTDDI = 31 AND WS-NSTMTMMI = 9) OR
           (WS-NSTMTDDI = 31 AND WS-NSTMTMMI = 4) OR
           (WS-NSTMTDDI = 31 AND WS-NSTMTMMI = 6) OR
           (WS-NSTMTDDI = 31 AND WS-NSTMTMMI = 11) OR
           (WS-NSTMTDDI > 29 AND WS-NSTMTMMI = 2)
              MOVE 'Incorrect date for NEXT STATEMENT.      '
                 TO MESSAGEO
              MOVE 'N' TO VALID-DATA-SW
           END-IF.

           IF (WS-NSTMTDDI = 31 AND WS-NSTMTMMI = 9) OR
           (WS-NSTMTDDI = 31 AND WS-NSTMTMMI = 4) OR
           (WS-NSTMTDDI = 31 AND WS-NSTMTMMI = 6) OR
           (WS-NSTMTDDI = 31 AND WS-NSTMTMMI = 11) OR
           (WS-NSTMTDDI > 29 AND WS-NSTMTMMI = 2)
               MOVE 'Incorrect date for NEXT STATEMENT.      '
                  TO MESSAGEO
              MOVE 'N' TO VALID-DATA-SW
           END-IF.

       VD999.
           EXIT.


       INQ-ACC-DATA SECTION.
       IAD010.
      *
      *    Set up the fields required by INQACC then link to it
      *
           INITIALIZE DFHCOMMAREA.

           MOVE ACCNOI TO COMM-ACCNO.
           SET COMM-PCB1-POINTER TO NULL.

           EXEC CICS LINK
              PROGRAM('INQACC')
              COMMAREA(DFHCOMMAREA)
              RESP(WS-CICS-RESP)
              RESP2(WS-CICS-RESP2)
              SYNCONRETURN
           END-EXEC.

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
              MOVE 'HBNK'      TO ABND-CODE

              EXEC CICS ASSIGN PROGRAM(ABND-PROGRAM)
              END-EXEC

              MOVE ZEROS      TO ABND-SQLCODE

              STRING 'IAD010  - LINK INQACC FAIL.'
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

              INITIALIZE WS-FAIL-INFO
              MOVE 'BNK1UAC - IAD010 - LINK INQACC  FAIL      '
                 TO WS-CICS-FAIL-MSG
              MOVE WS-CICS-RESP  TO WS-CICS-RESP-DISP
              MOVE WS-CICS-RESP2 TO WS-CICS-RESP2-DISP
              PERFORM ABEND-THIS-TASK
           END-IF.

      *
      *    Is it a valid record or not?
      *
           IF COMM-ACC-TYPE = SPACES AND
           COMM-LAST-STMT-DT = ZERO
              MOVE 'N' TO VALID-DATA-SW
              MOVE SPACES TO MESSAGEO
              MOVE 'This account number could not be found' TO
                 MESSAGEO
              MOVE -1 TO ACCNOL
           ELSE
              MOVE SPACES TO MESSAGEO
              MOVE 'Please amend fields and hit <pf5> to apply changes'
                 TO MESSAGEO
      *
      *       Move fields to the associated output fields
      *

              MOVE COMM-ACCNO       TO ACCNO2O

              MOVE COMM-CUSTNO      TO CUSTNOO
              MOVE COMM-SCODE       TO SORTCO
              MOVE COMM-ACC-TYPE    TO ACTYPEO
              MOVE COMM-INT-RATE    TO INTRT-PIC9
              MOVE INTRT-PIC9       TO INTRTO
              MOVE COMM-OPENED      TO WS-DATE-SPLIT
              MOVE WS-DATE-SPLIT-DD TO OPENDDO
              MOVE WS-DATE-SPLIT-MM TO OPENMMO
              MOVE WS-DATE-SPLIT-YY TO OPENYYO
              MOVE COMM-LAST-STMT-DT TO WS-DATE-SPLIT
              MOVE WS-DATE-SPLIT-DD TO LSTMTDDO
              MOVE WS-DATE-SPLIT-MM TO LSTMTMMO
              MOVE WS-DATE-SPLIT-YY TO LSTMTYYO
              MOVE COMM-NEXT-STMT-DT TO WS-DATE-SPLIT
              MOVE WS-DATE-SPLIT-DD TO NSTMTDDO
              MOVE WS-DATE-SPLIT-MM TO NSTMTMMO
              MOVE WS-DATE-SPLIT-YY TO NSTMTYYO
              MOVE COMM-OVERDRAFT   TO OVERDRO
              MOVE COMM-AVAIL-BAL   TO AVAILABLE-BALANCE-DISPLAY
              MOVE COMM-ACTUAL-BAL  TO ACTUAL-BALANCE-DISPLAY
              MOVE AVAILABLE-BALANCE-DISPLAY TO AVBALO
              MOVE ACTUAL-BALANCE-DISPLAY TO ACTBALO

              MOVE -1 TO ACCNOL

           END-IF.

       IAD999.
           EXIT.


       UPD-ACC-DATA SECTION.
       UAD010.
      *
      *    Set up the fields required by UPDACC then link to it
      *
           INITIALIZE DFHCOMMAREA.

           MOVE ACCNOI        TO COMM-ACCNO.
           IF ACCNOI = 99999999
             MOVE ACCNO2I        TO COMM-ACCNO
           END-IF
           MOVE CUSTNOI       TO COMM-CUSTNO.
           MOVE SORTCI        TO COMM-SCODE.
           MOVE ACTYPEI       TO COMM-ACC-TYPE.
           COMPUTE INTRTI-COMP-1 = FUNCTION NUMVAL(INTRTI).
           MOVE INTRTI-COMP-1 TO INT-RT-9.
           MOVE INT-RT-9      TO COMM-INT-RATE.
           MOVE OPENDDI       TO WS-DATE-SPLIT-DD.
           MOVE OPENMMI       TO  WS-DATE-SPLIT-MM.
           MOVE OPENYYI       TO WS-DATE-SPLIT-YY.
           MOVE WS-DATE-SPLIT TO COMM-OPENED.
           COMPUTE COMM-OVERDRAFT = FUNCTION NUMVAL(OVERDRI(1:OVERDRL)).
           MOVE LSTMTDDI      TO WS-DATE-SPLIT-DD.
           MOVE LSTMTMMI      TO WS-DATE-SPLIT-MM.
           MOVE LSTMTYYI      TO WS-DATE-SPLIT-YY.
           MOVE WS-DATE-SPLIT TO COMM-LAST-STMT-DT.
           MOVE NSTMTDDI      TO WS-DATE-SPLIT-DD.
           MOVE NSTMTMMI      TO WS-DATE-SPLIT-MM.
           MOVE NSTMTYYI      TO WS-DATE-SPLIT-YY.
           MOVE WS-DATE-SPLIT TO COMM-NEXT-STMT-DT.

      *
      *    Convert the screen formats for ACTBAL and AVBAL into proper
      *    numeric format (handling the decimal point and sign).
      *
           MOVE 0 TO WS-CONVERTED-VAL1.
           MOVE 0 TO WS-CONVERTED-VAL2.
           MOVE 0 TO WS-CONVERTED-VAL3.
           MOVE 0 TO WS-CONVERTED-VAL4.

           MOVE AVBALI        TO WS-CONVERT-PICX.
           COMPUTE WS-CONVERTED-VAL1 = WS-CONVERT-REMAIN / 100.
           COMPUTE WS-CONVERTED-VAL2 = WS-CONVERT-DEC.
           COMPUTE WS-CONVERTED-VAL3 = WS-CONVERTED-VAL1 +
                                           WS-CONVERTED-VAL2.

           IF WS-CONVERT-SIGN = '-'
              COMPUTE WS-CONVERTED-VAL4 = 0 - WS-CONVERTED-VAL3
           ELSE
              COMPUTE WS-CONVERTED-VAL4 = 0 + WS-CONVERTED-VAL3
           END-IF.

           MOVE WS-CONVERTED-VAL4 TO COMM-AVAIL-BAL.

           MOVE ACTBALI        TO WS-CONVERT-PICX.
           COMPUTE WS-CONVERTED-VAL1 = WS-CONVERT-REMAIN / 100.
           COMPUTE WS-CONVERTED-VAL2 = WS-CONVERT-DEC.
           COMPUTE WS-CONVERTED-VAL3 = WS-CONVERTED-VAL1 +
                                           WS-CONVERTED-VAL2.

           IF WS-CONVERT-SIGN = '-'
              COMPUTE WS-CONVERTED-VAL4 = 0 - WS-CONVERTED-VAL3
           ELSE
              COMPUTE WS-CONVERTED-VAL4 = 0 + WS-CONVERTED-VAL3
           END-IF.

           MOVE WS-CONVERTED-VAL4 TO COMM-ACTUAL-BAL.
           MOVE ' '           TO COMM-SUCCESS.

           EXEC CICS LINK
              PROGRAM('UPDACC')
              COMMAREA(DFHCOMMAREA)
              RESP(WS-CICS-RESP)
              RESP2(WS-CICS-RESP2)
              SYNCONRETURN
           END-EXEC.

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
              MOVE 'HBNK'      TO ABND-CODE

              EXEC CICS ASSIGN PROGRAM(ABND-PROGRAM)
              END-EXEC

              MOVE ZEROS      TO ABND-SQLCODE

              STRING 'UAD010  - LINK UPDACC FAIL.'
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

              INITIALIZE WS-FAIL-INFO
              MOVE 'BNK1UAC - UAD010 - LINK UPDACC  FAIL      '
                 TO WS-CICS-FAIL-MSG
              MOVE WS-CICS-RESP  TO WS-CICS-RESP-DISP
              MOVE WS-CICS-RESP2 TO WS-CICS-RESP2-DISP
              PERFORM ABEND-THIS-TASK
           END-IF.

      *
      *    Has the update worked or not?
      *
           IF COMM-SUCCESS = 'N'

              MOVE 'N' TO VALID-DATA-SW
              MOVE SPACES TO MESSAGEO
              MOVE 'Update unsuccessful, try again later.    ' TO
                 MESSAGEO
           ELSE
              MOVE SPACES TO MESSAGEO
              MOVE 'Account update successfully applied.     '
                 TO MESSAGEO
      *
      *       Move fields to the associated output fields
      *
              MOVE COMM-ACCNO       TO ACCNO2O
              MOVE COMM-CUSTNO      TO CUSTNOO
              MOVE COMM-SCODE       TO SORTCO
              MOVE COMM-ACC-TYPE    TO ACTYPEO
              MOVE COMM-INT-RATE    TO INTRT-PIC9
              MOVE INTRT-PIC9       TO INTRTO
              MOVE COMM-OPENED      TO WS-DATE-SPLIT
              MOVE WS-DATE-SPLIT-DD TO OPENDDO
              MOVE WS-DATE-SPLIT-MM TO OPENMMO
              MOVE WS-DATE-SPLIT-YY TO OPENYYO
              MOVE COMM-LAST-STMT-DT TO WS-DATE-SPLIT
              MOVE WS-DATE-SPLIT-DD TO LSTMTDDO
              MOVE WS-DATE-SPLIT-MM TO LSTMTMMO
              MOVE WS-DATE-SPLIT-YY TO LSTMTYYO
              MOVE COMM-NEXT-STMT-DT TO WS-DATE-SPLIT
              MOVE WS-DATE-SPLIT-DD TO NSTMTDDO
              MOVE WS-DATE-SPLIT-MM TO NSTMTMMO
              MOVE WS-DATE-SPLIT-YY TO NSTMTYYO
              MOVE COMM-OVERDRAFT   TO OVERDRO
              MOVE COMM-AVAIL-BAL   TO AVAILABLE-BALANCE-DISPLAY
              MOVE COMM-ACTUAL-BAL  TO ACTUAL-BALANCE-DISPLAY
              MOVE AVAILABLE-BALANCE-DISPLAY TO AVBALO
              MOVE ACTUAL-BALANCE-DISPLAY TO ACTBALO

           END-IF.


       UAD999.
           EXIT.


       SEND-MAP SECTION.
       SM010.
      *
      *    If the map needs to have its data erased
      *
           IF SEND-ERASE
              EXEC CICS SEND MAP('BNK1UA')
                 MAPSET('BNK1UAM')
                 FROM(BNK1UAO)
                 ERASE
                 CURSOR
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
                 MOVE 'HBNK'      TO ABND-CODE

                 EXEC CICS ASSIGN PROGRAM(ABND-PROGRAM)
                 END-EXEC

                 MOVE ZEROS      TO ABND-SQLCODE

                 STRING 'SM010 - SEND MAP ERASE FAIL.'
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


                 INITIALIZE WS-FAIL-INFO
                 MOVE 'BNK1UAC - SM010 - SEND MAP ERASE FAIL '
                    TO WS-CICS-FAIL-MSG
                 MOVE WS-CICS-RESP  TO WS-CICS-RESP-DISP
                 MOVE WS-CICS-RESP2 TO WS-CICS-RESP2-DISP
                 PERFORM ABEND-THIS-TASK
              END-IF

              GO TO SM999
           END-IF.

      *
      *    If the map just needs a resend of only the data
      *
           IF SEND-DATAONLY
              EXEC CICS SEND MAP('BNK1UA')
                 MAPSET('BNK1UAM')
                 FROM(BNK1UAO)
                 DATAONLY
                 CURSOR
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
                 MOVE 'HBNK'      TO ABND-CODE

                 EXEC CICS ASSIGN PROGRAM(ABND-PROGRAM)
                 END-EXEC

                 MOVE ZEROS      TO ABND-SQLCODE

                 STRING 'SM010 - SEND MAP DATAONLY FAIL.'
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

                 INITIALIZE WS-FAIL-INFO
                 MOVE 'BNK1UAC - SM010 - SEND MAP DATAONLY FAIL '
                    TO WS-CICS-FAIL-MSG
                 MOVE WS-CICS-RESP  TO WS-CICS-RESP-DISP
                 MOVE WS-CICS-RESP2 TO WS-CICS-RESP2-DISP
                 PERFORM ABEND-THIS-TASK
              END-IF

              GO TO SM999
           END-IF.

      *
      *    If we have elected to send the map and a beep
      *
           IF SEND-DATAONLY-ALARM
              EXEC CICS SEND MAP('BNK1UA')
                 MAPSET('BNK1UAM')
                 FROM(BNK1UAO)
                 DATAONLY
                 ALARM
                 CURSOR
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
                 MOVE 'HBNK'      TO ABND-CODE

                 EXEC CICS ASSIGN PROGRAM(ABND-PROGRAM)
                 END-EXEC

                 MOVE ZEROS      TO ABND-SQLCODE

                 STRING 'SM010 - SEND MAP DATAONLY ALARM FAIL.'
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


                 INITIALIZE WS-FAIL-INFO
                 MOVE 'BNK1UAC - SM010 - SEND MAP DATAONLY ALARM FAIL '
                    TO WS-CICS-FAIL-MSG
                 MOVE WS-CICS-RESP  TO WS-CICS-RESP-DISP
                 MOVE WS-CICS-RESP2 TO WS-CICS-RESP2-DISP
                 PERFORM ABEND-THIS-TASK
              END-IF

           END-IF.

       SM999.
           EXIT.


       SEND-TERMINATION-MSG SECTION.
       STM010.
      *
      *    Send the termination message
      *
           EXEC CICS SEND TEXT
              FROM(END-OF-SESSION-MESSAGE)
              ERASE
              FREEKB
              RESP(WS-CICS-RESP)
              RESP2(WS-CICS-RESP2)
           END-EXEC.

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
              MOVE 'HBNK'      TO ABND-CODE

              EXEC CICS ASSIGN PROGRAM(ABND-PROGRAM)
              END-EXEC

              MOVE ZEROS      TO ABND-SQLCODE

              STRING 'STM010 - SEND TEXT FAIL.'
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


              INITIALIZE WS-FAIL-INFO
              MOVE 'BNK1UAC - STM010 - SEND TEXT FAIL'
                 TO WS-CICS-FAIL-MSG
              MOVE WS-CICS-RESP  TO WS-CICS-RESP-DISP
              MOVE WS-CICS-RESP2 TO WS-CICS-RESP2-DISP
              PERFORM ABEND-THIS-TASK
           END-IF.

       STM999.
           EXIT.


       ABEND-THIS-TASK SECTION.
       ATT010.
           DISPLAY WS-FAIL-INFO.
           EXEC CICS ABEND
              ABCODE('HBNK')
              NODUMP
           END-EXEC.

       ATT999.
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


