       PROCESS CICS,NODYNAM,NSYMBOL(NATIONAL),TRUNC(STD)
       CBL CICS('SP,EDF')
      ******************************************************************
      *                                                                *
      *  Copyright contributors to the CICS Banking Sample Application *
      * (CBSA) project                                                 *
      *                                                                *
      ******************************************************************

      ******************************************************************
      * This is the Display Account program in the BANKING application
      * BMS suite. This program also handles requests to delete an
      * account.
      *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BNK1DAC.
       AUTHOR. Jon Collett.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      *SOURCE-COMPUTER.   IBM-370 WITH DEBUGGING MODE.
       SOURCE-COMPUTER.  IBM-370.
       OBJECT-COMPUTER.  IBM-370.

       INPUT-OUTPUT SECTION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * Copyright statement as a literal to go into the load module
       77 FILLER PIC X(34) VALUE 'Copyright contributors to the CICS'. 
       77 FILLER PIC X(34) VALUE 'Banking Sample Application (CBSA)'. 
       77 FILLER PIC X(8)  VALUE ' project'. 


       01 WS-CICS-WORK-AREA.
          03 WS-CICS-RESP              PIC S9(8) COMP VALUE 0.
          03 WS-CICS-RESP2             PIC S9(8) COMP VALUE 0.

       01 WS-FAIL-INFO.
          03 FILLER                    PIC X(9)  VALUE 'BNK1DAC  '.
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

       COPY BNK1DAM.

       COPY DFHAID.

       01 COMM-OPENED-SPLIT.
          03 COMM-OPENED-SPLIT-DD      PIC 99.
          03 COMM-OPENED-SPLIT-MM      PIC 99.
          03 COMM-OPENED-SPLIT-YY      PIC 9999.

       01 COMM-LAST-ST-SPLIT.
          03 COMM-LAST-ST-DD           PIC 99.
          03 COMM-LAST-ST-MM           PIC 99.
          03 COMM-LAST-ST-YY           PIC 9999.

       01 COMM-NEXT-ST-SPLIT.
          03 COMM-NEXT-ST-DD           PIC 99.
          03 COMM-NEXT-ST-MM           PIC 99.
          03 COMM-NEXT-ST-YY           PIC 9999.

       01 PARMS-SUBPGM.
          03 PARMS-SUBPGM-EYE          PIC X(4).
          88 PARMS-SUBPGM-EYE-VALID        VALUE 'ACCT'.
          03 PARMS-SUBPGM-CUSTNO       PIC X(10).
          03 PARMS-SUBPGM-SCODE        PIC X(6).
          03 PARMS-SUBPGM-ACCNO        PIC 9(8).
          03 PARMS-SUBPGM-ACC-TYPE     PIC X(8).
          03 PARMS-SUBPGM-INT-RATE     PIC 9(4)V99.
          03 PARMS-SUBPGM-OPENED       PIC 9(8).
          03 PARMS-SUBPGM-OVERDRAFT    PIC 9(8).
          03 PARMS-SUBPGM-LAST-STMT-DT PIC 9(8).
          03 PARMS-SUBPGM-NEXT-STMT-DT PIC 9(8).
          03 PARMS-SUBPGM-AVAIL-BAL    PIC S9(10)V99.
          03 PARMS-SUBPGM-ACTUAL-BAL   PIC S9(10)V99.
          03 PARMS-SUBPGM-SUCCESS      PIC X.
          03 PARMS-SUBPGM-FAIL-CD      PIC X.
          03 PARMS-SUBPGM-DEL-SUCCESS  PIC X.
          03 PARMS-SUBPGM-DEL-FAIL-CD  PIC X.
          03 PARMS-SUBPGM-DEL-APPLID   PIC X(8).
          03 PARMS-SUBPGM-DEL-PCB1     POINTER.
          03 PARMS-SUBPGM-DEL-PCB2     POINTER.
          03 PARMS-SUBPGM-DEL-PCB3     POINTER.

       COPY INQACC.

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
          03 WS-COMM-FAIL-CD           PIC X.
          03 WS-COMM-DEL-SUCCESS       PIC X.
          03 WS-COMM-DEL-FAIL-CD       PIC X.

       01 COMM-AVAIL-BAL-DISP          PIC 9(12).
       01 COMM-ACT-BAL-DISP            PIC 9(12).

       01 WS-STUFF.
          03 WS-AVAIL-BAL-UNSIGN       PIC 9(12).
          03 WS-ACT-BAL-UNSIGN         PIC 9(12).

       01 COMPANY-NAME-FULL            PIC X(32).

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
           03 COMM-EYE                 PIC X(4).
           03 COMM-CUSTNO              PIC X(10).
           03 COMM-SCODE               PIC X(6).
           03 COMM-ACCNO               PIC 9(8).
           03 COMM-ACC-TYPE            PIC X(8).
           03 COMM-INT-RATE            PIC 9(6).
           03 COMM-OPENED              PIC 9(8).
           03 COMM-OVERDRAFT           PIC 9(8).
           03 COMM-LAST-STMT-DT        PIC 9(8).
           03 COMM-NEXT-STMT-DT        PIC 9(8).
           03 COMM-AVAIL-BAL           PIC S9(10)V99.
           03 COMM-ACTUAL-BAL          PIC S9(10)V99.
           03 COMM-SUCCESS             PIC X.
           03 COMM-FAIL-CD             PIC X.
           03 COMM-DEL-SUCCESS         PIC X.
           03 COMM-DEL-FAIL-CD         PIC X.


       PROCEDURE DIVISION.
       PREMIERE SECTION.
       A010.

           EVALUATE TRUE
      *
      *       Is it the first time through? If so, send the map
      *       with erased (empty) data fields.
      *
              WHEN EIBCALEN = ZERO
                 MOVE LOW-VALUE TO BNK1DAO
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
      *       When Pf5 is pressed, process the map
      *
              WHEN EIBAID = DFHPF5
                 PERFORM PROCESS-MAP

      *
      *       When Pf12 is pressed, send a termination
      *       message.
      *
              WHEN EIBAID = DFHPF12
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
      *       When anything else happens, send the invalid key message
      *
              WHEN OTHER
                 MOVE LOW-VALUES TO BNK1DAO
                 MOVE 'Invalid key pressed.' TO MESSAGEO
                 MOVE -1 TO ACCNOL
                 SET SEND-DATAONLY-ALARM TO TRUE

                 PERFORM SEND-MAP

           END-EVALUATE.

      *
      *    Provided that we have been around this way before (i.e. it is
      *    NOT the first time through, put the data returned from the
      *    sub program into the area that we use as the COMMAREA on the
      *    RETURN.
      *
           IF EIBCALEN NOT = ZERO
              IF INQACC-EYE = 'ACCT'
                 MOVE INQACC-EYE          TO WS-COMM-EYE
                 MOVE INQACC-CUSTNO       TO WS-COMM-CUSTNO
                 MOVE INQACC-SCODE        TO WS-COMM-SCODE
                 MOVE INQACC-ACCNO        TO WS-COMM-ACCNO
                 MOVE INQACC-ACC-TYPE     TO WS-COMM-ACC-TYPE
                 MOVE INQACC-INT-RATE     TO WS-COMM-INT-RATE
                 MOVE INQACC-OPENED       TO WS-COMM-OPENED
                 MOVE INQACC-OVERDRAFT    TO WS-COMM-OVERDRAFT
                 MOVE INQACC-LAST-STMT-DT TO WS-COMM-LAST-STMT-DT
                 MOVE INQACC-NEXT-STMT-DT TO WS-COMM-NEXT-STMT-DT
                 MOVE INQACC-AVAIL-BAL    TO WS-COMM-AVAIL-BAL
                 MOVE INQACC-ACTUAL-BAL   TO WS-COMM-ACTUAL-BAL
                 MOVE INQACC-SUCCESS      TO WS-COMM-SUCCESS
              ELSE
                 INITIALIZE WS-COMM-AREA
              END-IF

           END-IF.

           EXEC CICS
              RETURN TRANSID('ODAC')
              COMMAREA(WS-COMM-AREA)
              LENGTH(102)
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

              STRING 'A010 - RETURN TRANSID(ODAC) FAIL.'
                    DELIMITED BY SIZE,
                    'EIBRESP=' DELIMITED BY SIZE,
                    ABND-RESPCODE DELIMITED BY SIZE,
                    ' RESP2=' DELIMITED BY SIZE,
                    ABND-RESP2CODE DELIMITED BY SIZE
                    INTO ABND-FREEFORM
              END-STRING

              EXEC CICS LINK PROGRAM(WS-ABEND-PGM)
                        COMMAREA(ABNDINFO-REC)
              END-EXEC

              INITIALIZE WS-FAIL-INFO
              MOVE 'BNK1DAC - A010 - RETURN TRANSID(ODAC) FAIL' TO
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

      *
      *    If enter is pressed validate the data
      *
           IF EIBAID = DFHENTER
      *
      *       Validate the received data
      *
              PERFORM EDIT-DATA

      *
      *       If the data passes validation go on to
      *       retrieve account information.
      *
              IF VALID-DATA
                 PERFORM GET-ACC-DATA
              ELSE
                 INITIALIZE PARMS-SUBPGM
              END-IF

           END-IF.

      *
      *    If pF5 (delete) is pressed validate the data
      *

           IF EIBAID = DFHPF5
              PERFORM VALIDATE-DATA

      *
      *       If the data passes validation go on to
      *       delete the account.
      *
              IF VALID-DATA
                 PERFORM DEL-ACC-DATA
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
              RECEIVE MAP('BNK1DA')
              MAPSET('BNK1DAM')
              INTO(BNK1DAI)
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
                    'EIBRESP=' DELIMITED BY SIZE,
                    ABND-RESPCODE DELIMITED BY SIZE,
                    ' RESP2=' DELIMITED BY SIZE,
                    ABND-RESP2CODE DELIMITED BY SIZE
                    INTO ABND-FREEFORM
              END-STRING

              EXEC CICS LINK PROGRAM(WS-ABEND-PGM)
                        COMMAREA(ABNDINFO-REC)
              END-EXEC


              INITIALIZE WS-FAIL-INFO
              MOVE 'BNK1DAC - RM010 - RECEIVE MAP FAIL ' TO
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
           IF ACCNOI = LOW-VALUES
           OR ACCNOL = 0
             MOVE 'Please enter an account number.' TO
                 MESSAGEO
             MOVE 'N' TO VALID-DATA-SW
             GO TO ED999
           END-IF

           EXEC CICS BIF DEEDIT
              FIELD(ACCNOI)
           END-EXEC

           IF ACCNOI NOT NUMERIC
              MOVE 'Please enter an account number.' TO
                 MESSAGEO
              MOVE 'N' TO VALID-DATA-SW
           END-IF.

       ED999.
           EXIT.


       VALIDATE-DATA SECTION.
       VD010.
      *
      *    Perform further validation on the incoming fields
      *
           IF COMM-SCODE = ZEROES OR LOW-VALUES
           OR COMM-ACCNO = ZEROES OR LOW-VALUES
              MOVE 'N' TO VALID-DATA-SW
              MOVE SPACES TO MESSAGEO
              STRING 'Please enter an account number.'
                  DELIMITED BY SIZE,
                  INTO MESSAGEO
              MOVE -1 TO ACCNOL
            END-IF.

       VD999.
           EXIT.


       GET-ACC-DATA SECTION.
       GAD010.
      *
      *    Set up the fields required by INQACC then link to it
      *
           INITIALIZE PARMS-SUBPGM INQACC-COMMAREA
           SET INQACC-PCB1-POINTER TO NULL
           MOVE ACCNOI TO PARMS-SUBPGM-ACCNO INQACC-ACCNO.

           EXEC CICS LINK
              PROGRAM('INQACC')
              COMMAREA(INQACC-COMMAREA)
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

              STRING 'GAD010  - LINK INQACC FAIL.'
                    DELIMITED BY SIZE,
                    'EIBRESP=' DELIMITED BY SIZE,
                    ABND-RESPCODE DELIMITED BY SIZE,
                    ' RESP2=' DELIMITED BY SIZE,
                    ABND-RESP2CODE DELIMITED BY SIZE
                    INTO ABND-FREEFORM
              END-STRING

              EXEC CICS LINK PROGRAM(WS-ABEND-PGM)
                        COMMAREA(ABNDINFO-REC)
              END-EXEC

              INITIALIZE WS-FAIL-INFO
              MOVE 'BNK1DAC - GAD010 - LINK INQACC  FAIL      '
                 TO WS-CICS-FAIL-MSG
              MOVE WS-CICS-RESP  TO WS-CICS-RESP-DISP
              MOVE WS-CICS-RESP2 TO WS-CICS-RESP2-DISP
              PERFORM ABEND-THIS-TASK
           END-IF.

      *
      *    Check to see if there was any data returned, if not output
      *    an error message
      *
           IF INQACC-ACC-TYPE  = SPACES AND
           INQACC-INT-RATE  = 0
           AND INQACC-SUCCESS = 'N'

              MOVE 'Sorry, but that account number was not found.' TO
                 MESSAGEO
              MOVE 'N' TO VALID-DATA-SW
              MOVE SPACES   TO SORTCO
              MOVE SPACES   TO CUSTNOO
              MOVE SPACES   TO ACCNO2O
              MOVE SPACES   TO ACTYPEO
              MOVE zero     TO INTRTO
              MOVE SPACES   TO COMM-OPENED-SPLIT
              MOVE SPACES   TO OPENDDO
              MOVE SPACES   TO OPENMMO
              MOVE SPACES   TO OPENYYO

              MOVE SPACES   TO OVERDRO

              MOVE SPACES   TO COMM-LAST-ST-SPLIT
              MOVE SPACES   TO LSTMTDDO
              MOVE SPACES   TO LSTMTMMO
              MOVE SPACES   TO LSTMTYYO

              MOVE SPACES   TO COMM-NEXT-ST-SPLIT
              MOVE SPACES   TO NSTMTDDO
              MOVE SPACES   TO NSTMTMMO
              MOVE SPACES   TO NSTMTYYO

              MOVE zero     TO AVAILABLE-BALANCE-DISPLAY
              MOVE zero     TO ACTUAL-BALANCE-DISPLAY
              MOVE available-balance-display TO AVBALO
              MOVE actual-balance-display    TO ACTBALO

              GO TO GAD999
           END-IF.

      *
      *    Set the values on the map
      *
           MOVE INQACC-SCODE             TO SORTCO.
           MOVE INQACC-CUSTNO            TO CUSTNOO.
           MOVE INQACC-ACCNO             TO ACCNO2O.
           MOVE INQACC-ACC-TYPE          TO ACTYPEO.
           MOVE INQACC-INT-RATE          TO INTRTO.

           MOVE INQACC-OPENED            TO COMM-OPENED-SPLIT.
           MOVE COMM-OPENED-SPLIT-DD     TO OPENDDO.
           MOVE COMM-OPENED-SPLIT-MM     TO OPENMMO.
           MOVE COMM-OPENED-SPLIT-YY     TO OPENYYO.

           MOVE INQACC-OVERDRAFT    TO OVERDRO.

           MOVE INQACC-LAST-STMT-DT TO COMM-LAST-ST-SPLIT.
           MOVE COMM-LAST-ST-DD           TO LSTMTDDO.
           MOVE COMM-LAST-ST-MM           TO LSTMTMMO.
           MOVE COMM-LAST-ST-YY           TO LSTMTYYO.

           MOVE INQACC-NEXT-STMT-DT TO COMM-NEXT-ST-SPLIT.
           MOVE COMM-NEXT-ST-DD           TO NSTMTDDO.
           MOVE COMM-NEXT-ST-MM           TO NSTMTMMO.
           MOVE COMM-NEXT-ST-YY           TO NSTMTYYO.

           MOVE INQACC-AVAIL-BAL      TO AVAILABLE-BALANCE-DISPLAY.
           MOVE INQACC-ACTUAL-BAL     TO ACTual-balance-display.
           MOVE available-balance-display TO AVBALO
           MOVE actual-balance-display    TO ACTBALO

           MOVE SPACES TO MESSAGEO.
           STRING 'If you wish to delete the Account press <PF5>.'
                 DELIMITED BY SIZE,
                 ' ' DELIMITED BY SIZE
                 INTO MESSAGEO.
       GAD999.
           EXIT.


       DEL-ACC-DATA SECTION.
       DAD010.
      *
      *    Set up the fields required by DELACC then link to it
      *
           INITIALIZE PARMS-SUBPGM
           COMPUTE PARMS-SUBPGM-ACCNO = FUNCTION NUMVAL(ACCNO2I)
           SET PARMS-SUBPGM-DEL-PCB1 TO NULL.
           SET PARMS-SUBPGM-DEL-PCB2 TO NULL.
           SET PARMS-SUBPGM-DEL-PCB3 TO NULL.

           EXEC CICS LINK
              PROGRAM('DELACC')
              COMMAREA(PARMS-SUBPGM)
              RESP(WS-CICS-RESP)
              RESP2(WS-CICS-RESP2)
              SYNCONRETURN
           END-EXEC.

           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)
              INITIALIZE WS-FAIL-INFO
              MOVE 'BNK1DAC - DAD010 - LINK DELACC  FAIL      '
                 TO WS-CICS-FAIL-MSG
              MOVE WS-CICS-RESP  TO WS-CICS-RESP-DISP
              MOVE WS-CICS-RESP2 TO WS-CICS-RESP2-DISP
              PERFORM ABEND-THIS-TASK
           END-IF.

      *
      *    Check to see if there was any data returned, if not output
      *    an error message
      *
           IF PARMS-SUBPGM-DEL-SUCCESS = 'N' AND
           PARMS-SUBPGM-DEL-FAIL-CD = '1'
              MOVE SPACES TO MESSAGEO
              STRING 'Sorry, but that account number was not found.'
                 DELIMITED BY SIZE,
                 ' Account NOT deleted.' DELIMITED BY SIZE
                 INTO MESSAGEO
              MOVE 'N' TO VALID-DATA-SW
              MOVE PARMS-SUBPGM-SCODE   TO SORTCO
              GO TO DAD999
           END-IF.

           IF PARMS-SUBPGM-DEL-SUCCESS = 'N' AND
           PARMS-SUBPGM-DEL-FAIL-CD = '2'
              MOVE SPACES TO MESSAGEO
              STRING 'Sorry, but a datastore error occurred.'
                 DELIMITED BY SIZE,
                 ' Account NOT deleted.' DELIMITED BY SIZE
                 INTO MESSAGEO
              MOVE 'N' TO VALID-DATA-SW
              MOVE PARMS-SUBPGM-SCODE   TO SORTCO
              GO TO DAD999
           END-IF.

           IF PARMS-SUBPGM-DEL-SUCCESS = 'N' AND
           PARMS-SUBPGM-DEL-FAIL-CD = '3'
              MOVE SPACES TO MESSAGEO
              STRING 'Sorry, but a delete error occurred.'
                 DELIMITED BY SIZE,
                 ' Account NOT deleted.' DELIMITED BY SIZE
                 INTO MESSAGEO
              MOVE 'N' TO VALID-DATA-SW
              MOVE PARMS-SUBPGM-SCODE   TO SORTCO
              GO TO DAD999
           END-IF.

           IF PARMS-SUBPGM-DEL-SUCCESS = 'N'
              MOVE SPACES TO MESSAGEO
              STRING 'Sorry, but a delete error occurred.'
                 DELIMITED BY SIZE,
                 ' Account NOT deleted.' DELIMITED BY SIZE
                 INTO MESSAGEO
              MOVE 'N' TO VALID-DATA-SW
              MOVE PARMS-SUBPGM-SCODE   TO SORTCO
              GO TO DAD999
           END-IF.

      *
      *    Set the values on the map
      *
           MOVE SPACES TO SORTCO.
           MOVE SPACES TO CUSTNOO.
           MOVE SPACES TO ACCNO2O.
           MOVE SPACES TO ACTYPEO.
           MOVE ZERO TO INTRTO.

           MOVE SPACES TO OPENDDO.
           MOVE SPACES TO OPENMMO.
           MOVE SPACES TO OPENYYO.

           MOVE SPACES TO OVERDRO.
           MOVE SPACES TO LSTMTDDO.
           MOVE SPACES TO LSTMTMMO.
           MOVE SPACES TO LSTMTYYO.

           MOVE SPACES TO NSTMTDDO.
           MOVE SPACES TO NSTMTMMO.
           MOVE SPACES TO NSTMTYYO.

           MOVE ZERO TO AVAILABLE-BALANCE-DISPLAY.
           MOVE ZERO TO ACTUAL-BALANCE-DISPLAY.
           MOVE AVAILABLE-BALANCE-DISPLAY TO AVBALO
           MOVE ACTUAL-BALANCE-DISPLAY    TO ACTBALO

           MOVE SPACES TO MESSAGEO.

           STRING 'Account ' DELIMITED BY SIZE,
                 PARMS-SUBPGM-ACCNO DELIMITED BY SIZE,
                 ' was successfully deleted.'
                 DELIMITED BY SIZE,
                 INTO MESSAGEO
           END-STRING.

       DAD999.
           EXIT.


       SEND-MAP SECTION.
       SM010.
      *
      *    If the map needs to have its data erased
      *
           IF SEND-ERASE

              EXEC CICS SEND MAP('BNK1DA')
                 MAPSET('BNK1DAM')
                 FROM(BNK1DAO)
                 ERASE
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
                       'EIBRESP=' DELIMITED BY SIZE,
                       ABND-RESPCODE DELIMITED BY SIZE,
                       ' RESP2=' DELIMITED BY SIZE,
                       ABND-RESP2CODE DELIMITED BY SIZE
                       INTO ABND-FREEFORM
                 END-STRING

                 EXEC CICS LINK PROGRAM(WS-ABEND-PGM)
                           COMMAREA(ABNDINFO-REC)
                 END-EXEC

                 INITIALIZE WS-FAIL-INFO
                 MOVE 'BNK1DAC - SM010 - SEND MAP ERASE FAIL '
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
              IF MESSAGEO IS NOT EQUAL SPACES
              AND MESSAGEO IS NOT EQUAL LOW-VALUES
                 MOVE 'Account lookup successful.' TO MESSAGEO
              END-IF

              EXEC CICS SEND MAP('BNK1DA')
                 MAPSET('BNK1DAM')
                 FROM(BNK1DAO)
                 DATAONLY
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
                       'EIBRESP=' DELIMITED BY SIZE,
                       ABND-RESPCODE DELIMITED BY SIZE,
                       ' RESP2=' DELIMITED BY SIZE,
                       ABND-RESP2CODE DELIMITED BY SIZE
                       INTO ABND-FREEFORM
                 END-STRING

                 EXEC CICS LINK PROGRAM(WS-ABEND-PGM)
                           COMMAREA(ABNDINFO-REC)
                 END-EXEC

                 INITIALIZE WS-FAIL-INFO
                 MOVE 'BNK1DAC - SM010 - SEND MAP DATAONLY FAIL '
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
              EXEC CICS SEND MAP('BNK1DA')
                 MAPSET('BNK1DAM')
                 FROM(BNK1DAO)
                 DATAONLY
                 ALARM
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
                       'EIBRESP=' DELIMITED BY SIZE,
                       ABND-RESPCODE DELIMITED BY SIZE,
                       ' RESP2=' DELIMITED BY SIZE,
                       ABND-RESP2CODE DELIMITED BY SIZE
                       INTO ABND-FREEFORM
                 END-STRING

                 EXEC CICS LINK PROGRAM(WS-ABEND-PGM)
                           COMMAREA(ABNDINFO-REC)
                 END-EXEC


                 INITIALIZE WS-FAIL-INFO
                 MOVE 'BNK1DAC - SM010 - SEND MAP DATAONLY ALARM FAIL '
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
                    'EIBRESP=' DELIMITED BY SIZE,
                    ABND-RESPCODE DELIMITED BY SIZE,
                    ' RESP2=' DELIMITED BY SIZE,
                    ABND-RESP2CODE DELIMITED BY SIZE
                    INTO ABND-FREEFORM
              END-STRING

              EXEC CICS LINK PROGRAM(WS-ABEND-PGM)
                        COMMAREA(ABNDINFO-REC)
              END-EXEC

              INITIALIZE WS-FAIL-INFO
              MOVE 'BNK1DAC - STM010 - SEND TEXT FAIL'
                 TO WS-CICS-FAIL-MSG
              MOVE WS-CICS-RESP  TO WS-CICS-RESP-DISP
              MOVE WS-CICS-RESP2 TO WS-CICS-RESP2-DISP
              PERFORM ABEND-THIS-TASK
           END-IF.

       STM999.
           EXIT.


       ABEND-THIS-TASK SECTION.
       ATT010.
           EXEC CICS ABEND
                ABCODE('HBNK')
                NODUMP
           END-EXEC.

       ATT999.
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
