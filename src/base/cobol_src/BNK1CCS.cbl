       PROCESS CICS,NODYNAM,NSYMBOL(NATIONAL),TRUNC(STD)
       CBL CICS('SP,EDF')
      ******************************************************************
      *                                                                *
      *  Copyright IBM Corp. 2023                                      *
      *                                                                *
      ******************************************************************


      ******************************************************************
      * This is the Create Customer program in the BANKING application
      * BMS suite.
      *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BNK1CCS.
       AUTHOR. Jon Collett.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      *SOURCE-COMPUTER.   IBM-370 WITH DEBUGGING MODE.
       SOURCE-COMPUTER.  IBM-370.
       OBJECT-COMPUTER.  IBM-370.

       INPUT-OUTPUT SECTION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-UCTRANS                  PIC S9(8) COMP VALUE 0.
       01 STORED-UCTRANS              PIC S9(8) COMP VALUE 0.

       01 WS-CICS-WORK-AREA.
          03 WS-CICS-RESP             PIC S9(8) COMP VALUE 0.
          03 WS-CICS-RESP2            PIC S9(8) COMP VALUE 0.

       01 WS-FAIL-INFO.
          03 FILLER                   PIC X(9)  VALUE 'BNK1CCS  '.
          03 WS-CICS-FAIL-MSG         PIC X(70) VALUE ' '.
          03 FILLER                   PIC X(6)  VALUE ' RESP='.
          03 WS-CICS-RESP-DISP        PIC 9(10) VALUE 0.
          03 FILLER                   PIC X(7)  VALUE ' RESP2='.
          03 WS-CICS-RESP2-DISP       PIC 9(10) VALUE 0.
          03 FILLER                   PIC X(15) VALUE ' ABENDING TASK.'.
       01 SWITCHES.
           03 VALID-DATA-SW           PIC X VALUE 'Y'.
              88 VALID-DATA           VALUE 'Y'.

       01 FLAGS.
           03 SEND-FLAG               PIC X.
              88 SEND-ERASE           VALUE '1'.
              88 SEND-DATAONLY        VALUE '2'.
              88 SEND-DATAONLY-ALARM  VALUE '3'.

       01 ACTION-ALPHA.
           03 ACTION-NUM              PIC 9.

      *
      * The end of session message
      *
       01 END-OF-SESSION-MESSAGE      PIC X(13) VALUE 'Session Ended'.

       01 RESPONSE-CODE               PIC S9(8) COMP.

       01 COMMUNICATION-AREA          PIC X.

       COPY BNK1CCM.

       COPY DFHAID.

       01 COMM-DOB-SPLIT.
          03 COMM-DOB-SPLIT-DD         PIC 99.
          03 COMM-DOB-SPLIT-MM         PIC 99.
          03 COMM-DOB-SPLIT-YYYY       PIC 9999.

       01 COMM-ADDR-SPLIT.
          03 COMM-ADDR-SPLIT1         PIC X(60).
          03 COMM-ADDR-SPLIT2         PIC X(60).
          03 COMM-ADDR-SPLIT3         PIC X(40).

       01 WS-COMM-AREA.
          03 WS-COMM-DATA               PIC X.
          03 WS-COMM-TERM               PIC S9(8) COMP.

       01 DATE-REFORMED.
          03 DOBDDI-CHAR               PIC XX.
          03 DOBDDI-REFORM REDEFINES DOBDDI-CHAR.
             05 DOBDDI-NUM             PIC 99.
          03 DOBMMI-CHAR               PIC XX.
          03 DOBMMI-REFORM REDEFINES DOBMMI-CHAR.
             05 DOBMMI-NUM             PIC 99.
          03 DOBYYI-CHAR               PIC XX.
          03 DOBYYI-REFORM REDEFINES DOBYYI-CHAR.
             05 DOBYYI-NUM             PIC 99.

       01 WS-ABCODE PIC XXXX.

       01 WS-ADDR-SPLIT.
          03 WS-ADDR-SPLIT1            PIC X(60).
          03 WS-ADDR-SPLIT2            PIC X(60).
          03 WS-ADDR-SPLIT3            PIC X(40).

       01 SUBPGM-PARMS.
          03 SUBPGM-EYECATCHER                 PIC X(4).
          03 SUBPGM-KEY.
             05 SUBPGM-SORTCODE                PIC 9(6) DISPLAY.
             05 SUBPGM-NUMBER                  PIC 9(10) DISPLAY.
          03 SUBPGM-NAME                       PIC X(60).
          03 SUBPGM-ADDRESS                    PIC X(160).
          03 SUBPGM-DATE-OF-BIRTH              PIC 9(8).
          03 SUBPGM-DOB-GROUP REDEFINES SUBPGM-DATE-OF-BIRTH.
             05 SUBPGM-BIRTH-DAY               PIC 99.
             05 SUBPGM-BIRTH-MONTH             PIC 99.
             05 SUBPGM-BIRTH-YEAR              PIC 9999.
          03 SUBPGM-CREDIT-SCORE               PIC 999.
          03 SUBPGM-CS-REVIEW-DATE             PIC 9(8).
          03 SUBPGM-SUCCESS                    PIC X.
          03 SUBPGM-FAIL-CODE                  PIC X.

       01 COMPANY-NAME-FULL PIC X(32).

       01 CURSOR-POSITION PIC S9(4) BINARY.

       01 WS-U-TIME                      PIC S9(15) COMP-3.
       01 WS-ORIG-DATE                   PIC X(10).
       01 WS-ORIG-DATE-GRP REDEFINES WS-ORIG-DATE.
          03 WS-ORIG-DATE-DD             PIC 99.
          03 FILLER                      PIC X.
          03 WS-ORIG-DATE-MM             PIC 99.
          03 FILLER                      PIC X.
          03 WS-ORIG-DATE-YYYY           PIC 9999.

       01 WS-ORIG-DATE-GRP-X.
          03 WS-ORIG-DATE-DD-X           PIC XX.
          03 FILLER                      PIC X VALUE '.'.
          03 WS-ORIG-DATE-MM-X           PIC XX.
          03 FILLER                      PIC X VALUE '.'.
          03 WS-ORIG-DATE-YYYY-X         PIC X(4).

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
          03 DFHCOMMAREA-DATA                  PIC X(5).


       PROCEDURE DIVISION.
       PREMIERE SECTION.
       A010.

           EXEC CICS HANDLE ABEND
              LABEL(HANDLE-ABEND)
           END-EXEC.

           EVALUATE TRUE
      *
      *       Is it the first time through? If so, send the map
      *       with erased (empty) data fields.
      *
              WHEN EIBCALEN = ZERO
                 MOVE LOW-VALUE TO BNK1CCO
                 MOVE SPACES TO CUSTTITO
                 MOVE SPACES TO CHRISTNO
                 MOVE SPACES TO CUSTINSO
                 MOVE SPACES TO CUSTSNO
                 MOVE SPACES TO CUSTAD1O
                 MOVE SPACES TO CUSTAD2O
                 MOVE SPACES TO CUSTAD3O

                 MOVE -1 TO CUSTTITL
                 SET SEND-ERASE TO TRUE
                 MOVE SPACES TO MESSAGEO

                 PERFORM STORE-TERM-DEF

                 PERFORM SEND-MAP

                 MOVE ' '            TO WS-COMM-AREA
                 MOVE STORED-UCTRANS TO WS-COMM-TERM

      *
      *       If a PA key is pressed, just carry on
      *
              WHEN EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
                 CONTINUE

      *
      *       When Pf3 is pressed, return to the main menu
      *
              WHEN EIBAID = DFHPF3

      *
      *          Set the terminal UCTRAN back to
      *          its starting position
      *
                 PERFORM RESTORE-TERM-DEF

                 EXEC CICS RETURN
                    TRANSID('OMEN')
                    IMMEDIATE
                    RESP(WS-CICS-RESP)
                    RESP2(WS-CICS-RESP2)
                 END-EXEC

      *
      *       If Pf12 is pressed, then send a termination
      *       message.
      *
              WHEN EIBAID = DFHPF12
                 PERFORM SEND-TERMINATION-MSG

      *
      *          Set the terminal UCTRAN back to
      *          its starting position
      *
                 PERFORM RESTORE-TERM-DEF

                 EXEC CICS
                    RETURN
                 END-EXEC

      *
      *       When CLEAR is pressed
      *
              WHEN EIBAID = DFHCLEAR
                 EXEC CICS SEND MAP('BNK1CCM')
                           MAPONLY
                           ERASE
                           FREEKB
                 END-EXEC

      *
      *          Set the terminal UCTRAN back to
      *          its starting position
      *
                 PERFORM RESTORE-TERM-DEF


                 EXEC CICS RETURN TRANSID('OCCS')
                           COMMAREA(WS-COMM-AREA)
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
                 MOVE DFHCOMMAREA TO WS-COMM-AREA
                 MOVE LOW-VALUES TO BNK1CCO
                 MOVE SPACES TO MESSAGEO
                 MOVE 'Invalid key pressed.' TO MESSAGEO
                 SET SEND-DATAONLY-ALARM TO TRUE
                 PERFORM SEND-MAP


           END-EVALUATE.

      *
      * Now RETURN
      *
           EXEC CICS
              RETURN TRANSID('OCCS')
              COMMAREA(WS-COMM-AREA)
              LENGTH(248)
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

              STRING 'A010 -RETURN TRANSID(OCCS) FAIL'
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
              MOVE 'BNK1CCS - A010 - RETURN TRANSID(OCCS) FAIL' TO
                 WS-CICS-FAIL-MSG
              MOVE WS-CICS-RESP  TO WS-CICS-RESP-DISP
              MOVE WS-CICS-RESP2 TO WS-CICS-RESP2-DISP

              PERFORM RESTORE-TERM-DEF
              PERFORM ABEND-THIS-TASK
           END-IF.

       A999.
           EXIT.


       PROCESS-MAP SECTION.
       PM010.
      *
      *    Retrieve the data from the map
      *
           MOVE DFHCOMMAREA TO WS-COMM-AREA.

           PERFORM RECEIVE-MAP.

      *
      *    Validate the received data
      *
           PERFORM EDIT-DATA.
      *
      *    If the data passes validation go on to
      *    create a CUSTOMER
      *
           IF VALID-DATA
              PERFORM CRE-CUST-DATA
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

      *
      *    To ensure that we get the correct case and the terminal
      *    doesn't change the case from ower case to upper case we
      *    we need to turn off upper case translation, do the receive
      *    and then turn it on again.
      *
           EXEC CICS INQUIRE
                TERMINAL(EIBTRMID)
                UCTRANST(WS-UCTRANS)
                RESP(WS-CICS-RESP)
                RESP2(WS-CICS-RESP2)
           END-EXEC.


      *
      *    If Uppercase translation is switched on, then set it to
      *    NOUCTRAN(451).
      *
           IF WS-UCTRANS = DFHVALUE(UCTRAN) OR
           WS-UCTRANS = DFHVALUE(TRANIDONLY)

              MOVE DFHVALUE(NOUCTRAN) TO WS-UCTRANS

              EXEC CICS SET TERMINAL(EIBTRMID)
                 UCTRANST(WS-UCTRANS)
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

                 STRING 'RM010 -RM010 (1) - SET TERMINAL '
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
                 MOVE 'BNK1CCS - RM010 (1) - SET TERMINAL UC FAIL ' TO
                    WS-CICS-FAIL-MSG
                 MOVE WS-CICS-RESP  TO WS-CICS-RESP-DISP
                 MOVE WS-CICS-RESP2 TO WS-CICS-RESP2-DISP
      D          DISPLAY 'Set terminal UC fail, going to'
      D                  'restore terminal.'
                 PERFORM RESTORE-TERM-DEF
                 PERFORM ABEND-THIS-TASK
              END-IF
           END-IF.

      *
      *    Receive the map AS IS
      *
           MOVE LOW-VALUES TO BNK1CCI.
           MOVE SPACES TO CUSTTITI.
           MOVE SPACES TO CHRISTNI.
           MOVE SPACES TO CUSTINSI.
           MOVE SPACES TO CUSTSNI.
           MOVE SPACES TO CUSTAD1I.
           MOVE SPACES TO CUSTAD2I.
           MOVE SPACES TO CUSTAD3I.
           MOVE 0 TO DOBDDI.
           MOVE 0 TO DOBMMI.
           MOVE 0 TO DOBYYI.
           MOVE 0 TO SORTCI.
           MOVE 0 TO CUSTNO2I.
           MOVE 0 TO CREDSCI.
           MOVE 0 TO SCRDTDDI.
           MOVE 0 TO SCRDTMMI.
           MOVE 0 TO SCRDTYYI.
           MOVE SPACES TO MESSAGEI.

           EXEC CICS
              RECEIVE MAP('BNK1CC')
              MAPSET('BNK1CCM')
              INTO(BNK1CCI)
              TERMINAL
              ASIS
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

              STRING 'RM010 - RM010 (2) - RECEIVE MAP FAIL '
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
              MOVE 'BNK1CCS - RM010 (2) - RECEIVE MAP FAIL ' TO
                 WS-CICS-FAIL-MSG
              MOVE WS-CICS-RESP  TO WS-CICS-RESP-DISP
              MOVE WS-CICS-RESP2 TO WS-CICS-RESP2-DISP

              PERFORM RESTORE-TERM-DEF
              PERFORM ABEND-THIS-TASK
           END-IF.

       RM999.
           EXIT.


       EDIT-DATA SECTION.
       ED010.
      *
      *    Perform validation on the incoming fields
      *
           IF CUSTNO2L > 0
              MOVE 'Please clear screen before creating new user'
                 TO MESSAGEO
              MOVE -1  TO CUSTTITL
              MOVE 'N' TO VALID-DATA-SW
              GO TO ED999
           END-IF

           IF CUSTTITL < 1 OR CUSTTITI = SPACES
           OR CUSTTITI = LOW-VALUES
              MOVE SPACES TO MESSAGEO

              STRING 'Valid titles are: Mr,Mrs,Miss,Ms,Dr,Professor,'
                    DELIMITED BY SIZE,
                     'Drs,Lord,Sir,Lady' DELIMITED BY SIZE
              INTO MESSAGEO
              MOVE -1 TO CUSTTITL
              MOVE 'N' TO VALID-DATA-SW
              GO TO ED999
           END-IF.

           MOVE SPACES TO MESSAGEO.

           IF CUSTTITL > 0

              EVALUATE CUSTTITI
                 WHEN 'MR________'
                    MOVE 'Mr' TO CUSTTITI
                    CONTINUE
                 WHEN 'Mr________'
                    MOVE 'Mr' TO CUSTTITI
                    CONTINUE
                 WHEN 'MR        '
                    MOVE 'Mr' TO CUSTTITI
                    CONTINUE
                 WHEN 'Mr         '
                    CONTINUE
                 WHEN 'mr________'
                    MOVE 'Mr' TO CUSTTITI
                    CONTINUE
                 WHEN 'MRS_______'
                    MOVE 'Mrs' TO CUSTTITI
                    CONTINUE
                 WHEN 'Mrs_______'
                    MOVE 'Mrs' TO CUSTTITI
                    CONTINUE
                 WHEN 'MRS       '
                    MOVE 'Mrs' TO CUSTTITI
                    CONTINUE
                 WHEN 'Mrs       '
                    CONTINUE
                 WHEN 'mrs_______'
                    MOVE 'Mrs' TO CUSTTITI
                    CONTINUE
                 WHEN 'MISS______'
                    MOVE 'Miss' TO CUSTTITI
                    CONTINUE
                 WHEN 'Miss______'
                    MOVE 'Miss' TO CUSTTITI
                    CONTINUE
                 WHEN 'MISS      '
                    MOVE 'Miss' TO CUSTTITI
                    CONTINUE
                 WHEN 'Miss      '
                    MOVE 'Miss' TO CUSTTITI
                    CONTINUE
                 WHEN 'miss______'
                    MOVE 'Miss' TO CUSTTITI
                    CONTINUE
                 WHEN 'MS________'
                    MOVE 'Ms' TO CUSTTITI
                    CONTINUE
                 WHEN 'Ms________'
                    MOVE 'Ms' TO CUSTTITI
                    CONTINUE
                 WHEN 'ms________'
                    MOVE 'Ms' TO CUSTTITI
                    CONTINUE
                 WHEN 'MS        '
                    MOVE 'Ms' TO CUSTTITI
                    CONTINUE
                 WHEN 'Ms        '
                    MOVE 'Ms' TO CUSTTITI
                    CONTINUE
                 WHEN 'ms        '
                    MOVE 'Ms' TO CUSTTITI
                    CONTINUE
                 WHEN 'DR________'
                    MOVE 'Dr' TO CUSTTITI
                    CONTINUE
                 WHEN 'Dr________'
                    MOVE 'Dr' TO CUSTTITI
                    CONTINUE
                 WHEN 'DR        '
                    MOVE 'Dr' TO CUSTTITI
                    CONTINUE
                 WHEN 'Dr        '
                    MOVE 'Dr' TO CUSTTITI
                    CONTINUE
                 WHEN 'dr        '
                    MOVE 'Dr' TO CUSTTITI
                    CONTINUE
                 WHEN 'dr________'
                    MOVE 'Dr' TO CUSTTITI
                    CONTINUE
                 WHEN 'DRS_______'
                    MOVE 'Drs' TO CUSTTITI
                    CONTINUE
                 WHEN 'Drs_______'
                    MOVE 'Drs' TO CUSTTITI
                    CONTINUE
                 WHEN 'DRS       '
                    MOVE 'Drs' TO CUSTTITI
                    CONTINUE
                 WHEN 'Drs       '
                    MOVE 'Drs' TO CUSTTITI
                    CONTINUE
                 WHEN 'drs       '
                    MOVE 'Drs' TO CUSTTITI
                    CONTINUE
                 WHEN 'drs_______'
                    MOVE 'Drs' TO CUSTTITI
                    CONTINUE
                 WHEN 'PROFESSOR_'
                    MOVE 'Professor' TO CUSTTITI
                    CONTINUE
                 WHEN 'Professor_'
                    MOVE 'Professor' TO CUSTTITI
                    CONTINUE
                 WHEN 'PROFESSOR '
                    MOVE 'Professor' TO CUSTTITI
                    CONTINUE
                 WHEN 'Professor '
                    MOVE 'Professor' TO CUSTTITI
                    CONTINUE
                 WHEN 'LORD______'
                    MOVE 'Lord' TO CUSTTITI
                    CONTINUE
                 WHEN 'Lord______'
                    MOVE 'Lord' TO CUSTTITI
                    CONTINUE
                 WHEN 'LORD      '
                    MOVE 'Lord' TO CUSTTITI
                    CONTINUE
                 WHEN 'Lord      '
                    MOVE 'Lord' TO CUSTTITI
                    CONTINUE
                 WHEN 'lord      '
                    MOVE 'Lord' TO CUSTTITI
                    CONTINUE
                 WHEN 'lord______'
                    MOVE 'Lord' TO CUSTTITI
                    CONTINUE
                 WHEN 'LADY______'
                    MOVE 'Lady' TO CUSTTITI
                    CONTINUE
                 WHEN 'Lady______'
                    MOVE 'Lady' TO CUSTTITI
                    CONTINUE
                 WHEN 'LADY      '
                    MOVE 'Lady' TO CUSTTITI
                    CONTINUE
                 WHEN 'Lady      '
                    MOVE 'Lady' TO CUSTTITI
                    CONTINUE
                 WHEN 'lady      '
                    MOVE 'Lady' TO CUSTTITI
                    CONTINUE
                 WHEN 'lady______'
                    MOVE 'Lady' TO CUSTTITI
                    CONTINUE
                 WHEN 'SIR_______'
                    MOVE 'Sir' TO CUSTTITI
                    CONTINUE
                 WHEN 'Sir_______'
                    MOVE 'Sir' TO CUSTTITI
                    CONTINUE
                 WHEN 'SIR       '
                    MOVE 'Sir' TO CUSTTITI
                    CONTINUE
                 WHEN 'Sir       '
                    MOVE 'Sir' TO CUSTTITI
                    CONTINUE
                 WHEN 'sir       '
                    MOVE 'Sir' TO CUSTTITI
                    CONTINUE
                 WHEN 'sir_______'
                    MOVE 'Sir' TO CUSTTITI
                    CONTINUE
                 WHEN OTHER
                    MOVE SPACES TO MESSAGEO

                    STRING 'Valid titles are: Mr,Mrs,Miss,Ms,Dr,Drs,'
                       DELIMITED BY SIZE,
                       'Professor,Lord,Sir,Lady' DELIMITED BY SIZE
                       INTO MESSAGEO
                    MOVE 'N' TO VALID-DATA-SW
                    MOVE -1 TO CUSTTITL
                    GO TO ED999
              END-EVALUATE
           END-IF.

           DISPLAY 'CHRISTNL IS ' CHRISTNL
           DISPLAY 'CUSTSNL  IS ' CUSTSNL
           DISPLAY 'CUSTAD1L IS ' CUSTAD1L
           IF CHRISTNL < 1 OR CHRISTNI = '____________________'
              OR CHRISTNI = SPACES
              MOVE SPACES TO MESSAGEO
              MOVE 'Please supply a valid First Name  ' TO
                 MESSAGEO
              MOVE -1 TO CHRISTNL

              MOVE 'N' TO VALID-DATA-SW
              GO TO ED999
           END-IF.

           IF CUSTSNL < 1 OR CUSTSNI = '____________________'
              OR CUSTSNI = SPACES
              MOVE SPACES TO MESSAGEO
              MOVE 'Please supply a valid Surname ' TO
                 MESSAGEO

              MOVE -1 TO CUSTSNL
              MOVE 'N' TO VALID-DATA-SW
              GO TO ED999
           END-IF.

           IF CUSTAD1I(1:1)= '_'  OR CUSTAD1L < 1
              OR CUSTAD1I = SPACES

              MOVE SPACES TO MESSAGEO
              MOVE 'Please supply a valid Address Line 1 ' TO
                 MESSAGEO
              MOVE 'N' TO VALID-DATA-SW
              MOVE -1 TO CUSTAD1L
              GO TO ED999
           END-IF.

           IF DOBDDL < 1 OR DOBDDI = '__'

              MOVE SPACES TO MESSAGEO
              MOVE 'Please supply a valid Date of Birth DD' TO
                 MESSAGEO
              MOVE 'N' TO VALID-DATA-SW
              MOVE -1 TO DOBDDL
              GO TO ED999
           END-IF.

           IF DOBMML < 1 OR DOBMMI = '__'

              MOVE SPACES TO MESSAGEO
              MOVE 'Please supply a valid Date of Birth MM' TO
                 MESSAGEO
              MOVE 'N' TO VALID-DATA-SW
              MOVE -1 TO DOBMML
              GO TO ED999
           END-IF.

           IF DOBYYL < 4 OR DOBYYI = '____'

              MOVE SPACES TO MESSAGEO
              MOVE 'Please supply a valid Date of Birth YYYY' TO
                 MESSAGEO
              MOVE 'N' TO VALID-DATA-SW
              MOVE -1 TO DOBYYL
              GO TO ED999
           END-IF.

           IF DOBDDI NOT NUMERIC

              MOVE SPACES TO MESSAGEO
              MOVE 'Non numeric Date of Birth DD entered      ' TO
                 MESSAGEO
              MOVE 'N' TO VALID-DATA-SW
              MOVE -1 TO DOBDDL
              GO TO ED999
           END-IF.

           IF DOBMMI NOT NUMERIC

              MOVE SPACES TO MESSAGEO
              MOVE 'Non numeric Date of Birth MM entered      ' TO
                 MESSAGEO
              MOVE 'N' TO VALID-DATA-SW
              MOVE -1 TO DOBMML
              GO TO ED999
           END-IF.

           IF DOBYYI NOT NUMERIC

              MOVE SPACES TO MESSAGEO
              MOVE 'Non numeric Date of Birth YYYY entered  ' TO
                 MESSAGEO
              MOVE 'N' TO VALID-DATA-SW
              MOVE -1 TO DOBYYL
              GO TO ED999
           END-IF.

           MOVE DOBDDI TO DOBDDI-CHAR.

           IF DOBDDI-NUM < 01 OR DOBDDI-NUM > 31

              MOVE SPACES TO MESSAGEO
              MOVE 'Please supply a valid Date of Birth (DD)    ' TO
                 MESSAGEO
              MOVE 'N' TO VALID-DATA-SW
              MOVE -1 TO DOBDDL
              GO TO ED999
           END-IF.

           MOVE DOBMMI TO DOBMMI-CHAR.
           IF DOBMMI-NUM < 01 OR DOBMMI-NUM > 12

              MOVE SPACES TO MESSAGEO
              MOVE 'Please supply a valid Date of Birth (MM)    ' TO
                 MESSAGEO
              MOVE 'N' TO VALID-DATA-SW
              MOVE -1 TO DOBMML
              GO TO ED999
           END-IF.

           IF CUSTTITL < 1 OR CHRISTNL < 1 OR CUSTSNL < 1
           OR CUSTAD1L < 1 OR DOBDDL < 1 OR DOBMML < 1
           OR DOBYYL < 1
              MOVE SPACES TO MESSAGEO
              MOVE 'Missing expected data.               ' TO
                 MESSAGEO
              EVALUATE TRUE
                 WHEN CUSTTITL < 1
                    MOVE -1 TO CUSTTITL

                 WHEN CHRISTNL < 1
                    MOVE -1 TO CHRISTNL

                 WHEN CUSTSNL < 1
                    MOVE -1 TO CUSTSNL

                 WHEN CUSTAD1L < 1
                    MOVE -1 TO CUSTAD1L

                 WHEN DOBDDL < 1
                    MOVE -1 TO DOBDDL

                 WHEN DOBMML < 1
                    MOVE -1 TO DOBMML

                 WHEN DOBYYL < 1
                    MOVE -1 TO DOBYYL

              END-EVALUATE

              MOVE 'N' TO VALID-DATA-SW
              GO TO ED999
           END-IF.

       ED999.
           EXIT.


       CRE-CUST-DATA SECTION.
       CCD010.
      *
      *    Set up the fields required by CRECUST then link to it
      *
           INITIALIZE SUBPGM-PARMS.
           MOVE 'CUST' TO SUBPGM-EYECATCHER.
           MOVE 'N' TO SUBPGM-SUCCESS.

      *
      *    Get rid of any underscores which may be present
      *
           INSPECT CUSTTITI REPLACING ALL '_' BY ' '.
           INSPECT CHRISTNI REPLACING ALL '_' BY ' '.
           INSPECT CUSTINSI REPLACING ALL '_' BY ' '.
           INSPECT CUSTSNI  REPLACING ALL '_' BY ' '.

           STRING CUSTTITI DELIMITED BY SPACE,
                  ' ' DELIMITED BY SIZE,
                  CHRISTNI DELIMITED BY SPACE,
                  ' ' DELIMITED BY SIZE,
                  CUSTINSI  DELIMITED BY SPACE,
                  ' ' DELIMITED BY SIZE,
                  CUSTSNI   DELIMITED BY SIZE
           INTO SUBPGM-NAME.

           INSPECT CUSTAD1I REPLACING ALL '_' BY ' '.
           INSPECT CUSTAD2I REPLACING ALL '_' BY ' '.
           INSPECT CUSTAD3I REPLACING ALL '_' BY ' '.

           STRING CUSTAD1I   DELIMITED BY SIZE,
                  CUSTAD2I   DELIMITED BY SIZE,
                  CUSTAD3I   DELIMITED BY SIZE
                  INTO SUBPGM-ADDRESS.

           MOVE DOBDDI TO SUBPGM-BIRTH-DAY.
           MOVE DOBMMI TO SUBPGM-BIRTH-MONTH.
           MOVE DOBYYI TO SUBPGM-BIRTH-YEAR.

           EXEC CICS LINK
              PROGRAM('CRECUST')
              COMMAREA(SUBPGM-PARMS)
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

              STRING 'CCD010 - LINK CRECUST FAIL  '
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
              MOVE 'BNK1CCS - CCD010 - LINK CRECUST FAIL     '
                 TO WS-CICS-FAIL-MSG
              MOVE WS-CICS-RESP  TO WS-CICS-RESP-DISP
              MOVE WS-CICS-RESP2 TO WS-CICS-RESP2-DISP

              PERFORM RESTORE-TERM-DEF
              PERFORM ABEND-THIS-TASK
           END-IF.

      *
      *    Check to see if the creation was successful or not
      *
           IF SUBPGM-SUCCESS = 'N'
              MOVE SPACES TO MESSAGEO
              MOVE 'Sorry but unable to create Customer record '
                TO MESSAGEO
              IF SUBPGM-FAIL-CODE = 'O'
                MOVE 'Sorry, customer is too old. Please check D.O.B.'
                  to messageo
              END-IF
              IF SUBPGM-FAIL-CODE = 'Y'
                MOVE 'Sorry, customer D.O.B. is in the future.'
                  to messageo
              END-IF
              IF SUBPGM-FAIL-CODE = 'Z'
                MOVE 'Sorry, customer D.O.B. is invalid.'
                  to messageo
              END-IF
              MOVE 'N' TO VALID-DATA-SW
           END-IF.

           IF SUBPGM-SUCCESS = 'Y'
              MOVE SPACES TO MESSAGEO
              MOVE
                'The Customer record has been successfully created'
                TO MESSAGEO

              MOVE SUBPGM-SORTCODE    TO SORTCO
              MOVE SUBPGM-NUMBER      TO CUSTNO2O
           END-IF.

      *
      *    Set the values on the map
      *
           MOVE SUBPGM-BIRTH-YEAR     TO DOBYYO.
           MOVE SUBPGM-BIRTH-MONTH    TO DOBMMO.
           MOVE SUBPGM-BIRTH-DAY      TO DOBDDO.
           MOVE SUBPGM-ADDRESS        TO WS-ADDR-SPLIT.
           MOVE WS-ADDR-SPLIT1        TO CUSTAD1O.
           MOVE WS-ADDR-SPLIT2        TO CUSTAD2O.
           MOVE WS-ADDR-SPLIT3        TO CUSTAD3O.
           MOVE SUBPGM-CREDIT-SCORE   TO CREDSCO.
           MOVE SUBPGM-CS-REVIEW-DATE(1:2)
                                      TO SCRDTDDO.
           MOVE SUBPGM-CS-REVIEW-DATE(3:2)
                                      TO SCRDTMMO.
           MOVE SUBPGM-CS-REVIEW-DATE(5:4)
                                      TO SCRDTYYO.

       CCD999.
           EXIT.


       STORE-TERM-DEF SECTION.
       STD010.

      *
      *    Inquire on the terminal and store the UCTRANS settings
      *
           EXEC CICS INQUIRE
                TERMINAL(EIBTRMID)
                UCTRANST(WS-UCTRANS)
                RESP(WS-CICS-RESP)
                RESP2(WS-CICS-RESP2)
           END-EXEC.

      *
      *    Store the original UCTRAN value
      *

           MOVE WS-UCTRANS TO STORED-UCTRANS.

      *
      *    If Uppercase translation is switched on, then set it to
      *    NOUCTRAN(451).
      *
           IF WS-UCTRANS = DFHVALUE(UCTRAN) OR
           WS-UCTRANS = DFHVALUE(TRANIDONLY)

              MOVE DFHVALUE(NOUCTRAN) TO WS-UCTRANS

              EXEC CICS SET TERMINAL(EIBTRMID)
                 UCTRANST(WS-UCTRANS)
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

                 STRING 'STD010 - SET TERMINAL UC FAIL '
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
                 MOVE 'BNK1CCS - STD010 - SET TERMINAL UC FAIL ' TO
                    WS-CICS-FAIL-MSG
                 MOVE WS-CICS-RESP  TO WS-CICS-RESP-DISP
                 MOVE WS-CICS-RESP2 TO WS-CICS-RESP2-DISP

                 PERFORM RESTORE-TERM-DEF
                 PERFORM ABEND-THIS-TASK
              END-IF
           END-IF.

       STD999.
           EXIT.


       RESTORE-TERM-DEF SECTION.
       RTD010.
      *
      *    We must now restore the UCTRAN setting back to what it
      *    was at the start
      *
           MOVE DFHCOMMAREA TO WS-COMM-AREA.

           MOVE WS-COMM-TERM TO WS-UCTRANS.

           EXEC CICS SET TERMINAL(EIBTRMID)
               UCTRANST(WS-UCTRANS)
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

              STRING 'RTD010 - SET TERMINAL UC FAIL '
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
              MOVE 'BNK1CCS - RTD010 - SET TERMINAL UC FAIL '
                 TO WS-CICS-FAIL-MSG
              MOVE WS-CICS-RESP  TO WS-CICS-RESP-DISP
              MOVE WS-CICS-RESP2 TO WS-CICS-RESP2-DISP
              PERFORM ABEND-THIS-TASK
           END-IF.

       RTD999.
           EXIT.


       SEND-MAP SECTION.
       SM010.
      *
      *    If the map needs to have its data erased
      *
           IF SEND-ERASE
              EXEC CICS SEND MAP('BNK1CC')
                  MAPSET('BNK1CCM')
                  FROM(BNK1CCO)
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

                 STRING 'SM010 - SEND MAP ERASE FAIL '
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
                 MOVE 'BNK1CCS - SM010 - SEND MAP ERASE FAIL '
                    TO WS-CICS-FAIL-MSG
                 MOVE WS-CICS-RESP  TO WS-CICS-RESP-DISP
                 MOVE WS-CICS-RESP2 TO WS-CICS-RESP2-DISP

                 PERFORM RESTORE-TERM-DEF
                 PERFORM ABEND-THIS-TASK
              END-IF

              GO TO SM999
           END-IF.

      *
      *    If the map just needs a resend of only the data
      *
           IF SEND-DATAONLY
              EXEC CICS SEND MAP('BNK1CC')
                 MAPSET('BNK1CCM')
                 FROM(BNK1CCO)
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

                 STRING 'SM010 - SEND MAP DATAONLY FAIL '
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
                 MOVE 'BNK1CCS - SM010 - SEND MAP DATAONLY FAIL '
                    TO WS-CICS-FAIL-MSG
                 MOVE WS-CICS-RESP  TO WS-CICS-RESP-DISP
                 MOVE WS-CICS-RESP2 TO WS-CICS-RESP2-DISP

                 PERFORM RESTORE-TERM-DEF
                 PERFORM ABEND-THIS-TASK
              END-IF

              GO TO SM999
           END-IF.

      *
      *    If we have elected to send the map and a beep
      *
           IF SEND-DATAONLY-ALARM
              EXEC CICS SEND MAP('BNK1CC')
                 MAPSET('BNK1CCM')
                 FROM(BNK1CCO)
                 ALARM
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

                 STRING 'SM010 - SEND MAP DATAONLY ALARM FAIL '
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
                 MOVE 'BNK1CCS - SM010 - SEND MAP DATAONLY ALARM FAIL '
                    TO WS-CICS-FAIL-MSG
                 MOVE WS-CICS-RESP  TO WS-CICS-RESP-DISP
                 MOVE WS-CICS-RESP2 TO WS-CICS-RESP2-DISP

                 PERFORM RESTORE-TERM-DEF
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

              STRING 'STM010 - SEND TEXT FAIL'
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
              MOVE 'BNK1CCS - STM010 - SEND TEXT FAIL'
                 TO WS-CICS-FAIL-MSG
              MOVE WS-CICS-RESP  TO WS-CICS-RESP-DISP
              MOVE WS-CICS-RESP2 TO WS-CICS-RESP2-DISP

              PERFORM RESTORE-TERM-DEF
              PERFORM ABEND-THIS-TASK
           END-IF.

       STM999.
           EXIT.


       ABEND-THIS-TASK SECTION.
       ATT010.

      *
      *    Restore the terminal UCTRAN back to its starting position
      *
           MOVE DFHCOMMAREA  TO WS-COMM-AREA.
           MOVE WS-COMM-TERM TO WS-UCTRANS.

           EXEC CICS SET TERMINAL(EIBTRMID)
               UCTRANST(WS-UCTRANS)
               RESP(WS-CICS-RESP)
               RESP2(WS-CICS-RESP2)
           END-EXEC.

           DISPLAY WS-FAIL-INFO.

           EXEC CICS ABEND
              ABCODE('HBNK')
              NODUMP
              CANCEL
           END-EXEC.

       ATT999.
           EXIT.


       POPULATE-TIME-DATE SECTION.
       PTD10.

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


       HANDLE-ABEND SECTION.
       HA010.

           PERFORM RESTORE-TERM-DEF.

           EXEC CICS ASSIGN ABCODE(WS-ABCODE)
           END-EXEC.

           EXEC CICS HANDLE ABEND CANCEL
           END-EXEC.

           EXEC CICS ABEND ABCODE(WS-ABCODE)
              NODUMP
           END-EXEC.

       HA999.
           EXIT.


