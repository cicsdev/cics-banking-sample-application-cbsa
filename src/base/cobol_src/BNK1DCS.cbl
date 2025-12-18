       PROCESS CICS,NODYNAM,NSYMBOL(NATIONAL),TRUNC(STD)
       CBL CICS('SP,EDF')
      ******************************************************************
      *                                                                *
      *  Copyright IBM Corp. 2023                                      *
      *                                                                *
      ******************************************************************


      ******************************************************************
      * This is the Display Customer program in the BANKING application
      * BMS suite. It will not only display customer details, it will
      * also delete the customer record if pf5 is pressed or
      * will allow the customer record to be updated if
      * pf10 is pressed.
      *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BNK1DCS.
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
          03 WS-CICS-RESP             PIC S9(8) COMP VALUE 0.
          03 WS-CICS-RESP2            PIC S9(8) COMP VALUE 0.

       01 WS-FAIL-INFO.
          03 FILLER                   PIC X(9)  VALUE 'BNK1DCS  '.
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

       COPY BNK1DCM.

       COPY DFHAID.

       01 WS-UCTRANS                  PIC S9(8) COMP VALUE 0.
       01 STORED-UCTRANS              PIC S9(8) COMP VALUE 0.

       01 COMM-DOB-SPLIT.
          03 COMM-DOB-SPLIT-DD         PIC 99.
          03 COMM-DOB-SPLIT-MM         PIC 99.
          03 COMM-DOB-SPLIT-YYYY       PIC 9999.

       01 COMM-ADDR-SPLIT.
          03 COMM-ADDR-SPLIT1         PIC X(60).
          03 COMM-ADDR-SPLIT2         PIC X(60).
          03 COMM-ADDR-SPLIT3         PIC X(40).

       01 COMM-DOB-UPD-FORMAT.
          03 COMM-DOB-UPD-X.
             05 COMM-DOBX-DD     PIC XX.
             05 COMM-DOBX-MM     PIC XX.
             05 COMM-DOBX-YYYY   PIC XXXX.
          03 COMM-DOB-UPD-9 REDEFINES COMM-DOB-UPD-X.
             05 COMM-DOB9-DD     PIC 99.
             05 COMM-DOB9-MM     PIC 99.
             05 COMM-DOB9-YYYY   PIC 9999.

       01 COMM-CS-REVIEW-UPD-FORMAT.
          03 COMM-CS-REVIEW-UPD-X.
             05 COMM-CS-REVIEWX-DD     PIC XX.
             05 COMM-CS-REVIEWX-MM     PIC XX.
             05 COMM-CS-REVIEWX-YYYY   PIC XXXX.
          03 COMM-CS-REVIEW-UPD-9 REDEFINES COMM-CS-REVIEW-UPD-X.
             05 COMM-CS-REVIEW9-DD     PIC 99.
             05 COMM-CS-REVIEW9-MM     PIC 99.
             05 COMM-CS-REVIEW9-YYYY   PIC 9999.

       01 CREDIT-SCORE-UPD-FORMAT.
          03 CREDIT-SCORE-UPD-X.
             05 CREDIT-SCORE-X   PIC X(3).
          03 CREDIT-SCORE-UPD-9 REDEFINES CREDIT-SCORE-UPD-X.
             05 CREDIT-SCORE-9   PIC 9(3).

       01 COMM-ADDR-UPD-SPLIT.
          03 COMM-ADDR-UPD1           PIC X(60).
          03 COMM-ADDR-UPD2           PIC X(60).
          03 COMM-ADDR-UPD3           PIC X(40).

       01 INQCUST-COMMAREA.
           COPY INQCUST.

       01 DELCUS-COMMAREA.
           COPY DELCUS.

       01 UPDCUST-COMMAREA.
           COPY UPDCUST.

       01 WS-COMM-AREA.
          03 WS-COMM-TERM              PIC S9(8) COMP.
          03 WS-COMM-EYE               PIC X(4).
          03 WS-COMM-SCODE             PIC X(6).
          03 WS-COMM-CUSTNO            PIC X(10).
          03 WS-COMM-NAME              PIC X(60).
          03 WS-COMM-ADDR              PIC X(160).
          03 WS-COMM-DOB               PIC 9(8).
          03 WS-COMM-CREDIT-SCORE      PIC 9(3).
          03 WS-COMM-CS-REVIEW-DATE    PIC 9(8).
          03 WS-COMM-DEL-SUCCESS       PIC X.
          03 WS-COMM-DEL-FAIL-CD       PIC X.
          03 WS-COMM-UPDATE            PIC X.

       01 WS-VALIDATE-NAME             PIC X(60) VALUE ' '.
       01 WS-UNSTR-TITLE               PIC X(9)  VALUE ' '.
       01 WS-TITLE-VALID               PIC X     VALUE ' '.

       COPY DFHBMSCA.

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
          03 COMM-TERM                 PIC S9(8) COMP.
          03 COMM-EYE                  PIC X(4).
          03 COMM-SCODE                PIC X(6).
          03 COMM-CUSTNO               PIC X(10).
          03 COMM-NAME                 PIC X(60).
          03 COMM-ADDR                 PIC X(160).
          03 COMM-DOB                  PIC 9(8).
          03 COMM-CREDIT-SCORE         PIC 9(3).
          03 COMM-CS-REVIEW-DATE       PIC 9(8).
          03 COMM-DEL-SUCCESS          PIC X.
          03 COMM-DEL-FAIL-CD          PIC X.
          03 COMM-UPD                  PIC X.


       PROCEDURE DIVISION USING DFHCOMMAREA.
       PREMIERE SECTION.
       A010.

      *
      *    Set up the Abend handling
      *
           EXEC CICS HANDLE ABEND
                LABEL(ABEND-HANDLING)
           END-EXEC.

           EVALUATE TRUE

      *
      *       Is it the first time through? If so, send the map
      *       with erased (empty) data fields.
      *
              WHEN EIBCALEN = ZERO
                 MOVE LOW-VALUE TO BNK1DCO
                 MOVE -1 TO CUSTNOL
                 SET SEND-ERASE TO TRUE
                 INITIALIZE WS-COMM-AREA

                 PERFORM STORE-TERM-DEF

                 MOVE STORED-UCTRANS TO WS-COMM-TERM

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

      *
      *          Set the terminal UCTRAN back to its starting position
      *
                 PERFORM RESTORE-TERM-DEF

                 EXEC CICS RETURN
                    TRANSID('OMEN')
                    IMMEDIATE
                    RESP(WS-CICS-RESP)
                    RESP2(WS-CICS-RESP2)
                 END-EXEC

      *
      *       When Pf5 is pressed (delete customer record), process the
      *       incoming data
      *
              WHEN EIBAID = DFHPF5
                 PERFORM PROCESS-MAP

      *
      *       When Pf10 is pressed (update customer record), process the
      *       incoming data
      *
              WHEN EIBAID = DFHPF10
                 PERFORM PROCESS-MAP

      *
      *       If the aid or Pf12 is pressed, then send a termination
      *       message.
      *
              WHEN EIBAID = DFHAID OR DFHPF12
      *
      *          Set the terminal UCTRAN back to its starting position
      *
                 PERFORM RESTORE-TERM-DEF

                 PERFORM SEND-TERMINATION-MSG

                 EXEC CICS
                    RETURN
                 END-EXEC

      *
      *       When CLEAR is pressed
      *
              WHEN EIBAID = DFHCLEAR
      *
      *          Set the terminal UCTRAN back to its starting position
      *
                 PERFORM RESTORE-TERM-DEF

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
                 MOVE SPACES                 TO MESSAGEO
                 MOVE 'Invalid key pressed.' TO MESSAGEO
                 MOVE -1 TO CUSTNOL
                 SET SEND-DATAONLY-ALARM TO TRUE
                 PERFORM SEND-MAP

           END-EVALUATE.

      *
      *     Having processed the input or processed the error, check
      *     to see if it is the first time through and if it is NOT
      *     the first time through, then set the return information
      *     accordingly.
      *
           IF EIBCALEN NOT = ZERO
              MOVE COMM-TERM OF DFHCOMMAREA   TO WS-COMM-TERM
              MOVE COMM-EYE OF DFHCOMMAREA    TO WS-COMM-EYE
              MOVE COMM-SCODE OF DFHCOMMAREA  TO WS-COMM-SCODE
              MOVE COMM-CUSTNO OF DFHCOMMAREA TO WS-COMM-CUSTNO
              MOVE COMM-NAME OF DFHCOMMAREA   TO WS-COMM-NAME
              MOVE COMM-ADDR OF DFHCOMMAREA   TO WS-COMM-ADDR
              MOVE COMM-DOB OF DFHCOMMAREA    TO WS-COMM-DOB
              MOVE COMM-CREDIT-SCORE OF DFHCOMMAREA
                 TO WS-COMM-CREDIT-SCORE
              MOVE COMM-CS-REVIEW-DATE OF DFHCOMMAREA
                 TO WS-COMM-CS-REVIEW-DATE
               MOVE COMM-UPD OF DFHCOMMAREA   TO WS-COMM-UPDATE
           END-IF.

           EXEC CICS
              RETURN TRANSID('ODCS')
              COMMAREA(WS-COMM-AREA)
              LENGTH(266)
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

              STRING 'A010 - RETURN TRANSID(ODCS) FAIL'
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
              MOVE 'BNK1DCS - A010 - RETURN TRANSID(ODCS) FAIL' TO
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
           PERFORM RECEIVE-MAP.

      *
      *    Check to see if it just DFHENTER by itself
      *    if it is, then validate the data.
      *
           IF EIBAID = DFHENTER AND COMM-UPD NOT = 'Y'
              MOVE -1 TO CUSTNOL

              PERFORM EDIT-DATA

      *
      *       If the data passes validation go on to
      *       get the customer record
      *
              IF VALID-DATA

                 PERFORM GET-CUST-DATA
              ELSE
                 INITIALIZE INQCUST-COMMAREA
                 SET INQCUST-PCB-POINTER TO NULL
              END-IF
           END-IF.

      *
      *    Check to see if it DFHENTER with the UPDATE flag set.
      *    If it is then, we need to validate and process the
      *    updated data.
      *
           IF EIBAID = DFHENTER AND COMM-UPD = 'Y'
              PERFORM EDIT-DATA2
      *
      *       If the data passes validation go on to
      *       update the customer record
      *
              IF VALID-DATA
                 PERFORM UPDATE-CUST-DATA
              END-IF
           END-IF.

      *
      *    Check to see if it is pF5 (delete customer), validate
      *    & process accordingly (delete the record).
      *
           IF EIBAID = DFHPF5
              PERFORM EDIT-DATA

              PERFORM VALIDATE-DATA
      *
      *       If the data passes validation go on to
      *       delete the customer record
      *
              IF VALID-DATA
                 PERFORM DEL-CUST-DATA
              END-IF
              MOVE -1 TO CUSTNOL

           END-IF.

      *
      *    If it is pf10 (update customer) request to
      *    unlock the fields to allow an update to take place.
      *
           IF EIBAID = DFHPF10
              PERFORM EDIT-DATA
              PERFORM VALIDATE-DATA
      *
      *       If the data passes both sets of validation unprotect
      *       the data on the screen
      *
              IF VALID-DATA
                 PERFORM UNPROT-CUST-DATA
                 MOVE SPACES TO MESSAGEO
                 STRING 'Amend data then press <ENTER>.'
                    DELIMITED BY SIZE,
                    ' '
                    DELIMITED BY SIZE
                    INTO MESSAGEO
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

      *
      *    To ensure that we get the correct case and the
      *    terminal doesn't change the case from ower case to
      *    upper case we need to turn off upper case translation, do
      *    the receive and then turn it on again.
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

                 STRING 'RM010 (1) - SET TERMINAL UC FAIL '
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
                 MOVE 'BNK1DCS - RM010 (1) - SET TERMINAL UC FAIL ' TO
                    WS-CICS-FAIL-MSG
                 MOVE WS-CICS-RESP  TO WS-CICS-RESP-DISP
                 MOVE WS-CICS-RESP2 TO WS-CICS-RESP2-DISP

                 PERFORM RESTORE-TERM-DEF
                 PERFORM ABEND-THIS-TASK
              END-IF
           END-IF.

      *
      *    RECEIVE map ASIS
      *
           EXEC CICS
              RECEIVE MAP('BNK1DC')
              MAPSET('BNK1DCM')
              INTO(BNK1DCI)
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

              STRING 'RM010 - RECEIVE MAP FAIL '
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
              MOVE 'BNK1DCS - RM010 - RECEIVE MAP FAIL ' TO
                 WS-CICS-FAIL-MSG
              MOVE WS-CICS-RESP  TO WS-CICS-RESP-DISP
              MOVE WS-CICS-RESP2 TO WS-CICS-RESP2-DISP

              PERFORM RESTORE-TERM-DEF
              PERFORM ABEND-THIS-TASK
           END-IF.

           MOVE CUSTNOI TO INQCUST-CUSTNO.
           MOVE SORTCI  TO INQCUST-SCODE.

       RM999.
           EXIT.


       EDIT-DATA SECTION.
       ED010.
      *
      *    Perform validation on the incoming fields
      *
           IF CUSTNOL = ZERO OR CUSTNOI = LOW-VALUES
              MOVE SPACES TO MESSAGEO
              MOVE 'Please enter a customer number.' TO
                 MESSAGEO
              MOVE 'N' TO VALID-DATA-SW
              MOVE -1  TO CUSTNOL
              GO TO ED999
           END-IF.


           EXEC CICS BIF DEEDIT
              FIELD(CUSTNOI)
           END-EXEC

           IF CUSTNOI NOT NUMERIC
              MOVE SPACES TO MESSAGEO
              MOVE 'Please enter a customer number.' TO
                 MESSAGEO
              MOVE 'N' TO VALID-DATA-SW
              MOVE -1  TO CUSTNOL
           END-IF.

       ED999.
           EXIT.


       EDIT-DATA2 SECTION.
       ED2010.
      *
      *    Perform more validation
      *

      *
      *    Validate the amended data from the update starting with
      *    Customer Name, this must have a valid title at the begining.
      *
           MOVE CUSTNAMI TO WS-VALIDATE-NAME.

           UNSTRING WS-VALIDATE-NAME DELIMITED BY SPACE
              INTO WS-UNSTR-TITLE.

           MOVE ' ' TO WS-TITLE-VALID.

           EVALUATE WS-UNSTR-TITLE

              WHEN 'Professor'
                 MOVE 'Y' TO WS-TITLE-VALID

              WHEN 'Mr       '
                 MOVE 'Y' TO WS-TITLE-VALID

              WHEN 'Mrs      '
                 MOVE 'Y' TO WS-TITLE-VALID

              WHEN 'Miss     '
                 MOVE 'Y' TO WS-TITLE-VALID

              WHEN 'Ms       '
                 MOVE 'Y' TO WS-TITLE-VALID

              WHEN 'Dr       '
                 MOVE 'Y' TO WS-TITLE-VALID

              WHEN 'Drs      '
                 MOVE 'Y' TO WS-TITLE-VALID

              WHEN 'Lord     '
                 MOVE 'Y' TO WS-TITLE-VALID

              WHEN 'Sir      '
                 MOVE 'Y' TO WS-TITLE-VALID

              WHEN 'Lady     '
                 MOVE 'Y' TO WS-TITLE-VALID

              WHEN OTHER
                 MOVE 'N' TO WS-TITLE-VALID

           END-EVALUATE.

      *
      *    Check to see if the title was valid or not
      *
           IF WS-TITLE-VALID = 'N'
              MOVE SPACES TO MESSAGEO
              STRING 'Valid titles are: Mr,Mrs,Miss,Ms,Dr,Professor,'
                    DELIMITED BY SIZE,
                     'Drs,Lord,Sir,Lady' DELIMITED BY SIZE
                 INTO MESSAGEO
              MOVE 'N' TO VALID-DATA-SW
              move -1 to custnaml
           END-IF.

      *
      *    Ensure that the address hasn't been set to spaces,
      *    anything else (other than spaces) is considered to
      *    be valid.
      *
           IF CUSTAD1I = SPACES AND
           CUSTAD2I = SPACES AND
           CUSTAD3I = SPACES
              MOVE SPACES TO MESSAGEO
              STRING 'Address must not be all spaces'
                    DELIMITED BY SIZE,
                     ' - please reenter' DELIMITED BY SIZE
                 INTO MESSAGEO
              MOVE 'N' TO VALID-DATA-SW
              move -1 to CUSTAD1L
           END-IF.

       ED2999.
           EXIT.


       VALIDATE-DATA SECTION.
       VD010.
      *
      *    Perform even more attribute validation
      *
      *
      *    This hasn't been set, so we're relying on it being random!
      *
           IF INQCUST-SCODE = '000000'
              MOVE 'N' TO VALID-DATA-SW
              MOVE SPACES TO MESSAGEO
              STRING 'The Sort code / Customer number combination is'
                  DELIMITED BY SIZE,
                  ' not VALID.' DELIMITED BY SIZE
                  INTO MESSAGEO
           END-IF.
           IF CUSTNOI = ZERO OR CUSTNOI = '9999999999'
              MOVE 'N' TO VALID-DATA-SW
              MOVE SPACES TO MESSAGEO
              STRING 'The customer number is'
                  DELIMITED BY SIZE,
                  ' not VALID.' DELIMITED BY SIZE
                  INTO MESSAGEO
           END-IF.

       VD999.
           EXIT.


       GET-CUST-DATA SECTION.
       GCD010.
      *
      *    Set up the fields required by INQCUST then link to it
      *
           INITIALIZE INQCUST-COMMAREA.
           SET INQCUST-PCB-POINTER TO NULL.

           MOVE CUSTNOI TO INQCUST-CUSTNO.

           EXEC CICS LINK
              PROGRAM('INQCUST')
              COMMAREA(INQCUST-COMMAREA)
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

              STRING 'GCD010 - LINK INQCUST  FAIL '
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
              MOVE 'BNK1DCS - GCD010 - LINK INQCUST  FAIL      '
                 TO WS-CICS-FAIL-MSG
              MOVE WS-CICS-RESP  TO WS-CICS-RESP-DISP
              MOVE WS-CICS-RESP2 TO WS-CICS-RESP2-DISP

              PERFORM RESTORE-TERM-DEF
              PERFORM ABEND-THIS-TASK
           END-IF.

      *
      *    Check to see if there was any data returned,
      *    if not output an error message
      *
           IF INQCUST-NAME = SPACES AND INQCUST-ADDR = SPACES
              MOVE SPACES TO MESSAGEO
              MOVE 'Sorry, but that customer number was not found.' TO
                 MESSAGEO
              MOVE 'N' TO VALID-DATA-SW
              MOVE SPACES   TO SORTCO
              MOVE SPACES TO CUSTNO2O CUSTNAMO
              MOVE SPACES TO CUSTAD1O CUSTAD2O CUSTAD3O
              MOVE SPACES TO DOBDDO DOBMMO DOBYYO
              MOVE SPACES TO CREDSCO SCRDTDDO SCRDTMMO SCRDTYYO
              move -1 to custnol
              GO TO GCD999
           END-IF.
      *
      *    Set the values on the map
      *
           MOVE INQCUST-SCODE       TO SORTCO.
           MOVE INQCUST-CUSTNO      TO CUSTNO2O.
           MOVE INQCUST-NAME        TO CUSTNAMO.

           MOVE INQCUST-ADDR TO COMM-ADDR-SPLIT.
           MOVE COMM-ADDR-SPLIT1 TO CUSTAD1O.
           MOVE COMM-ADDR-SPLIT2 TO CUSTAD2O.
           MOVE COMM-ADDR-SPLIT3 TO CUSTAD3O.

           MOVE INQCUST-DOB          TO COMM-DOB-SPLIT.
           MOVE COMM-DOB-SPLIT-DD   TO DOBDDO.
           MOVE COMM-DOB-SPLIT-MM   TO DOBMMO.
           MOVE COMM-DOB-SPLIT-YYYY TO DOBYYO.

           MOVE INQCUST-CREDIT-SCORE TO CREDIT-SCORE-9.
           MOVE CREDIT-SCORE-X TO CREDSCO.

           MOVE INQCUST-CS-REVIEW-DD  TO
              SCRDTDDO.
           MOVE INQCUST-CS-REVIEW-MM TO
              SCRDTMMO.
           MOVE INQCUST-CS-REVIEW-YYYY TO
              SCRDTYYO.

           MOVE SPACES TO MESSAGEO.
           IF CUSTNOI = ZERO OR CUSTNOI = '9999999999'
             MOVE   'Customer lookup successful.'
                     TO MESSAGEO
           ELSE
           STRING 'Customer lookup successful. <PF5> to Delete. <PF10'
                 DELIMITED BY SIZE,
                '> to Update.                       '
                 DELIMITED BY SIZE
                 INTO MESSAGEO
           END-IF.
       GCD999.
           EXIT.


       DEL-CUST-DATA SECTION.
       DCD010.
      *
      *    Set up the fields required by DELCUS then link to it
      *
           INITIALIZE DELCUS-COMMAREA
           MOVE CUSTNO2I TO COMM-CUSTNO OF DELCUS-COMMAREA.

           EXEC CICS LINK
              PROGRAM('DELCUS')
              COMMAREA(DELCUS-COMMAREA)
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

              STRING 'DCD010 - LINK DELCUS  FAIL '
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
              MOVE 'BNK1DCS - DCD010 - LINK DELCUS  FAIL      '
                 TO WS-CICS-FAIL-MSG
              MOVE WS-CICS-RESP  TO WS-CICS-RESP-DISP
              MOVE WS-CICS-RESP2 TO WS-CICS-RESP2-DISP

              PERFORM RESTORE-TERM-DEF
              PERFORM ABEND-THIS-TASK
           END-IF.

      *
      *    Check to see if there was any data returned,
      *    if not output an error message
      *
           IF COMM-DEL-SUCCESS OF DELCUS-COMMAREA = 'N' AND
           COMM-DEL-FAIL-CD OF DELCUS-COMMAREA = '1'
              MOVE SPACES TO MESSAGEO
              STRING 'Sorry but that Cust no was not found.'
                 DELIMITED BY SIZE,
                 ' Customer NOT deleted.' DELIMITED BY SIZE
                 INTO MESSAGEO
              MOVE 'N' TO VALID-DATA-SW
              MOVE -1  TO CUSTNO2L
              MOVE COMM-SCODE OF DELCUS-COMMAREA   TO SORTCO
              GO TO DCD999
           END-IF.

           IF COMM-DEL-SUCCESS OF DELCUS-COMMAREA = 'N' AND
           COMM-DEL-FAIL-CD OF DELCUS-COMMAREA = '2'
              MOVE SPACES TO MESSAGEO
              STRING 'Sorry but a datastore error occurred.'
                 DELIMITED BY SIZE,
                 ' Action NOT applied.  ' DELIMITED BY SIZE
                 INTO MESSAGEO
              MOVE 'N' TO VALID-DATA-SW
              MOVE COMM-SCODE OF DELCUS-COMMAREA   TO SORTCO
              GO TO DCD999
           END-IF.

           IF COMM-DEL-SUCCESS OF DELCUS-COMMAREA = 'N' AND
           COMM-DEL-FAIL-CD OF DELCUS-COMMAREA = '3'
              MOVE SPACES TO MESSAGEO
              STRING 'Sorry but a delete error occurred.'
                 DELIMITED BY SIZE,
                 ' Customer NOT deleted.' DELIMITED BY SIZE
                 INTO MESSAGEO
              MOVE 'N' TO VALID-DATA-SW
              MOVE COMM-SCODE OF DELCUS-COMMAREA   TO SORTCO
              GO TO DCD999
           END-IF.

           IF COMM-DEL-SUCCESS OF DELCUS-COMMAREA = 'N'
              MOVE SPACES TO MESSAGEO
              STRING 'Sorry but an error occurred.'
                 DELIMITED BY SIZE,
                 ' Customer NOT deleted.' DELIMITED BY SIZE
                 INTO MESSAGEO
              MOVE 'N' TO VALID-DATA-SW
              MOVE COMM-SCODE OF DELCUS-COMMAREA   TO SORTCO
              GO TO DCD999
           END-IF.

      *
      *    Set up map data
      *
           MOVE SPACES TO SORTCO.
           MOVE SPACES TO CUSTNO2O.
           MOVE SPACES TO CUSTNAMO.
           MOVE SPACES TO CUSTAD1O.
           MOVE SPACES TO CUSTAD2O.
           MOVE SPACES TO CUSTAD3O.
           MOVE SPACES TO DOBDDO.
           MOVE SPACES TO DOBMMO.
           MOVE SPACES TO DOBYYO.
           MOVE SPACES TO CREDSCO.
           MOVE SPACES TO SCRDTDDO.
           MOVE SPACES TO SCRDTMMO.
           MOVE SPACES TO SCRDTYYO.

           MOVE SPACES TO MESSAGEO.

           STRING 'Customer ' DELIMITED BY SIZE,
              COMM-CUSTNO OF DELCUS-COMMAREA DELIMITED BY SIZE,
              ' and associated accounts were successfully'
              DELIMITED BY SIZE,
              ' deleted.'
              DELIMITED BY SIZE,
           INTO MESSAGEO.

       DCD999.
           EXIT.


       UPDATE-CUST-DATA SECTION.
       UPDCD010.
      *
      *    Set up the fields required by UPDUCUST then link to it
      *
           INITIALIZE UPDCUST-COMMAREA.

           MOVE SORTCI TO COMM-SCODE OF UPDCUST-COMMAREA.
           MOVE CUSTNO2I TO COMM-CUSTNO OF UPDCUST-COMMAREA.
           MOVE CUSTNAMI TO COMM-NAME OF UPDCUST-COMMAREA.
           STRING CUSTAD1I
                    DELIMITED BY SIZE,
                  CUSTAD2I
                    DELIMITED BY SIZE,
                  CUSTAD3I
                    DELIMITED BY SIZE,
           INTO COMM-ADDR OF UPDCUST-COMMAREA.

           MOVE DOBDDI TO COMM-DOBX-DD.
           MOVE DOBMMI TO COMM-DOBX-MM.
           MOVE DOBYYI TO COMM-DOBX-YYYY.
           MOVE COMM-DOB-UPD-9 TO COMM-DOB OF UPDCUST-COMMAREA.
           MOVE CREDSCI TO CREDIT-SCORE-X.
           MOVE CREDIT-SCORE-9 TO COMM-CREDIT-SCORE OF UPDCUST-COMMAREA.

           MOVE SCRDTDDI TO COMM-CS-REVIEWX-DD.
           MOVE SCRDTMMI TO COMM-CS-REVIEWX-MM.
           MOVE SCRDTYYI TO COMM-CS-REVIEWX-YYYY.
           MOVE COMM-CS-REVIEW-UPD-9
             TO COMM-CS-REVIEW-DATE OF UPDCUST-COMMAREA.

           MOVE SPACE TO COMM-UPD-SUCCESS.
           MOVE SPACE TO COMM-UPD-FAIL-CD.

           EXEC CICS LINK
              PROGRAM('UPDCUST')
              COMMAREA(UPDCUST-COMMAREA)
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

              STRING 'UAPDC010 - LINK UPDCUST  FAIL'
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
              MOVE 'BNK1DCS - UPDCD010 - LINK UPDCUST  FAIL    '
                 TO WS-CICS-FAIL-MSG
              MOVE WS-CICS-RESP  TO WS-CICS-RESP-DISP
              MOVE WS-CICS-RESP2 TO WS-CICS-RESP2-DISP

              PERFORM RESTORE-TERM-DEF
              PERFORM ABEND-THIS-TASK
           END-IF.


      *
      *    Check to see if there was any data returned,
      *    if not output an error message
      *
           IF COMM-UPD-SUCCESS = 'N'

              EVALUATE COMM-UPD-FAIL-CD
                 WHEN '1'
                    MOVE SPACES TO MESSAGEO
                    STRING 'Sorry but that Cust no was not found.'
                       DELIMITED BY SIZE,
                       ' Customer NOT updated.' DELIMITED BY SIZE
                       INTO MESSAGEO
                    MOVE 'N' TO VALID-DATA-SW
                    MOVE -1 TO CUSTNO2L
                    MOVE COMM-SCODE OF UPDCUST-COMMAREA   TO SORTCO
                    GO TO UPDCD999

                 WHEN '2'
                    MOVE SPACES TO MESSAGEO
                    STRING 'Sorry but a datastore error occurred.'
                       DELIMITED BY SIZE,
                       ' Customer NOT updated.' DELIMITED BY SIZE
                       INTO MESSAGEO
                    MOVE 'N' TO VALID-DATA-SW
                    MOVE -1 TO CUSTNO2L
                    MOVE COMM-SCODE OF UPDCUST-COMMAREA   TO SORTCO
                    GO TO UPDCD999

                 WHEN '3'
                    MOVE SPACES TO MESSAGEO
                    STRING 'Sorry but an update error occurred.'
                       DELIMITED BY SIZE,
                       ' Customer NOT updated.' DELIMITED BY SIZE
                       INTO MESSAGEO
                    MOVE 'N' TO VALID-DATA-SW
                    MOVE COMM-SCODE OF UPDCUST-COMMAREA   TO SORTCO
                    MOVE -1 TO CUSTNO2L
                    GO TO UPDCD999

                 WHEN OTHER
                    MOVE SPACES TO MESSAGEO
                    STRING 'Sorry but an unknown error occurred.'
                       DELIMITED BY SIZE,
                       ' Customer NOT updated.' DELIMITED BY SIZE
                       INTO MESSAGEO
                    MOVE 'N' TO VALID-DATA-SW
                    MOVE COMM-SCODE OF UPDCUST-COMMAREA   TO SORTCO
                    MOVE -1 TO CUSTNO2L
                    GO TO UPDCD999

              END-EVALUATE

           END-IF.

      *
      *    Set up the map data
      *
           MOVE COMM-SCODE  OF UPDCUST-COMMAREA TO SORTCO.
           MOVE COMM-CUSTNO OF UPDCUST-COMMAREA TO CUSTNO2O.
           MOVE COMM-NAME   OF UPDCUST-COMMAREA TO CUSTNAMO.

           MOVE COMM-ADDR  OF UPDCUST-COMMAREA TO COMM-ADDR-SPLIT.
           MOVE COMM-ADDR-SPLIT1     TO CUSTAD1O.
           MOVE COMM-ADDR-SPLIT2     TO CUSTAD2O.
           MOVE COMM-ADDR-SPLIT3     TO CUSTAD3O.

           MOVE COMM-DOB   OF UPDCUST-COMMAREA TO COMM-DOB-SPLIT.
           MOVE COMM-DOB-SPLIT-DD    TO DOBDDO.
           MOVE COMM-DOB-SPLIT-MM    TO DOBMMO.
           MOVE COMM-DOB-SPLIT-YYYY  TO DOBYYO.

           MOVE COMM-CREDIT-SCORE OF UPDCUST-COMMAREA TO CREDIT-SCORE-9.
           MOVE CREDIT-SCORE-X TO CREDSCO.

           MOVE COMM-CS-DAY   OF UPDCUST-COMMAREA
              TO SCRDTDDO.
           MOVE COMM-CS-MONTH OF UPDCUST-COMMAREA
              TO SCRDTMMO.
           MOVE COMM-CS-YEAR  OF UPDCUST-COMMAREA
              TO SCRDTYYO.

           MOVE SPACES TO MESSAGEO.
           STRING 'Customer ' DELIMITED BY SIZE,
                 COMM-CUSTNO OF UPDCUST-COMMAREA DELIMITED BY SIZE,
                 ' was updated successfully'
                 DELIMITED BY SIZE,
                 INTO MESSAGEO.
           MOVE -1  TO CUSTNOL.
           PERFORM  PROT-CUST-DATA.

       UPDCD999.
           EXIT.


       UNPROT-CUST-DATA SECTION.
       UCD010.

      *
      *    Unprotect the fields on the screen to allow
      *    editing. Preserve the information on the screen in
      *    the comm area
      *
           MOVE 'CUST'   TO COMM-EYE OF DFHCOMMAREA.
           MOVE SORTCO   TO COMM-SCODE OF DFHCOMMAREA.
           MOVE CUSTNO2O TO COMM-CUSTNO OF DFHCOMMAREA.
           MOVE CUSTNAMO TO COMM-NAME OF DFHCOMMAREA.

           MOVE CUSTAD1O TO COMM-ADDR-UPD1 .
           MOVE CUSTAD2O TO COMM-ADDR-UPD2.
           MOVE CUSTAD3O TO COMM-ADDR-UPD3.
           MOVE COMM-ADDR-UPD-SPLIT TO COMM-ADDR OF DFHCOMMAREA.

           MOVE DOBDDO   TO COMM-DOBX-DD.
           MOVE DOBMMO   TO COMM-DOBX-MM.
           MOVE DOBYYO   TO COMM-DOBX-YYYY.
           MOVE COMM-DOB-UPD-9 TO COMM-DOB OF DFHCOMMAREA.

           MOVE CREDSCO  TO CREDIT-SCORE-X.
           MOVE CREDIT-SCORE-9 TO COMM-CREDIT-SCORE OF DFHCOMMAREA.

           MOVE SCRDTDDO TO COMM-CS-REVIEWX-DD.
           MOVE SCRDTMMO TO COMM-CS-REVIEWX-MM.
           MOVE SCRDTYYO TO COMM-CS-REVIEWX-YYYY.
           MOVE COMM-CS-REVIEW-UPD-9
              TO COMM-CS-REVIEW-DATE OF DFHCOMMAREA.

      *
      *    Set a flag to indicate that this is preserved
      *    UPDATE data
      *
           MOVE 'Y'      TO COMM-UPD.

      *
      *    At the moment the fields are protected, but we
      *    need to allow editing on the map for Customer Name
      *    and Customer Address, the remaining fields
      *    (Customer Sortcode, and DOB) are already protected.
      *

           MOVE DFHGREEN TO CUSTNAMC.
           MOVE 'A' TO CUSTNAMA.
           MOVE DFHUNDLN TO CUSTNAMH.

      *
      *    Postion the cursor at the customer name
      *
           MOVE -1 TO CUSTNAML.

           MOVE DFHGREEN TO CUSTAD1C.
           MOVE 'A' TO CUSTAD1A.
           MOVE DFHUNDLN TO CUSTAD1H.

           MOVE DFHGREEN TO CUSTAD2C.
           MOVE 'A' TO CUSTAD2A.
           MOVE DFHUNDLN TO CUSTAD2H.

           MOVE DFHGREEN TO CUSTAD3C.
           MOVE 'A' TO CUSTAD3A.
           MOVE DFHUNDLN TO CUSTAD3H.

      *
      *    The Customer Number field is editable and we
      *    need to dynamically protect that (to stop editing of it).
      *
           MOVE DFHBMASK TO CUSTNOA.

      *
      *    Also, make the Customer Number field white (neutral) the
      *    same as other protected fields and remove the underscore.
      *
           MOVE DFHNEUTR TO CUSTNOC.
           MOVE HIGH-VALUES  TO CUSTNOH.

       UCD999.
           EXIT.


       SEND-MAP SECTION.
       SM010.
      *
      *    If the map needs to have its data erased
      *
           IF SEND-ERASE
              EXEC CICS SEND MAP('BNK1DC')
                 MAPSET('BNK1DCM')
                 FROM(BNK1DCO)
                 ERASE
                 CURSOR
                 FREEKB
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
                 MOVE 'BNK1DCS - SM010 - SEND MAP ERASE FAIL '
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
              EXEC CICS SEND MAP('BNK1DC')
                 MAPSET('BNK1DCM')
                 FROM(BNK1DCO)
                 DATAONLY
                 CURSOR
                 FREEKB
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
                 MOVE 'BNK1DCS - SM010 - SEND MAP DATAONLY FAIL '
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
              EXEC CICS SEND MAP('BNK1DC')
                 MAPSET('BNK1DCM')
                 FROM(BNK1DCO)
                 DATAONLY
                 CURSOR
                 ALARM
                 FREEKB
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
                 MOVE 'BNK1DCS - SM010 - SEND MAP DATAONLY ALARM FAIL '
                    TO WS-CICS-FAIL-MSG
                 MOVE WS-CICS-RESP  TO WS-CICS-RESP-DISP
                 MOVE WS-CICS-RESP2 TO WS-CICS-RESP2-DISP

                 PERFORM RESTORE-TERM-DEF
                 PERFORM ABEND-THIS-TASK
              END-IF

           END-IF.

       SM999.
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
      *    If Uppercase translation is switched on,
      *    then set it to NOUCTRAN(451).
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
                 MOVE 'BNK1DCS - STD010 - SET TERMINAL UC FAIL ' TO
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

              STRING 'RTD010 - SET TERMINAL UC FAIL'
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
              MOVE 'BNK1DCS - RTD010 - SET TERMINAL UC FAIL '
                 TO WS-CICS-FAIL-MSG
              MOVE WS-CICS-RESP  TO WS-CICS-RESP-DISP
              MOVE WS-CICS-RESP2 TO WS-CICS-RESP2-DISP
      *
      *       If restoring the terminal definition
      *       doesn't work, we can't
      *       do it again. This led to AICA abends!
      *
              PERFORM ABEND-THIS-TASK
           END-IF.

       RTD999.
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
              MOVE 'BNK1DCS - STM010 - SEND TEXT FAIL'
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


       PROT-CUST-DATA SECTION.
       PCD010.
      *
      *    Resolve data on the map
      *
           MOVE COMM-SCODE OF UPDCUST-COMMAREA TO SORTCO.
           MOVE COMM-CUSTNO OF UPDCUST-COMMAREA TO CUSTNO2O.
           MOVE COMM-NAME OF UPDCUST-COMMAREA   TO CUSTNAMO.

           MOVE COMM-ADDR OF UPDCUST-COMMAREA TO COMM-ADDR-SPLIT.
           MOVE COMM-ADDR-SPLIT1     TO CUSTAD1O.
           MOVE COMM-ADDR-SPLIT2     TO CUSTAD2O.
           MOVE COMM-ADDR-SPLIT3     TO CUSTAD3O.

           MOVE COMM-DOB OF UPDCUST-COMMAREA TO COMM-DOB-SPLIT.
           MOVE COMM-DOB-SPLIT-DD    TO DOBDDO.
           MOVE COMM-DOB-SPLIT-MM    TO DOBMMO.
           MOVE COMM-DOB-SPLIT-YYYY  TO DOBYYO.

           MOVE 'CUST'   TO COMM-EYE OF DFHCOMMAREA.
           MOVE SORTCO   TO COMM-SCODE OF DFHCOMMAREA.
           MOVE CUSTNO2O TO COMM-CUSTNO OF DFHCOMMAREA.
           MOVE CUSTNAMO TO COMM-NAME OF DFHCOMMAREA.

           MOVE CUSTAD1O TO COMM-ADDR-UPD1.
           MOVE CUSTAD2O TO COMM-ADDR-UPD2.
           MOVE CUSTAD3O TO COMM-ADDR-UPD3.
           MOVE COMM-ADDR-UPD-SPLIT TO COMM-ADDR OF DFHCOMMAREA.


           MOVE DOBDDO   TO COMM-DOBX-DD.
           MOVE DOBMMO   TO COMM-DOBX-MM.
           MOVE DOBYYO   TO COMM-DOBX-YYYY.
           MOVE COMM-DOB-UPD-9 TO COMM-DOB OF DFHCOMMAREA.

           MOVE CREDSCO  TO CREDIT-SCORE-X.
           MOVE CREDIT-SCORE-9 TO COMM-CREDIT-SCORE OF DFHCOMMAREA.

           MOVE SCRDTDDO TO COMM-CS-REVIEWX-DD.
           MOVE SCRDTMMO TO COMM-CS-REVIEWX-MM.
           MOVE SCRDTYYO TO COMM-CS-REVIEWX-YYYY.

           MOVE COMM-CS-REVIEW-UPD-9
              TO COMM-CS-REVIEW-DATE OF DFHCOMMAREA.

      *
      *    Set a flag to indicate that this is preserved UPDATE data
      *
           MOVE 'N'      TO COMM-UPD.

      *
      *    At the moment the fields are protected, but we need to allow
      *    editing on the map for Customer Name and Customer Address,
      *    the remaining fields (Customer Sortcode, and DOB) are already
      *    protected.
      *
           MOVE DFHNEUTR TO CUSTNO2C.
           MOVE DFHBMPRF TO CUSTNO2A.

           MOVE DFHNEUTR TO CUSTNAMC.
           MOVE DFHBMPRF TO CUSTNAMA.
           MOVE HIGH-VALUES  TO CUSTNAMH.

      *
      *    Postion the cursor at the customer number
      *
           MOVE -1 TO CUSTNOL.


           MOVE DFHNEUTR TO CUSTAD1C.
           MOVE DFHBMPRF TO CUSTAD1A.
           MOVE HIGH-VALUES  TO CUSTAD1H.
.

           MOVE DFHNEUTR TO CUSTAD2C.
           MOVE DFHBMPRF TO CUSTAD2A.
           MOVE HIGH-VALUES  TO CUSTAD2H.


           MOVE DFHNEUTR TO CUSTAD3C.
           MOVE DFHBMPRF TO CUSTAD3A.
           MOVE HIGH-VALUES  TO CUSTAD3H.

      *
      *    The Customer Number field is editable and we
      *    need to allow this again
      *
           MOVE DFHBMFSE TO CUSTNOA.

      *
      *    Also, make the Customer Number field green the
      *    same as other unprotected fields and add the underscore.
      *
           MOVE DFHGREEN TO CUSTNOC.
           MOVE DFHUNDLN TO CUSTNOH.
           MOVE COMM-CUSTNO OF UPDCUST-COMMAREA TO CUSTNOO CUSTNO2O.

       PCD999.
           EXIT.


       ABEND-HANDLING SECTION.
       AH010.
           PERFORM RESTORE-TERM-DEF.

           EXEC CICS HANDLE
              ABEND
              CANCEL
           END-EXEC.

           PERFORM ABEND-THIS-TASK.

       AH999.
           EXIT.
