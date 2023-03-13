       PROCESS CICS,NODYNAM,NSYMBOL(NATIONAL),TRUNC(STD)
       CBL CICS('SP,EDF')
      ******************************************************************
      *                                                                *
      *  Copyright IBM Corp. 2023                                      *
      *                                                                *
      ******************************************************************

      ******************************************************************
      * This program lists accounts belonging to a specified customer
      * number.
      ******************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. BNK1CCA.
       AUTHOR. James O'Grady.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      *SOURCE-COMPUTER.   IBM-370 WITH DEBUGGING MODE.
        SOURCE-COMPUTER.  IBM-370.
       OBJECT-COMPUTER.  IBM-370.

       INPUT-OUTPUT SECTION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 WS-CICS-WORK-AREA.
          03 WS-CICS-RESP             PIC S9(8) COMP VALUE 0.
          03 WS-CICS-RESP2            PIC S9(8) COMP VALUE 0.

       01 WS-FAIL-INFO.
          03 FILLER                   PIC X(9)  value 'BNK1CCA  '.
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

       COPY BNK1ACC.

       COPY DFHAID.

       01 COMM-DOB-SPLIT.
          03 COMM-DOB-SPLIT-DD         PIC 99.
          03 COMM-DOB-SPLIT-MM         PIC 99.
          03 COMM-DOB-SPLIT-YYYY       PIC 9999.

       01 COMM-ADDR-SPLIT.
          03 COMM-ADDR-SPLIT1          PIC X(60).
          03 COMM-ADDR-SPLIT2          PIC X(60).
          03 COMM-ADDR-SPLIT3          PIC X(40).

       01 WS-COMM-AREA.
          03 WS-COMM-EYE               PIC X(4).
          03 WS-COMM-SCODE             PIC X(6).
          03 WS-COMM-CUSTNO            PIC X(10).
          03 WS-COMM-NAME              PIC X(60).
          03 WS-COMM-ADDR              PIC X(160).
          03 WS-COMM-DOB               PIC 9(8).

       01 WS-CONVERSION.
          03 WS-AVAIL-BAL-9            PIC 9(10)V99.
          03 WS-AVAIL-BAL-CONV-X REDEFINES WS-AVAIL-BAL-9.
             05 WS-AVAIL-BAL-X-PND         PIC X(10).
             05 WS-AVAIL-BAL-X-PNCE        PIC X(2).
          03 WS-ACT-BAL-9              PIC 9(10)V99.
          03 WS-ACT-BAL-CONV-X REDEFINES WS-ACT-BAL-9.
             05 WS-ACT-BAL-X-PND           PIC X(10).
             05 WS-ACT-BAL-X-PNCE          PIC X(2).

       01 INQACCCU-PROGRAM             PIC X(8) VALUE 'INQACCCU'.
       01 INQACCCU-COMMAREA.
          COPY INQACCCU.

       01 WS-INDEX                     PIC S9(8) BINARY.
       01 SCODE-CHAR                   PIC X(6).
       01 ACCNO-CHAR                   PIC X(8).
       01 NUMBER-OF-ACCOUNTS-DISPLAY   PIC Z9 DISPLAY.

       01 WS-AVAIL-BAL-SIGN            PIC X VALUE ' '.
       01 WS-ACT-BAL-SIGN              PIC X VALUE ' '.

       01 DUMP-TITLE                   PIC X(16)
                                          VALUE 'BNK1CCA DUMP NOW'.

       01 COMPANY-NAME-FULL            PIC X(32).

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
          03 COMM-SCODE                PIC X(6).
          03 COMM-CUSTNO               PIC X(10).
          03 COMM-NAME                 PIC X(60).
          03 COMM-ADDR                 PIC X(160).
          03 COMM-DOB                  PIC 9(8).



       PROCEDURE DIVISION.
       PREMIERE SECTION.
       A010.

           EVALUATE TRUE

      *
      *       Is it the first time through? If so, send the map
      *       with erased (empty) data fields.
      *
              WHEN EIBCALEN = ZERO
                 MOVE LOW-VALUE TO BNK1ACCO
                 MOVE -1 TO CUSTNOL
                 SET SEND-ERASE TO TRUE
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
      *       When enter is presseed then process the content
      *
              WHEN EIBAID = DFHENTER
                  PERFORM PROCESS-MAP

      *
      *       When anything else happens, send the invalid key message
      *
              WHEN OTHER
                 MOVE LOW-VALUES TO BNK1ACCO
                 MOVE 'Invalid key pressed.' TO MESSAGEO
                  MOVE -1 TO CUSTNOL
                 SET SEND-DATAONLY-ALARM TO TRUE
                 PERFORM SEND-MAP

           END-EVALUATE.

           EXEC CICS
               RETURN TRANSID('OCCA')
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

              STRING 'A010 - RETURN TRANSID(OCCA) FAIL'
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
              MOVE 'BNK1CCA - A010 - RETURN TRANSID(OCCA) FAIL' TO
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
      *    Validate the received data
      *
           PERFORM EDIT-DATA.

      *
      *    If the data passes validation go on to
      *    retrieve the CUSTOMER information
      *
           IF VALID-DATA
              PERFORM GET-CUST-DATA
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
            INITIALIZE BNK1ACCI.
            EXEC CICS
               RECEIVE MAP('BNK1ACC')
               MAPSET('BNK1ACC')
               INTO(BNK1ACCI)
               RESP(WS-CICS-RESP)
               RESP2(WS-CICS-RESP2)
            END-EXEC.

            IF WS-CICS-RESP NOT = DFHRESP(NORMAL)

      *
      *        Preserve the RESP and RESP2, then set up the
      *        standard ABEND info before getting the applid,
      *        date/time etc. and linking to the Abend Handler
      *        program.
      *
               INITIALIZE ABNDINFO-REC
               MOVE EIBRESP    TO ABND-RESPCODE
               MOVE EIBRESP2   TO ABND-RESP2CODE
      *
      *        Get supplemental information
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
               MOVE 'BNKMENU - RM010 - RECEIVE MAP FAIL ' TO
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
           IF CUSTNOI NOT NUMERIC
              MOVE 'Please enter a customer number.' TO
                  MESSAGEO
              MOVE 'N' TO VALID-DATA-SW
           END-IF.

        ED999.
            EXIT.


        GET-CUST-DATA SECTION.
        GCD010.
      *
      *    Set up the fields required by INQACCCU then link to it
      *
           MOVE 20 TO NUMBER-OF-ACCOUNTS.
           MOVE 'N' TO COMM-SUCCESS OF INQACCCU-COMMAREA.

           MOVE CUSTNOI TO CUSTOMER-NUMBER OF INQACCCU-COMMAREA.
           SET COMM-PCB-POINTER TO NULL.

      *
      *    Link to INQACCCU
      *
           EXEC CICS LINK
               PROGRAM(INQACCCU-PROGRAM)
               COMMAREA(INQACCCU-COMMAREA)
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

              STRING 'GCD010 - LINK INQACCCU FAIL.'
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
              MOVE 'BNK1CCA - GCD010 - LINK INQACCCU  FAIL      '
                 TO WS-CICS-FAIL-MSG
              MOVE WS-CICS-RESP  TO WS-CICS-RESP-DISP
              MOVE WS-CICS-RESP2 TO WS-CICS-RESP2-DISP
              PERFORM ABEND-THIS-TASK
           END-IF.

      *
      *    If no matching customer was found
      *
           IF CUSTOMER-FOUND = 'N'
              MOVE SPACES TO MESSAGEO
              STRING 'Unable to find customer '
                 CUSTOMER-NUMBER DELIMITED BY SIZE
              INTO MESSAGEO

      *
      *       Empty the account numbers in the on screen array
      *
              PERFORM VARYING WS-INDEX FROM 1 BY 1 UNTIL
              WS-INDEX > 10
                 MOVE SPACES TO ACCOUNTO(WS-INDEX)
              END-PERFORM

              GO TO GCD999
           END-IF.

      *
      *    If a customer was found, empty the account numbers
      *    in the on screen array
      *
           PERFORM VARYING WS-INDEX FROM 1 BY 1 UNTIL
           WS-INDEX > 10
              MOVE SPACES to ACCOUNTO(ws-index)
           END-PERFORM.

           IF NUMBER-OF-ACCOUNTS = ZERO
              MOVE 'No accounts found for customer' to MESSAGEO
           ELSE
              IF COMM-SUCCESS = 'N'
                 MOVE SPACES TO MESSAGEO
                 STRING 'Error accessing accounts for customer '
                    CUSTOMER-NUMBER '.' DELIMITED BY SIZE
                 INTO MESSAGEO
              ELSE
                 MOVE NUMBER-OF-ACCOUNTS TO NUMBER-OF-ACCOUNTS-DISPLAY
                 MOVE SPACES TO MESSAGEO
                 STRING NUMBER-OF-ACCOUNTS-DISPLAY
                    DELIMITED BY SIZE,
                    ' accounts found' DELIMITED BY SIZE
                 INTO MESSAGEO
              END-IF

      *
      *       Populate the account number/s on screen (along
      *       with other information returned from INQACCCU).
      *
              PERFORM VARYING WS-INDEX FROM 1 BY 1 UNTIL
              WS-INDEX > NUMBER-OF-ACCOUNTS
              OR WS-INDEX > 10
                 MOVE COMM-SCODE OF INQACCCU-COMMAREA(WS-INDEX)
                    TO SCODE-CHAR
                 MOVE COMM-ACCNO(WS-INDEX) TO ACCNO-CHAR
                 MOVE SPACES TO ACCOUNTO(WS-INDEX)
                 MOVE ' ' TO WS-AVAIL-BAL-SIGN
                 MOVE ' ' TO WS-ACT-BAL-SIGN

                 IF COMM-AVAIL-BAL(WS-INDEX) < 0
                    MOVE '-' TO WS-AVAIL-BAL-SIGN
                 ELSE
                    MOVE '+' TO WS-AVAIL-BAL-SIGN
                 END-IF

                 IF COMM-ACTUAL-BAL(WS-INDEX) < 0
                    MOVE '-' TO WS-ACT-BAL-SIGN
                 ELSE
                    MOVE '+' TO WS-ACT-BAL-SIGN
                 END-IF

                 MOVE COMM-AVAIL-BAL(WS-INDEX) TO
                    WS-AVAIL-BAL-9
                 MOVE COMM-ACTUAL-BAL(WS-INDEX) TO
                    WS-ACT-BAL-9

                 STRING
                    SCODE-CHAR  DELIMITED BY SIZE
                    '      '    DELIMITED BY SIZE
                    ACCNO-CHAR  DELIMITED BY SIZE
                    '         ' DELIMITED BY SIZE
                    COMM-ACC-TYPE(WS-INDEX)
                                DELIMITED BY SIZE
                    '       '
                                DELIMITED BY SIZE
                    WS-AVAIL-BAL-SIGN
                                DELIMITED BY SIZE
                    WS-AVAIL-BAL-X-PND
                                DELIMITED BY SIZE
                    '.'
                                DELIMITED BY SIZE
                    WS-AVAIL-BAL-X-PNCE
                                DELIMITED BY SIZE
                    '  '
                                DELIMITED BY SIZE
                    WS-ACT-BAL-SIGN
                                DELIMITED BY SIZE
                    WS-ACT-BAL-X-PND
                               DELIMITED BY SIZE
                    '.'
                               DELIMITED BY SIZE
                    WS-ACT-BAL-X-PNCE
                               DELIMITED BY SIZE
                 INTO ACCOUNTO(WS-INDEX)
              END-PERFORM

           END-IF.

        GCD999.
            EXIT.


        SEND-MAP SECTION.
        SM010.

      *
      *    If the map needs to have its data erased
      *
           IF SEND-ERASE
               EXEC CICS SEND MAP('BNK1ACC')
                  MAPSET('BNK1ACC')
                  FROM(BNK1ACCO)
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
                 MOVE 'BNK1CCA - SM010 - SEND MAP ERASE FAIL '
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
              EXEC CICS SEND MAP('BNK1ACC')
                 MAPSET('BNK1ACC')
                 FROM(BNK1ACCO)
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
                 MOVE 'BNK1CCA - SM010 - SEND MAP DATAONLY FAIL '
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
              EXEC CICS SEND MAP('BNK1ACC')
                 MAPSET('BNK1ACC')
                 FROM(BNK1ACCO)
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
                 MOVE 'BNK1CCA - SM010 - SEND MAP DATAONLY ALARM FAIL '
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
              MOVE 'BNK1CCA - STM010 - SEND TEXT FAIL'
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
