       CBL CICS('SP,EDF')
       CBL SQL
      ******************************************************************
      *                                                                *
      *  Copyright IBM Corp. 2023                                      *
      *                                                                *
      ******************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTCTRL.
       AUTHOR. OGRADYJ.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      * SOURCE-COMPUTER.  IBM-370 WITH DEBUGGING MODE.
       SOURCE-COMPUTER.  IBM-370.
       OBJECT-COMPUTER.  IBM-370.

       INPUT-OUTPUT SECTION.



       DATA DIVISION.
       FILE SECTION.


       WORKING-STORAGE SECTION.

       01 SYSIDERR-RETRY PIC 999.

       LOCAL-STORAGE SECTION.
       COPY SORTCODE.
       01 CUSTOMER-KY.
          03 REQUIRED-SORT-CODE   PIC 9(6) VALUE 0.
          03 REQUIRED-CUST-NUMBER PIC 9(10) VALUE 0.


       01 EXIT-VSAM-READ       PIC X VALUE 'N'.
       01 EXIT-DB2-READ        PIC X VALUE 'N'.

       01 WS-V-RETRIED         PIC X VALUE 'N'.
       01 WS-D-RETRIED         PIC X VALUE 'N'.

       01 SQLCODE-DISPLAY                 PIC S9(8) DISPLAY
           SIGN LEADING SEPARATE.




       01 WS-PASSED-DATA.
          02 WS-TEST-KEY                             PIC X(4).
          02 WS-SORT-CODE                            PIC 9(6).
          02 WS-CUSTOMER-RANGE.
             07 WS-CUSTOMER-RANGE-TOP                PIC X.
             07 WS-CUSTOMER-RANGE-MIDDLE             PIC X.
             07 WS-CUSTOMER-RANGE-BOTTOM             PIC X.

       01 WS-SORT-DIV.
          03 WS-SORT-DIV1                    PIC XX.
          03 WS-SORT-DIV2                    PIC XX.
          03 WS-SORT-DIV3                    PIC XX.

       01 WS-DISP-CUST-NO-VAL                PIC S9(18) DISPLAY.

       01 WS-CUST-REC-LEN                    PIC S9(4) COMP VALUE 0.

       01 NCS-UPDATED                        PIC X VALUE 'N'.

       01 WS-EIBTASKN12                  PIC 9(12) VALUE 0.
       77 PROCTRAN-RETRY PIC 999.

       01 CUSTOMER-KY2.
          03 REQUIRED-SORT-CODE2   PIC 9(6) VALUE 0.
          03 REQUIRED-CUST-NUMBER2 PIC 9(10) VALUE 0.

       01 CUSTOMER-KY2-BYTES REDEFINES CUSTOMER-KY2 PIC X(16).

       01 HIGHEST-CUST-NUMBER  PIC 9(10) VALUE 0.

       01 WS-CICS-RESP PIC S9(8) BINARY.
       01 WS-CICS-RESP2 PIC S9(8) BINARY.

      * Pull in the SQL COMMAREA
       EXEC SQL
          INCLUDE SQLCA
       END-EXEC.

       01 HV-NUMBER-OF-CUSTOMERS PIC S9(8) BINARY.
       01 HV-CUSTOMER-SORTCODE PIC X(6).




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
           COPY CUSTCTRL.


       PROCEDURE DIVISION USING DFHCOMMAREA.
       PREMIERE SECTION.
       P010.
           MOVE SORTCODE TO
              REQUIRED-SORT-CODE.

           PERFORM GET-NUMBER-OF-CUSTOMERS-VSAM


      D    DISPLAY 'OUTPUT DATA IS='
      D       DFHCOMMAREA.


           PERFORM GET-ME-OUT-OF-HERE.

       P999.
           EXIT.




       GET-NUMBER-OF-CUSTOMERS-VSAM SECTION.
       WCV010.

           INITIALIZE DFHCOMMAREA.


           MOVE ZERO TO CUSTOMER-CONTROL-SORTCODE
           MOVE ALL '9' TO CUSTOMER-CONTROL-NUMBER


           EXEC CICS READ
                FILE('CUSTOMER')
                INTO(DFHCOMMAREA)
                RIDFLD(CUSTOMER-CONTROL-KEY)
                KEYLENGTH(16)
                RESP(WS-CICS-RESP)
                RESP2(WS-CICS-RESP2)
           END-EXEC.

           if ws-cics-resp = dfhresp(sysiderr)
             perform varying SYSIDERR-RETRY from 1 by 1
             until SYSIDERR-RETRY > 100
             or ws-cics-resp = dfhresp(normal)
             or ws-cics-resp is not equal to dfhresp(sysiderr)
               exec cics delay for seconds(3)
               end-exec
                EXEC CICS READ
                  FILE('CUSTOMER')
                  INTO(DFHCOMMAREA)
                  RIDFLD(CUSTOMER-CONTROL-KEY)
                  KEYLENGTH(16)
                  RESP(WS-CICS-RESP)
                  RESP2(WS-CICS-RESP2)
               END-EXEC
             end-perform
           end-if

      *
      * Check if the READ was unsuccessful and take action.
      *
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)
              MOVE 'N' TO CUSTOMER-CONTROL-SUCCESS-FLAG
              MOVE '1' TO CUSTOMER-CONTROL-FAIL-CODE
           END-IF.

       WCV999.
           EXIT.




      *
      * Finish
      *
       GET-ME-OUT-OF-HERE SECTION.
       GMOFH010.

           EXEC CICS RETURN
           END-EXEC.

       GMOFH999.
           EXIT.

      /
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

