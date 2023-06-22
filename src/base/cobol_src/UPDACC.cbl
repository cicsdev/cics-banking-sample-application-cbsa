       PROCESS CICS,NODYNAM,NSYMBOL(NATIONAL),TRUNC(STD)
       CBL CICS('SP,EDF,DLI')
       CBL SQL
      ******************************************************************
      *                                                                *
      *  Copyright IBM Corp. 2023                                      *
      *                                                                *
      ******************************************************************
      ******************************************************************
      * This program gets called when someone updates the account
      * details (this excludes the balance which must be updated by
      * crediting or debitting money to/from the account).
      *
      * The program receives as input all of the fields which make
      * up the Account record.
      *
      * It then accesses DB2 datastore & updates the associated
      * Account record.
      *
      * Because it is only permissible to change a limited number of
      * fields on the Account record, and it is NOT possible to amend
      * the balance, no record needs to be written to PR0CTRAN (as the
      * the balance cannot be amended using this method). At the
      * current time, no details of the change are recorded (all that
      * can be amended is the Account Type, the Interest Rate,
      * the overdraft limit and the last and next statement dates). The
      * presentation layer is responsible for ensuring that the fields
      * are validated.
      *
      * If the Account cannot be updated then a failure flag is returned
      * to the calling program.
      *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. UPDACC.
       AUTHOR. Jon Collett.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      * SOURCE-COMPUTER.   IBM-370 WITH DEBUGGING MODE.
       SOURCE-COMPUTER.  IBM-370.
       OBJECT-COMPUTER.  IBM-370.

       INPUT-OUTPUT SECTION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       COPY SORTCODE.



       77 SYSIDERR-RETRY                PIC 999.

      * Get the ACCOUNT DB2 copybook
           EXEC SQL
              INCLUDE ACCDB2
           END-EXEC.

      * ACCOUNT Host variables for DB2
       01 HOST-ACCOUNT-ROW.
          03 HV-ACCOUNT-EYECATCHER      PIC X(4).
          03 HV-ACCOUNT-CUST-NO         PIC X(10).
          03 HV-ACCOUNT-KEY.
             05 HV-ACCOUNT-SORTCODE     PIC X(6).
             05 HV-ACCOUNT-ACC-NO       PIC X(8).
          03 HV-ACCOUNT-ACC-TYPE        PIC X(8).
          03 HV-ACCOUNT-INT-RATE        PIC S9(4)V99 COMP-3.
          03 HV-ACCOUNT-OPENED          PIC X(10).
          03 HV-ACCOUNT-OVERDRAFT-LIM   PIC S9(9) COMP.
          03 HV-ACCOUNT-LAST-STMT       PIC X(10).
          03 HV-ACCOUNT-NEXT-STMT       PIC X(10).
          03 HV-ACCOUNT-AVAIL-BAL       PIC S9(10)V99 COMP-3.
          03 HV-ACCOUNT-ACTUAL-BAL      PIC S9(10)V99 COMP-3.


      * Pull in the SQL COMMAREA
        EXEC SQL
          INCLUDE SQLCA
        END-EXEC.

        01 SQLCODE-DISPLAY             PIC S9(8) DISPLAY
            SIGN LEADING SEPARATE.

       01 WS-CICS-WORK-AREA.
          05 WS-CICS-RESP               PIC S9(8) COMP.
          05 WS-CICS-RESP2              PIC S9(8) COMP.


       LOCAL-STORAGE SECTION.
       01 DB2-DATE-REFORMAT.
          03 DB2-DATE-REF-YR            PIC 9(4).
          03 FILLER                     PIC X.
          03 DB2-DATE-REF-MNTH          PIC 99.
          03 FILLER                     PIC X.
          03 DB2-DATE-REF-DAY           PIC 99.


       01 WS-ACC-DATA.
          COPY ACCOUNT.

       01 WS-EIBTASKN12                 PIC 9(12)     VALUE 0.
       01 WS-SQLCODE-DISP               PIC 9(9)      VALUE 0.

      *
      * Pull in the input and output data structures
      *
       01 DESIRED-ACC-KEY.
          03 DESIRED-SORT-CODE          PIC 9(6).
          03 DESIRED-ACC-NO             PIC 9(8).

       01 NEW-ACCOUNT-AVAILABLE-BALANCE PIC S9(10)V99 VALUE 0.
       01 NEW-ACCOUNT-ACTUAL-BALANCE    PIC S9(10)V99 VALUE 0.
       01 WS-ACC-REC-LEN                PIC S9(4) COMP
                                                      VALUE 0.

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

       01 REJ-REASON                    PIC XX        VALUE SPACES.

       01 CUSTOMER-KY.
          03 REQUIRED-SORT-CODE         PIC 9(6)      VALUE 0.
          03 REQUIRED-ACC-NUM           PIC 9(8)      VALUE 0.

       01 STORM-DRAIN-CONDITION         PIC X(20).

       01 WS-TIME-DATA.
          03 WS-TIME-NOW                PIC 9(6).
          03 WS-TIME-NOW-GRP REDEFINES WS-TIME-NOW.
             05 WS-TIME-NOW-GRP-HH      PIC 99.
             05 WS-TIME-NOW-GRP-MM      PIC 99.
             05 WS-TIME-NOW-GRP-SS      PIC 99.

       01 WS-ABEND-PGM                  PIC X(8)      VALUE 'ABNDPROC'.

       01 ABNDINFO-REC.
           COPY ABNDINFO.

       LINKAGE SECTION.
       01 DFHCOMMAREA.
           COPY UPDACC.

       PROCEDURE DIVISION.
       PREMIERE SECTION.
       A010.

           MOVE SORTCODE TO COMM-SCODE.
           MOVE SORTCODE TO DESIRED-SORT-CODE.

      *
      *           Update the account information
      *
           PERFORM UPDATE-ACCOUNT-DB2

      *
      *    The COMMAREA values have now been set so all we need to do
      *    is finish
      *

           PERFORM GET-ME-OUT-OF-HERE.

       A999.
           EXIT.


       UPDATE-ACCOUNT-DB2 SECTION.
       UAD010.

      *
      *    Position ourself at the matching account record
      *

           MOVE COMM-ACCNO TO DESIRED-ACC-NO.
           MOVE DESIRED-SORT-CODE TO HV-ACCOUNT-SORTCODE.
           MOVE DESIRED-ACC-NO TO HV-ACCOUNT-ACC-NO.

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
              WHERE  (ACCOUNT_SORTCODE = :HV-ACCOUNT-SORTCODE AND
                      ACCOUNT_NUMBER = :HV-ACCOUNT-ACC-NO)
           END-EXEC.

      *
      *    Check that the READ was successful. If not mark the return
      *    field as not successful
      *
           IF SQLCODE NOT = 0

              MOVE 'N' TO COMM-SUCCESS
              MOVE SQLCODE TO SQLCODE-DISPLAY
              DISPLAY 'ERROR: UPDACC returned ' SQLCODE-DISPLAY
              ' on SELECT'
              GO TO UAD999

           END-IF.

      *
      *    If the RESP CODE was OK then update the record
      *
      *

      * During BMS processing the whole account record is
      * returned in the comm area on the update. However, the same
      * is NOT true if the update is coming in via the API.
      *
      * For example, if the API update supplies an account type
      * change, and doesn't supply the other fields, the interest
      * rate and the overdraft limit will come in as zeros. If
      * the updater was only intending to change the account type
      * from a LOAN account into an ISA, in this situation it
      * would also update the overdraft limit and the interest rate
      * to be zero. This may not have been the intention.
      *
      * To avoid this, we will put a rule into the Customer Service
      * interface that on an account update you must always supply the
      * account type AND the interest rate AND the overdraft limit.
      * But that won't stop someone else from using the API directly
      * and unintentionally setting the overdraft and interest rate
      * to zeros when they update just the account type.
      *
      * The best that we can hope to do is to say if we receive
      * an account update and the account type contains all
      * spaces or it starts with a space, then we will reject that
      * update as not being valid. This let's through updates
      * where an account type IS supplied and the overdraft limit and
      * or the interest rate are zero.
      *

           IF (COMM-ACC-TYPE = SPACES OR COMM-ACC-TYPE(1:1) = ' ')
              MOVE 'N' TO COMM-SUCCESS
              DISPLAY 'ERROR: UPDACC has invalid account-type'
              GO TO UAD999

           END-IF.

           MOVE COMM-ACC-TYPE  TO HV-ACCOUNT-ACC-TYPE.
           MOVE COMM-OVERDRAFT TO HV-ACCOUNT-OVERDRAFT-LIM.
           MOVE COMM-INT-RATE  TO HV-ACCOUNT-INT-RATE.

           EXEC SQL
              UPDATE ACCOUNT
              SET ACCOUNT_TYPE = :HV-ACCOUNT-ACC-TYPE,
                  ACCOUNT_INTEREST_RATE = :HV-ACCOUNT-INT-RATE,
                  ACCOUNT_OVERDRAFT_LIMIT = :HV-ACCOUNT-OVERDRAFT-LIM
              WHERE (ACCOUNT_SORTCODE = :HV-ACCOUNT-SORTCODE AND
                     ACCOUNT_NUMBER = :HV-ACCOUNT-ACC-NO)
           END-EXEC.

      *
      *    If the SQLCODE was NOT OK then we need to mark it as failed
      *
           IF SQLCODE NOT = 0
              MOVE 'N' TO COMM-SUCCESS
              MOVE SQLCODE TO SQLCODE-DISPLAY
              DISPLAY 'ERROR: UPDACC returned ' SQLCODE-DISPLAY
              ' on UPDATE'
              GO TO UAD999
           END-IF.

      *
      *    If the SQLCODE was normal then we just need to set the
      *    SUCCESS CODE
      *
           MOVE HV-ACCOUNT-EYECATCHER TO COMM-EYE.
           MOVE HV-ACCOUNT-CUST-NO    TO COMM-CUSTNO.
           MOVE HV-ACCOUNT-SORTCODE   TO COMM-SCODE.
           MOVE HV-ACCOUNT-ACC-NO     TO COMM-ACCNO.
           MOVE HV-ACCOUNT-ACC-TYPE   TO COMM-ACC-TYPE.
           MOVE HV-ACCOUNT-INT-RATE   TO COMM-INT-RATE.
           INITIALIZE DB2-DATE-REFORMAT.
           MOVE HV-ACCOUNT-OPENED     TO DB2-DATE-REFORMAT.
           MOVE DB2-DATE-REF-YR       TO COMM-OPENED-YEAR.
           MOVE DB2-DATE-REF-MNTH     TO COMM-OPENED-MONTH.
           MOVE DB2-DATE-REF-DAY      TO COMM-OPENED-DAY.
           MOVE HV-ACCOUNT-OVERDRAFT-LIM TO COMM-OVERDRAFT.
           INITIALIZE DB2-DATE-REFORMAT.
           MOVE HV-ACCOUNT-LAST-STMT  TO DB2-DATE-REFORMAT.
           MOVE DB2-DATE-REF-YR       TO COMM-LASTST-YEAR.
           MOVE DB2-DATE-REF-MNTH     TO COMM-LASTST-MONTH.
           MOVE DB2-DATE-REF-DAY      TO COMM-LASTST-DAY.
           INITIALIZE DB2-DATE-REFORMAT.
           MOVE HV-ACCOUNT-NEXT-STMT  TO DB2-DATE-REFORMAT.
           MOVE DB2-DATE-REF-YR       TO COMM-NEXTST-YEAR.
           MOVE DB2-DATE-REF-MNTH     TO COMM-NEXTST-MONTH.
           MOVE DB2-DATE-REF-DAY      TO COMM-NEXTST-DAY.
           MOVE HV-ACCOUNT-AVAIL-BAL  TO COMM-AVAIL-BAL.
           MOVE HV-ACCOUNT-ACTUAL-BAL TO COMM-ACTUAL-BAL.

           MOVE 'Y' TO COMM-SUCCESS.

       UAD999.
           EXIT.







      *    MOVE COMM-ACC-TYPE     TO
      *       HV-ACCOUNT-ACC-TYPE.
      *    MOVE COMM-INT-RATE     TO
      *       HV-ACCOUNT-INT-RATE.
      *    MOVE COMM-OVERDRAFT    TO
      *       HV-ACCOUNT-OVERDRAFT-LIM.
      *    MOVE COMM-LAST-STMT-DT TO
      *       HV-ACCOUNT-LAST-STMT.
      *    MOVE COMM-NEXT-STMT-DT TO
      *       HV-ACCOUNT-NEXT-STMT.
      *
      *    MOVE HV-ACCOUNT-AVAIL-BAL TO COMM-AVAIL-BAL.
      *    MOVE HV-ACCOUNT-ACTUAL-BAL TO COMM-ACTUAL-BAL.
      *
      *    EXEC SQL
      *       UPDATE ACCOUNT
      *       SET ACCOUNT_TYPE = :HV-ACCOUNT-ACC-TYPE,
      *           ACCOUNT_INTEREST_RATE = :HV-ACCOUNT-INT-RATE,
      *           ACCOUNT_OVERDRAFT_LIMIT = :HV-ACCOUNT-OVERDRAFT-LIM
      *       WHERE (ACCOUNT_SORTCODE = :HV-ACCOUNT-SORTCODE AND
      *              ACCOUNT_NUMBER = :HV-ACCOUNT-ACC-NO)
      *    END-EXEC.
      *
      *
      *    If the SQLCODE was NOT OK then we need to mark it as failed
      *
      *    IF SQLCODE NOT = 0
      *       MOVE 'N' TO COMM-SUCCESS
      *
      *       GO TO UAD999
      *
      *    END-IF.
      *
      *
      *    If the SQLCODE was normal then we just need to set the
      *    SUCCESS CODE
      *
      *    MOVE 'Y' TO COMM-SUCCESS.
      *
      *UAD999.
      *    EXIT.


       GET-ME-OUT-OF-HERE SECTION.
       GMOOH010.

           EXEC CICS RETURN
           END-EXEC.

       GMOOH999.
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


