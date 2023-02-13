      ******************************************************************
      *                                                                *
      *  Copyright IBM Corp. 2023                                      *
      *                                                                *
      *                                                                *
      ******************************************************************
           03 PROC-TRAN-DATA.
              05 PROC-TRAN-EYE-CATCHER        PIC X(4).
              88 PROC-TRAN-VALID VALUE 'PRTR'.
              05 PROC-TRAN-LOGICAL-DELETE-AREA REDEFINES
                 PROC-TRAN-EYE-CATCHER.
                 07 PROC-TRAN-LOGICAL-DELETE-FLAG PIC X.
                 88 PROC-TRAN-LOGICALLY-DELETED VALUE X'FF'.
                 07 FILLER PIC X(3).
              05 PROC-TRAN-ID.
                 07 PROC-TRAN-SORT-CODE       PIC 9(6).
                 07 PROC-TRAN-NUMBER          PIC 9(8).
              05 PROC-TRAN-DATE               PIC 9(8).
              05 PROC-TRAN-DATE-GRP REDEFINES PROC-TRAN-DATE.
                 07 PROC-TRAN-DATE-GRP-YYYY   PIC 9999.
                 07 PROC-TRAN-DATE-GRP-MM     PIC 99.
                 07 PROC-TRAN-DATE-GRP-DD     PIC 99.
              05 PROC-TRAN-TIME               PIC 9(6).
              05 PROC-TRAN-TIME-GRP REDEFINES PROC-TRAN-TIME.
                 07 PROC-TRAN-TIME-GRP-HH     PIC 99.
                 07 PROC-TRAN-TIME-GRP-MM     PIC 99.
                 07 PROC-TRAN-TIME-GRP-SS     PIC 99.
              05 PROC-TRAN-REF                PIC 9(12).
              05 PROC-TRAN-TYPE               PIC X(3).
              88 PROC-TY-CHEQUE-ACKNOWLEDGED      VALUE 'CHA'.
              88 PROC-TY-CHEQUE-FAILURE           VALUE 'CHF'.
              88 PROC-TY-CHEQUE-PAID-IN           VALUE 'CHI'.
              88 PROC-TY-CHEQUE-PAID-OUT          VALUE 'CHO'.
              88 PROC-TY-CREDIT                   VALUE 'CRE'.
              88 PROC-TY-DEBIT                    VALUE 'DEB'.
              88 PROC-TY-WEB-CREATE-ACCOUNT       VALUE 'ICA'.
              88 PROC-TY-WEB-CREATE-CUSTOMER      VALUE 'ICC'.
              88 PROC-TY-WEB-DELETE-ACCOUNT       VALUE 'IDA'.
              88 PROC-TY-WEB-DELETE-CUSTOMER      VALUE 'IDC'.
              88 PROC-TY-BRANCH-CREATE-ACCOUNT    VALUE 'OCA'.
              88 PROC-TY-BRANCH-CREATE-CUSTOMER   VALUE 'OCC'.
              88 PROC-TY-BRANCH-DELETE-ACCOUNT    VALUE 'ODA'.
              88 PROC-TY-BRANCH-DELETE-CUSTOMER   VALUE 'ODC'.
              88 PROC-TY-CREATE-SODD              VALUE 'OCS'.
              88 PROC-TY-PAYMENT-CREDIT           VALUE 'PCR'.
              88 PROC-TY-PAYMENT-DEBIT            VALUE 'PDR'.
              88 PROC-TY-TRANSFER                 VALUE 'TFR'.
              05 PROC-TRAN-DESC               PIC X(40).
              05 PROC-TRAN-DESC-XFR REDEFINES PROC-TRAN-DESC.
                07 PROC-TRAN-DESC-XFR-HEADER PIC X(26).
                88 PROC-TRAN-DESC-XFR-FLAG
                   VALUE 'TRANSFER'.
                07 PROC-TRAN-DESC-XFR-SORTCODE
                   PIC 9(6).
                07 PROC-TRAN-DESC-XFR-ACCOUNT
                   PIC 9(8).
              05 PROC-TRAN-DESC-DELACC REDEFINES PROC-TRAN-DESC.
                07 PROC-DESC-DELACC-CUSTOMER PIC 9(10).
                07 PROC-DESC-DELACC-ACCTYPE PIC X(8).
                07 PROC-DESC-DELACC-LAST-DD PIC 99.
                07 PROC-DESC-DELACC-LAST-MM PIC 99.
                07 PROC-DESC-DELACC-LAST-YYYY PIC 9999.
                07 PROC-DESC-DELACC-NEXT-DD PIC 99.
                07 PROC-DESC-DELACC-NEXT-MM PIC 99.
                07 PROC-DESC-DELACC-NEXT-YYYY PIC 9999.
                07 PROC-DESC-DELACC-FOOTER PIC X(6).
                88 PROC-DESC-DELACC-FLAG
                   VALUE 'DELETE'.
              05 PROC-TRAN-DESC-CREACC REDEFINES PROC-TRAN-DESC.
                07 PROC-DESC-CREACC-CUSTOMER PIC 9(10).
                07 PROC-DESC-CREACC-ACCTYPE PIC X(8).
                07 PROC-DESC-CREACC-LAST-DD PIC 99.
                07 PROC-DESC-CREACC-LAST-MM PIC 99.
                07 PROC-DESC-CREACC-LAST-YYYY PIC 9999.
                07 PROC-DESC-CREACC-NEXT-DD PIC 99.
                07 PROC-DESC-CREACC-NEXT-MM PIC 99.
                07 PROC-DESC-CREACC-NEXT-YYYY PIC 9999.
                07 PROC-DESC-CREACC-FOOTER PIC X(6).
                88 PROC-DESC-CREACC-FLAG
                   VALUE 'CREATE'.
              05 PROC-TRAN-DESC-DELCUS REDEFINES PROC-TRAN-DESC.
                07 PROC-DESC-DELCUS-SORTCODE PIC 9(6).
                07 PROC-DESC-DELCUS-CUSTOMER PIC 9(10).
                07 PROC-DESC-DELCUS-NAME     PIC X(14).
                07 PROC-DESC-DELCUS-DOB-YYYY  PIC 9999.
                07 PROC-DESC-DELCUS-FILLER    PIC X.
                88 PROC-DESC-DELCUS-FILLER-SET VALUE '-'.
                07 PROC-DESC-DELCUS-DOB-MM    PIC 99.
                07 PROC-DESC-DELCUS-FILLER2   PIC X.
                88 PROC-DESC-DELCUS-FILLER2-SET VALUE '-'.
                07 PROC-DESC-DELCUS-DOB-DD    PIC 99.
              05 PROC-TRAN-DESC-CRECUS REDEFINES PROC-TRAN-DESC.
                07 PROC-DESC-CRECUS-SORTCODE PIC 9(6).
                07 PROC-DESC-CRECUS-CUSTOMER PIC 9(10).
                07 PROC-DESC-CRECUS-NAME     PIC X(14).
                07 PROC-DESC-CRECUS-DOB-YYYY  PIC 9999.
                07 PROC-DESC-CRECUS-FILLER    PIC X.
                88 PROC-DESC-CRECUS-FILLER-SET VALUE '-'.
                07 PROC-DESC-CRECUS-DOB-MM    PIC 99.
                07 PROC-DESC-CRECUS-FILLER2   PIC X.
                88 PROC-DESC-CRECUS-FILLER2-SET VALUE '-'.
                07 PROC-DESC-CRECUS-DOB-DD    PIC 99.
              05 PROC-TRAN-AMOUNT             PIC S9(10)V99.
