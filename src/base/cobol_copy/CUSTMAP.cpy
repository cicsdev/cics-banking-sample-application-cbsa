      ******************************************************************
      *                                                                *
      *  Copyright IBM Corp. 2022                                      *
      *                                                                *
      *                                                                *
      ******************************************************************
       01  CSTMAP1I.
           02  FILLER PIC X(12).
           02  CUSTEYEL    COMP  PIC  S9(4).
           02  CUSTEYEF    PICTURE X.
           02  FILLER REDEFINES CUSTEYEF.
             03 CUSTEYEA    PICTURE X.
           02  FILLER   PICTURE X(6).
           02  CUSTEYEI  PIC X(4).
           02  CUSTKEYL    COMP  PIC  S9(4).
           02  CUSTKEYF    PICTURE X.
           02  FILLER REDEFINES CUSTKEYF.
             03 CUSTKEYA    PICTURE X.
           02  FILLER   PICTURE X(6).
           02  CUSTKEYI  PIC X(10).
           02  CUSTNAMEL    COMP  PIC  S9(4).
           02  CUSTNAMEF    PICTURE X.
           02  FILLER REDEFINES CUSTNAMEF.
             03 CUSTNAMEA    PICTURE X.
           02  FILLER   PICTURE X(6).
           02  CUSTNAMEI  PIC X(60).
           02  ADDR1L    COMP  PIC  S9(4).
           02  ADDR1F    PICTURE X.
           02  FILLER REDEFINES ADDR1F.
             03 ADDR1A    PICTURE X.
           02  FILLER   PICTURE X(6).
           02  ADDR1I  PIC X(60).
           02  ADDR2L    COMP  PIC  S9(4).
           02  ADDR2F    PICTURE X.
           02  FILLER REDEFINES ADDR2F.
             03 ADDR2A    PICTURE X.
           02  FILLER   PICTURE X(6).
           02  ADDR2I  PIC X(60).
           02  ADDR3L    COMP  PIC  S9(4).
           02  ADDR3F    PICTURE X.
           02  FILLER REDEFINES ADDR3F.
             03 ADDR3A    PICTURE X.
           02  FILLER   PICTURE X(6).
           02  ADDR3I  PIC X(40).
           02  CUSTDOBL    COMP  PIC  S9(4).
           02  CUSTDOBF    PICTURE X.
           02  FILLER REDEFINES CUSTDOBF.
             03 CUSTDOBA    PICTURE X.
           02  FILLER   PICTURE X(6).
           02  CUSTDOBI  PIC X(10).
       01  CSTMAP1O REDEFINES CSTMAP1I.
           02  FILLER PIC X(12).
           02  FILLER PICTURE X(3).
           02  CUSTEYEC    PICTURE X.
           02  CUSTEYEP    PICTURE X.
           02  CUSTEYEH    PICTURE X.
           02  CUSTEYEV    PICTURE X.
           02  CUSTEYEU    PICTURE X.
           02  CUSTEYEM    PICTURE X.
           02  CUSTEYEO  PIC X(4).
           02  FILLER PICTURE X(3).
           02  CUSTKEYC    PICTURE X.
           02  CUSTKEYP    PICTURE X.
           02  CUSTKEYH    PICTURE X.
           02  CUSTKEYV    PICTURE X.
           02  CUSTKEYU    PICTURE X.
           02  CUSTKEYM    PICTURE X.
           02  CUSTKEYO  PIC X(10).
           02  FILLER PICTURE X(3).
           02  CUSTNAMEC    PICTURE X.
           02  CUSTNAMEP    PICTURE X.
           02  CUSTNAMEH    PICTURE X.
           02  CUSTNAMEV    PICTURE X.
           02  CUSTNAMEU    PICTURE X.
           02  CUSTNAMEM    PICTURE X.
           02  CUSTNAMEO  PIC X(60).
           02  FILLER PICTURE X(3).
           02  ADDR1C    PICTURE X.
           02  ADDR1P    PICTURE X.
           02  ADDR1H    PICTURE X.
           02  ADDR1V    PICTURE X.
           02  ADDR1U    PICTURE X.
           02  ADDR1M    PICTURE X.
           02  ADDR1O  PIC X(60).
           02  FILLER PICTURE X(3).
           02  ADDR2C    PICTURE X.
           02  ADDR2P    PICTURE X.
           02  ADDR2H    PICTURE X.
           02  ADDR2V    PICTURE X.
           02  ADDR2U    PICTURE X.
           02  ADDR2M    PICTURE X.
           02  ADDR2O  PIC X(60).
           02  FILLER PICTURE X(3).
           02  ADDR3C    PICTURE X.
           02  ADDR3P    PICTURE X.
           02  ADDR3H    PICTURE X.
           02  ADDR3V    PICTURE X.
           02  ADDR3U    PICTURE X.
           02  ADDR3M    PICTURE X.
           02  ADDR3O  PIC X(40).
           02  FILLER PICTURE X(3).
           02  CUSTDOBC    PICTURE X.
           02  CUSTDOBP    PICTURE X.
           02  CUSTDOBH    PICTURE X.
           02  CUSTDOBV    PICTURE X.
           02  CUSTDOBU    PICTURE X.
           02  CUSTDOBM    PICTURE X.
           02  CUSTDOBO  PIC X(10).
