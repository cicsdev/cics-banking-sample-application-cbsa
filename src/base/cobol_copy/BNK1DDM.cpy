      ******************************************************************
      *                                                                *
      *  Copyright IBM Corp. 2022                                      *
      *                                                                *
      *                                                                *
      ******************************************************************
       01  BNK1DDI.
           02  FILLER PIC X(12).
           02  COMPANYL    COMP  PIC  S9(4).
           02  COMPANYF    PICTURE X.
           02  FILLER REDEFINES COMPANYF.
             03 COMPANYA    PICTURE X.
           02  FILLER   PICTURE X(6).
           02  COMPANYI  PIC X(56).
           02  MESSAGEL    COMP  PIC  S9(4).
           02  MESSAGEF    PICTURE X.
           02  FILLER REDEFINES MESSAGEF.
             03 MESSAGEA    PICTURE X.
           02  FILLER   PICTURE X(6).
           02  MESSAGEI  PIC X(79).
           02  DUMMYL    COMP  PIC  S9(4).
           02  DUMMYF    PICTURE X.
           02  FILLER REDEFINES DUMMYF.
             03 DUMMYA    PICTURE X.
           02  FILLER   PICTURE X(6).
           02  DUMMYI  PIC X(1).
       01  BNK1DDO REDEFINES BNK1DDI.
           02  FILLER PIC X(12).
           02  FILLER PICTURE X(3).
           02  COMPANYC    PICTURE X.
           02  COMPANYP    PICTURE X.
           02  COMPANYH    PICTURE X.
           02  COMPANYV    PICTURE X.
           02  COMPANYU    PICTURE X.
           02  COMPANYM    PICTURE X.
           02  COMPANYO  PIC X(56).
           02  FILLER PICTURE X(3).
           02  MESSAGEC    PICTURE X.
           02  MESSAGEP    PICTURE X.
           02  MESSAGEH    PICTURE X.
           02  MESSAGEV    PICTURE X.
           02  MESSAGEU    PICTURE X.
           02  MESSAGEM    PICTURE X.
           02  MESSAGEO  PIC X(79).
           02  FILLER PICTURE X(3).
           02  DUMMYC    PICTURE X.
           02  DUMMYP    PICTURE X.
           02  DUMMYH    PICTURE X.
           02  DUMMYV    PICTURE X.
           02  DUMMYU    PICTURE X.
           02  DUMMYM    PICTURE X.
           02  DUMMYO  PIC X(1).
