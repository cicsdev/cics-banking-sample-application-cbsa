//*
//* Copyright IBM Corp. 2023
//*
//*CREL005 JOB ,,CLASS=A,MSGCLASS=H,                                    00000110
//*  NOTIFY=&SYSUID,                                                    00000210
//*  MSGLEVEL=(1,1)
//*                                           00000310
//**********************************************************            00001000
//**           CREATE THE PDSE                            **            00002000
//**********************************************************            00003000
//STEP10  EXEC PGM=IEFBR14                                              00004000
//DD01      DD DSN=&HLQ..CICSBSA.LKED,
//            DISP=(NEW,CATLG,DELETE),                                  00006000
//            SPACE=(CYL,(1,2)),
//            DCB=(LRECL=80,BLKSIZE=3120,DSORG=PO,                      00008008
//            RECFM=FB),DSNTYPE=LIBRARY                                 00008100