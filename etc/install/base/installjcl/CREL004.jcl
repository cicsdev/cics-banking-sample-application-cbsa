//*
//* Copyright IBM Corp. 2023
//*
//*CREL004 JOB ,,CLASS=A,MSGCLASS=H,                                    00000109
//*  NOTIFY=&SYSUID,                                                    00000209
//*  MSGLEVEL=(1,1)
//*                          00000309
//**********************************************************            00001000
//**           CREATE THE PDSE                            **            00002000
//**********************************************************            00003000
//STEP10  EXEC PGM=IEFBR14                                              00004000
//DD01      DD DSN=&HLQ..CICSBSA.DBRM,
//            DISP=(NEW,CATLG,DELETE),                                  00006000
//            SPACE=(CYL,(30,20)),
//            DCB=(LRECL=80,BLKSIZE=23440,DSORG=PO,                     00008007
//            RECFM=FB),DSNTYPE=LIBRARY                                 00008107