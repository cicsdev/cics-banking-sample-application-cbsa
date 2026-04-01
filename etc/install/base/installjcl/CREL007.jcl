//*
//* Copyright IBM Corp. 2026
//*
//*CREL007 JOB ,,CLASS=A,MSGCLASS=H,                                    00000104
//*  NOTIFY=&SYSUID,                                                    00000204
//*  MSGLEVEL=(1,1)
//*
//**********************************************************            00001000
//**           CREATE THE PDSE                            **            00002000
//**********************************************************            00003000
//STEP10  EXEC PGM=IEFBR14                                              00004000
//DD01      DD DSN=&HLQ..CICSBSA.DEBUG,
//             DISP=(NEW,CATLG,DELETE),                                 00006000
//             SPACE=(CYL,(5,1)),
//             DCB=(LRECL=1024,BLKSIZE=1024,DSORG=PO,RECFM=FB),         00008000
//             DSNTYPE=LIBRARY                                          00009000
