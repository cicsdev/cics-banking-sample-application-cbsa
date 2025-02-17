//*
//* Copyright IBM Corp. 2023
//*
//*
//*CREDB2L JOB ,,CLASS=A,MSGCLASS=H,                                    00000103
//*  NOTIFY=&SYSUID,                                                    00000203
//*  MSGLEVEL=(1,1)                                                     00000204
//*                                                                     00000303
//**********************************************************            00001001
//**           CREATE THE PDSE                            **            00002001
//**********************************************************            00003001
//STEP10  EXEC PGM=IEFBR14                                              00004001
//DD01      DD DSN=&HLQ..DB2.JCL.INSTALL,
//             DISP=(NEW,CATLG,DELETE),                                 00006001
//             SPACE=(CYL,(2,1)),
//             DCB=(LRECL=80,BLKSIZE=0,DSORG=PO,RECFM=FB),              00008001
//             DSNTYPE=LIBRARY                                          00009001
//**********************************************************            00009101
//**           CREATE AN EMPTY PDSE MEMBER CALLED EMPTY   **            00009202
//**********************************************************            00009301
//STEP20  EXEC PGM=ICEGENER                                             00009401
//SYSUT1    DD *                                                        00009501
//SYSUT2    DD DSN=&HLQ..DB2.JCL.INSTALL(EMPTY),DISP=SHR
//SYSPRINT  DD SYSOUT=*                                                 00009701
//SYSIN     DD DUMMY                                                    00009801
//**********************************************************            00009901
