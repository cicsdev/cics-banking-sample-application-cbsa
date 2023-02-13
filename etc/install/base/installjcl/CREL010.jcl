//*
//* Copyright IBM Corp. 2023       
//*
//*CREL010 JOB ,,CLASS=A,MSGCLASS=H,                                    00000109
//*  NOTIFY=&SYSUID,                                                    00000209
//*  MSGLEVEL=(1,1)                      
//*                               00000309
//**********************************************************            00001000
//**           CREATE THE PDSE                            **            00002000
//**********************************************************            00003000
//STEP10  EXEC PGM=IEFBR14                                              00004000
//DD01      DD DSN=CBSA.CICSBSA.DSECT,                                  00005008
//            DISP=(NEW,CATLG,DELETE),                                  00006012
//            UNIT=DISK,SPACE=(CYL,(5,1)),                              00007012
//            DCB=(LRECL=80,BLKSIZE=3120,DSORG=PO,RECFM=FB),            00008012
//            DSNTYPE=LIBRARY                                           00009012
//**********************************************************            00009100
//**           CREATE AN EMPTY PDSE MEMBER CALLED EMPTY   **            00009210
//**********************************************************            00009300
//STEP20  EXEC PGM=ICEGENER                                             00009400
//SYSUT1    DD *                                                        00009500
//SYSUT2    DD DSN=CBSA.CICSBSA.DSECT(EMPTY),DISP=SHR                   00009608
//SYSPRINT  DD SYSOUT=*                                                 00009700
//SYSIN     DD DUMMY                                                    00009800
//**********************************************************            00009900