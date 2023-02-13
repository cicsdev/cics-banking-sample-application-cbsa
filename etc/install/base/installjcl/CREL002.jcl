//*
//* Copyright IBM Corp. 2023       
//*
//*CREL002 JOB ,,CLASS=A,MSGCLASS=H,                                    00000107
//*  NOTIFY=&SYSUID,                                                    00000207
//*  MSGLEVEL=(1,1)                 
//*                                    00000307
//**********************************************************            00001000
//**           CREATE THE PDSE                            **            00002000
//**********************************************************            00003000
//STEP10  EXEC PGM=IEFBR14                                              00004000
//DD01      DD DSN=CBSA.CICSBSA.COPYLIB,                                00005004
//            DISP=(NEW,CATLG,DELETE),                                  00006008
//            UNIT=DISK,SPACE=(CYL,(5,1)),                              00007008
//            DCB=(LRECL=80,BLKSIZE=3120,DSORG=PO,RECFM=FB),            00008008
//            DSNTYPE=LIBRARY                                           00009008
//**********************************************************            00009100
//**           CREATE AN EMPTY PDSE MEMBER CALLED MEMBER1 **            00009200
//**********************************************************            00009300
//STEP20  EXEC PGM=ICEGENER                                             00009400
//SYSUT1    DD *                                                        00009500
//SYSUT2    DD DSN=CBSA.CICSBSA.COPYLIB(EMPTY),DISP=SHR                 00009604
//SYSPRINT  DD SYSOUT=*                                                 00009700
//SYSIN     DD DUMMY                                                    00009800
//**********************************************************            00009900