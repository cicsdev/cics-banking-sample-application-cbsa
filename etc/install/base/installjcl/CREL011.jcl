//* Licensed Materials - Property of IBM
//*
//* (c) Copyright IBM Corp. 2020.
//*
//* US Government Users Restricted Rights - Use, duplication or
//* disclosure restricted by GSA ADP Schedule Contract with IBM Corp.
//*CREL011 JOB ,,CLASS=A,MSGCLASS=H,                                    00000113
//*  NOTIFY=&SYSUID,                                                    00000200
//*  MSGLEVEL=(1,1)                  
//*                                   00000300
//**********************************************************            00001000
//**           CREATE THE PDSE                            **            00002000
//**********************************************************            00003000
//STEP10  EXEC PGM=IEFBR14                                              00004000
//DD01      DD DSN=CBSA.CICSBSA.REORG,                                  00005013
//            DISP=(NEW,CATLG,DELETE),                                  00006000
//            UNIT=DISK,SPACE=(CYL,(5,1)),                              00007000
//            DCB=(LRECL=80,BLKSIZE=3120,DSORG=PO,RECFM=FB),            00008000
//            DSNTYPE=LIBRARY                                           00009000
//**********************************************************            00009100
//**           CREATE AN EMPTY PDSE MEMBER CALLED EMPTY   **            00009200
//**********************************************************            00009300
//STEP20  EXEC PGM=ICEGENER                                             00009400
//SYSUT1    DD *                                                        00009500
//SYSUT2    DD DSN=CBSA.CICSBSA.REORG(EMPTY),DISP=SHR                   00009613
//SYSPRINT  DD SYSOUT=*                                                 00009700
//SYSIN     DD DUMMY                                                    00009800
//**********************************************************            00009900