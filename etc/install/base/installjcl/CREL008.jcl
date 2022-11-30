//* Licensed Materials - Property of IBM
//*
//* (c) Copyright IBM Corp. 2020.
//*
//* US Government Users Restricted Rights - Use, duplication or
//* disclosure restricted by GSA ADP Schedule Contract with IBM Corp.
//*CREL008 JOB ,,CLASS=A,MSGCLASS=H,                                    00000109
//*  NOTIFY=&SYSUID,                                                    00000209
//*  MSGLEVEL=(1,1)            
//*                                         00000309
//**********************************************************            00001000
//**           CREATE THE PDSE                            **            00002000
//**********************************************************            00003000
//STEP10  EXEC PGM=IEFBR14                                              00004000
//DD01      DD DSN=CBSA.CICSBSA.CBSAMOD,                                00005006
//            DISP=(NEW,CATLG,DELETE),                                  00006000
//            UNIT=DISK,SPACE=(CYL,(50,20)),                            00007007
//            DCB=(LRECL=80,BLKSIZE=23440,DSORG=PO,                     00008007
//            RECFM=FB),DSNTYPE=LIBRARY                                 00008107