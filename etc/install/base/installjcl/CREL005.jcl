//* Licensed Materials - Property of IBM
//*
//* (c) Copyright IBM Corp. 2020.
//*
//* US Government Users Restricted Rights - Use, duplication or
//* disclosure restricted by GSA ADP Schedule Contract with IBM Corp.
//*CREL005 JOB ,,CLASS=A,MSGCLASS=H,                                    00000110
//*  NOTIFY=&SYSUID,                                                    00000210
//*  MSGLEVEL=(1,1)          
//*                                           00000310
//**********************************************************            00001000
//**           CREATE THE PDSE                            **            00002000
//**********************************************************            00003000
//STEP10  EXEC PGM=IEFBR14                                              00004000
//DD01      DD DSN=CBSA.CICSBSA.LKED,                                   00005008
//            DISP=(NEW,CATLG,DELETE),                                  00006000
//            UNIT=DISK,SPACE=(CYL,(1,2)),                              00007008
//            DCB=(LRECL=80,BLKSIZE=3120,DSORG=PO,                      00008008
//            RECFM=FB),DSNTYPE=LIBRARY                                 00008100