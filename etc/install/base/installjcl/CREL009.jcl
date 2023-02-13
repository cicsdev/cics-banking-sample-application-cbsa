//*
//* Copyright IBM Corp. 2023       
//*
//*CREL009 JOB ,,CLASS=A,MSGCLASS=H,                                    00000110
//*  NOTIFY=&SYSUID,                                                    00000210
//*  MSGLEVEL=(1,1)   
//*                                                  00000310
//**********************************************************            00001000
//**           CREATE THE PDSE                            **            00002000
//**********************************************************            00003000
//STEP10  EXEC PGM=IEFBR14                                              00004000
//DD01      DD DSN=CBSA.CICSBSA.COBOL,                                  00005008
//            DISP=(NEW,CATLG,DELETE),                                  00006000
//            UNIT=DISK,SPACE=(CYL,(7,5)),                              00007008
//            DCB=(LRECL=80,BLKSIZE=3120,DSORG=PO,                      00008008
//            RECFM=FB),DSNTYPE=LIBRARY                                 00008100
//**********************************************************            00009100
//**           CREATE AN EMPTY PDSE MEMBER CALLED EMPTY   **            00009200
//**********************************************************            00009300
//STEP20  EXEC PGM=ICEGENER                                             00009400
//SYSUT1    DD *                                                        00009500
//SYSUT2    DD DSN=CBSA.CICSBSA.COBOL(EMPTY),DISP=SHR                   00009608
//SYSPRINT  DD SYSOUT=*                                                 00009700
//SYSIN     DD DUMMY                                                    00009800
//**********************************************************            00009900