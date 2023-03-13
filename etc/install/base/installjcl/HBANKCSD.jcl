//HBANKCSD JOB ,,CLASS=A,MSGCLASS=H,
//  NOTIFY=&SYSUID,
//  MSGLEVEL=(1,1)
//*
//* Copyright IBM Corp. 2023
//*
//*
//* Please change @CICS_PREFIX@ to DFH560.CICS
//*  - to the prefix for CICS system datasets
//*
//* Change @CICS_PREFIX@ to DFH560.CICS
//*  - to the prefix of the CICS DFHCSD dataset
//*
//* Please change @CICS_PREFIX@ to CBSA.JCL.INSTALL
//*  - to THIS PDS name
//*
//DFHCSDUP    EXEC PGM=DFHCSDUP,REGION=0M
//STEPLIB  DD DSN=@CICS_PREFIX@.SDFHLOAD,DISP=SHR
//DFHCSD   DD DSN=@CICS_PREFIX@.DFHCSD,DISP=SHR
//SYSPRINT DD   SYSOUT=*
//CBDOUT   DD   SYSOUT=*
//AMSDUMP  DD   SYSOUT=*
//SYSIN    DD DISP=SHR,DSN=@CICS_PREFIX@(BANK)
