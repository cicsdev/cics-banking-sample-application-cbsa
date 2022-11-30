//HBANKCSD JOB ,,CLASS=A,MSGCLASS=H,
//  NOTIFY=&SYSUID,
//  MSGLEVEL=(1,1)
//*
//* Copyright contributors to the CICS Banking Sample Application 
//* (CBSA) project     
//*
//*
//* Please change @CICS_PREFIX@ to DFH560.CICS
//*  - to the prefix for CICS system datasets
//*
//* Change @CICS_CSD_HLQ@ to DFH560.CICS
//*  - to the prefix of the CICS DFHCSD dataset
//*
//* Please change @BANK_INSTALL@ to CBSA.JCL.INSTALL
//*  - to THIS PDS name
//*
//DFHCSDUP    EXEC PGM=DFHCSDUP,REGION=0M
//STEPLIB  DD DSN=DFH560.CICS.SDFHLOAD,DISP=SHR
//DFHCSD   DD DSN=DFH560.CICS.DFHCSD,DISP=SHR
//SYSPRINT DD   SYSOUT=*
//CBDOUT   DD   SYSOUT=*
//AMSDUMP  DD   SYSOUT=*
//SYSIN    DD DISP=SHR,DSN=CBSA.JCL.INSTALL(BANK)
