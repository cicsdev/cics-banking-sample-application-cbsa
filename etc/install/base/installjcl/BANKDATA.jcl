//*
//* Copyright IBM Corp. 2023       
//*
//BANKDATA JOB 'DB2',NOTIFY=&SYSUID,CLASS=A,MSGCLASS=H,
//          MSGLEVEL=(1,1),REGION=4M
//*
//*
//*
//* Change @BANK_PREFIX@ to be CBSA.CICSBSA
//*  - the high level qualifiers of your VSAM *
//*
//* Change @DB2_HLQ@ to be DSNC10
//*  - to the high level qualifiers of your DB2  *
//*
//* Change @BANK_DBRMLIB@ to CBSA.CICSBSA.DBRM
//* - to the name of the DBRM Lib              *
//*
//* Change @DB2_SUBSYSTEM@ to DBCG
//* - to the name of the DB2 Subsystem        *
//*
//* Change @DB2_PLAN@ to CBSA
//* - to the name of the DB2 Plan
//*
//* Change @BANK_LOADLIB@ to CBSA.CICSBSA.LOADLIB
//* - to the name of the load library
//*
//******************************************************************
//******************************************************************
//******************************************************************
//* BANKDAT1                                                       *
//*                                                                *
//*
//*
//*                                                                *
//******************************************************************
//BANKDAT0 EXEC PGM=IDCAMS
//SYSOUT DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN DD *
  DELETE CBSA.CICSBSA.ABNDFILE
  SET MAXCC=0
  DEFINE CLUSTER (NAME(CBSA.CICSBSA.ABNDFILE)-
                     CYL(6 6) -
                     KEYS(12 0) -
                     RECORDSIZE(681 681) -
                     SHAREOPTIONS(2 3) -
                     INDEXED -
                     LOG(NONE) -
                     REUSE -
                     FREESPACE(3 3))-
            DATA (NAME(CBSA.CICSBSA.ABNDFLE.DATA) -
                 ) -
            INDEX (NAME(CBSA.CICSBSA.ABNDFILE.INDEX) -
                 )
/*
//BANKDAT1 EXEC PGM=IDCAMS
//SYSOUT DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN DD *
  DELETE CBSA.CICSBSA.CUSTOMER
  SET MAXCC=0
   DEFINE CLUSTER (NAME(CBSA.CICSBSA.CUSTOMER)-
                      CYL(50 50) -
                      KEYS(16 4) -
                      RECORDSIZE(259,259) -
                      SHAREOPTIONS(2 3) -
                      INDEXED -
                       -
                      LOG(UNDO) -
                       -
                       -
                     ) -
             DATA (NAME(CBSA.CICSBSA.CUSTOMER.DATA) -
                  ) -
             INDEX (NAME(CBSA.CICSBSA.CUSTOMER.INDEX) -
                  )
/*
/*
//******************************************************************
//*
//******************************************************************
//* BANKDAT5                                                       *
//*                                                                *
//* Executes Cobol program BANKDATA, which generates random data   *
//*                                                                *
//* The parameters are:                                            *
//* 1) Starting Customer Number                                    *
//* 2) Final Customer Number                                       *
//* 3) Customer Number Increment (1 for every number)              *
//* 4) Random number seed                                          *
//* 6) Optionally. supply a named counter pool                     *
//******************************************************************
//******************************************************************
//BANKDAT5 EXEC PGM=IKJEFT01,REGION=0M
//STEPLIB  DD DISP=SHR,DSN=CBSA.CICSBSA.DBRM
//         DD DISP=SHR,DSN=CBSA.CICSBSA.LOADLIB
//         DD DISP=SHR,DSN=DSNC10.SDSNLOAD
//VSAM     DD DISP=SHR,DSN=CBSA.CICSBSA.CUSTOMER
//*INPUT FILES
//*OUTPUT FILES
//SYSPRINT DD SYSOUT=*
//SYSABOUT DD SYSOUT=*
//SYSDBOUT DD SYSOUT=*
//DISPLAY  DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//SYSTSPRT DD SYSOUT=*
//SYSTSIN  DD *
 DSN SYSTEM(DBCG)
 RUN PROGRAM(BANKDATA)  -
 PLAN(CBSA) -
 PARM('1,10000,1,1000000000000000') -
 LIB('CBSA.CICSBSA.LOADLIB')
 END
/*
