//INSTDB2 JOB 'DB2',NOTIFY=&SYSUID,CLASS=A,MSGCLASS=H,
//          MSGLEVEL=(1,1),REGION=4M
//*
//* Copyright IBM Corp. 2023
//*
//*
// EXPORT SYMLIST=* 
// JCLLIB ORDER=CBSA.DB2.JCL.INSTALL
// INCLUDE MEMBER=DEFAULT
//* 
//* Please change DSNV12DP to the appropriate value for your installation
//* This is the integrated catalog facility catalog for the storage group
//*
//* This is your Db2 load library
//JOBLIB  DD  DISP=SHR,DSN=&DB2HLQ..SDSNLOAD
//GRANT   EXEC PGM=IKJEFT01,DYNAMNBR=20
//SYSTSPRT DD  SYSOUT=*
//SYSPRINT DD  SYSOUT=*
//SYSUDUMP DD  SYSOUT=*
//* What is the name of your Db2 subsystem?
//* Program DSNTEP2 is a Db2 utility
//* The plan and library are installation dependent
//SYSTSIN  DD  *,SYMBOLS=(EXECSYS)   
  DSN SYSTEM(&DB2SYS.)
  RUN PROGRAM(DSNTEP2)  PLAN(&DSNTEPP) -
       LIB('&DSNTEPL') PARMS('/ALIGN(MID)')
  END
//* The following works on a standalone system
//* The table names cannot be changed, but other
//* parameters can be. Please change them in the bind
//* job if you do. 
//* The plan must match that in the INSTALL(BANK) member
//SYSIN    DD  *,SYMBOLS=(EXECSYS)
SET CURRENT SQLID = '&BANKUSER';

CREATE DATABASE CBSA
       BUFFERPOOL BP1
       INDEXBP BP2;

CREATE STOGROUP ACCOUNT VOLUMES('*','*','*','*','*') VCAT DSNV12DP;


CREATE TABLESPACE ACCOUNT IN CBSA USING STOGROUP ACCOUNT;

CREATE TABLE ACCOUNT (
                    ACCOUNT_EYECATCHER             CHAR(4),
                    ACCOUNT_CUSTOMER_NUMBER        CHAR(10),
                    ACCOUNT_SORTCODE               CHAR(6) NOT NULL,
                    ACCOUNT_NUMBER                 CHAR(8) NOT NULL,
                    ACCOUNT_TYPE                   CHAR(8),
                    ACCOUNT_INTEREST_RATE          DECIMAL(6, 2),
                    ACCOUNT_OPENED                 DATE,
                    ACCOUNT_OVERDRAFT_LIMIT        INTEGER,
                    ACCOUNT_LAST_STATEMENT         DATE,
                    ACCOUNT_NEXT_STATEMENT         DATE,
                    ACCOUNT_AVAILABLE_BALANCE      DECIMAL(12, 2),
                    ACCOUNT_ACTUAL_BALANCE         DECIMAL(12, 2)
                   )
IN CBSA.ACCOUNT   NOT VOLATILE
CARDINALITY  AUDIT NONE  DATA CAPTURE NONE;

CREATE UNIQUE INDEX ACCTINDX
  ON ACCOUNT(ACCOUNT_SORTCODE,ACCOUNT_NUMBER)
  USING STOGROUP ACCOUNT;

CREATE INDEX ACCTCUST
   ON ACCOUNT(ACCOUNT_SORTCODE,ACCOUNT_CUSTOMER_NUMBER)
   USING STOGROUP ACCOUNT;

CREATE STOGROUP PROCTRAN VOLUMES('*','*','*','*','*') VCAT DSNV12DP;

CREATE TABLESPACE PROCTRAN IN CBSA USING STOGROUP PROCTRAN;

CREATE TABLE PROCTRAN
                  (
                    PROCTRAN_EYECATCHER            CHAR(4),
                    PROCTRAN_SORTCODE              CHAR(6) NOT NULL,
                    PROCTRAN_NUMBER                CHAR(8) NOT NULL,
                    PROCTRAN_DATE                  DATE,
                    PROCTRAN_TIME                  CHAR(6),
                    PROCTRAN_REF                   CHAR(12),
                    PROCTRAN_TYPE                  CHAR(3),
                    PROCTRAN_DESC                  CHAR(40),
                    PROCTRAN_AMOUNT                DECIMAL(12, 2)
                   )
IN CBSA.PROCTRAN  NOT VOLATILE
CARDINALITY  AUDIT NONE  DATA CAPTURE NONE;

CREATE STOGROUP CONTROL VOLUMES('*','*','*','*','*') VCAT DSNV12DP;

CREATE TABLESPACE CONTROL IN CBSA USING STOGROUP CONTROL;

CREATE TABLE CONTROL (
                    CONTROL_NAME                   CHAR(32),
                    CONTROL_VALUE_NUM              INTEGER,
                    CONTROL_VALUE_STR              CHAR(40)
                   )
IN CBSA.CONTROL  NOT VOLATILE
CARDINALITY  AUDIT NONE  DATA CAPTURE NONE;

CREATE UNIQUE INDEX CONTINDX
 ON CONTROL(CONTROL_NAME)
 USING STOGROUP CONTROL;

/*