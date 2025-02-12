//*
//* Copyright IBM Corp. 2025
//*
//* Please change @DB2_HLQ@
//*  (to the high level qualifiers of your
//* system's DB2 datasets (SDSNLOAD et cetera)
//*
//* Please change @BANK_DBRMLIB@
//* (to the name you chose
//* for the DBRMLIB provided by Hursley Bank)
//*
//* Please change @DB2_SUBSYSTEM@
//* (to the name of your chosen DB2
//* subsystem (which must be running on the same MVS image as this
//* batch job) for example DBCG
//*
//* Please change @BANK_PACKAGE@
//* to the name you have chosen for the
//* package name in DB2, for example PCBSA.
//*
//* Please change @DB2_OWNER@
//* to the userid that will own the DB2
//* resources (IBMUSER)
//*
//* Please change @BANK_PLAN@ to
//*  to the desired DB2 plan name
//* This must match the CSD and the Db2 Install job
//*
//* Please change @DB2_DSNTEP_PLAN@
//* to the name of the plan that was
//* used for the DSNTEP2 utility program
//*
//* Please change @DB2_DSNTEP_LOADLIB@ to
//* to the name of the load library
//* that contains the DSNTEP2 utility program
//*
//* Please change @BANK_USER@
//* to the userid that will be running the application (CICSUSER)
// SET DB2HLQ=@DB2_HLQ@
// SET DB2SYS=@DB2_SUBSYSTEM@
// SET DB2OWNER=@DB2_OWNER@
// SET BANKDBRM=@BANK_DBRMLIB@
// SET BANKPLAN=@BANK_PLAN@
// SET BANKPKGE=@BANK_PACKAGE@
// SET DSNTEPP=@DB2_DSNTEP_PLAN@
// SET DSNTEPL=@DB2_DSNTEP_LOADLIB@
// SET BANKUSER=@BANK_USER@
