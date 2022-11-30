//* Licensed Materials - Property of IBM
//*
//* (c) Copyright IBM Corp. 2020.
//*
//* US Government Users Restricted Rights - Use, duplication or
//* disclosure restricted by GSA ADP Schedule Contract with IBM Corp.
//*
//ASMNCOPT JOB ,,CLASS=A,MSGCLASS=H,
//  NOTIFY=&SYSUID,
//  MSGLEVEL=(1,1)
// JCLLIB ORDER=CBSA.CICSBSA.BUILDJCL
// INCLUDE MEMBER=DEFAULT
//*
//*
//*------------------------------------------------------------------
//* Sample job to assemble a Named Counter Options Table
//*
//*------------------------------------------------------------------
//*
//*
//*
//*
//ASM      EXEC PGM=ASMA90,PARM=('RENT,DECK,NOOBJECT,LIST')
//SYSLIB   DD DISP=SHR,DSN=SYS1.MACLIB
//         DD DISP=SHR,DSN=SYS1.MODGEN
//         DD DISP=SHR,DSN=&CICSHLQ..SDFHSAMP
//         DD DISP=SHR,DSN=&CICSHLQ..SDFHMAC
//SYSUT1   DD UNIT=SYSDA,SPACE=(CYL,(10,10))
//SYSUT2   DD UNIT=SYSDA,SPACE=(CYL,(10,10))
//SYSUT3   DD UNIT=SYSDA,SPACE=(CYL,(10,10))
//SYSPUNCH DD DISP=(,PASS),DSN=&&SYSMOD,
//            UNIT=SYSDA,DCB=BLKSIZE=3120,SPACE=(CYL,(1,1))
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DISP=SHR,DSN=&CBSAHLQ..ASM(DFHNCOPT)
//LKED     EXEC PGM=IEWL,COND=(7,LT,ASM),PARM='RENT,LIST,XREF'
//SYSLMOD  DD DISP=SHR,DSN=&CBSAHLQ..AUTHLIB
//SYSUT1   DD UNIT=SYSDA,DCB=BLKSIZE=1024,SPACE=(CYL,(1,1))
//SYSPRINT DD SYSOUT=*
//SYSLIN DD DISP=(OLD,DELETE),DSN=&&SYSMOD
//       DD DDNAME=SYSIN
//SYSIN DD *
  NAME DFHNCOPT(R)
/*