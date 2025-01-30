//*
//* Copyright IBM Corp. 2023       
//*
//BATCH  PROC MEMBER=
//COBOL  EXEC PGM=IGYCRCTL,REGION=0M,
//            PARM=(NODYNAM,LIB,RENT,LIST,MAP,XREF,OPT,
//   'APOST,TRUNC(OPT),NOCICS')
//STEPLIB  DD DSN=&COBOLHLQ..SIGYCOMP,DISP=SHR
//         DD DISP=SHR,DSN=&CICSHLQ..SDFHLOAD
//         DD DISP=SHR,DSN=&DB2HLQ..SDSNLOAD
//SYSLIB   DD DISP=SHR,DSN=&CBSAHLQ..DSECT
//         DD DISP=SHR,DSN=&CICSHLQ..SDFHCOB
//         DD DISP=SHR,DSN=&LEHLQ..SCEESAMP
//         DD DISP=SHR,DSN=&CICSHLQ..SDFHSAMP
//         DD DISP=SHR,DSN=&LEHLQ..SCEESAMP
//DBRMLIB   DD DISP=SHR,
//          DSN=&CBSAHLQ..DBRM(&MEMBER)
//SYSPRINT DD SYSOUT=*
//SYSLIN DD DDNAME=OUT
//OUT       DD DISP=SHR,
//          DSN=&CBSAHLQ..CBSAMOD(&MEMBER)
//SYSUT1   DD UNIT=SYSALLDA,SPACE=(TRK,(350,100))
//SYSUT2   DD UNIT=SYSALLDA,SPACE=(TRK,(350,100))
//SYSUT3   DD UNIT=SYSALLDA,SPACE=(TRK,(350,100))
//SYSUT4   DD UNIT=SYSALLDA,SPACE=(TRK,(350,100))
//SYSUT5   DD UNIT=SYSALLDA,SPACE=(TRK,(350,100))
//SYSUT6   DD UNIT=SYSALLDA,SPACE=(TRK,(350,100))
//SYSUT7   DD UNIT=SYSALLDA,SPACE=(TRK,(350,100))
//SYSUT8   DD UNIT=SYSALLDA,SPACE=(TRK,(350,100))
//SYSUT9   DD UNIT=SYSALLDA,SPACE=(TRK,(350,100))
//SYSUT10  DD UNIT=SYSALLDA,SPACE=(TRK,(350,100))
//SYSUT11  DD UNIT=SYSALLDA,SPACE=(TRK,(350,100))
//SYSUT12  DD UNIT=SYSALLDA,SPACE=(TRK,(350,100))
//SYSUT13  DD UNIT=SYSALLDA,SPACE=(TRK,(350,100))
//SYSUT14  DD UNIT=SYSALLDA,SPACE=(TRK,(350,100))
//SYSUT15  DD UNIT=SYSALLDA,SPACE=(TRK,(350,100))
//SYSMDECK DD UNIT=SYSALLDA,SPACE=(TRK,(350,100))
//SYSIN    DD DISP=SHR,DSN=&CBSAHLQ..COBOL(&MEMBER)
//*
//LKED   EXEC PGM=IEWL,REGION=4M,
//          PARM=(XREF,LIST,LET,RENT,'AMODE(31)','RMODE(ANY)'),
//          COND=(7,LT,COBOL)
//SYSLIB   DD DISP=SHR,DSN=&CICSHLQ..SDFHLOAD
//         DD DISP=SHR,DSN=&DB2HLQ..SDSNLOAD
//         DD DSN=&LEHLQ..SCEELKED,DISP=SHR
//SYSUT1   DD UNIT=SYSDA,DCB=BLKSIZE=1024,
//            SPACE=(1024,(200,20))
//SYSPRINT DD SYSOUT=*
//CBSAMOD   DD DISP=SHR,
//         DSN=&CBSAHLQ..CBSAMOD
//SYSLMOD  DD DISP=SHR,DSN=&CBSAHLQ..LOADLIB
//SYSDEFSD DD DUMMY ()
//SYSLIN   DD DDNAME=IN
//IN    DD DISP=SHR,DSN=&CBSAHLQ..LKED(&MEMBER)