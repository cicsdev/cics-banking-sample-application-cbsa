//*
//* Copyright IBM Corp. 2023
//*
//* Proc to assemble BMS maps
//MAPGEN PROC  MEMBER=,                      NAME OF MAPSET - REQUIRED
//             A=,                           A=A FOR ALIGNED MAP
//             AMODE=31,                     24/31
//             RMODE=ANY,                    24/ANY
//             ASMBLR=ASMA90,                ASSEMBLER PROGRAM NAME
//             REG=2048K,                    REGION FOR ASSEMBLY
//             OUTC=A,                       PRINT SYSOUT CLASS
//             WORK=SYSDA                    WORK FILE UNIT
//COPY     EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=&OUTC
//SYSUT2   DD DSN=&&TEMPM,UNIT=&WORK,DISP=(,PASS),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=400),
//            SPACE=(400,(50,50))
//SYSIN    DD DUMMY
//SYSUT1   DD DISP=SHR,DSN=&CBSAHLQ..BMS(&MEMBER)
//ASMMAP   EXEC PGM=&ASMBLR,REGION=&REG,
//* NOLOAD CHANGED TO NOOBJECT
//  PARM='SYSPARM(&A.MAP),DECK,NOOBJECT'
//SYSPRINT DD SYSOUT=&OUTC
//SYSLIB   DD DSN=&CICSHLQ..SDFHMAC,DISP=SHR
//         DD DSN=SYS1.MACLIB,DISP=SHR
//SYSUT1   DD UNIT=&WORK,SPACE=(CYL,(5,5))
//SYSUT2   DD UNIT=&WORK,SPACE=(CYL,(5,5))
//SYSUT3   DD UNIT=&WORK,SPACE=(CYL,(5,5))
//SYSPUNCH DD DSN=&&MAP,DISP=(,PASS),UNIT=&WORK,
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=400),
//            SPACE=(400,(50,50))
//SYSIN    DD DSN=&&TEMPM,DISP=(OLD,PASS)
//LINKMAP  EXEC PGM=IEWL,PARM=('LIST,LET,XREF,RMODE(&RMODE)',
//            'AMODE(&AMODE)')
//SYSPRINT DD SYSOUT=&OUTC
//SYSLMOD  DD DSN=&CBSAHLQ..LOADLIB(&MEMBER),DISP=SHR
//SYSUT1   DD UNIT=&WORK,SPACE=(1024,(20,20))
//SYSLIN   DD DSN=&&MAP,DISP=(OLD,DELETE)
//* NOLOAD CHANGED TO NOOBJECT
//ASMDSECT EXEC PGM=&ASMBLR,REGION=&REG,
//  PARM='SYSPARM(&A.DSECT),DECK,NOOBJECT'
//SYSPRINT DD SYSOUT=&OUTC
//SYSLIB   DD DSN=&CICSHLQ..SDFHMAC,DISP=SHR
//         DD DSN=SYS1.MACLIB,DISP=SHR
//SYSUT1   DD UNIT=&WORK,SPACE=(CYL,(5,5))
//SYSUT2   DD UNIT=&WORK,SPACE=(CYL,(5,5))
//SYSUT3   DD UNIT=&WORK,SPACE=(CYL,(5,5))
//SYSPUNCH DD DSN=&CBSAHLQ..DSECT(&MEMBER),DISP=OLD
//SYSIN    DD DSN=&&TEMPM,DISP=(OLD,DELETE)
