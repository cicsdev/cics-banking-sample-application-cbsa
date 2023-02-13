*
* Copyright IBM Corp. 2023       
*
*
TRTABSZ=12288,                                                          00010000
APPLID=(CICSTS56,CICSTS56)                APPLID CICSTS56 IS DEFAULT    00020000
AICONS=AUTO                               MVS CONSOLE SUPPORT           00030000
AUXTR=OFF                                 TURN OFF AUX TRACE            00040000
AUXTRSW=NEXT                              ROTATE TO NEXT WHEN FULL      00050000
CICSSVC=216                               SVC NUMBER FOR CICS           00060000
CPSMCONN=NO                                                             00061003
DB2CONN=YES                                                             00062011
EDSALIM=100M                              FOR PR JVM                    00070000
DSALIM=7M                                                               00080000
FEPI=YES                                  START FEPI INTERFACE     
START=INITIAL     
INITPARM=(EQA0CPLT='NLE,STK,NWP')                                       00091007
JVMPROFILEDIR=/var/cics/JVMProfiles/                                    00092008
USSHOME=/usr/lpp/cicsts/cicsts56                                        00100002
USSCONFIG=/usr/lpp/cicsts/cicsts56/dfhconfig                            00110002
GMTEXT='WELCOME TO CICS TS 5.6'                                         00111000
GRPLIST=(XYZLIST,CICSTS56)                USE CICS LOGGER OFF           00112010
GTFTR=ON                                  GTF TRACE ON                  00120000
IRCSTRT=NO                                DO NOT START IRC AUTOMATICALLY00130000
ISC=NO                                    DO NOT INCLUDE ISC/MRO        00140000
KEYRING=CICSRNG                                                         00140106
PLTPI=YES                                                               00141004
PLTSD=SD                                                                00142004
RLS=NO                                    NO RLS SUPPORT YET            00150000
SPOOL=YES                                                               00150109
GMTRAN=CESN                                                             00151003
SEC=YES                                                                 00152003
XTRAN=NO                                                                00153003
XCMD=NO                                                                 00154003
XDCT=NO                                                                 00155003
XFCT=NO                                                                 00156003
XHFS=NO                                                                 00157003
XJCT=NO                                                                 00158003
XPPT=NO                                                                 00159003
XPCT=NO                                                                 00159103
XPSB=NO                                                                 00159203
XPTKT=NO                                                                00159303
XRES=NO                                                                 00159403
SIT=6$                                    USE SUPPLIED SIT 6$           00170000
STATRCD=OFF                               RECORDS STATISTICS TO SMF     00180000
SYSIDNT=S730                              SYSIDNT IS S730               00190000
SPCTRSO=(1,2)                             TRACE ON SOCKET DOMAIN        00200000
SPCTRWB=(1,2)                             TRACE WEB INTERFACE           00210000
TCPIP=YES                                 FOR TCPIP & IIOP SERVICES     00220003
FCT=NO,                                                                 00230000
TCT=NO,                                                                 00240000
SRT=1$,                                   ADD RECOVERY                  00250000
PGRET=P/,                                                               00260000
PGPURGE=T/,                                                             00270000
PGCOPY=C/,                                                              00280000
PGCHAIN=X/,                                                             00290000
.END                                                                    00310000