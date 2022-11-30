//ZOSCSEC JOB ,,CLASS=A,MSGCLASS=H,
//  NOTIFY=&SYSUID,
//  MSGLEVEL=(1,1)
//* Copyright contributors to the CICS Banking Sample Application 
//* (CBSA) project     
//*
//BPXIT EXEC PGM=BPXBATCH,PARM='SH chmod -R g+rwx "/var/zosconnect/v3r0+
//             /servers/defaultServer/resources/zosconnect"'
//STDOUT DD *
//STDENV DD *
//*
//