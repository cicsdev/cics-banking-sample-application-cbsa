//ZOSCSEC JOB ,,CLASS=A,MSGCLASS=H,
//  NOTIFY=&SYSUID,
//  MSGLEVEL=(1,1)
//*
//* Copyright IBM Corp. 2023       
//*
//*
//BPXIT EXEC PGM=BPXBATCH,PARM='SH chmod -R g+rwx "/var/zosconnect/v3r0+
//             /servers/defaultServer/resources/zosconnect"'
//STDOUT DD *
//STDENV DD *
//*
//