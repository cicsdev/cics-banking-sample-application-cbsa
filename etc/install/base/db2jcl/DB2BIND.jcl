//DB2BIND JOB 'DB2',NOTIFY=&SYSUID,CLASS=A,MSGCLASS=H,       
//          MSGLEVEL=(1,1),REGION=4M                         
//*                                                          
//* Copyright IBM Corp. 2023                                 
//*                                                          
// EXPORT SYMLIST=*                                          
// JCLLIB ORDER=CBSA.DB2.JCL.INSTALL                         
// INCLUDE MEMBER=DEFAULT                                    
//*                                                          
//*                                                          
//*                                                          
//BIND    EXEC PGM=IKJEFT01                                  
//STEPLIB  DD  DISP=SHR,DSN=&DB2HLQ..SDSNEXIT                
//         DD  DISP=SHR,DSN=&DB2HLQ..SDSNLOAD                
//DBRMLIB  DD DISP=SHR,DSN=&BANKDBRM(CREACC)                 
//     DD DISP=SHR,DSN=&BANKDBRM(CRECUST)                    
//     DD DISP=SHR,DSN=&BANKDBRM(DBCRFUN)                    
//     DD DISP=SHR,DSN=&BANKDBRM(DELACC)     
//     DD DISP=SHR,DSN=&BANKDBRM(DELCUS)     
//     DD DISP=SHR,DSN=&BANKDBRM(INQACC)     
//     DD DISP=SHR,DSN=&BANKDBRM(INQACCCU)   
//     DD DISP=SHR,DSN=&BANKDBRM(BANKDATA)   
//     DD DISP=SHR,DSN=&BANKDBRM(UPDACC)     
//     DD DISP=SHR,DSN=&BANKDBRM(XFRFUN)     
//SYSPRINT DD  SYSOUT=*                      
//SYSTSPRT DD  SYSOUT=*                      
//SYSUDUMP DD  SYSOUT=*                      
//SYSTSIN DD *,SYMBOLS=(EXECSYS)             
 DSN S(&DB2SYS.)                             
                                             
 BIND PACKAGE(&BANKPKGE) OWNER(&DB2OWNER) -  
 QUALIFIER(&DB2OWNER) -                      
 MEMBER(CREACC) -                            
 ACTION(REPLACE)                             
                                             
 BIND PACKAGE(&BANKPKGE) OWNER(&DB2OWNER) -  
 QUALIFIER(&DB2OWNER) -                      
 MEMBER(CRECUST) -                           
 ACTION(REPLACE)                            
                                            
 BIND PACKAGE(&BANKPKGE) OWNER(&DB2OWNER) - 
 QUALIFIER(&DB2OWNER) -                     
 MEMBER(DBCRFUN) -                          
 ACTION(REPLACE)                            
                                            
 BIND PACKAGE(&BANKPKGE) OWNER(&DB2OWNER) - 
 QUALIFIER(&DB2OWNER) -                     
 MEMBER(DELACC) -                           
 ACTION(REPLACE)                            
                                            
 BIND PACKAGE(&BANKPKGE) OWNER(&DB2OWNER) - 
 QUALIFIER(&DB2OWNER) -                     
 MEMBER(DELCUS) -                           
 ACTION(REPLACE)                            
                                            
 BIND PACKAGE(&BANKPKGE) OWNER(&DB2OWNER) - 
 QUALIFIER(&DB2OWNER) -                     
 MEMBER(INQACC) -                           
 ACTION(REPLACE)                            
                                              
 BIND PACKAGE(&BANKPKGE) OWNER(&DB2OWNER) -   
 QUALIFIER(&DB2OWNER) -                       
 MEMBER(INQACCCU) -                           
 ACTION(REPLACE)                              
                                              
 BIND PACKAGE(&BANKPKGE) OWNER(&DB2OWNER) -   
 QUALIFIER(&DB2OWNER) -                       
 MEMBER(BANKDATA) -                           
 ACTION(REPLACE)                              
                                              
 BIND PACKAGE(&BANKPKGE) OWNER(&DB2OWNER) -   
 QUALIFIER(&DB2OWNER) -                       
 MEMBER(UPDACC) -                             
 ACTION(REPLACE)                              
                                              
 BIND PACKAGE(&BANKPKGE) OWNER(&DB2OWNER) -   
 QUALIFIER(&DB2OWNER) -                       
 MEMBER(XFRFUN) -                             
 ACTION(REPLACE)                              
                                              
  BIND PLAN(&BANKPLAN) -                                               
   OWNER(&DB2OWNER) -                                                  
   ISOLATION(UR) -                                                     
   PKLIST( -                                                           
   NULLID.*,&BANKPKGE..* )                                             
 END                                                                   
//******************************************************************** 
//***    GRANT EXECUTE AUTHORITY ON PLAN CBSA                          
//******************************************************************** 
//GRANT EXEC PGM=IKJEFT01,REGION=0M                                    
//STEPLIB  DD  DISP=SHR,DSN=&DB2HLQ..SDSNEXIT                          
//         DD  DISP=SHR,DSN=&DB2HLQ..SDSNLOAD                          
//SYSUDUMP DD SYSOUT=*                                                 
//SYSPRINT DD SYSOUT=*                                                 
//SYSTSPRT DD SYSOUT=*                                                 
//SYSTSIN DD *,SYMBOLS=(EXECSYS)                                       
DSN SYSTEM(&DB2SYS)                                                    
RUN PROGRAM(DSNTEP2) PLAN(&DSNTEPP) -                                  
LIB('&DSNTEPL')                                                        
//SYSIN DD *,SYMBOLS=EXECSYS                                           
 SET CURRENT SQLID = '&DB2OWNER';                                      
 GRANT EXECUTE ON PLAN &BANKPLAN TO &BANKUSER;                         
 GRANT SELECT, INSERT, UPDATE, DELETE ON TABLE   
     &DB2OWNER..ACCOUNT TO &BANKUSER;            
 GRANT SELECT, INSERT, UPDATE, DELETE ON TABLE   
     &DB2OWNER..PROCTRAN TO &BANKUSER;           
 GRANT SELECT, INSERT, UPDATE, DELETE ON TABLE   
     &DB2OWNER..CONTROL TO &BANKUSER;            
/*                                               