       CBL CICS('SP,EDF')
      ******************************************************************
      *                                                                *
      *  Copyright contributors to the CICS Banking Sample Application *
      * (CBSA) project                                                 *
      *                                                                *
      ******************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. GETCOMPY.
       AUTHOR. James O'Grady.


       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      *SOURCE-COMPUTER.   IBM-370 WITH DEBUGGING MODE.
       SOURCE-COMPUTER.  IBM-370.
       OBJECT-COMPUTER.  IBM-370.

       INPUT-OUTPUT SECTION.


       DATA DIVISION.
       FILE SECTION.


       WORKING-STORAGE SECTION.
      * Copyright statement as a literal to go into the load module
       77 FILLER PIC X(34) VALUE 'Copyright contributors to the CICS'. 
       77 FILLER PIC X(34) VALUE 'Banking Sample Application (CBSA)'. 
       77 FILLER PIC X(8)  VALUE ' project'. 


       LINKAGE SECTION.
       01 DFHCOMMAREA.
           COPY GETCOMPY.



       PROCEDURE DIVISION USING DFHCOMMAREA.
       PREMIERE SECTION.
       A010.
           move 'CICS Bank Sample Application' to COMPANY-NAME.

           EXEC CICS RETURN
           END-EXEC.

           GOBACK.
