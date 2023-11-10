       CBL CICS('SP,EDF')
      ******************************************************************
      *                                                                *
      *  Copyright IBM Corp. 2023                                      *
      *                                                                *
      ******************************************************************


       IDENTIFICATION DIVISION.
       PROGRAM-ID. APICTRL.
       AUTHOR. BKELLER.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      * SOURCE-COMPUTER.  IBM-370 WITH DEBUGGING MODE.
       SOURCE-COMPUTER.  IBM-370.
       OBJECT-COMPUTER.  IBM-370.

       INPUT-OUTPUT SECTION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      * Common defintions                                              *
      *----------------------------------------------------------------*

      * Working variables

      * Commarea structure for UPDCUST
       01 UPDCUST-COMMAREA.
           COPY UPDCUST.

       01 operation                      PIC X(1) VALUE 'U'.
      *----------------------------------------------------------------*

      ******************************************************************
      *    L I N K A G E   S E C T I O N
      ******************************************************************
       LINKAGE SECTION.
       01 DFHCOMMAREA.
           COPY APIADDR.

      ******************************************************************
      *    P R O C E D U R E S
      ******************************************************************
       PROCEDURE DIVISION USING DFHCOMMAREA.
       PREMIERE SECTION.
       P010.



           EVALUATE operation 
               WHEN 'U'
      *        Call routine to perform for update customer
                   PERFORM CHANGE-ADDRESS 

               WHEN 'C'
      *        Call routine to perform for create customer
                   PERFORM CUST-CREATE 

               WHEN 'R'
      *        Call routine to perform customer lookup
                   PERFORM CUST-LOOKUP 

               WHEN 'D'
      *        Call routine to delete customer
                   PERFORM CUST-DELETE 

               WHEN OTHER
      *        Request is not recognised or supported
                   PERFORM GET-ME-OUT-OF-HERE 

           END-EVALUATE 

      * Return to caller
           PERFORM GET-ME-OUT-OF-HERE.

       P999.
           EXIT.
      *----------------------------------------------------------------*

      *================================================================*
      * Procedure to link to updcust program to change customer        *
      *   address                                                      *
      *================================================================*
       CHANGE-ADDRESS SECTION.
       AD010.

           INITIALIZE UPDCUST-COMMAREA
           MOVE API-ADDR to COMM-ADDR
           move API-NAME to COMM-NAME
           move API-CUSTNO to COMM-CUSTNO

      *    Link to UPDCUST
      *
           EXEC CICS LINK
              PROGRAM('UPDCUST')
              COMMAREA(UPDCUST-COMMAREA )
              SYNCONRETURN
           END-EXEC.

           move COMM-ADDR to API-ADDR
           move COMM-CUSTNO  to API-CUSTNO
           move COMM-NAME to API-NAME.

       AD999.
           EXIT.

      *================================================================*
      * Procedure to link to INQCUST program to inquire on customer    *
      *   details                                                      *
      *================================================================*
       CUST-LOOKUP SECTION.
       AD010.

           INITIALIZE UPDCUST-COMMAREA
           MOVE API-ADDR to COMM-ADDR
           move API-NAME to COMM-NAME
           move API-CUSTNO to COMM-CUSTNO

      *    Link to UPDCUST
      *
           EXEC CICS LINK
              PROGRAM('INQCUST')
              COMMAREA(UPDCUST-COMMAREA )
              SYNCONRETURN
           END-EXEC.

           move COMM-ADDR to API-ADDR
           move COMM-CUSTNO  to API-CUSTNO
           move COMM-NAME to API-NAME.

       AD999.
           EXIT.

      *================================================================*
      * Procedure to link to DELCUS program to delete customer record  *
      *                                                                *
      *================================================================*
       CUST-DELETE SECTION.
       AD010.

           INITIALIZE UPDCUST-COMMAREA
           MOVE API-ADDR to COMM-ADDR
           move API-NAME to COMM-NAME
           move API-CUSTNO to COMM-CUSTNO

      *    Link to UPDCUST
      *
           EXEC CICS LINK
              PROGRAM('DELCUST')
              COMMAREA(UPDCUST-COMMAREA )
              SYNCONRETURN
           END-EXEC.

           move COMM-ADDR to API-ADDR
           move COMM-CUSTNO  to API-CUSTNO
           move COMM-NAME to API-NAME.

       AD999.
           EXIT.

      *================================================================*
      * Procedure to link to CUSTCRE program to create customer record *
      *                                                                *
      *================================================================*
       CUST-CREATE SECTION.
       AD010.

           INITIALIZE UPDCUST-COMMAREA
           MOVE API-ADDR to COMM-ADDR
           move API-NAME to COMM-NAME
           move API-CUSTNO to COMM-CUSTNO

      *    Link to UPDCUST
      *
           EXEC CICS LINK
              PROGRAM('CRECUST')
              COMMAREA(UPDCUST-COMMAREA )
              SYNCONRETURN
           END-EXEC.

           move COMM-ADDR to API-ADDR
           move COMM-CUSTNO  to API-CUSTNO
           move COMM-NAME to API-NAME.

       AD999.
           EXIT.
      *
      * Finish
      *
       GET-ME-OUT-OF-HERE SECTION.
       GMOFH010.

           EXEC CICS RETURN
           END-EXEC.

       GMOFH999.
           EXIT.
