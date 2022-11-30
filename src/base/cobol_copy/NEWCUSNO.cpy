      ******************************************************************
      *                                                                *
      *  Copyright contributors to the CICS Banking Sample Application *
      * (CBSA) project                                                 *
      *                                                                *
      ******************************************************************
          03 NEWCUSNO-FUNCTION  PIC X.
          88 NEWCUSNO-FUNCTION-GETNEW VALUE 'G'.
          88 NEWCUSNO-FUNCTION-ROLLBACK VALUE 'R'.
          88 NEWCUSNO-FUNCTION-CURRENT VALUE 'C'.
          03 CUSTOMER-NUMBER                  PIC 9(10) DISPLAY.
          03 NEWCUSNO-SUCCESS                    PIC X.
          03 NEWCUSNO-FAIL-CODE                  PIC X.
