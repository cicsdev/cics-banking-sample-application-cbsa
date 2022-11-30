      ******************************************************************
      *                                                                *
      *  Copyright contributors to the CICS Banking Sample Application *
      * (CBSA) project                                                 *
      *                                                                *
      ******************************************************************
           EXEC SQL DECLARE STTESTER.CONTROL TABLE
                (CONTROL_NAME       CHAR(32) NOT NULL,
                 CONTROL_VALUE_NUM  INTEGER,
                 CONTROL_VALUE_STR  CHAR(40))
           END-EXEC.
