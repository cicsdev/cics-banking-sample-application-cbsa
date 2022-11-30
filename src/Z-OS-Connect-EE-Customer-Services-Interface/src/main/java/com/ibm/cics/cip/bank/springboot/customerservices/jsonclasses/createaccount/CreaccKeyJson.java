/*
 *
 *    Copyright contributors to the CICS Banking Sample Application (CBSA) project
 *
 */
package com.ibm.cics.cip.bank.springboot.customerservices.jsonclasses.createaccount;

import com.fasterxml.jackson.databind.annotation.JsonNaming;
import com.ibm.cics.cip.bank.springboot.customerservices.JsonPropertyNamingStrategy;

@JsonNaming(JsonPropertyNamingStrategy.class)
public class CreaccKeyJson {

    static final String COPYRIGHT =
      "Copyright contributors to the CICS Banking Sample Application (CBSA) project.";

    private int COMM_SORTCODE = 0;
    private int COMM_NUMBER = 0;

    public CreaccKeyJson() {
        
    }

    public int getCOMM_SORTCODE() {
        return COMM_SORTCODE;
    }

    public void setCOMM_SORTCODE(int cOMM_SORTCODE) {
        COMM_SORTCODE = cOMM_SORTCODE;
    }

    public int getCOMM_NUMBER() {
        return COMM_NUMBER;
    }

    public void setCOMM_NUMBER(int cOMM_NUMBER) {
        COMM_NUMBER = cOMM_NUMBER;
    }

    @Override
    public String toString() {
        return "CreaccKeyJson [COMM_NUMBER=" + COMM_NUMBER + ", COMM_SORTCODE=" + COMM_SORTCODE + "]";
    }
    
    

}
