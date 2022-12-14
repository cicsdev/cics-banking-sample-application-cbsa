/*
 *
 *    Copyright IBM Corp. 2022
 *
 *
 */
package com.ibm.cics.cip.bank.springboot.customerservices.jsonclasses.listaccounts;

import com.fasterxml.jackson.databind.annotation.JsonNaming;
import com.ibm.cics.cip.bank.springboot.customerservices.JsonPropertyNamingStrategy;

@JsonNaming(JsonPropertyNamingStrategy.class)
public class ListAccJson {

    static final String COPYRIGHT =
      "Copyright IBM Corp. 2022";

    
    private InqAccczJson INQACCCZ;

    public ListAccJson() {

    }

    public InqAccczJson getINQACCCZ() {
        return INQACCCZ;
    }

    public void setINQACCCZ(InqAccczJson iNQACCCZ) {
        INQACCCZ = iNQACCCZ;
    }

    @Override
    public String toString() {
        return "ListAccJson [INQACCCZ=" + INQACCCZ + "]";
    }
}
