/*
 *
 *    Copyright contributors to the CICS Banking Sample Application (CBSA) project
 *
 */
package com.ibm.cics.cip.bank.springboot.customerservices.jsonclasses.customerenquiry;

import com.fasterxml.jackson.databind.annotation.JsonNaming;
import com.ibm.cics.cip.bank.springboot.customerservices.JsonPropertyNamingStrategy;

@JsonNaming(JsonPropertyNamingStrategy.class)
public class InqCustReviewDate {

    static final String COPYRIGHT =
      "Copyright contributors to the CICS Banking Sample Application (CBSA) project.";
    
    private int INQCUST_CS_REVIEW_YYYY;
    private int INQCUST_CS_REVIEW_MM;
    private int INQCUST_CS_REVIEW_DD;

    public InqCustReviewDate() {

    }

    public int getINQCUST_CS_REVIEW_YYYY() {
        return INQCUST_CS_REVIEW_YYYY;
    }

    public void setINQCUST_CS_REVIEW_YYYY(int iNQCUST_CS_REVIEW_YYYY) {
        INQCUST_CS_REVIEW_YYYY = iNQCUST_CS_REVIEW_YYYY;
    }

    public int getINQCUST_CS_REVIEW_MM() {
        return INQCUST_CS_REVIEW_MM;
    }

    public void setINQCUST_CS_REVIEW_MM(int iNQCUST_CS_REVIEW_MM) {
        INQCUST_CS_REVIEW_MM = iNQCUST_CS_REVIEW_MM;
    }

    public int getINQCUST_CS_REVIEW_DD() {
        return INQCUST_CS_REVIEW_DD;
    }

    public void setINQCUST_CS_REVIEW_DD(int iNQCUST_CS_REVIEW_DD) {
        INQCUST_CS_REVIEW_DD = iNQCUST_CS_REVIEW_DD;
    }

    @Override
    public String toString() {
        return INQCUST_CS_REVIEW_DD + "/" + INQCUST_CS_REVIEW_MM + "/" + INQCUST_CS_REVIEW_YYYY;
    }

}
