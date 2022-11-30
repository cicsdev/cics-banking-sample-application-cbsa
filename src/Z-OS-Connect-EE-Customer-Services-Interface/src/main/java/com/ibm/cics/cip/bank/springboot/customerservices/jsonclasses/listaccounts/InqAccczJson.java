/*
 *
 *    Copyright contributors to the CICS Banking Sample Application (CBSA) project
 *
 */
package com.ibm.cics.cip.bank.springboot.customerservices.jsonclasses.listaccounts;

import java.util.List;

import com.fasterxml.jackson.databind.annotation.JsonNaming;
import com.ibm.cics.cip.bank.springboot.customerservices.JsonPropertyNamingStrategy;

@JsonNaming(JsonPropertyNamingStrategy.class)
public class InqAccczJson {

    static final String COPYRIGHT =
      "Copyright contributors to the CICS Banking Sample Application (CBSA) project.";

    private int COMM_FAIL_CODE;
    private int CUSTOMER_NUMBER;
    private List<AccountDetails> ACCOUNT_DETAILS;
    private String COMM_PCB_POINTER;
    private String CUSTOMER_FOUND;
    private String COMM_SUCCESS;

    public InqAccczJson() {

    }

    public int getCOMM_FAIL_CODE() {
        return COMM_FAIL_CODE;
    }

    public void setCOMM_FAIL_CODE(int cOMM_FAIL_CODE) {
        COMM_FAIL_CODE = cOMM_FAIL_CODE;
    }

    public int getCUSTOMER_NUMBER() {
        return CUSTOMER_NUMBER;
    }

    public void setCUSTOMER_NUMBER(int cUSTOMER_NUMBER) {
        CUSTOMER_NUMBER = cUSTOMER_NUMBER;
    }

    public List<AccountDetails> getACCOUNT_DETAILS() {
        return ACCOUNT_DETAILS;
    }

    public void setACCOUNT_DETAILS(List<AccountDetails> aCCOUNT_DETAILS) {
        ACCOUNT_DETAILS = aCCOUNT_DETAILS;
    }

    public String getCOMM_PCB_POINTER() {
        return COMM_PCB_POINTER;
    }

    public void setCOMM_PCB_POINTER(String cOMM_PCB_POINTER) {
        COMM_PCB_POINTER = cOMM_PCB_POINTER;
    }

    public String getCUSTOMER_FOUND() {
        return CUSTOMER_FOUND;
    }

    public void setCUSTOMER_FOUND(String cUSTOMER_FOUND) {
        CUSTOMER_FOUND = cUSTOMER_FOUND;
    }

    public String getCOMM_SUCCESS() {
        return COMM_SUCCESS;
    }

    public void setCOMM_SUCCESS(String cOMM_SUCCESS) {
        COMM_SUCCESS = cOMM_SUCCESS;
    }

    @Override
    public String toString() {
        return "InqAccczJson [ACCOUNT_DETAILS=" + ACCOUNT_DETAILS + ", COMM_FAIL_CODE=" + COMM_FAIL_CODE
                + ", COMM_PCB_POINTER=" + COMM_PCB_POINTER + ", COMM_SUCCESS=" + COMM_SUCCESS + ", CUSTOMER_FOUND="
                + CUSTOMER_FOUND + ", CUSTOMER_NUMBER=" + CUSTOMER_NUMBER + "]";
    }
}
