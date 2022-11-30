/*
 *
 *    Copyright contributors to the CICS Banking Sample Application (CBSA) project
 *
 */
package com.ibm.cics.cip.bank.springboot.customerservices.jsonclasses.createcustomer;

import com.fasterxml.jackson.databind.annotation.JsonNaming;
import com.ibm.cics.cip.bank.springboot.customerservices.JsonPropertyNamingStrategy;
import com.ibm.cics.cip.bank.springboot.customerservices.jsonclasses.createaccount.CreaccKeyJson;

@JsonNaming(JsonPropertyNamingStrategy.class)
public class CrecustJson {

    static final String COPYRIGHT =
      "Copyright contributors to the CICS Banking Sample Application (CBSA) project.";
    
    private String COMM_EYECATCHER = "    ";
    private CreaccKeyJson COMM_KEY = new CreaccKeyJson();
    private String COMM_NAME;
    private String COMM_ADDRESS;
    private String COMM_DATE_OF_BIRTH;
    private int COMM_CREDIT_SCORE = 0;
    private String COMM_CS_REVIEW_DATE = "0";
    private String COMM_SUCCESS;
    private String COMM_FAIL_CODE;

    public CrecustJson(String custName, String custAddress, String custDob) {
        COMM_NAME = custName;
        COMM_ADDRESS = custAddress;
        // COMM_NAME = String.format("%-61s", custName);
        // COMM_ADDRESS = String.format("%-161s", custAddress);
        COMM_DATE_OF_BIRTH = custDob;
    }

    public CrecustJson() {

    }

    public String getCOMM_EYECATCHER() {
        return COMM_EYECATCHER;
    }
    public void setCOMM_EYECATCHER(String cOMM_EYECATCHER) {
        COMM_EYECATCHER = cOMM_EYECATCHER;
    }
    public CreaccKeyJson getCOMM_KEY() {
        return COMM_KEY;
    }
    public void setCOMM_KEY(CreaccKeyJson cOMM_KEY) {
        COMM_KEY = cOMM_KEY;
    }
    public String getCOMM_NAME() {
        return COMM_NAME;
    }
    public void setCOMM_NAME(String cOMM_NAME) {
        COMM_NAME = cOMM_NAME;
    }
    public String getCOMM_ADDRESS() {
        return COMM_ADDRESS;
    }
    public void setCOMM_ADDRESS(String cOMM_ADDRESS) {
        COMM_ADDRESS = cOMM_ADDRESS;
    }
    public String getCOMM_DATE_OF_BIRTH() {
        return COMM_DATE_OF_BIRTH;
    }
    public void setCOMM_DATE_OF_BIRTH(String cOMM_DATE_OF_BIRTH) {
        COMM_DATE_OF_BIRTH = cOMM_DATE_OF_BIRTH;
    }
    public int getCOMM_CREDIT_SCORE() {
        return COMM_CREDIT_SCORE;
    }
    public void setCOMM_CREDIT_SCORE(int cOMM_CREDIT_SCORE) {
        COMM_CREDIT_SCORE = cOMM_CREDIT_SCORE;
    }
    public String getCOMM_CS_REVIEW_DATE() {
        return COMM_CS_REVIEW_DATE;
    }
    public void setCOMM_CS_REVIEW_DATE(String cOMM_CS_REVIEW_DATE) {
        COMM_CS_REVIEW_DATE = cOMM_CS_REVIEW_DATE;
    }
    public String getCOMM_SUCCESS() {
        return COMM_SUCCESS;
    }
    public void setCOMM_SUCCESS(String cOMM_SUCCESS) {
        COMM_SUCCESS = cOMM_SUCCESS;
    }
    public String getCOMM_FAIL_CODE() {
        return COMM_FAIL_CODE;
    }
    public void setCOMM_FAIL_CODE(String cOMM_FAIL_CODE) {
        COMM_FAIL_CODE = cOMM_FAIL_CODE;
    }

    
}
