/*
 *
 *    Copyright IBM Corp. 2022
 *
 *
 */
package com.ibm.cics.cip.bank.springboot.customerservices.jsonclasses.updatecustomer;

import com.fasterxml.jackson.databind.annotation.JsonNaming;
import com.ibm.cics.cip.bank.springboot.customerservices.JsonPropertyNamingStrategy;

@JsonNaming(JsonPropertyNamingStrategy.class)
public class UpdcustJson {

    static final String COPYRIGHT =
      "Copyright IBM Corp. 2022";

    
    private String COMM_EYE = "    ";
    private String COMM_SCODE = "";
    private String COMM_CUSTNO = " ";
    private String COMM_NAME = " ";
    private String COMM_ADDR = " ";
    private int COMM_DOB = 0;
    private int COMM_CREDIT_SCORE = 0;
    private int COMM_CS_REVIEW_DATE = 0;
    private String COMM_UPD_SUCCESS = " ";
    private String COMM_UPD_FAIL_CD = " ";

    public UpdcustJson(String cOMM_CUSTNO, String cOMM_NAME, String cOMM_ADDR, String cOMM_DOB, int cOMM_CREDIT_SCORE,
            String cOMM_CS_REVIEW_DATE) {
        // Some values need to be padded out when not full
        COMM_CUSTNO = String.format("%10s", cOMM_CUSTNO).replace(" ", "0");
        if (!cOMM_NAME.equals(" ")) COMM_NAME = String.format("%-60s", cOMM_NAME);
        if (!cOMM_ADDR.equals(" ")) COMM_ADDR = String.format("%-160s", cOMM_ADDR);

        // These convert strings to ints - they use ternary operators to prevent a NumberFormatException on an empty String, as 0 would be okay by default.
        COMM_DOB = cOMM_DOB.equals("") ? 0 : Integer.parseInt(cOMM_DOB);
        COMM_CS_REVIEW_DATE = cOMM_CS_REVIEW_DATE.equals("") ? 0 : Integer.parseInt(cOMM_CS_REVIEW_DATE);

        // Doesn't need conversion as it isn't ever not an int
        COMM_CREDIT_SCORE = cOMM_CREDIT_SCORE;
    }

    public UpdcustJson() {

    }

    public String getCOMM_EYE() {
        return COMM_EYE;
    }
    public void setCOMM_EYE(String cOMM_EYE) {
        COMM_EYE = cOMM_EYE;
    }
    public String getCOMM_SCODE() {
        return COMM_SCODE;
    }
    public void setCOMM_SCODE(String cOMM_SCODE) {
        COMM_SCODE = cOMM_SCODE;
    }
    public String getCOMM_CUSTNO() {
        return COMM_CUSTNO;
    }
    public void setCOMM_CUSTNO(String cOMM_CUSTNO) {
        COMM_CUSTNO = cOMM_CUSTNO;
    }
    public String getCOMM_NAME() {
        return COMM_NAME;
    }
    public void setCOMM_NAME(String cOMM_NAME) {
        COMM_NAME = cOMM_NAME;
    }
    public String getCOMM_ADDR() {
        return COMM_ADDR;
    }
    public void setCOMM_ADDR(String cOMM_ADDR) {
        COMM_ADDR = cOMM_ADDR;
    }
    public int getCOMM_DOB() {
        return COMM_DOB;
    }
    public void setCOMM_DOB(int cOMM_DOB) {
        COMM_DOB = cOMM_DOB;
    }
    public int getCOMM_CREDIT_SCORE() {
        return COMM_CREDIT_SCORE;
    }
    public void setCOMM_CREDIT_SCORE(int cOMM_CREDIT_SCORE) {
        COMM_CREDIT_SCORE = cOMM_CREDIT_SCORE;
    }
    public int getCOMM_CS_REVIEW_DATE() {
        return COMM_CS_REVIEW_DATE;
    }
    public void setCOMM_CS_REVIEW_DATE(int cOMM_CS_REVIEW_DATE) {
        COMM_CS_REVIEW_DATE = cOMM_CS_REVIEW_DATE;
    }
    public String getCOMM_UPD_SUCCESS() {
        return COMM_UPD_SUCCESS;
    }
    public void setCOMM_UPD_SUCCESS(String cOMM_UPD_SUCCESS) {
        COMM_UPD_SUCCESS = cOMM_UPD_SUCCESS;
    }
    public String getCOMM_UPD_FAIL_CD() {
        return COMM_UPD_FAIL_CD;
    }
    public void setCOMM_UPD_FAIL_CD(String cOMM_UPD_FAIL_CD) {
        COMM_UPD_FAIL_CD = cOMM_UPD_FAIL_CD;
    }


    @Override
    public String toString() {
        return "UpdcustJson [COMM_ADDR=" + COMM_ADDR + ", COMM_CREDIT_SCORE=" + COMM_CREDIT_SCORE
                + ", COMM_CS_REVIEW_DATE=" + COMM_CS_REVIEW_DATE + ", COMM_CUSTNO=" + COMM_CUSTNO + ", COMM_DOB="
                + COMM_DOB + ", COMM_EYE=" + COMM_EYE + ", COMM_NAME=" + COMM_NAME + ", COMM_SCODE=" + COMM_SCODE
                + ", COMM_UPD_FAIL_CD=" + COMM_UPD_FAIL_CD + ", COMM_UPD_SUCCESS=" + COMM_UPD_SUCCESS + "]";
    }

    
}
