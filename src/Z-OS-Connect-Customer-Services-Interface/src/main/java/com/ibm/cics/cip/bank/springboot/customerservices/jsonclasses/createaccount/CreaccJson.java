/*
 *
 *    Copyright IBM Corp. 2022
 *
 *
 */
package com.ibm.cics.cip.bank.springboot.customerservices.jsonclasses.createaccount;

import com.fasterxml.jackson.databind.annotation.JsonNaming;
import com.ibm.cics.cip.bank.springboot.customerservices.JsonPropertyNamingStrategy;

@JsonNaming(JsonPropertyNamingStrategy.class)
public class CreaccJson {

    static final String COPYRIGHT =
      "Copyright IBM Corp. 2022";

    
    private String COMM_ACC_TYPE;
    private String COMM_CUSTNO;
    private String COMM_EYECATCHER;
    private CreaccKeyJson COMM_KEY = new CreaccKeyJson();
    private float COMM_INT_RT = 0;
    private int COMM_OPENED = 0;
    private float COMM_OVERDR_LIM = 0;
    private int COMM_LAST_STMT_DT = 0;
    private int COMM_NEXT_STMT_DT = 0;
    private float COMM_AVAIL_BAL = 0;
    private float COMM_ACT_BAL = 0;
    private String COMM_SUCCESS = " ";
    private String COMM_FAIL_CODE = " ";

    public CreaccJson(String accountType, String accountNumber, float overdraftLimit, float interestRate) {
        COMM_ACC_TYPE = String.format("%-8s", accountType);
        COMM_CUSTNO = String.format("%8s", accountNumber).replace(" ", "0");

        COMM_OVERDR_LIM = overdraftLimit;
        COMM_INT_RT = interestRate;
        
        COMM_EYECATCHER = "    ";
        COMM_KEY = new CreaccKeyJson();

    }

    public CreaccJson() {

    }

    public String getCOMM_EYECATCHER() {
        return COMM_EYECATCHER;
    }
    public void setCOMM_EYECATCHER(String cOMM_EYECATCHER) {
        COMM_EYECATCHER = cOMM_EYECATCHER;
    }
    public String getCOMM_CUSTNO() {
        return COMM_CUSTNO;
    }
    public void setCOMM_CUSTNO(String cOMM_CUSTNO) {
        COMM_CUSTNO = cOMM_CUSTNO;
    }
    public CreaccKeyJson getCOMM_KEY() {
        return COMM_KEY;
    }
    public void setCOMM_KEY(CreaccKeyJson cOMM_KEY) {
        COMM_KEY = cOMM_KEY;
    }
    public String getCOMM_ACC_TYPE() {
        return COMM_ACC_TYPE;
    }
    public void setCOMM_ACC_TYPE(String cOMM_ACC_TYPE) {
        COMM_ACC_TYPE = cOMM_ACC_TYPE;
    }
    public float getCOMM_INT_RT() {
        return COMM_INT_RT;
    }
    public void setCOMM_INT_RT(float cOMM_INT_RT) {
        COMM_INT_RT = cOMM_INT_RT;
    }
    public int getCOMM_OPENED() {
        return COMM_OPENED;
    }
    public void setCOMM_OPENED(int cOMM_OPENED) {
        COMM_OPENED = cOMM_OPENED;
    }
    public float getCOMM_OVERDR_LIM() {
        return COMM_OVERDR_LIM;
    }
    public void setCOMM_OVERDR_LIM(float cOMM_OVERDR_LIM) {
        COMM_OVERDR_LIM = cOMM_OVERDR_LIM;
    }
    public int getCOMM_LAST_STMT_DT() {
        return COMM_LAST_STMT_DT;
    }
    public void setCOMM_LAST_STMT_DT(int cOMM_LAST_STMT_DT) {
        COMM_LAST_STMT_DT = cOMM_LAST_STMT_DT;
    }
    public int getCOMM_NEXT_STMT_DT() {
        return COMM_NEXT_STMT_DT;
    }
    public void setCOMM_NEXT_STMT_DT(int cOMM_NEXT_STMT_DT) {
        COMM_NEXT_STMT_DT = cOMM_NEXT_STMT_DT;
    }
    public float getCOMM_AVAIL_BAL() {
        return COMM_AVAIL_BAL;
    }
    public void setCOMM_AVAIL_BAL(float cOMM_AVAIL_BAL) {
        COMM_AVAIL_BAL = cOMM_AVAIL_BAL;
    }
    public float getCOMM_ACT_BAL() {
        return COMM_ACT_BAL;
    }
    public void setCOMM_ACT_BAL(float cOMM_ACT_BAL) {
        COMM_ACT_BAL = cOMM_ACT_BAL;
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
