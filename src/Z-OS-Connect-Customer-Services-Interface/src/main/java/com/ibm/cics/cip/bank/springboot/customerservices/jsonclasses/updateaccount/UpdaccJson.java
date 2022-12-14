/*
 *
 *    Copyright IBM Corp. 2022
 *
 *
 */
package com.ibm.cics.cip.bank.springboot.customerservices.jsonclasses.updateaccount;

import com.fasterxml.jackson.databind.annotation.JsonNaming;
import com.ibm.cics.cip.bank.springboot.customerservices.JsonPropertyNamingStrategy;
import com.ibm.cics.cip.bank.springboot.customerservices.jsonclasses.createaccount.AccountType;

@JsonNaming(JsonPropertyNamingStrategy.class)
public class UpdaccJson {

    static final String COPYRIGHT =
      "Copyright IBM Corp. 2022";


    private String COMM_EYE;
    private String COMM_CUSTNO;
    private String COMM_SCODE;
    private int COMM_ACCNO;
    private String COMM_ACC_TYPE = "        ";
    private float COMM_INT_RATE = 0;
    private String COMM_OPENED = " ";
    private int COMM_OVERDRAFT = 0;
    private String COMM_LAST_STMT_DT = " ";
    private String COMM_NEXT_STMT_DT = " ";
    private float COMM_AVAIL_BAL = 0;
    private float COMM_ACTUAL_BAL = 0;
    private String COMM_SUCCESS = " ";

    public UpdaccJson(String cOMM_CUSTNO, int cOMM_ACCNO, AccountType cOMM_ACC_TYPE, float cOMM_INT_RATE,
            String cOMM_OPENED, int cOMM_OVERDRAFT, String cOMM_LAST_STMT_DT, String cOMM_NEXT_STMT_DT,
            float cOMM_AVAIL_BAL, float cOMM_ACTUAL_BAL) {
        COMM_CUSTNO = cOMM_CUSTNO;
        COMM_ACCNO = cOMM_ACCNO;
        COMM_INT_RATE = cOMM_INT_RATE;
        COMM_OPENED = cOMM_OPENED;
        COMM_OVERDRAFT = cOMM_OVERDRAFT;
        COMM_LAST_STMT_DT = cOMM_LAST_STMT_DT;
        COMM_NEXT_STMT_DT = cOMM_NEXT_STMT_DT;
        COMM_AVAIL_BAL = cOMM_AVAIL_BAL;
        COMM_ACTUAL_BAL = cOMM_ACTUAL_BAL;
        if (cOMM_ACC_TYPE == null) {
            COMM_ACC_TYPE = "        ";
        } else {
            COMM_ACC_TYPE = String.format("%-8s", cOMM_ACC_TYPE.toString());
        }
    }

    public UpdaccJson() {

    }

    public String getCOMM_EYE() {
        return COMM_EYE;
    }

    public void setCOMM_EYE(String cOMM_EYE) {
        COMM_EYE = cOMM_EYE;
    }

    public String getCOMM_CUSTNO() {
        return COMM_CUSTNO;
    }

    public void setCOMM_CUSTNO(String cOMM_CUSTNO) {
        COMM_CUSTNO = cOMM_CUSTNO;
    }

    public String getCOMM_SCODE() {
        return COMM_SCODE;
    }

    public void setCOMM_SCODE(String cOMM_SCODE) {
        COMM_SCODE = cOMM_SCODE;
    }

    public int getCOMM_ACCNO() {
        return COMM_ACCNO;
    }

    public void setCOMM_ACCNO(int cOMM_ACCNO) {
        COMM_ACCNO = cOMM_ACCNO;
    }

    public String getCOMM_ACC_TYPE() {
        return COMM_ACC_TYPE;
    }

    public void setCOMM_ACC_TYPE(String cOMM_ACC_TYPE) {
        COMM_ACC_TYPE = cOMM_ACC_TYPE;
    }

    public void setCOMM_ACC_TYPE(AccountType accountType) {
        if (accountType == null) {
            COMM_ACC_TYPE = "        ";
        } else {
            COMM_ACC_TYPE = String.format("%-8s", accountType.toString());
        }
    }

    public float getCOMM_INT_RATE() {
        return COMM_INT_RATE;
    }

    public void setCOMM_INT_RATE(float cOMM_INT_RATE) {
        COMM_INT_RATE = cOMM_INT_RATE;
    }

    public String getCOMM_OPENED() {
        return COMM_OPENED;
    }

    public void setCOMM_OPENED(String cOMM_OPENED) {
        COMM_OPENED = cOMM_OPENED;
    }

    public int getCOMM_OVERDRAFT() {
        return COMM_OVERDRAFT;
    }

    public void setCOMM_OVERDRAFT(int cOMM_OVERDRAFT) {
        COMM_OVERDRAFT = cOMM_OVERDRAFT;
    }

    public String getCOMM_LAST_STMT_DT() {
        return COMM_LAST_STMT_DT;
    }

    public void setCOMM_LAST_STMT_DT(String cOMM_LAST_STMT_DT) {
        COMM_LAST_STMT_DT = cOMM_LAST_STMT_DT;
    }

    public String getCOMM_NEXT_STMT_DT() {
        return COMM_NEXT_STMT_DT;
    }

    public void setCOMM_NEXT_STMT_DT(String cOMM_NEXT_STMT_DT) {
        COMM_NEXT_STMT_DT = cOMM_NEXT_STMT_DT;
    }

    public float getCOMM_AVAIL_BAL() {
        return COMM_AVAIL_BAL;
    }

    public void setCOMM_AVAIL_BAL(float cOMM_AVAIL_BAL) {
        COMM_AVAIL_BAL = cOMM_AVAIL_BAL;
    }

    public float getCOMM_ACTUAL_BAL() {
        return COMM_ACTUAL_BAL;
    }

    public void setCOMM_ACTUAL_BAL(float cOMM_ACTUAL_BAL) {
        COMM_ACTUAL_BAL = cOMM_ACTUAL_BAL;
    }

    public String getCOMM_SUCCESS() {
        return COMM_SUCCESS;
    }

    public void setCOMM_SUCCESS(String cOMM_SUCCESS) {
        COMM_SUCCESS = cOMM_SUCCESS;
    }

    @Override
    public String toString() {
        return "UpdaccJson [COMM_ACCNO=" + COMM_ACCNO + ", COMM_ACC_TYPE=" + COMM_ACC_TYPE + ", COMM_ACTUAL_BAL="
                + COMM_ACTUAL_BAL + ", COMM_AVAIL_BAL=" + COMM_AVAIL_BAL + ", COMM_CUSTNO=" + COMM_CUSTNO
                + ", COMM_EYE=" + COMM_EYE + ", COMM_INT_RATE=" + COMM_INT_RATE + ", COMM_LAST_STMT_DT="
                + COMM_LAST_STMT_DT + ", COMM_NEXT_STMT_DT=" + COMM_NEXT_STMT_DT + ", COMM_OPENED=" + COMM_OPENED
                + ", COMM_OVERDRAFT=" + COMM_OVERDRAFT + ", COMM_SCODE=" + COMM_SCODE + ", COMM_SUCCESS=" + COMM_SUCCESS
                + "]";
    }

    

}
