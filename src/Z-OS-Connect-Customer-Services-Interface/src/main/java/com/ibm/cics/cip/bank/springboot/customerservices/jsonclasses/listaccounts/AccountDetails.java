/*
 *
 *    Copyright IBM Corp. 2022
 *
 *
 */
package com.ibm.cics.cip.bank.springboot.customerservices.jsonclasses.listaccounts;

import com.fasterxml.jackson.databind.annotation.JsonNaming;
import com.ibm.cics.cip.bank.springboot.customerservices.JsonPropertyNamingStrategy;
import com.ibm.cics.cip.bank.springboot.customerservices.OutputFormatUtils;

@JsonNaming(JsonPropertyNamingStrategy.class)
public class AccountDetails {

    static final String COPYRIGHT =
      "Copyright IBM Corp. 2022";


    private float COMM_ACTUAL_BAL;
    private float COMM_AVAIL_BAL;
    private int COMM_SCODE;
    private float COMM_INT_RATE;
    private String COMM_EYE;
    private int COMM_OPENED;
    private int COMM_CUSTNO;
    private int COMM_NEXT_STMT_DT;
    private String COMM_ACC_TYPE;
    private int COMM_OVERDRAFT;
    private int COMM_ACCNO;
    private int COMM_LAST_STMT_DT;

    public AccountDetails() {

    }

    public float getCOMM_ACTUAL_BAL() {
        return COMM_ACTUAL_BAL;
    }
    public void setCOMM_ACTUAL_BAL(float cOMM_ACTUAL_BAL) {
        COMM_ACTUAL_BAL = cOMM_ACTUAL_BAL;
    }
    
    public float getCOMM_AVAIL_BAL() {
        return COMM_AVAIL_BAL;
    }

    public void setCOMM_AVAIL_BAL(float cOMM_AVAIL_BAL) {
        COMM_AVAIL_BAL = cOMM_AVAIL_BAL;
    }

    public int getCOMM_SCODE() {
        return COMM_SCODE;
    }
    public void setCOMM_SCODE(int cOMM_SCODE) {
        COMM_SCODE = cOMM_SCODE;
    }
    public float getCOMM_INT_RATE() {
        return COMM_INT_RATE;
    }
    public void setCOMM_INT_RATE(float cOMM_INT_RATE) {
        COMM_INT_RATE = cOMM_INT_RATE;
    }
    public String getCOMM_EYE() {
        return COMM_EYE;
    }
    public void setCOMM_EYE(String cOMM_EYE) {
        COMM_EYE = cOMM_EYE;
    }
    public int getCOMM_OPENED() {
        return COMM_OPENED;
    }
    public void setCOMM_OPENED(int cOMM_OPENED) {
        COMM_OPENED = cOMM_OPENED;
    }
    public int getCOMM_CUSTNO() {
        return COMM_CUSTNO;
    }
    public void setCOMM_CUSTNO(int cOMM_CUSTNO) {
        COMM_CUSTNO = cOMM_CUSTNO;
    }
    public int getCOMM_NEXT_STMT_DT() {
        return COMM_NEXT_STMT_DT;
    }
    public void setCOMM_NEXT_STMT_DT(int cOMM_NEXT_STMT_DT) {
        COMM_NEXT_STMT_DT = cOMM_NEXT_STMT_DT;
    }
    public String getCOMM_ACC_TYPE() {
        return COMM_ACC_TYPE;
    }
    public void setCOMM_ACC_TYPE(String cOMM_ACC_TYPE) {
        COMM_ACC_TYPE = cOMM_ACC_TYPE;
    }
    public int getCOMM_OVERDRAFT() {
        return COMM_OVERDRAFT;
    }
    public void setCOMM_OVERDRAFT(int cOMM_OVERDRAFT) {
        COMM_OVERDRAFT = cOMM_OVERDRAFT;
    }
    public int getCOMM_ACCNO() {
        return COMM_ACCNO;
    }
    public void setCOMM_ACCNO(int cOMM_ACCNO) {
        COMM_ACCNO = cOMM_ACCNO;
    }
    public int getCOMM_LAST_STMT_DT() {
        return COMM_LAST_STMT_DT;
    }
    public void setCOMM_LAST_STMT_DT(int cOMM_LAST_STMT_DT) {
        COMM_LAST_STMT_DT = cOMM_LAST_STMT_DT;
    }



    @Override
    public String toString() {
        return "AccountDetails [COMM_ACCNO=" + COMM_ACCNO + ", COMM_ACC_TYPE=" + COMM_ACC_TYPE + ", COMM_ACTUAL_BAL="
                + COMM_ACTUAL_BAL + ", COMM_CUSTNO=" + COMM_CUSTNO + ", COMM_EYE=" + COMM_EYE + ", COMM_INT_RATE="
                + COMM_INT_RATE + ", COMM_LAST_STMT_DT=" + COMM_LAST_STMT_DT + ", COMM_NEXT_STMT_DT="
                + COMM_NEXT_STMT_DT + ", COMM_OPENED=" + COMM_OPENED + ", COMM_OVERDRAFT=" + COMM_OVERDRAFT
                + ", COMM_SCODE=" + COMM_SCODE + "]";
    }

    public String toPrettyString() {
        String output = "";
        output += "Account Number:       " + OutputFormatUtils.leadingZeroes(8, COMM_ACCNO) + "\n"
                + "Sort Code:            " + String.format("%06d", COMM_SCODE) + "\n"
                + "Customer Number:      " + OutputFormatUtils.leadingZeroes(10, COMM_CUSTNO) + "\n"
                + "Account Type:         " + COMM_ACC_TYPE + "\n"
                + "Available Balance:    " + String.format("%.02f", COMM_AVAIL_BAL) + "\n"
                + "Actual Balance:       " + String.format("%.02f", COMM_ACTUAL_BAL) + "\n"
                + "Interest Rate:        " + String.format("%.02f", COMM_INT_RATE) + "\n"
                + "Overdraft:            " + COMM_OVERDRAFT + "\n"
                + "Account Opened: " + OutputFormatUtils.date(COMM_OPENED) + "\n"
                + "Next Statement Date:  " + OutputFormatUtils.date(COMM_NEXT_STMT_DT) + "\n"
                + "Last Statement Date:  " + OutputFormatUtils.date(COMM_LAST_STMT_DT) + "\n";
        return output;
    }
    
}
