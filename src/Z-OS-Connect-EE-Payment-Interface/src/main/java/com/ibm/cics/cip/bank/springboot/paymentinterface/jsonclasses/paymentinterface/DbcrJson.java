/*
 *
 *    Copyright contributors to the CICS Banking Sample Application (CBSA) project
 *
 */
package com.ibm.cics.cip.bank.springboot.paymentinterface.jsonclasses.paymentinterface;

import com.fasterxml.jackson.databind.annotation.JsonNaming;
import com.ibm.cics.cip.bank.springboot.paymentinterface.JsonPropertyNamingStrategy;

@JsonNaming(JsonPropertyNamingStrategy.class)
public class DbcrJson {

    static final String COPYRIGHT =
      "Copyright contributors to the CICS Banking Sample Application (CBSA) project.";

    private String COMM_ACCNO;
    private float COMM_AMT;
    private int COMM_SORTC = 0;
    private int COMM_AV_BAL = 0;
    private int COMM_ACT_BAL = 0;
    private OriginJson COMM_ORIGIN;
    private String COMM_SUCCESS = " ";
    private String COMM_FAIL_CODE = " ";

    public DbcrJson() {

    }

    public DbcrJson(TransferForm transferForm) {
        COMM_ORIGIN = new OriginJson(transferForm.getOrganisation());

        // accno
        // Pads out with zeroes to the length specified
        COMM_ACCNO = String.format("%8s", transferForm.getAcctNumber()).replace(" ", "0");

        // Make the amount positive or negative based on wether debit or credit is selected
        COMM_AMT = transferForm.isDebit() ? (transferForm.getAmount() * -1) : transferForm.getAmount();
    }

    public String getCOMM_ACCNO() {
        return COMM_ACCNO;
    }

    public void setCOMM_ACCNO(String cOMM_ACCNO) {
        COMM_ACCNO = cOMM_ACCNO;
    }

    public float getCOMM_AMT() {
        return COMM_AMT;
    }

    public void setCOMM_AMT(float cOMM_AMT) {
        COMM_AMT = cOMM_AMT;
    }

    public int getCOMM_SORTC() {
        return COMM_SORTC;
    }

    public void setCOMM_SORTC(int cOMM_SORTC) {
        COMM_SORTC = cOMM_SORTC;
    }

    public int getCOMM_AV_BAL() {
        return COMM_AV_BAL;
    }

    public void setCOMM_AV_BAL(int cOMM_AV_BAL) {
        COMM_AV_BAL = cOMM_AV_BAL;
    }

    public int getCOMM_ACT_BAL() {
        return COMM_ACT_BAL;
    }

    public void setCOMM_ACT_BAL(int cOMM_ACT_BAL) {
        COMM_ACT_BAL = cOMM_ACT_BAL;
    }

    public OriginJson getCOMM_ORIGIN() {
        return COMM_ORIGIN;
    }

    public void setCOMM_ORIGIN(OriginJson cOMM_ORIGIN) {
        COMM_ORIGIN = cOMM_ORIGIN;
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

    @Override
    public String toString() {
        return "DbcrJson [COMM_ACCNO=" + COMM_ACCNO + ", COMM_ACT_BAL=" + COMM_ACT_BAL + ", COMM_AMT=" + COMM_AMT
                + ", COMM_AV_BAL=" + COMM_AV_BAL + ", COMM_FAIL_CODE=" + COMM_FAIL_CODE + ", COMM_ORIGIN="
                + COMM_ORIGIN.toString() + ", COMM_SORTC=" + COMM_SORTC + ", COMM_SUCCESS=" + COMM_SUCCESS + "]";
    }

}

// Schema to replicate:
// {
// "PAYDBCR": {
// "COMM_ACCNO": "00000002",
// "COMM_AMT": -0.10,
// "COMM_SORTC": 0,
// "COMM_AV_BAL": 0,
// "COMM_ACT_BAL": 0,
// "COMM_ORIGIN": {
// "COMM_APPLID": "NETFLIX ",
// "COMM_USERID": "LTD ",
// "COMM_FACILITY_NAME": " ",
// "COMM_NETWRK_ID": " ",
// "COMM_FACILTYPE": 0760,
// "FILL_0": " "
// },
// "COMM_SUCCESS": " ",
// "COMM_FAIL_CODE": " "
// }
// }
