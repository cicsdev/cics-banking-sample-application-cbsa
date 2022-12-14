/*
 *
 *    Copyright IBM Corp. 2022
 *
 *
 */
package com.ibm.cics.cip.bank.springboot.customerservices.jsonclasses.deleteaccount;

import com.fasterxml.jackson.databind.annotation.JsonNaming;
import com.ibm.cics.cip.bank.springboot.customerservices.JsonPropertyNamingStrategy;
import com.ibm.cics.cip.bank.springboot.customerservices.OutputFormatUtils;

@JsonNaming(JsonPropertyNamingStrategy.class)
public class DeleteAccountJson {

    static final String COPYRIGHT =
      "Copyright IBM Corp. 2022";

    
    private DelaccJson DELACC_COMMAREA;

    public DeleteAccountJson() {

    }

    public DelaccJson getDELACC_COMMAREA() {
        return DELACC_COMMAREA;
    }

    public void setDELACC_COMMAREA(DelaccJson dELACC_COMMAREA) {
        DELACC_COMMAREA = dELACC_COMMAREA;
    }

    @Override
    public String toString() {
        return "DeleteAccountJson [DELACC_COMMAREA=" + DELACC_COMMAREA.toString() + "]";
    }
    
    public String toPrettyString() {
        DelaccJson accInfo = DELACC_COMMAREA;
        String output = "";
        output += "Account Number: " + OutputFormatUtils.leadingZeroes(8, accInfo.getDELACC_ACCNO()) + "\n"
                + "Sort Code: " + accInfo.getDELACC_SCODE() + "\n"
                + "Account Type: " + accInfo.getDELACC_ACC_TYPE() + "\n"
                + "Customer Number: " + OutputFormatUtils.leadingZeroes(10, accInfo.getDELACC_CUSTNO()) + "\n"
                + "Interest Rate: " + String.format("%.02f", accInfo.getDELACC_INT_RATE()) + "\n"
                + "Overdraft Limit: " + accInfo.getDELACC_OVERDRAFT() + "\n"
                + "Available Balance: " + String.format("%.02f", accInfo.getDELACC_AVAIL_BAL()) + "\n"
                + "Actual Balance: " + String.format("%.02f", accInfo.getDELACC_ACTUAL_BAL()) + "\n"
                + "Account Opened: " + OutputFormatUtils.date(accInfo.getDELACC_OPENED()) + "\n"
                + "Last Statement Date: " + OutputFormatUtils.date(accInfo.getDELACC_LAST_STMT_DT()) + "\n"
                + "Next Statement Date: " + OutputFormatUtils.date(accInfo.getDELACC_NEXT_STMT_DT()) + "\n";
        return output;
    }
}
