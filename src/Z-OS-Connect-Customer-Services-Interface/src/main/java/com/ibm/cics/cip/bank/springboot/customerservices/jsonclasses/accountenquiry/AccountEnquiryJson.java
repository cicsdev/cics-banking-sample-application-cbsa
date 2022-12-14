/*
 *
 *    Copyright IBM Corp. 2022
 *
 *
 */
package com.ibm.cics.cip.bank.springboot.customerservices.jsonclasses.accountenquiry;

import com.fasterxml.jackson.databind.annotation.JsonNaming;
import com.ibm.cics.cip.bank.springboot.customerservices.JsonPropertyNamingStrategy;
import com.ibm.cics.cip.bank.springboot.customerservices.OutputFormatUtils;

@JsonNaming(JsonPropertyNamingStrategy.class)
public class AccountEnquiryJson {

    static final String COPYRIGHT =
      "Copyright IBM Corp. 2022";

    
    private InqaccJson INQACC_COMMAREA;

    public AccountEnquiryJson() {

    }

    public AccountEnquiryJson(AccountEnquiryForm accountEnquiryForm) {
        INQACC_COMMAREA = new InqaccJson();
    }

    public InqaccJson getINQACC_COMMAREA() {
        return INQACC_COMMAREA;
    }

    public void setINQACC_COMMAREA(InqaccJson iNQACC_COMMAREA) {
        INQACC_COMMAREA = iNQACC_COMMAREA;
    }

    @Override
    public String toString() {
        return "AccountEnquiryJson [INQACC_COMMAREA=" + INQACC_COMMAREA.toString() + "]";
    }

    public String toPrettyString() {
        InqaccJson acctInfo = INQACC_COMMAREA;
        String output = "";
        output += "Account Number: " + OutputFormatUtils.leadingZeroes(8, acctInfo.getINQACC_ACCNO()) + "\n"
                + "Customer Number: " + OutputFormatUtils.leadingZeroes(10, acctInfo.getINQACC_CUSTNO()) + "\n"
                + "Account Type: " + acctInfo.getINQACC_ACC_TYPE() + "\n"
                + "Available Balance: " + String.format("%.02f", acctInfo.getINQACC_AVAIL_BAL()) + "\n"
                + "Actual Balance: " + String.format("%.02f", acctInfo.getINQACC_ACTUAL_BAL()) + "\n"
                + "Interest Rate: " + String.format("%.02f", acctInfo.getINQACC_INT_RATE()) + "\n"
                + "Overdraft: " + acctInfo.getINQACC_OVERDRAFT() + "\n"
                + "Account Opened: " + OutputFormatUtils.date(acctInfo.getINQACC_OPENED()) + "\n"
                + "Next Statement Date: " + OutputFormatUtils.date(acctInfo.getINQACC_NEXT_STMT_DT()) + "\n"
                + "Last Statement Date: " + OutputFormatUtils.date(acctInfo.getINQACC_LAST_STMT_DT()) + "\n"
                + "Sort Code: " + OutputFormatUtils.leadingZeroes(6, acctInfo.getINQACC_SCODE()) + "\n";
        return output;
    }
}
