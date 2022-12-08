/*
 *
 *    Copyright contributors to the CICS Banking Sample Application (CBSA) project
 *
 */
package com.ibm.cics.cip.bank.springboot.customerservices.jsonclasses.createaccount;

import com.fasterxml.jackson.databind.annotation.JsonNaming;
import com.ibm.cics.cip.bank.springboot.customerservices.JsonPropertyNamingStrategy;
import com.ibm.cics.cip.bank.springboot.customerservices.OutputFormatUtils;

@JsonNaming(JsonPropertyNamingStrategy.class)
public class CreateAccountJson {

    static final String COPYRIGHT =
      "Copyright contributors to the CICS Banking Sample Application (CBSA) project.";
    
    private CreaccJson CREACC;

    public CreateAccountJson(CreateAccountForm createAccountForm) {
        CREACC = new CreaccJson(createAccountForm.getAccountType().toString(), createAccountForm.getCustNumber(), createAccountForm.getOverdraftLimit(), createAccountForm.getInterestRate());

    }

    public CreateAccountJson() {

    }

    public CreaccJson getCREACC() {
        return CREACC;
    }

    public void setCREACC(CreaccJson cREACC) {
        CREACC = cREACC;
    }

    @Override
    public String toString() {
        return "CreateAccountJson [CREACC=" + CREACC + "]";
    }

    public String toPrettyString() {
        CreaccJson accInfo = CREACC;
        String output = "";
        output += "Account Number: " + OutputFormatUtils.leadingZeroes(8, accInfo.getCOMM_KEY().getCOMM_NUMBER()) + "\n"
                + "Sort Code: " + OutputFormatUtils.leadingZeroes(6, accInfo.getCOMM_KEY().getCOMM_SORTCODE()) + "\n"
                + "Account Type: " + accInfo.getCOMM_ACC_TYPE() + "\n"
                + "Customer Number: " + OutputFormatUtils.leadingZeroes(10, accInfo.getCOMM_CUSTNO()) + "\n"
                + "Interest Rate: " + String.format("%.02f", accInfo.getCOMM_INT_RT()) + "\n"
                + "Overdraft Limit: " + accInfo.getCOMM_OVERDR_LIM() + "\n"
                + "Available Balance: " + String.format("%.02f", accInfo.getCOMM_AVAIL_BAL()) + "\n"
                + "Actual Balance: " + String.format("%.02f", accInfo.getCOMM_ACT_BAL()) + "\n"
                + "Account Opened: " + OutputFormatUtils.date(accInfo.getCOMM_OPENED()) + "\n"
                + "Last Statement Date: " + OutputFormatUtils.date(accInfo.getCOMM_LAST_STMT_DT()) + "\n"
                + "Next Statement Date: " + OutputFormatUtils.date(accInfo.getCOMM_NEXT_STMT_DT()) + "\n";
        return output;
    }

}
