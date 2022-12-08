/*
 *
 *    Copyright contributors to the CICS Banking Sample Application (CBSA) project
 *
 */
package com.ibm.cics.cip.bank.springboot.customerservices.jsonclasses.updatecustomer;

import com.fasterxml.jackson.databind.annotation.JsonNaming;
import com.ibm.cics.cip.bank.springboot.customerservices.JsonPropertyNamingStrategy;
import com.ibm.cics.cip.bank.springboot.customerservices.OutputFormatUtils;

@JsonNaming(JsonPropertyNamingStrategy.class)
public class UpdateCustomerJson {

    static final String COPYRIGHT =
      "Copyright contributors to the CICS Banking Sample Application (CBSA) project.";
    
    UpdcustJson UPDCUST;

    public UpdateCustomerJson(UpdateCustomerForm updateCustomerForm) {
        UPDCUST = new UpdcustJson(
            updateCustomerForm.getCustNumber(), 
            updateCustomerForm.getCustName(), 
            updateCustomerForm.getCustAddress(), 
            updateCustomerForm.getCustDoB(), 
            updateCustomerForm.getCustCreditScore(), 
            updateCustomerForm.getCustReviewDate());
    }

    public UpdateCustomerJson() {

    }

    public UpdcustJson getUPDCUST() {
        return UPDCUST;
    }

    public void setUPDCUST(UpdcustJson uPDCUST) {
        UPDCUST = uPDCUST;
    }

    @Override
    public String toString() {
        return "UpdateCustomerJson [UPDCUST=" + UPDCUST + "]";
    }

    public String toPrettyString() {
        String output = "";
        output += "Customer Number:       " + OutputFormatUtils.leadingZeroes(10, UPDCUST.getCOMM_CUSTNO()) + "\n"
                + "Sort Code:      " + UPDCUST.getCOMM_SCODE() + "\n"
                + "Customer Name:         " + UPDCUST.getCOMM_NAME() + "\n"
                + "Customer Address:    " + UPDCUST.getCOMM_ADDR() + "\n"
                + "Date of Birth:       " + OutputFormatUtils.date(UPDCUST.getCOMM_DOB()) + "\n"
                + "Credit score:        " + UPDCUST.getCOMM_CREDIT_SCORE() + "\n"
                + "Next review date:            " + UPDCUST.getCOMM_CS_REVIEW_DATE() + "\n";
        return output;
    }
}
