/*
 *
 *    Copyright IBM Corp. 2022
 *
 *
 */
package com.ibm.cics.cip.bank.springboot.customerservices.jsonclasses.createcustomer;


import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;

public class CreateCustomerForm {

    static final String COPYRIGHT =
      "Copyright IBM Corp. 2022";


    
    @NotNull
    @Size(max=61)
    private String custName;

    @NotNull
    @Size(max=161)
    private String custAddress;

    @NotNull
    @Size(min=8, max = 8)
    private String custDob;

    public CreateCustomerForm() {

    }

    public String getCustName() {
        return custName;
    }

    public void setCustName(@NotNull String custName) {
        this.custName = custName == "" ? null : custName;
    }

    public String getCustAddress() {
        return custAddress;
    }

    public void setCustAddress(@NotNull String custAddress) {
        this.custAddress = custAddress == "" ? null : custAddress;
    }

    public String getCustDob() {
        return custDob;
    }

    public void setCustDob(String custDob) {
        this.custDob = "";
        this.custDob += custDob.substring(8, 10) + custDob.substring(5, 7) + custDob.substring(0, 4);
    }

    @Override
    public String toString() {
        return "CreateCustomerForm [custAddress=" + custAddress + ", custDob=" + custDob + ", custName=" + custName
                + "]";
    }

    public boolean isValidTitle()
    {
    	if(this.custName == null) return false;
    	String[] elements = custName.split(" ");
    	if (elements == null || elements.length <3) return false;
    	if(elements[0].contentEquals("Mr")) return true;
    	if(elements[0].contentEquals("Mrs")) return true;
    	if(elements[0].contentEquals("Miss")) return true;
    	if(elements[0].contentEquals("Ms")) return true;
    	if(elements[0].contentEquals("Dr")) return true;
    	if(elements[0].contentEquals("Professor")) return true;
    	if(elements[0].contentEquals("Drs")) return true;
    	if(elements[0].contentEquals("Lord")) return true;
    	if(elements[0].contentEquals("Sir")) return true;
    	if(elements[0].contentEquals("Lady")) return true;
    	return false;
    }
    
}
