/*
 *
 *    Copyright IBM Corp. 2022
 *
 *
 */
package com.ibm.cics.cip.bank.springboot.customerservices.jsonclasses.updatecustomer;

import com.fasterxml.jackson.databind.annotation.JsonNaming;
import com.ibm.cics.cip.bank.springboot.customerservices.JsonPropertyNamingStrategy;
import com.ibm.cics.cip.bank.springboot.customerservices.OutputFormatUtils;

@JsonNaming(JsonPropertyNamingStrategy.class)
public class UpdateCustomerJson
{

	static final String COPYRIGHT = "Copyright IBM Corp. 2022";

	UpdcustJson updcust;


	public UpdateCustomerJson(UpdateCustomerForm updateCustomerForm)
	{
		updcust = new UpdcustJson(updateCustomerForm.getCustNumber(),
				updateCustomerForm.getCustName(),
				updateCustomerForm.getCustAddress(),
				updateCustomerForm.getCustDoB(),
				updateCustomerForm.getCustCreditScore(),
				updateCustomerForm.getCustReviewDate());
	}


	public UpdateCustomerJson()
	{

	}


	public UpdcustJson getUpducst()
	{
		return updcust;
	}


	public void setUpdcust(UpdcustJson updCustIn)
	{
		updcust = updCustIn;
	}


	@Override
	public String toString()
	{
		return "UpdateCustomerJson [UPDCUST=" + updcust + "]";
	}


	public String toPrettyString()
	{
		String output = "";
		output += "Customer Number:       "
				+ OutputFormatUtils.leadingZeroes(10, updcust.getCommCustno())
				+ "\n" + "Sort Code:      " + updcust.getCommSortcode() + "\n"
				+ "Customer Name:         " + updcust.getCommName() + "\n"
				+ "Customer Address:    " + updcust.getCommAddress() + "\n"
				+ "Date of Birth:       "
				+ OutputFormatUtils.date(updcust.getCommDateOfBirth()) + "\n"
				+ "Credit score:        " + updcust.getCommCreditScore() + "\n"
				+ "Next review date:            "
				+ updcust.getCommCreditScoreReviewDate() + "\n";
		return output;
	}
}
