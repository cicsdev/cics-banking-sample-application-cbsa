/*                                                                        */
/* Copyright IBM Corp. 2023                                               */
/*                                                                        */
package com.ibm.cics.cip.bank.springboot.customerservices.jsonclasses.updatecustomer;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.annotation.JsonNaming;
import com.ibm.cics.cip.bank.springboot.customerservices.JsonPropertyNamingStrategy;
import com.ibm.cics.cip.bank.springboot.customerservices.OutputFormatUtils;

@JsonNaming(JsonPropertyNamingStrategy.class)
public class UpdateCustomerJson
{



	@JsonProperty("UpdCust")
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


	public UpdcustJson getUpdcust()
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
		return "UpdateCustomerJson [UpdCust=" + updcust + "]";
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
