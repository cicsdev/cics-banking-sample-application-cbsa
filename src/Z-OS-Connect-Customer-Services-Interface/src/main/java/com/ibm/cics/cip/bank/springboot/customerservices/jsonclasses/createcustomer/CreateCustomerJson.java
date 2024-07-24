/*                                                                        */
/* Copyright IBM Corp. 2023                                               */
/*                                                                        */
package com.ibm.cics.cip.bank.springboot.customerservices.jsonclasses.createcustomer;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.annotation.JsonNaming;
import com.ibm.cics.cip.bank.springboot.customerservices.JsonPropertyNamingStrategy;
import com.ibm.cics.cip.bank.springboot.customerservices.OutputFormatUtils;

@JsonNaming(JsonPropertyNamingStrategy.class)
public class CreateCustomerJson
{


	@JsonProperty("CRECUST")
	private CrecustJson creCust;


	public CreateCustomerJson(CreateCustomerForm form)
	{
		creCust = new CrecustJson(form.getCustName(), form.getCustAddress(),
				form.getCustDob());
	}


	public CreateCustomerJson()
	{

	}


	public CrecustJson getCreCust()
	{
		return creCust;
	}


	public void setCreCust(CrecustJson creCustIn)
	{
		creCust = creCustIn;
	}


	@Override
	public String toString()
	{
		return "CreateCustomerJson [CreCust=" + creCust + "]";
	}


	public String toPrettyString()
	{
		String output = "";
		output += "Customer Number:       "
				+ OutputFormatUtils.leadingZeroes(10,
						creCust.getCommKey().getCommNumber())
				+ "\n" + "Sort Code:      "
				+ String.format("%06d", creCust.getCommKey().getCommSortcode())
				+ "\n" + "Customer Name:         " + creCust.getCommName()
				+ "\n" + "Customer Address:    " + creCust.getCommAddress()
				+ "\n" + "Date of Birth:       "
				+ OutputFormatUtils.date(creCust.getCommDateOfBirth()) + "\n"
				+ "Credit score:        " + creCust.getCommCreditScore() + "\n"
				+ "Next review date:            "
				+ creCust.getCommCsReviewDate() + "\n";
		return output;
	}
}
