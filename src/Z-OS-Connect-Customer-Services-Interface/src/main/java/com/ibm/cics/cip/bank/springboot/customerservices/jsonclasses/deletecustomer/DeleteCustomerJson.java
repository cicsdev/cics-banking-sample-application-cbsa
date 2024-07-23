/*                                                                        */
/* Copyright IBM Corp. 2023                                               */
/*                                                                        */
package com.ibm.cics.cip.bank.springboot.customerservices.jsonclasses.deletecustomer;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.annotation.JsonNaming;
import com.ibm.cics.cip.bank.springboot.customerservices.JsonPropertyNamingStrategy;
import com.ibm.cics.cip.bank.springboot.customerservices.OutputFormatUtils;

@JsonNaming(JsonPropertyNamingStrategy.class)
public class DeleteCustomerJson
{



	@JsonProperty("DELCUS")
	private DelcusJson delcus;


	public DeleteCustomerJson()
	{
		super();
	}


	public DelcusJson getDelcus()
	{
		return delcus;
	}


	public void setDelcus(DelcusJson delcusIn)
	{
		delcus = delcusIn;
	}


	@Override
	public String toString()
	{
		return "DeleteCustomerJson [DelCus=" + delcus.toString() + "]";
	}


	public String toPrettyString()
	{
		String output = "";
		output += "Customer Number:       "
				+ OutputFormatUtils.leadingZeroes(10, delcus.getCommCustno())
				+ "\n" + "Sort Code:      " + delcus.getCommSortcode() + "\n"
				+ "Customer Name:         " + delcus.getCommName() + "\n"
				+ "Customer Address:    " + delcus.getCommAddress() + "\n"
				+ "Date of Birth:       "
				+ OutputFormatUtils.date(delcus.getCommDateOfBirth()) + "\n"
				+ "Credit score:        " + delcus.getCommCreditScore() + "\n"
				+ "Next review date:            " + delcus.getCommCsReviewDate()
				+ "\n";
		return output;
	}
}
