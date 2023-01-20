/*
 *
 *    Copyright IBM Corp. 2022
 *
 *
 */
package com.ibm.cics.cip.bank.springboot.customerservices.jsonclasses.deletecustomer;

import com.fasterxml.jackson.databind.annotation.JsonNaming;
import com.ibm.cics.cip.bank.springboot.customerservices.JsonPropertyNamingStrategy;
import com.ibm.cics.cip.bank.springboot.customerservices.OutputFormatUtils;

@JsonNaming(JsonPropertyNamingStrategy.class)
public class DeleteCustomerJson
{

	static final String COPYRIGHT = "Copyright IBM Corp. 2022";

	private DelcusJson DELCUS;

	public DeleteCustomerJson()
	{
	}

	public DelcusJson getDELCUS()
	{
		return DELCUS;
	}

	public void setDELCUS(DelcusJson dELCUS)
	{
		DELCUS = dELCUS;
	}

	@Override
	public String toString()
	{
		return "DeleteCustomerJson [DELCUS=" + DELCUS.toString() + "]";
	}

	public String toPrettyString()
	{
		String output = "";
		output += "Customer Number:       " + OutputFormatUtils.leadingZeroes(10, DELCUS.getCOMM_CUSTNO()) + "\n"
				+ "Sort Code:      " + DELCUS.getCOMM_SCODE() + "\n" + "Customer Name:         " + DELCUS.getCOMM_NAME()
				+ "\n" + "Customer Address:    " + DELCUS.getCOMM_ADDR() + "\n" + "Date of Birth:       "
				+ OutputFormatUtils.date(DELCUS.getCOMM_DOB()) + "\n" + "Credit score:        "
				+ DELCUS.getCOMM_CREDIT_SCORE() + "\n" + "Next review date:            "
				+ DELCUS.getCOMM_CS_REVIEW_DATE() + "\n";
		return output;
	}
}
