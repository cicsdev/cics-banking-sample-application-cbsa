/*
 *
 *    Copyright IBM Corp. 2022
 *
 *
 */
package com.ibm.cics.cip.bank.springboot.customerservices.jsonclasses.createcustomer;

import com.fasterxml.jackson.databind.annotation.JsonNaming;
import com.ibm.cics.cip.bank.springboot.customerservices.JsonPropertyNamingStrategy;
import com.ibm.cics.cip.bank.springboot.customerservices.OutputFormatUtils;

@JsonNaming(JsonPropertyNamingStrategy.class)
public class CreateCustomerJson
{

	static final String COPYRIGHT = "Copyright IBM Corp. 2022";

	private CrecustJson CRECUST;

	public CreateCustomerJson(CreateCustomerForm form)
	{
		CRECUST = new CrecustJson(form.getCustName(), form.getCustAddress(), form.getCustDob());
	}

	public CreateCustomerJson()
	{

	}

	public CrecustJson getCRECUST()
	{
		return CRECUST;
	}

	public void setCRECUST(CrecustJson cRECUST)
	{
		CRECUST = cRECUST;
	}

	@Override
	public String toString()
	{
		return "CreateCustomerJson [CRECUST=" + CRECUST + "]";
	}

	public String toPrettyString()
	{
		String output = "";
		output += "Customer Number:       "
				+ OutputFormatUtils.leadingZeroes(10, CRECUST.getCOMM_KEY().getCommNumber()) + "\n"
				+ "Sort Code:      " + String.format("%06d", CRECUST.getCOMM_KEY().getCommSortcode()) + "\n"
				+ "Customer Name:         " + CRECUST.getCOMM_NAME() + "\n" + "Customer Address:    "
				+ CRECUST.getCOMM_ADDRESS() + "\n" + "Date of Birth:       "
				+ OutputFormatUtils.date(CRECUST.getCOMM_DATE_OF_BIRTH()) + "\n" + "Credit score:        "
				+ CRECUST.getCOMM_CREDIT_SCORE() + "\n" + "Next review date:            "
				+ CRECUST.getCOMM_CS_REVIEW_DATE() + "\n";
		return output;
	}
}
