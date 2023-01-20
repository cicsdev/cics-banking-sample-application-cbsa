/*
 *
 *    Copyright IBM Corp. 2022
 *
 *
 */
package com.ibm.cics.cip.bank.springboot.customerservices.jsonclasses.customerenquiry;

import com.fasterxml.jackson.databind.annotation.JsonNaming;
import com.ibm.cics.cip.bank.springboot.customerservices.JsonPropertyNamingStrategy;
import com.ibm.cics.cip.bank.springboot.customerservices.OutputFormatUtils;

@JsonNaming(JsonPropertyNamingStrategy.class)
public class CustomerEnquiryJson
{

	static final String COPYRIGHT = "Copyright IBM Corp. 2022";

	private InqCustZJson INQCUSTZ;

	public CustomerEnquiryJson()
	{
	}

	public InqCustZJson getINQCUSTZ()
	{
		return INQCUSTZ;
	}

	public void setINQCUSTZ(InqCustZJson iNQCUSTZ)
	{
		INQCUSTZ = iNQCUSTZ;
	}

	@Override
	public String toString()
	{
		return "CustomerEnquiryJson [INQCUSTZ=" + INQCUSTZ + "]";
	}

	public String toPrettyString()
	{
		InqCustZJson custInfo = INQCUSTZ;
		String output = "";
		output += "Customer Number:    " + OutputFormatUtils.leadingZeroes(10, custInfo.getINQCUST_CUSTNO()) + "\n"
				+ "Customer Name:   " + custInfo.getINQCUST_NAME() + "\n" + "Customer Address:      "
				+ custInfo.getINQCUST_ADDR() + "\n" + "Date of Birth: " + custInfo.getINQCUST_DOB() + "\n"
				+ "Credit Score:    " + custInfo.getINQCUST_CREDIT_SCORE() + "\n" + "Review Date:     "
				+ custInfo.getINQCUST_CS_REVIEW_DT() + "\n" + "Sort Code:           "
				+ String.format("%06d", custInfo.getINQCUST_SCODE()) + "\n";
		return output;
	}

}
