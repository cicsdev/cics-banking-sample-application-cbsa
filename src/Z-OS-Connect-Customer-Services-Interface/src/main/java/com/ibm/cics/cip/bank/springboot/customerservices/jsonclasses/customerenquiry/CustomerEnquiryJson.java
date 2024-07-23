/*                                                                        */
/* Copyright IBM Corp. 2023                                               */
/*                                                                        */
package com.ibm.cics.cip.bank.springboot.customerservices.jsonclasses.customerenquiry;

import com.fasterxml.jackson.databind.annotation.JsonNaming;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.ibm.cics.cip.bank.springboot.customerservices.JsonPropertyNamingStrategy;
import com.ibm.cics.cip.bank.springboot.customerservices.OutputFormatUtils;

@JsonNaming(JsonPropertyNamingStrategy.class)

public class CustomerEnquiryJson
{

	@JsonProperty("INQCUSTZ")
	private InqCustZJson INQCUSTZ;


	public CustomerEnquiryJson()
	{
		super();
	}


	public InqCustZJson getInqCustZ()
	{
		return INQCUSTZ;
	}



	public void setInqCustZ(InqCustZJson inqCustZIn)
	{
		INQCUSTZ = inqCustZIn;
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
		output += "Customer Number:    "
				+ OutputFormatUtils.leadingZeroes(10,
						custInfo.getInqcustCustno())
				+ "\n" + "Customer Name:   " + custInfo.getInqcustName() + "\n"
				+ "Customer Address:      " + custInfo.getInqcustAddress()
				+ "\n" + "Date of Birth: " + custInfo.getInqcustDob() + "\n"
				+ "Credit Score:    " + custInfo.getInqcustCreditScore() + "\n"
				+ "Review Date:     " + custInfo.getInqcustCsReviewDate() + "\n"
				+ "Sort Code:           "
				+ String.format("%06d", custInfo.getInqcustSortcode()) + "\n";
		return output;
	}

}
