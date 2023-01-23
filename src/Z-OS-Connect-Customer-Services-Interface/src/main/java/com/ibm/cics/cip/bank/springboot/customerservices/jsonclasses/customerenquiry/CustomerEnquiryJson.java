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

	private InqCustZJson inqCustZ;


	public CustomerEnquiryJson()
	{
		super();
	}


	public InqCustZJson getInqCustZ()
	{
		return inqCustZ;
	}


	public void setInqCustZ(InqCustZJson inqCustZIn)
	{
		inqCustZ = inqCustZIn;
	}


	@Override
	public String toString()
	{
		return "CustomerEnquiryJson [INQCUSTZ=" + inqCustZ + "]";
	}


	public String toPrettyString()
	{
		InqCustZJson custInfo = inqCustZ;
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
