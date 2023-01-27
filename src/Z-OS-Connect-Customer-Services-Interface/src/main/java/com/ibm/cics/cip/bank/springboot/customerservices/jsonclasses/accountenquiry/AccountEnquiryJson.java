/*
 *
 *    Copyright IBM Corp. 2022
 *
 *
 */
package com.ibm.cics.cip.bank.springboot.customerservices.jsonclasses.accountenquiry;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.annotation.JsonNaming;
import com.ibm.cics.cip.bank.springboot.customerservices.JsonPropertyNamingStrategy;
import com.ibm.cics.cip.bank.springboot.customerservices.OutputFormatUtils;

@JsonNaming(JsonPropertyNamingStrategy.class)
public class AccountEnquiryJson
{

	static final String COPYRIGHT = "Copyright IBM Corp. 2022";

	private static final String FLOAT_FORMAT = "%.02f";

	@JsonProperty("InqAcc")
	private InqaccJson inqaccCommarea;


	public AccountEnquiryJson()
	{
		inqaccCommarea = new InqaccJson();
	}


	public InqaccJson getInqaccCommarea()
	{
		return inqaccCommarea;
	}


	public void setInqaccCommarea(InqaccJson inqaccCommareaIn)
	{
		inqaccCommarea = inqaccCommareaIn;
	}


	@Override
	public String toString()
	{
		return "AccountEnquiryJson [INQACC_COMMAREA="
				+ inqaccCommarea.toString() + "]";
	}


	public String toPrettyString()
	{
		InqaccJson acctInfo = inqaccCommarea;
		String output = "";
		output += "Account Number: "
				+ OutputFormatUtils.leadingZeroes(8, acctInfo.getInqaccAccno())
				+ "\n" + "Customer Number: "
				+ OutputFormatUtils.leadingZeroes(10,
						acctInfo.getInqaccCustno())
				+ "\n" + "Account Type: " + acctInfo.getInqaccAccType() + "\n"
				+ "Available Balance: "
				+ String.format(FLOAT_FORMAT,
						acctInfo.getInqaccAvailableBalance())
				+ "\n" + "Actual Balance: "
				+ String.format(FLOAT_FORMAT, acctInfo.getInqaccActualBalance())
				+ "\n" + "Interest Rate: "
				+ String.format(FLOAT_FORMAT, acctInfo.getInqaccInterestRate())
				+ "\n" + "Overdraft: " + acctInfo.getInqaccOverdraft() + "\n"
				+ "Account Opened: "
				+ OutputFormatUtils.date(acctInfo.getInqaccOpened()) + "\n"
				+ "Next Statement Date: "
				+ OutputFormatUtils.date(acctInfo.getInqaccNextStatementDate())
				+ "\n" + "Last Statement Date: "
				+ OutputFormatUtils.date(acctInfo.getInqaccLastStatementDate())
				+ "\n" + "Sort Code: " + OutputFormatUtils.leadingZeroes(6,
						acctInfo.getInqaccSortcode())
				+ "\n";
		return output;
	}
}
