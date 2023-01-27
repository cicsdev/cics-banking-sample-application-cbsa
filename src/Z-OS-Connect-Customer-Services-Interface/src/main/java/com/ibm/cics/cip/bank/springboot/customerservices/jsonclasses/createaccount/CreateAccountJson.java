/*
 *
 *    Copyright IBM Corp. 2022
 *
 *
 */
package com.ibm.cics.cip.bank.springboot.customerservices.jsonclasses.createaccount;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.annotation.JsonNaming;
import com.ibm.cics.cip.bank.springboot.customerservices.JsonPropertyNamingStrategy;
import com.ibm.cics.cip.bank.springboot.customerservices.OutputFormatUtils;

@JsonNaming(JsonPropertyNamingStrategy.class)
public class CreateAccountJson
{

	static final String COPYRIGHT = "Copyright IBM Corp. 2022";

	private static final String FLOAT_FORMAT = "%.02f";

	@JsonProperty("CreAcc")	
	private CreaccJson creAcc;


	public CreateAccountJson(CreateAccountForm createAccountForm)
	{
		creAcc = new CreaccJson(createAccountForm.getAccountType().toString(),
				createAccountForm.getCustNumber(),
				createAccountForm.getOverdraftLimit(),
				createAccountForm.getInterestRate());

	}


	public CreateAccountJson()
	{

	}


	public CreaccJson getCreAcc()
	{
		return creAcc;
	}


	public void setCreAcc(CreaccJson creAccIn)
	{
		creAcc = creAccIn;
	}


	@Override
	public String toString()
	{
		return "CreateAccountJson [CreAcc=" + creAcc + "]";
	}


	public String toPrettyString()
	{
		CreaccJson accInfo = creAcc;
		String output = "";
		output += "Account Number: "
				+ OutputFormatUtils.leadingZeroes(8,
						accInfo.getCommKey().getCommNumber())
				+ "\n" + "Sort Code: "
				+ OutputFormatUtils.leadingZeroes(6,
						accInfo.getCommKey().getCommSortcode())
				+ "\n" + "Account Type: " + accInfo.getCommAccType() + "\n"
				+ "Customer Number: "
				+ OutputFormatUtils.leadingZeroes(10, accInfo.getCommCustno())
				+ "\n" + "Interest Rate: "
				+ String.format(FLOAT_FORMAT, accInfo.getCommInterestRate())
				+ "\n" + "Overdraft Limit: " + accInfo.getCommOverdraftLimit()
				+ "\n" + "Available Balance: "
				+ String.format(FLOAT_FORMAT, accInfo.getCommAvailableBalance())
				+ "\n" + "Actual Balance: "
				+ String.format(FLOAT_FORMAT, accInfo.getCommActualBalance())
				+ "\n" + "Account Opened: "
				+ OutputFormatUtils.date(accInfo.getCommOpened()) + "\n"
				+ "Last Statement Date: "
				+ OutputFormatUtils.date(accInfo.getCommLastStatementDate())
				+ "\n" + "Next Statement Date: "
				+ OutputFormatUtils.date(accInfo.getCommNextStatementDate())
				+ "\n";
		return output;
	}

}
