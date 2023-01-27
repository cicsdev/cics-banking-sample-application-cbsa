/*
 *
 *    Copyright IBM Corp. 2022
 *
 *
 */
package com.ibm.cics.cip.bank.springboot.customerservices.jsonclasses.updateaccount;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.annotation.JsonNaming;
import com.ibm.cics.cip.bank.springboot.customerservices.JsonPropertyNamingStrategy;
import com.ibm.cics.cip.bank.springboot.customerservices.OutputFormatUtils;

@JsonNaming(JsonPropertyNamingStrategy.class)
public class UpdateAccountJson
{

	static final String COPYRIGHT = "Copyright IBM Corp. 2022";

	private static final String FLOAT_FORMAT = "%.02f";

	@JsonProperty("UpdAcc")
	UpdaccJson updacc;


	public UpdateAccountJson(UpdateAccountForm updateAccountForm)
	{
		updacc = new UpdaccJson(updateAccountForm.getCustNumber(),
				updateAccountForm.getAcctNumber(),
				updateAccountForm.getAcctType(),
				updateAccountForm.getAcctInterestRateFloat(),
				updateAccountForm.getAcctOpenedDate(),
				updateAccountForm.getAcctOverdraftInt(),
				updateAccountForm.getAcctLastStatementDate(),
				updateAccountForm.getAcctNextStatementDate(),
				updateAccountForm.getAcctAvailableBalance(),
				updateAccountForm.getAcctActualBalance());
	}


	public UpdateAccountJson()
	{

	}


	public UpdaccJson getUpdacc()
	{
		return updacc;
	}


	public void setUpdacc(UpdaccJson updaccIn)
	{
		updacc = updaccIn;
	}


	@Override
	public String toString()
	{
		return "UpdateAccountJson [UpdAcc=" + updacc.toString() + "]";
	}


	public String toPrettyString()
	{
		UpdaccJson accInfo = updacc;
		String output = "";
		output += "Account Number: "
				+ OutputFormatUtils.leadingZeroes(8, accInfo.getCommAccno())
				+ "\n" + "Sort Code: " + accInfo.getCommSortcode() + "\n"
				+ "Account Type: " + accInfo.getCommAccountType() + "\n"
				+ "Customer Number: "
				+ OutputFormatUtils.leadingZeroes(10, accInfo.getCommCustNo())
				+ "\n" + "Interest Rate: "
				+ String.format(FLOAT_FORMAT, accInfo.getCommInterestRate())
				+ "\n" + "Overdraft Limit: " + accInfo.getCommOverdraft() + "\n"
				+ "Available Balance: "
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
