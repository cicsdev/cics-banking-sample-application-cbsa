/*
 *
 *    Copyright IBM Corp. 2022
 *
 *
 */
package com.ibm.cics.cip.bank.springboot.customerservices.jsonclasses.updateaccount;

import com.fasterxml.jackson.databind.annotation.JsonNaming;
import com.ibm.cics.cip.bank.springboot.customerservices.JsonPropertyNamingStrategy;
import com.ibm.cics.cip.bank.springboot.customerservices.OutputFormatUtils;

@JsonNaming(JsonPropertyNamingStrategy.class)
public class UpdateAccountJson
{

	static final String COPYRIGHT = "Copyright IBM Corp. 2022";
	private static final String FLOAT_FORMAT = "%.02f";

	UpdaccJson UPDACC;

	public UpdateAccountJson(UpdateAccountForm updateAccountForm)
	{
		UPDACC = new UpdaccJson(updateAccountForm.getCustNumber(), updateAccountForm.getAcctNumber(),
				updateAccountForm.getAcctType(), updateAccountForm.getAcctInterestRateFloat(),
				updateAccountForm.getAcctOpenedDate(), updateAccountForm.getAcctOverdraftInt(),
				updateAccountForm.getAcctLastStatementDate(), updateAccountForm.getAcctNextStatementDate(),
				updateAccountForm.getAcctAvailableBalance(), updateAccountForm.getAcctActualBalance());
	}

	public UpdateAccountJson()
	{

	}

	public UpdaccJson getUPDACC()
	{
		return UPDACC;
	}

	public void setUPDACC(UpdaccJson uPDACC)
	{
		UPDACC = uPDACC;
	}

	@Override
	public String toString()
	{
		return "UpdateAccountJson [UPDACC=" + UPDACC.toString() + "]";
	}

	public String toPrettyString()
	{
		UpdaccJson accInfo = UPDACC;
		String output = "";
		output += "Account Number: " + OutputFormatUtils.leadingZeroes(8, accInfo.getCOMM_ACCNO()) + "\n"
				+ "Sort Code: " + accInfo.getCOMM_SCODE() + "\n" + "Account Type: " + accInfo.getCOMM_ACC_TYPE() + "\n"
				+ "Customer Number: " + OutputFormatUtils.leadingZeroes(10, accInfo.getCOMM_CUSTNO()) + "\n"
				+ "Interest Rate: " + String.format(FLOAT_FORMAT, accInfo.getCOMM_INT_RATE()) + "\n" + "Overdraft Limit: "
				+ accInfo.getCOMM_OVERDRAFT() + "\n" + "Available Balance: "
				+ String.format(FLOAT_FORMAT, accInfo.getCOMM_AVAIL_BAL()) + "\n" + "Actual Balance: "
				+ String.format(FLOAT_FORMAT, accInfo.getCOMM_ACTUAL_BAL()) + "\n" + "Account Opened: "
				+ OutputFormatUtils.date(accInfo.getCOMM_OPENED()) + "\n" + "Last Statement Date: "
				+ OutputFormatUtils.date(accInfo.getCOMM_LAST_STMT_DT()) + "\n" + "Next Statement Date: "
				+ OutputFormatUtils.date(accInfo.getCOMM_NEXT_STMT_DT()) + "\n";
		return output;
	}
}
