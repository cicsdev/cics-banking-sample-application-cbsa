/*                                                                        */
/* Copyright IBM Corp. 2023                                               */
/*                                                                        */
package com.ibm.cics.cip.bank.springboot.customerservices.jsonclasses.listaccounts;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.annotation.JsonNaming;
import com.ibm.cics.cip.bank.springboot.customerservices.JsonPropertyNamingStrategy;
import com.ibm.cics.cip.bank.springboot.customerservices.OutputFormatUtils;

@JsonNaming(JsonPropertyNamingStrategy.class)
public class AccountDetails
{



	private static final String FLOAT_FORMAT = "%.02f";

	@JsonProperty("COMM_ACTUAL_BAL")
	private float commActualBalance;

	@JsonProperty("COMM_AVAIL_BAL")
	private float commAvailableBalance;

	@JsonProperty("COMM_SCODE")
	private int commSortcode;

	@JsonProperty("COMM_INT_RATE")
	private float commInterestRate;

	@JsonProperty("COMM_EYE")
	private String commEye;

	@JsonProperty("COMM_OPENED")
	private int commOpened;

	@JsonProperty("COMM_CUSTNO")
	private int commCustno;

	@JsonProperty("COMM_NEXT_STMT_DT")
	private int commNextStatementDate;

	@JsonProperty("COMM_ACC_TYPE")
	private String commAccType;

	@JsonProperty("COMM_OVERDRAFT")
	private int commOverdraft;

	@JsonProperty("COMM_ACCNO")
	private int commAccno;

	@JsonProperty("COMM_LAST_STMT_DT")
	private int commLastStatementDate;


	public float getCommActualBalance()
	{
		return commActualBalance;
	}


	public void setCommActualBalance(float commActualBalanceIn)
	{
		commActualBalance = commActualBalanceIn;
	}


	public float getCommAvailableBalance()
	{
		return commAvailableBalance;
	}


	public void setCommAvailableBalance(float commAvailableBalanceIn)
	{
		commAvailableBalance = commAvailableBalanceIn;
	}


	public int getCommSortcode()
	{
		return commSortcode;
	}


	public void setCommSortcode(int commSortcodeIn)
	{
		commSortcode = commSortcodeIn;
	}


	public float getCommInterestRate()
	{
		return commInterestRate;
	}


	public void setCommInterestRate(float commInterestRateIn)
	{
		commInterestRate = commInterestRateIn;
	}


	public String getCommEye()
	{
		return commEye;
	}


	public void setCommEye(String commEyeIn)
	{
		commEye = commEyeIn;
	}


	public int getCommOpened()
	{
		return commOpened;
	}


	public void setCommOpened(int commOpenedIn)
	{
		commOpened = commOpenedIn;
	}


	public int getCommCustno()
	{
		return commCustno;
	}


	public void setCommCustno(int commCustnoIn)
	{
		commCustno = commCustnoIn;
	}


	public int getCommNextStatementDate()
	{
		return commNextStatementDate;
	}


	public void setCommNextStatementDate(int commNextStatementDateIn)
	{
		commNextStatementDate = commNextStatementDateIn;
	}


	public String getCommAccType()
	{
		return commAccType;
	}


	public void setCommAccType(String commAccTypeIn)
	{
		commAccType = commAccTypeIn;
	}


	public int getCommOverdraft()
	{
		return commOverdraft;
	}


	public void setCommOverdraft(int commOverdraftIn)
	{
		commOverdraft = commOverdraftIn;
	}


	public int getCommAccno()
	{
		return commAccno;
	}


	public void setCommAccno(int commAccnoIn)
	{
		commAccno = commAccnoIn;
	}


	public int getCommLastStatementDate()
	{
		return commLastStatementDate;
	}


	public void setCommLastStatementDate(int commLastStatementDateIn)
	{
		commLastStatementDate = commLastStatementDateIn;
	}


	@Override
	public String toString()
	{
		return "AccountDetails [CommAccno=" + commAccno + ", CommAccType="
				+ commAccType + ", CommActualBal=" + commActualBalance
				+ ", CommCustno=" + commCustno + ", CommEye=" + commEye
				+ ", CommIntRate=" + commInterestRate + ", CommLastStmtDt="
				+ commLastStatementDate + ", CommNextStmtDt="
				+ commNextStatementDate + ", CommOpened=" + commOpened
				+ ", CommOverdraft=" + commOverdraft + ", CommScode="
				+ commSortcode + "]";
	}


	public String toPrettyString()
	{
		String output = "";
		output += "Account Number:       "
				+ OutputFormatUtils.leadingZeroes(8, commAccno) + "\n"
				+ "Sort Code:            " + String.format("%06d", commSortcode)
				+ "\n" + "Customer Number:      "
				+ OutputFormatUtils.leadingZeroes(10, commCustno) + "\n"
				+ "Account Type:         " + commAccType + "\n"
				+ "Available Balance:    "
				+ String.format(FLOAT_FORMAT, commAvailableBalance) + "\n"
				+ "Actual Balance:       "
				+ String.format(FLOAT_FORMAT, commActualBalance) + "\n"
				+ "Interest Rate:        "
				+ String.format(FLOAT_FORMAT, commInterestRate) + "\n"
				+ "Overdraft:            " + commOverdraft + "\n"
				+ "Account Opened: " + OutputFormatUtils.date(commOpened) + "\n"
				+ "Next Statement Date:  "
				+ OutputFormatUtils.date(commNextStatementDate) + "\n"
				+ "Last Statement Date:  "
				+ OutputFormatUtils.date(commLastStatementDate) + "\n";
		return output;
	}

}
