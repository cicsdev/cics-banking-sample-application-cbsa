/*                                                                        */
/* Copyright IBM Corp. 2023                                               */
/*                                                                        */
package com.ibm.cics.cip.bank.springboot.customerservices.jsonclasses.updateaccount;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.annotation.JsonNaming;
import com.ibm.cics.cip.bank.springboot.customerservices.JsonPropertyNamingStrategy;
import com.ibm.cics.cip.bank.springboot.customerservices.jsonclasses.createaccount.AccountType;

@JsonNaming(JsonPropertyNamingStrategy.class)
public class UpdaccJson
{



	@JsonProperty("COMM_EYE")
	private String commEye;

	@JsonProperty("COMM_CUSTNO")
	private String commCustno;

	@JsonProperty("COMM_SCODE")
	private String commSortcode;

	@JsonProperty("COMM_ACCNO")
	private int commAccno;

	@JsonProperty("COMM_INT_RATE")
	private float commInterestRate;

	@JsonProperty("COMM_OPENED")
	private String commOpened;

	@JsonProperty("COMM_OVERDRAFT")
	private int commOverdraft;

	@JsonProperty("COMM_LAST_STMT_DT")
	private String commLastStatementDate;

	@JsonProperty("COMM_NEXT_STMT_DT")
	private String commNextStatementDate;

	@JsonProperty("COMM_AVAIL_BAL")
	private float commAvailableBalance;

	@JsonProperty("COMM_ACTUAL_BAL")
	private float commActualBalance;

	@JsonProperty("COMM_SUCCESS")
	private String commSuccess;

	private static final String SPACES = "        ";

	@JsonProperty("COMM_ACC_TYPE")
	private String commAccountType = SPACES;


	public UpdaccJson(String commCustNoIn, int commAccNoIn,
			AccountType commAccountTypeIn, float commInterestRateIn,
			String commOpenedIn, int commOverdraftIn,
			String commLastStatementDateIn, String commNextStatementDateIn,
			float commAvailableBalanceIn, float commActualBalanceIn)
	{
		commCustno = commCustNoIn;
		commAccno = commAccNoIn;
		commInterestRate = commInterestRateIn;
		commOpened = commOpenedIn;
		commOverdraft = commOverdraftIn;
		commLastStatementDate = commLastStatementDateIn;
		commNextStatementDate = commNextStatementDateIn;
		commAvailableBalance = commAvailableBalanceIn;
		commActualBalance = commActualBalanceIn;
		if (commAccountTypeIn == null)
		{
			commAccountType = SPACES;
		}
		else
		{
			commAccountType = String.format("%-8s",
					commAccountTypeIn.toString());
		}
	}


	public UpdaccJson()
	{

	}


	public String getCommEye()
	{
		return commEye;
	}


	public void setCommEye(String commEyeIn)
	{
		commEye = commEyeIn;
	}


	public String getCommCustNo()
	{
		return commCustno;
	}


	public void setCommCustNo(String commCustNoIn)
	{
		commCustno = commCustNoIn;
	}


	public String getCommSortcode()
	{
		return commSortcode;
	}


	public void setCommSortcode(String commSortcodeIn)
	{
		commSortcode = commSortcodeIn;
	}


	public int getCommAccno()
	{
		return commAccno;
	}


	public void setCommAccno(int commAccnoIn)
	{
		commAccno = commAccnoIn;
	}


	public String getCommAccountType()
	{
		return commAccountType;
	}


	public void setCommAccountType(String commAccountTypeIn)
	{
		commAccountType = commAccountTypeIn;
	}


	public void setCommAccountType(AccountType accountTypeIn)
	{
		if (accountTypeIn == null)
		{
			commAccountType = SPACES;
		}
		else
		{
			commAccountType = String.format("%-8s", accountTypeIn.toString());
		}
	}


	public float getCommInterestRate()
	{
		return commInterestRate;
	}


	public void setCommInterestRate(float commInterestRateIn)
	{
		commInterestRate = commInterestRateIn;
	}


	public String getCommOpened()
	{
		return commOpened;
	}


	public void setCommOpened(String commOpenedIn)
	{
		commOpened = commOpenedIn;
	}


	public int getCommOverdraft()
	{
		return commOverdraft;
	}


	public void setCommOverdraft(int commOverdraftIn)
	{
		commOverdraft = commOverdraftIn;
	}


	public String getCommLastStatementDate()
	{
		return commLastStatementDate;
	}


	public void setCommLastStatementDate(String commLastStatementDateIn)
	{
		commLastStatementDate = commLastStatementDateIn;
	}


	public String getCommNextStatementDate()
	{
		return commNextStatementDate;
	}


	public void setCommNextStatementDate(String commNextStatementDateIn)
	{
		commNextStatementDate = commNextStatementDateIn;
	}


	public float getCommAvailableBalance()
	{
		return commAvailableBalance;
	}


	public void setCommAvailableBalance(float commAvailableBalanceIn)
	{
		commAvailableBalance = commAvailableBalanceIn;
	}


	public float getCommActualBalance()
	{
		return commActualBalance;
	}


	public void setCommActualBalance(float commActualBalanceIn)
	{
		commActualBalance = commActualBalanceIn;
	}


	public String getCommSuccess()
	{
		return commSuccess;
	}


	public void setCommSuccess(String commSuccessIn)
	{
		commSuccess = commSuccessIn;
	}


	@Override
	public String toString()
	{
		return "UpdaccJson [CommAccno=" + commAccno + ", CommAccType="
				+ commAccountType + ", CommActualBal=" + commActualBalance
				+ ", CommAvailBal=" + commAvailableBalance + ", CommCustno="
				+ commCustno + ", CommEye=" + commEye + ", CommIntRate="
				+ commInterestRate + ", CommLastStmtDt="
				+ commLastStatementDate + ", CommNextStmtDt="
				+ commNextStatementDate + ", CommOpened=" + commOpened
				+ ", CommOverdraft=" + commOverdraft + ", CommScode="
				+ commSortcode + ", CommSuccess=" + commSuccess + "]";
	}

}
