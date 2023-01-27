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
import com.ibm.cics.cip.bank.springboot.customerservices.jsonclasses.createaccount.AccountType;

@JsonNaming(JsonPropertyNamingStrategy.class)
public class UpdaccJson
{

	static final String COPYRIGHT = "Copyright IBM Corp. 2022";

	@JsonProperty("CommEye")
	private String commEye;

	@JsonProperty("CommCustno")
	private String commCustno;

	@JsonProperty("CommScode")
	private String commSortcode;

	@JsonProperty("CommAccno")
	private int commAccno;

	@JsonProperty("CommIntRate")
	private float commInterestRate;

	@JsonProperty("CommOpened")
	private String commOpened;

	@JsonProperty("CommOverdraft")
	private int commOverdraft;

	@JsonProperty("CommLastStmtDt")
	private String commLastStatementDate;

	@JsonProperty("CommNextStmtDt")
	private String commNextStatementDate;

	@JsonProperty("CommAvailBal")
	private float commAvailableBalance;

	@JsonProperty("CommActualBal")
	private float commActualBalance;

	@JsonProperty("CommSuccess")
	private String commSuccess;

	private static final String SPACES = "        ";

	@JsonProperty("CommAccType")
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
