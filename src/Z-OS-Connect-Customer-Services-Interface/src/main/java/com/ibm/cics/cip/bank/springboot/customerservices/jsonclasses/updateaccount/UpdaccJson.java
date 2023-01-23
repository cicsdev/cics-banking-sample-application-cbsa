/*
 *
 *    Copyright IBM Corp. 2022
 *
 *
 */
package com.ibm.cics.cip.bank.springboot.customerservices.jsonclasses.updateaccount;

import com.fasterxml.jackson.databind.annotation.JsonNaming;
import com.ibm.cics.cip.bank.springboot.customerservices.JsonPropertyNamingStrategy;
import com.ibm.cics.cip.bank.springboot.customerservices.jsonclasses.createaccount.AccountType;

@JsonNaming(JsonPropertyNamingStrategy.class)
public class UpdaccJson
{

	static final String COPYRIGHT = "Copyright IBM Corp. 2022";

	private String commEye;

	private String commCustno;

	private String commSortcode;

	private int commAccno;

	private float commInterestRate;

	private String commOpened;

	private int commOverdraft;

	private String commLastStatementDate;

	private String commNextStatementDate;

	private float commAvailableBalance;

	private float commActualBalance;

	private String commSuccess;

	private static final String SPACES = "        ";

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
		return "UpdaccJson [COMM_ACCNO=" + commAccno + ", COMM_ACC_TYPE="
				+ commAccountType + ", COMM_ACTUAL_BAL=" + commActualBalance
				+ ", COMM_AVAIL_BAL=" + commAvailableBalance + ", COMM_CUSTNO="
				+ commCustno + ", COMM_EYE=" + commEye + ", COMM_INT_RATE="
				+ commInterestRate + ", COMM_LAST_STMT_DT="
				+ commLastStatementDate + ", COMM_NEXT_STMT_DT="
				+ commNextStatementDate + ", COMM_OPENED=" + commOpened
				+ ", COMM_OVERDRAFT=" + commOverdraft + ", COMM_SCODE="
				+ commSortcode + ", COMM_SUCCESS=" + commSuccess + "]";
	}

}
