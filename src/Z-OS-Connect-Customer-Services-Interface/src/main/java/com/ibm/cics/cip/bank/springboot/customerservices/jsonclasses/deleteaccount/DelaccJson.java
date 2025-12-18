/*                                                                        */
/* Copyright IBM Corp. 2023                                               */
/*                                                                        */
package com.ibm.cics.cip.bank.springboot.customerservices.jsonclasses.deleteaccount;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.annotation.JsonNaming;
import com.ibm.cics.cip.bank.springboot.customerservices.JsonPropertyNamingStrategy;
import com.ibm.cics.cip.bank.springboot.customerservices.jsonclasses.createaccount.AccountType;

@JsonNaming(JsonPropertyNamingStrategy.class)
public class DelaccJson
{



	@JsonProperty("DelAccSuccess")
	private String delaccSuccess;

	@JsonProperty("DelAccLastStmtDt")
	private String delaccLastStatementDate;

	@JsonProperty("DelAccIntRate")
	private float delaccInterestRate;

	@JsonProperty("DelAccFailCd")
	private int delaccDelFailCode;

	@JsonProperty("DelAccScode")
	private String delaccSortcode;

	@JsonProperty("DelAccDelPcb1")
	private String delaccDelPcb1;

	@JsonProperty("DelAccOpened")
	private String delaccOpened;

	@JsonProperty("DelAccAccType")
	private AccountType delaccAccType;

	@JsonProperty("DelAccNextStmtDt")
	private String delaccNextStatementDate;

	@JsonProperty("DelAccActualBal")
	private float delaccActualBalance;

	@JsonProperty("DelAccAvailBal")
	private float delaccAvailableBalance;

	@JsonProperty("DelAccCustno")
	private String delaccCustno;

	@JsonProperty("DelAccDelPcb3")
	private String delaccDelPcb3;

	@JsonProperty("DelAccDelPcb2")
	private String delaccDelPcb2;

	@JsonProperty("DelAccAccno")
	private int delaccAccno;

	@JsonProperty("DelAccOverdraft")
	private int delaccOverdraft;

	@JsonProperty("DelAccDelFailCd")
	private int delaccFailCode;

	@JsonProperty("DelAccEye")
	private String delaccEye;

	@JsonProperty("DelAccDelApplid")
	private String delaccDelApplid;

	@JsonProperty("DelAccDelSuccess")
	private String delaccDelSuccess;


	public DelaccJson()
	{
		super();
	}


	public String getDelaccSuccess()
	{
		return delaccSuccess;
	}


	public void setDelaccSuccess(String delaccSuccessIn)
	{
		delaccSuccess = delaccSuccessIn;
	}


	public String getDelaccLastStatementDate()
	{
		return delaccLastStatementDate;
	}


	public void setDelAccLastStatementDate(String delaccLastStatementDateIn)
	{
		delaccLastStatementDate = delaccLastStatementDateIn;
	}


	public float getDelaccInterestRate()
	{
		return delaccInterestRate;
	}


	public void setDelaccInterestRate(float delaccInterestRateIn)
	{
		delaccInterestRate = delaccInterestRateIn;
	}


	public int getDelaccDelFailCode()
	{
		return delaccDelFailCode;
	}


	public void delaccDelFailCode(int delaccDelFailCodeIn)
	{
		delaccDelFailCode = delaccDelFailCodeIn;
	}


	public String getDelaccSortcode()
	{
		return delaccSortcode;
	}


	public void setDelaccSortcode(String delaccSortcodeIn)
	{
		delaccSortcode = delaccSortcodeIn;
	}


	public String getDelaccDelPcb1()
	{
		return delaccDelPcb1;
	}


	public void setDelaccDelPcb1(String delaccDelPcb1In)
	{
		delaccDelPcb1 = delaccDelPcb1In;
	}


	public String getDelaccOpened()
	{
		return delaccOpened;
	}


	public void setDelaccOpened(String delaccOpenedIn)
	{
		delaccOpened = delaccOpenedIn;
	}


	public AccountType getDelaccAccType()
	{
		return delaccAccType;
	}


	public void setDelaccAccType(AccountType delaccAccTypeIn)
	{
		delaccAccType = delaccAccTypeIn;
	}


	// If you delete the same account twice, there's no value for an empty
	// string in AccountType, so this sets it to null.
	public void setDelaccAccType(String delaccAccTypeIn)
	{
		if (delaccAccTypeIn.equals(""))
		{
			delaccAccType = null;
		}
		else
		{
			delaccAccType = AccountType.valueOf(delaccAccTypeIn.toUpperCase());
		}
	}


	public String getDelaccNextStatementDate()
	{
		return delaccNextStatementDate;
	}


	public void setDelaccNextStatementDate(String delaccNextStatementDateIn)
	{
		delaccNextStatementDate = delaccNextStatementDateIn;
	}


	public float getDelaccActualBalance()
	{
		return delaccActualBalance;
	}


	public void setDelaccActualBalance(float delaccActualBalanceIn)
	{
		delaccActualBalance = delaccActualBalanceIn;
	}


	public float getDelaccAvailableBalance()
	{
		return delaccAvailableBalance;
	}


	public void setDelaccAvailableBalance(float delaccAvailableBalanceIn)
	{
		delaccAvailableBalance = delaccAvailableBalanceIn;
	}


	public String getDelaccCustno()
	{
		return delaccCustno;
	}


	public void setDelaccCustno(String delAccCustnoIn)
	{
		delaccCustno = delAccCustnoIn;
	}


	public String getDelaccDelPcb3()
	{
		return delaccDelPcb3;
	}


	public void setDelaccDelPcb3(String delaccDelPcb3In)
	{
		delaccDelPcb3 = delaccDelPcb3In;
	}


	public String getDelaccDelPcb2()
	{
		return delaccDelPcb2;
	}


	public void setDelaccDelPcb2(String delaccDelPcb2In)
	{
		delaccDelPcb2 = delaccDelPcb2In;
	}


	public int getDelaccAccno()
	{
		return delaccAccno;
	}


	public void setDelaccAccno(int delaccAccnoIn)
	{
		delaccAccno = delaccAccnoIn;
	}


	public int getDelaccOverdraft()
	{
		return delaccOverdraft;
	}


	public void setDelaccOverdraft(int delaccOverdraftIn)
	{
		delaccOverdraft = delaccOverdraftIn;
	}


	public int getDelaccFailCode()
	{
		return delaccFailCode;
	}


	public void setDelaccFailCode(int delaccFailCodeIn)
	{
		delaccFailCode = delaccFailCodeIn;
	}


	public String getDelaccEye()
	{
		return delaccEye;
	}


	public void setDelaccEye(String delaccEyeIn)
	{
		delaccEye = delaccEyeIn;
	}


	public String getDelaccDelApplid()
	{
		return delaccDelApplid;
	}


	public void setDelaccDelApplid(String delaccDelApplidIn)
	{
		delaccDelApplid = delaccDelApplidIn;
	}


	public String getDelaccDelSuccess()
	{
		return delaccDelSuccess;
	}


	public void setDelaccDelSuccess(String delaccDelSuccessIn)
	{
		delaccDelSuccess = delaccDelSuccessIn;
	}


	@Override
	public String toString()
	{
		return "DelaccJson [DelAccAccno=" + delaccAccno + ", DelAccAccType="
				+ delaccAccType + ", DelAccActualBal=" + delaccActualBalance
				+ ", DelAccAvailBal=" + delaccAvailableBalance
				+ ", DelAccCustno=" + delaccCustno + ", DelAccDelApplid="
				+ delaccDelApplid + ", DelAccDelFailCd=" + delaccDelFailCode
				+ ", DelAccDelPcb1=" + delaccDelPcb1 + ", DelAccDelPcb2="
				+ delaccDelPcb2 + ", DelAccDelPcb3=" + delaccDelPcb3
				+ ", DelAccDelSuccess=" + delaccDelSuccess + ", DelAccEye="
				+ delaccEye + ", DelAccDelFailCd=" + delaccFailCode
				+ ", DelAccIntRate=" + delaccInterestRate
				+ ", DelAccLastStmtDt=" + delaccLastStatementDate
				+ ", DelAccNextStmtDt=" + delaccNextStatementDate
				+ ", DelAccOpened=" + delaccOpened + ", DelAccOverdraft="
				+ delaccOverdraft + ", DelAccScode=" + delaccSortcode
				+ ", DelAccDelSuccess=" + delaccSuccess + "]";
	}

}
