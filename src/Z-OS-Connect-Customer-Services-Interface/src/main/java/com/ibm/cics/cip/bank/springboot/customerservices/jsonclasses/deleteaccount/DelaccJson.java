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



	@JsonProperty("DELACC_SUCCESS")
	private String delaccSuccess;

	@JsonProperty("DELACC_LAST_STMT_DT")
	private String delaccLastStatementDate;

	@JsonProperty("DELACC_INT_RATE")
	private float delaccInterestRate;

	@JsonProperty("DELACC_DEL_FAIL_CD")
	private int delaccDelFailCode;

	@JsonProperty("DELACC_SCODE")
	private String delaccSortcode;

	@JsonProperty("DELACC_DEL_PCB1")
	private String delaccDelPcb1;

	@JsonProperty("DELACC_OPENED")
	private String delaccOpened;

	@JsonProperty("DELACC_ACC_TYPE")
	private AccountType delaccAccType;

	@JsonProperty("DELACC_NEXT_STMT_DT")
	private String delaccNextStatementDate;

	@JsonProperty("DELACC_ACTUAL_BAL")
	private float delaccActualBalance;

	@JsonProperty("DELACC_AVAIL_BAL")
	private float delaccAvailableBalance;

	@JsonProperty("DELACC_CUSTNO")
	private String delaccCustno;

	@JsonProperty("DELACC_DEL_PCB3")
	private String delaccDelPcb3;

	@JsonProperty("DELACC_DEL_PCB2")
	private String delaccDelPcb2;

	@JsonProperty("DELACC_ACCNO")
	private int delaccAccno;

	@JsonProperty("DELACC_OVERDRAFT")
	private int delaccOverdraft;
	
	@JsonProperty("DELACC_FAIL_CD")
	private int delaccFailCode;

	@JsonProperty("DELACC_EYE")
	private String delaccEye;

	@JsonProperty("DELACC_DEL_APPLID")
	private String delaccDelApplid;

	@JsonProperty("DELACC_DEL_SUCCESS")
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
