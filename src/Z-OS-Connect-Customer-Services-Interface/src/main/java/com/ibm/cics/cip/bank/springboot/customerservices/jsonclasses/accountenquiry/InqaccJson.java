/*                                                                        */
/* Copyright IBM Corp. 2023                                               */
/*                                                                        */
package com.ibm.cics.cip.bank.springboot.customerservices.jsonclasses.accountenquiry;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.annotation.JsonNaming;
import com.ibm.cics.cip.bank.springboot.customerservices.JsonPropertyNamingStrategy;

@JsonNaming(JsonPropertyNamingStrategy.class)
public class InqaccJson
{


	@JsonProperty("INQACC_PCB1_POINTER")
	private String inqaccPcb1Pointer;

	@JsonProperty("INQACC_OVERDRAFT")
	private int inqaccOverdraft;

	@JsonProperty("INQACC_LAST_STMT_DT")
	private int inqaccLastStatementDate;

	@JsonProperty("INQACC_SCODE")
	private int inqaccSortcode;

	@JsonProperty("INQACC_ACTUAL_BAL")
	private float inqaccActualBalance;

	@JsonProperty("INQACC_ACCNO")
	private int inqaccAccno;

	@JsonProperty("INQACC_OPENED")
	private int inqaccOpened;

	@JsonProperty("INQACC_CUSTNO")
	private int inqaccCustno;

	@JsonProperty("INQACC_ACC_TYPE")
	private String inqaccAccType;

	@JsonProperty("INQACC_NEXT_STMT_DT")
	private int inqaccNextStatementDate;

	@JsonProperty("INQACC_AVAIL_BAL")
	private float inqaccAvailableBalance;

	@JsonProperty("INQACC_EYE")
	private String inqaccEyecatcher;

	@JsonProperty("INQACC_SUCCESS")
	private String inqaccSuccess;

	@JsonProperty("INQACC_INT_RATE")
	private float inqaccInterestRate;


	public String getInqaccPcb1Pointer()
	{
		return inqaccPcb1Pointer;
	}


	public void setInqaccPcb1Pointer(String inqaccPcb1PointerIn)
	{
		inqaccPcb1Pointer = inqaccPcb1PointerIn;
	}


	public int getInqaccOverdraft()
	{
		return inqaccOverdraft;
	}


	public void setInqaccOverdraft(int inqaccOverdraftIn)
	{
		inqaccOverdraft = inqaccOverdraftIn;
	}


	public int getInqaccLastStatementDate()
	{
		return inqaccLastStatementDate;
	}


	public void setInqaccLastStatementDate(int inqaccLastStatementDateIn)
	{
		inqaccLastStatementDate = inqaccLastStatementDateIn;
	}


	public int getInqaccSortcode()
	{
		return inqaccSortcode;
	}


	public void setInqaccSortcode(int inqaccSortcodeIn)
	{
		inqaccSortcode = inqaccSortcodeIn;
	}


	public float getInqaccActualBalance()
	{
		return inqaccActualBalance;
	}


	public void setInqaccActualBalance(float inqaccActualBalanceIn)
	{
		inqaccActualBalance = inqaccActualBalanceIn;
	}


	public int getInqaccAccno()
	{
		return inqaccAccno;
	}


	public void setInqaccAccno(int inqaccAccnoIn)
	{
		inqaccAccno = inqaccAccnoIn;
	}


	public int getInqaccOpened()
	{
		return inqaccOpened;
	}


	public void setInqaccOpened(int inqaccOpenedIn)
	{
		inqaccOpened = inqaccOpenedIn;
	}


	public int getInqaccCustno()
	{
		return inqaccCustno;
	}


	public void setInqaccCustno(int inqaccCustnoIn)
	{
		inqaccCustno = inqaccCustnoIn;
	}


	public String getInqaccAccType()
	{
		return inqaccAccType;
	}


	public void setInqaccAccType(String inaccAccTypeIn)
	{
		inqaccAccType = inaccAccTypeIn;
	}


	public int getInqaccNextStatementDate()
	{
		return inqaccNextStatementDate;
	}


	public void setInqaccNextStatementDate(int inqaccNextStatementDateIn)
	{
		inqaccNextStatementDate = inqaccNextStatementDateIn;
	}


	public float getInqaccAvailableBalance()
	{
		return inqaccAvailableBalance;
	}


	public void setInqaccAvailableBalance(float inqaccAvailableBalanceIn)
	{
		inqaccAvailableBalance = inqaccAvailableBalanceIn;
	}


	public String getInqaccEyecatcher()
	{
		return inqaccEyecatcher;
	}


	public void setInqaccEyecatcher(String inqaccEyecatcherIn)
	{
		inqaccEyecatcher = inqaccEyecatcherIn;
	}


	public String getInaccSuccess()
	{
		return inqaccSuccess;
	}


	public void setInqaccSuccess(String inqaccSuccessIn)
	{
		inqaccSuccess = inqaccSuccessIn;
	}


	public float getInqaccInterestRate()
	{
		return inqaccInterestRate;
	}


	public void setInqaccInterestRate(float inqaccInterestRateIn)
	{
		inqaccInterestRate = inqaccInterestRateIn;
	}


	@Override
	public String toString()
	{
		return "InqaccJson [InqAccAccno=" + inqaccAccno + ", InqAccAccType="
				+ inqaccAccType + ", InqAccActualBal=" + inqaccActualBalance
				+ ", InqAccAvailBal=" + inqaccAvailableBalance
				+ ", InqAccCustno=" + inqaccCustno + ", InqAccEye="
				+ inqaccEyecatcher + ", InqAccIntRate=" + inqaccInterestRate
				+ ", InqAccLastStmtDt=" + inqaccLastStatementDate
				+ ", InqAccNextStmtDt=" + inqaccNextStatementDate
				+ ", InqAccOpened=" + inqaccOpened + ", InqAccOverdraft="
				+ inqaccOverdraft + ", InqAccPcb1Pointer=" + inqaccPcb1Pointer
				+ ", InqAccScode=" + inqaccSortcode + ", InqAccSuccess="
				+ inqaccSuccess + "]";
	}

}
