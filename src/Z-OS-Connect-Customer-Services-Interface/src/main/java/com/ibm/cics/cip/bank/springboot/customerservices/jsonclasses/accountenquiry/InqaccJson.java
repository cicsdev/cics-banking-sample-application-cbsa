/*
 *
 *    Copyright IBM Corp. 2022
 *
 *
 */
package com.ibm.cics.cip.bank.springboot.customerservices.jsonclasses.accountenquiry;

import com.fasterxml.jackson.databind.annotation.JsonNaming;
import com.ibm.cics.cip.bank.springboot.customerservices.JsonPropertyNamingStrategy;

@JsonNaming(JsonPropertyNamingStrategy.class)
public class InqaccJson
{

	static final String COPYRIGHT = "Copyright IBM Corp. 2022";

	private String inqaccPcb1Pointer;
	private int inqaccOverdraft;
	private int inqaccLastStatementDate;
	private int inqaccSortcode;
	private float inqaccActualBalance;
	private int inqaccAccno;
	private int inqaccOpened;
	private int inqaccCustno;
	private String inqaccAccType;
	private int inqaccNextStatementDate;
	private float inqaccAvailableBalance;
	private String inqaccEyecatcher;
	private String inqaccSuccess;
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
		return "InqaccJson [INQACC_ACCNO=" + inqaccAccno + ", INQACC_ACC_TYPE=" + inqaccAccType + ", INQACC_ACTUAL_BAL="
				+ inqaccActualBalance + ", INQACC_AVAIL_BAL=" + inqaccAvailableBalance + ", INQACC_CUSTNO="
				+ inqaccCustno + ", INQACC_EYE=" + inqaccEyecatcher + ", INQACC_INT_RATE=" + inqaccInterestRate
				+ ", INQACC_LAST_STMT_DT=" + inqaccLastStatementDate + ", INQACC_NEXT_STMT_DT="
				+ inqaccNextStatementDate + ", INQACC_OPENED=" + inqaccOpened + ", INQACC_OVERDRAFT=" + inqaccOverdraft
				+ ", INQACC_PCB1_POINTER=" + inqaccPcb1Pointer + ", INQACC_SCODE=" + inqaccSortcode
				+ ", INQACC_SUCCESS=" + inqaccSuccess + "]";
	}

}
