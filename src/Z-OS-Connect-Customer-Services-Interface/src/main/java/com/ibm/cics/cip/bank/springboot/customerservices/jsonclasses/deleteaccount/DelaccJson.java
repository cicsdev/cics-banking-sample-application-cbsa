/*
 *
 *    Copyright IBM Corp. 2022
 *
 *
 */
package com.ibm.cics.cip.bank.springboot.customerservices.jsonclasses.deleteaccount;

import com.fasterxml.jackson.databind.annotation.JsonNaming;
import com.ibm.cics.cip.bank.springboot.customerservices.JsonPropertyNamingStrategy;
import com.ibm.cics.cip.bank.springboot.customerservices.jsonclasses.createaccount.AccountType;

@JsonNaming(JsonPropertyNamingStrategy.class)
public class DelaccJson
{

	static final String COPYRIGHT = "Copyright IBM Corp. 2022";

	private String delaccSuccess;
	private String delaccLastStatementDate;
	private float delaccInterestRate;
	private int delaccDelFailCode;
	private String delaccSortcode;
	private String delaccDelPcb1;
	private String delaccOpened;
	private AccountType delaccAccType;
	private String delaccNextStatementDate;
	private float delaccActualBalance;
	private float delaccAvailableBalance;
	private String delaccCustno;
	private String delaccDelPcb3;
	private String delaccDelPcb2;
	private int delaccAccno;
	private int delaccOverdraft;
	private int delaccFailCode;
	private String delaccEye;
	private String delaccDelApplid;
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
		return "DelaccJson [DELACC_ACCNO=" + delaccAccno + ", DELACC_ACC_TYPE=" + delaccAccType
				+ ", DELACC_ACTUAL_BAL=" + delaccActualBalance + ", DELACC_AVAIL_BAL=" + delaccAvailableBalance
				+ ", DELACC_CUSTNO=" + delaccCustno + ", DELACC_DEL_APPLID=" + delaccDelApplid
				+ ", DELACC_DEL_FAIL_CD=" + delaccDelFailCode + ", DELACC_DEL_PCB1=" + delaccDelPcb1
				+ ", DELACC_DEL_PCB2=" + delaccDelPcb2 + ", DELACC_DEL_PCB3=" + delaccDelPcb3
				+ ", DELACC_DEL_SUCCESS=" + delaccDelSuccess + ", DELACC_EYE=" + delaccEye + ", DELACC_FAIL_CD="
				+ delaccFailCode + ", DELACC_INT_RATE=" + delaccInterestRate + ", DELACC_LAST_STMT_DT="
				+ delaccLastStatementDate + ", DELACC_NEXT_STMT_DT=" + delaccNextStatementDate + ", DELACC_OPENED="
				+ delaccOpened + ", DELACC_OVERDRAFT=" + delaccOverdraft + ", DELACC_SCODE=" + delaccSortcode
				+ ", DELACC_SUCCESS=" + delaccSuccess + "]";
	}

}
