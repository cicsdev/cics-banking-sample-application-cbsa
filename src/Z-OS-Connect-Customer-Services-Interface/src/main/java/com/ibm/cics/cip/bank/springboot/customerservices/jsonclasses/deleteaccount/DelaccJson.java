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

	private String DELACC_SUCCESS;
	private String DELACC_LAST_STMT_DT;
	private float DELACC_INT_RATE;
	private int DELACC_DEL_FAIL_CD;
	private String DELACC_SCODE;
	private String DELACC_DEL_PCB1;
	private String DELACC_OPENED;
	private AccountType DELACC_ACC_TYPE;
	private String DELACC_NEXT_STMT_DT;
	private float DELACC_ACTUAL_BAL;
	private float DELACC_AVAIL_BAL;
	private String DELACC_CUSTNO;
	private String DELACC_DEL_PCB3;
	private String DELACC_DEL_PCB2;
	private int DELACC_ACCNO;
	private int DELACC_OVERDRAFT;
	private int DELACC_FAIL_CD;
	private String DELACC_EYE;
	private String DELACC_DEL_APPLID;
	private String DELACC_DEL_SUCCESS;

	public DelaccJson()
	{

	}

	public String getDELACC_SUCCESS()
	{
		return DELACC_SUCCESS;
	}

	public void setDELACC_SUCCESS(String dELACC_SUCCESS)
	{
		DELACC_SUCCESS = dELACC_SUCCESS;
	}

	public String getDELACC_LAST_STMT_DT()
	{
		return DELACC_LAST_STMT_DT;
	}

	public void setDELACC_LAST_STMT_DT(String dELACC_LAST_STMT_DT)
	{
		DELACC_LAST_STMT_DT = dELACC_LAST_STMT_DT;
	}

	public float getDELACC_INT_RATE()
	{
		return DELACC_INT_RATE;
	}

	public void setDELACC_INT_RATE(float dELACC_INT_RATE)
	{
		DELACC_INT_RATE = dELACC_INT_RATE;
	}

	public int getDELACC_DEL_FAIL_CD()
	{
		return DELACC_DEL_FAIL_CD;
	}

	public void setDELACC_DEL_FAIL_CD(int dELACC_DEL_FAIL_CD)
	{
		DELACC_DEL_FAIL_CD = dELACC_DEL_FAIL_CD;
	}

	public String getDELACC_SCODE()
	{
		return DELACC_SCODE;
	}

	public void setDELACC_SCODE(String dELACC_SCODE)
	{
		DELACC_SCODE = dELACC_SCODE;
	}

	public String getDELACC_DEL_PCB1()
	{
		return DELACC_DEL_PCB1;
	}

	public void setDELACC_DEL_PCB1(String dELACC_DEL_PCB1)
	{
		DELACC_DEL_PCB1 = dELACC_DEL_PCB1;
	}

	public String getDELACC_OPENED()
	{
		return DELACC_OPENED;
	}

	public void setDELACC_OPENED(String dELACC_OPENED)
	{
		DELACC_OPENED = dELACC_OPENED;
	}

	public AccountType getDELACC_ACC_TYPE()
	{
		return DELACC_ACC_TYPE;
	}

	public void setDELACC_ACC_TYPE(AccountType dELACC_ACC_TYPE)
	{
		DELACC_ACC_TYPE = dELACC_ACC_TYPE;
	}

	// If you delete the same account twice, there's no value for an empty
	// string in AccountType, so this sets it to null.
	public void setDELACC_ACC_TYPE(String dELACC_ACC_TYPE)
	{
		if (dELACC_ACC_TYPE.equals(""))
		{
			DELACC_ACC_TYPE = null;
		}
		else
		{
			DELACC_ACC_TYPE = AccountType.valueOf(dELACC_ACC_TYPE.toUpperCase());
		}
	}

	public String getDELACC_NEXT_STMT_DT()
	{
		return DELACC_NEXT_STMT_DT;
	}

	public void setDELACC_NEXT_STMT_DT(String dELACC_NEXT_STMT_DT)
	{
		DELACC_NEXT_STMT_DT = dELACC_NEXT_STMT_DT;
	}

	public float getDELACC_ACTUAL_BAL()
	{
		return DELACC_ACTUAL_BAL;
	}

	public void setDELACC_ACTUAL_BAL(float dELACC_ACTUAL_BAL)
	{
		DELACC_ACTUAL_BAL = dELACC_ACTUAL_BAL;
	}

	public float getDELACC_AVAIL_BAL()
	{
		return DELACC_AVAIL_BAL;
	}

	public void setDELACC_AVAIL_BAL(float dELACC_AVAIL_BAL)
	{
		DELACC_AVAIL_BAL = dELACC_AVAIL_BAL;
	}

	public String getDELACC_CUSTNO()
	{
		return DELACC_CUSTNO;
	}

	public void setDELACC_CUSTNO(String dELACC_CUSTNO)
	{
		DELACC_CUSTNO = dELACC_CUSTNO;
	}

	public String getDELACC_DEL_PCB3()
	{
		return DELACC_DEL_PCB3;
	}

	public void setDELACC_DEL_PCB3(String dELACC_DEL_PCB3)
	{
		DELACC_DEL_PCB3 = dELACC_DEL_PCB3;
	}

	public String getDELACC_DEL_PCB2()
	{
		return DELACC_DEL_PCB2;
	}

	public void setDELACC_DEL_PCB2(String dELACC_DEL_PCB2)
	{
		DELACC_DEL_PCB2 = dELACC_DEL_PCB2;
	}

	public int getDELACC_ACCNO()
	{
		return DELACC_ACCNO;
	}

	public void setDELACC_ACCNO(int dELACC_ACCNO)
	{
		DELACC_ACCNO = dELACC_ACCNO;
	}

	public int getDELACC_OVERDRAFT()
	{
		return DELACC_OVERDRAFT;
	}

	public void setDELACC_OVERDRAFT(int dELACC_OVERDRAFT)
	{
		DELACC_OVERDRAFT = dELACC_OVERDRAFT;
	}

	public int getDELACC_FAIL_CD()
	{
		return DELACC_FAIL_CD;
	}

	public void setDELACC_FAIL_CD(int dELACC_FAIL_CD)
	{
		DELACC_FAIL_CD = dELACC_FAIL_CD;
	}

	public String getDELACC_EYE()
	{
		return DELACC_EYE;
	}

	public void setDELACC_EYE(String dELACC_EYE)
	{
		DELACC_EYE = dELACC_EYE;
	}

	public String getDELACC_DEL_APPLID()
	{
		return DELACC_DEL_APPLID;
	}

	public void setDELACC_DEL_APPLID(String dELACC_DEL_APPLID)
	{
		DELACC_DEL_APPLID = dELACC_DEL_APPLID;
	}

	public String getDELACC_DEL_SUCCESS()
	{
		return DELACC_DEL_SUCCESS;
	}

	public void setDELACC_DEL_SUCCESS(String dELACC_DEL_SUCCESS)
	{
		DELACC_DEL_SUCCESS = dELACC_DEL_SUCCESS;
	}

	@Override
	public String toString()
	{
		return "DelaccJson [DELACC_ACCNO=" + DELACC_ACCNO + ", DELACC_ACC_TYPE=" + DELACC_ACC_TYPE
				+ ", DELACC_ACTUAL_BAL=" + DELACC_ACTUAL_BAL + ", DELACC_AVAIL_BAL=" + DELACC_AVAIL_BAL
				+ ", DELACC_CUSTNO=" + DELACC_CUSTNO + ", DELACC_DEL_APPLID=" + DELACC_DEL_APPLID
				+ ", DELACC_DEL_FAIL_CD=" + DELACC_DEL_FAIL_CD + ", DELACC_DEL_PCB1=" + DELACC_DEL_PCB1
				+ ", DELACC_DEL_PCB2=" + DELACC_DEL_PCB2 + ", DELACC_DEL_PCB3=" + DELACC_DEL_PCB3
				+ ", DELACC_DEL_SUCCESS=" + DELACC_DEL_SUCCESS + ", DELACC_EYE=" + DELACC_EYE + ", DELACC_FAIL_CD="
				+ DELACC_FAIL_CD + ", DELACC_INT_RATE=" + DELACC_INT_RATE + ", DELACC_LAST_STMT_DT="
				+ DELACC_LAST_STMT_DT + ", DELACC_NEXT_STMT_DT=" + DELACC_NEXT_STMT_DT + ", DELACC_OPENED="
				+ DELACC_OPENED + ", DELACC_OVERDRAFT=" + DELACC_OVERDRAFT + ", DELACC_SCODE=" + DELACC_SCODE
				+ ", DELACC_SUCCESS=" + DELACC_SUCCESS + "]";
	}

}
