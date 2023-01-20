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

	private String INQACC_PCB1_POINTER;
	private int INQACC_OVERDRAFT;
	private int INQACC_LAST_STMT_DT;
	private int INQACC_SCODE;
	private float INQACC_ACTUAL_BAL;
	private int INQACC_ACCNO;
	private int INQACC_OPENED;
	private int INQACC_CUSTNO;
	private String INQACC_ACC_TYPE;
	private int INQACC_NEXT_STMT_DT;
	private float INQACC_AVAIL_BAL;
	private String INQACC_EYE;
	private String INQACC_SUCCESS;
	private float INQACC_INT_RATE;

	public InqaccJson()
	{

	}

	public String getINQACC_PCB1_POINTER()
	{
		return INQACC_PCB1_POINTER;
	}

	public void setINQACC_PCB1_POINTER(String iNQACC_PCB1_POINTER)
	{
		INQACC_PCB1_POINTER = iNQACC_PCB1_POINTER;
	}

	public int getINQACC_OVERDRAFT()
	{
		return INQACC_OVERDRAFT;
	}

	public void setINQACC_OVERDRAFT(int iNQACC_OVERDRAFT)
	{
		INQACC_OVERDRAFT = iNQACC_OVERDRAFT;
	}

	public int getINQACC_LAST_STMT_DT()
	{
		return INQACC_LAST_STMT_DT;
	}

	public void setINQACC_LAST_STMT_DT(int iNQACC_LAST_STMT_DT)
	{
		INQACC_LAST_STMT_DT = iNQACC_LAST_STMT_DT;
	}

	public int getINQACC_SCODE()
	{
		return INQACC_SCODE;
	}

	public void setINQACC_SCODE(int iNQACC_SCODE)
	{
		INQACC_SCODE = iNQACC_SCODE;
	}

	public float getINQACC_ACTUAL_BAL()
	{
		return INQACC_ACTUAL_BAL;
	}

	public void setINQACC_ACTUAL_BAL(float iNQACC_ACTUAL_BAL)
	{
		INQACC_ACTUAL_BAL = iNQACC_ACTUAL_BAL;
	}

	public int getINQACC_ACCNO()
	{
		return INQACC_ACCNO;
	}

	public void setINQACC_ACCNO(int iNQACC_ACCNO)
	{
		INQACC_ACCNO = iNQACC_ACCNO;
	}

	public int getINQACC_OPENED()
	{
		return INQACC_OPENED;
	}

	public void setINQACC_OPENED(int iNQACC_OPENED)
	{
		INQACC_OPENED = iNQACC_OPENED;
	}

	public int getINQACC_CUSTNO()
	{
		return INQACC_CUSTNO;
	}

	public void setINQACC_CUSTNO(int iNQACC_CUSTNO)
	{
		INQACC_CUSTNO = iNQACC_CUSTNO;
	}

	public String getINQACC_ACC_TYPE()
	{
		return INQACC_ACC_TYPE;
	}

	public void setINQACC_ACC_TYPE(String iNQACC_ACC_TYPE)
	{
		INQACC_ACC_TYPE = iNQACC_ACC_TYPE;
	}

	public int getINQACC_NEXT_STMT_DT()
	{
		return INQACC_NEXT_STMT_DT;
	}

	public void setINQACC_NEXT_STMT_DT(int iNQACC_NEXT_STMT_DT)
	{
		INQACC_NEXT_STMT_DT = iNQACC_NEXT_STMT_DT;
	}

	public float getINQACC_AVAIL_BAL()
	{
		return INQACC_AVAIL_BAL;
	}

	public void setINQACC_AVAIL_BAL(float iNQACC_AVAIL_BAL)
	{
		INQACC_AVAIL_BAL = iNQACC_AVAIL_BAL;
	}

	public String getINQACC_EYE()
	{
		return INQACC_EYE;
	}

	public void setINQACC_EYE(String iNQACC_EYE)
	{
		INQACC_EYE = iNQACC_EYE;
	}

	public String getINQACC_SUCCESS()
	{
		return INQACC_SUCCESS;
	}

	public void setINQACC_SUCCESS(String iNQACC_SUCCESS)
	{
		INQACC_SUCCESS = iNQACC_SUCCESS;
	}

	public float getINQACC_INT_RATE()
	{
		return INQACC_INT_RATE;
	}

	public void setINQACC_INT_RATE(float iNQACC_INT_RATE)
	{
		INQACC_INT_RATE = iNQACC_INT_RATE;
	}

	@Override
	public String toString()
	{
		return "InqaccJson [INQACC_ACCNO=" + INQACC_ACCNO + ", INQACC_ACC_TYPE=" + INQACC_ACC_TYPE
				+ ", INQACC_ACTUAL_BAL=" + INQACC_ACTUAL_BAL + ", INQACC_AVAIL_BAL=" + INQACC_AVAIL_BAL
				+ ", INQACC_CUSTNO=" + INQACC_CUSTNO + ", INQACC_EYE=" + INQACC_EYE + ", INQACC_INT_RATE="
				+ INQACC_INT_RATE + ", INQACC_LAST_STMT_DT=" + INQACC_LAST_STMT_DT + ", INQACC_NEXT_STMT_DT="
				+ INQACC_NEXT_STMT_DT + ", INQACC_OPENED=" + INQACC_OPENED + ", INQACC_OVERDRAFT=" + INQACC_OVERDRAFT
				+ ", INQACC_PCB1_POINTER=" + INQACC_PCB1_POINTER + ", INQACC_SCODE=" + INQACC_SCODE
				+ ", INQACC_SUCCESS=" + INQACC_SUCCESS + "]";
	}

}
