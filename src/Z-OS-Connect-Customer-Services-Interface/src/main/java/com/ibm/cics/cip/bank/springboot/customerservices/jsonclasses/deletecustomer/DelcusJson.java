/*
 *
 *    Copyright IBM Corp. 2022
 *
 *
 */
package com.ibm.cics.cip.bank.springboot.customerservices.jsonclasses.deletecustomer;

import com.fasterxml.jackson.databind.annotation.JsonNaming;
import com.ibm.cics.cip.bank.springboot.customerservices.JsonPropertyNamingStrategy;

@JsonNaming(JsonPropertyNamingStrategy.class)
public class DelcusJson
{

	static final String COPYRIGHT = "Copyright IBM Corp. 2022";

	private String COMM_ADDR;
	private int COMM_SCODE;
	private String COMM_NAME;
	private String COMM_DEL_SUCCESS;
	private String COMM_EYE;
	private String COMM_CS_REVIEW_DATE;
	private int COMM_CUSTNO;
	private int COMM_DEL_FAIL_CD;
	private int COMM_CREDIT_SCORE;
	private String COMM_DOB;

	public DelcusJson()
	{

	}

	public String getCOMM_ADDR()
	{
		return COMM_ADDR;
	}

	public void setCOMM_ADDR(String cOMM_ADDR)
	{
		COMM_ADDR = cOMM_ADDR;
	}

	public int getCOMM_SCODE()
	{
		return COMM_SCODE;
	}

	public void setCOMM_SCODE(int cOMM_SCODE)
	{
		COMM_SCODE = cOMM_SCODE;
	}

	public String getCOMM_NAME()
	{
		return COMM_NAME;
	}

	public void setCOMM_NAME(String cOMM_NAME)
	{
		COMM_NAME = cOMM_NAME;
	}

	public String getCOMM_DEL_SUCCESS()
	{
		return COMM_DEL_SUCCESS;
	}

	public void setCOMM_DEL_SUCCESS(String cOMM_DEL_SUCCESS)
	{
		COMM_DEL_SUCCESS = cOMM_DEL_SUCCESS;
	}

	public String getCOMM_EYE()
	{
		return COMM_EYE;
	}

	public void setCOMM_EYE(String cOMM_EYE)
	{
		COMM_EYE = cOMM_EYE;
	}

	public String getCOMM_CS_REVIEW_DATE()
	{
		return COMM_CS_REVIEW_DATE;
	}

	public void setCOMM_CS_REVIEW_DATE(String cOMM_CS_REVIEW_DATE)
	{
		COMM_CS_REVIEW_DATE = cOMM_CS_REVIEW_DATE;
	}

	public int getCOMM_CUSTNO()
	{
		return COMM_CUSTNO;
	}

	public void setCOMM_CUSTNO(int cOMM_CUSTNO)
	{
		COMM_CUSTNO = cOMM_CUSTNO;
	}

	public int getCOMM_DEL_FAIL_CD()
	{
		return COMM_DEL_FAIL_CD;
	}

	public void setCOMM_DEL_FAIL_CD(int cOMM_DEL_FAIL_CD)
	{
		COMM_DEL_FAIL_CD = cOMM_DEL_FAIL_CD;
	}

	public int getCOMM_CREDIT_SCORE()
	{
		return COMM_CREDIT_SCORE;
	}

	public void setCOMM_CREDIT_SCORE(int cOMM_CREDIT_SCORE)
	{
		COMM_CREDIT_SCORE = cOMM_CREDIT_SCORE;
	}

	public String getCOMM_DOB()
	{
		return COMM_DOB;
	}

	public void setCOMM_DOB(String cOMM_DOB)
	{
		COMM_DOB = cOMM_DOB;
	}

	@Override
	public String toString()
	{
		return "DelcusJson [COMM_ADDR=" + COMM_ADDR + ", COMM_CREDIT_SCORE=" + COMM_CREDIT_SCORE
				+ ", COMM_CS_REVIEW_DATE=" + COMM_CS_REVIEW_DATE + ", COMM_CUSTNO=" + COMM_CUSTNO
				+ ", COMM_DEL_FAIL_CD=" + COMM_DEL_FAIL_CD + ", COMM_DEL_SUCCESS=" + COMM_DEL_SUCCESS + ", COMM_DOB="
				+ COMM_DOB + ", COMM_EYE=" + COMM_EYE + ", COMM_NAME=" + COMM_NAME + ", COMM_SCODE=" + COMM_SCODE + "]";
	}

}
