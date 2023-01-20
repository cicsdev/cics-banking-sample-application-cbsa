/*
 *
 *    Copyright IBM Corp. 2022
 *
 *
 */
package com.ibm.cics.cip.bank.springboot.customerservices.jsonclasses.customerenquiry;

import com.fasterxml.jackson.databind.annotation.JsonNaming;
import com.ibm.cics.cip.bank.springboot.customerservices.JsonPropertyNamingStrategy;

@JsonNaming(JsonPropertyNamingStrategy.class)
public class InqCustZJson
{

	static final String COPYRIGHT = "Copyright IBM Corp. 2022";

	private String INQCUST_NAME;
	private int INQCUST_CREDIT_SCORE;
	private String INQCUST_INQ_SUCCESS;
	private String INQCUST_ADDR;
	private int INQCUST_SCODE;
	private int INQCUST_INQ_FAIL_CD;
	private InqCustDob INQCUST_DOB;
	private String INQCUST_EYE;
	private String INQCUST_PCB_POINTER;
	private int INQCUST_CUSTNO;
	private InqCustReviewDate INQCUST_CS_REVIEW_DT;

	public InqCustZJson()
	{

	}

	public String getINQCUST_NAME()
	{
		return INQCUST_NAME;
	}

	public void setINQCUST_NAME(String iNQCUST_NAME)
	{
		INQCUST_NAME = iNQCUST_NAME;
	}

	public int getINQCUST_CREDIT_SCORE()
	{
		return INQCUST_CREDIT_SCORE;
	}

	public void setINQCUST_CREDIT_SCORE(int iNQCUST_CREDIT_SCORE)
	{
		INQCUST_CREDIT_SCORE = iNQCUST_CREDIT_SCORE;
	}

	public String getINQCUST_INQ_SUCCESS()
	{
		return INQCUST_INQ_SUCCESS;
	}

	public void setINQCUST_INQ_SUCCESS(String iNQCUST_INQ_SUCCESS)
	{
		INQCUST_INQ_SUCCESS = iNQCUST_INQ_SUCCESS;
	}

	public String getINQCUST_ADDR()
	{
		return INQCUST_ADDR;
	}

	public void setINQCUST_ADDR(String iNQCUST_ADDR)
	{
		INQCUST_ADDR = iNQCUST_ADDR;
	}

	public int getINQCUST_SCODE()
	{
		return INQCUST_SCODE;
	}

	public void setINQCUST_SCODE(int iNQCUST_SCODE)
	{
		INQCUST_SCODE = iNQCUST_SCODE;
	}

	public int getINQCUST_INQ_FAIL_CD()
	{
		return INQCUST_INQ_FAIL_CD;
	}

	public void setINQCUST_INQ_FAIL_CD(int iNQCUST_INQ_FAIL_CD)
	{
		INQCUST_INQ_FAIL_CD = iNQCUST_INQ_FAIL_CD;
	}

	public InqCustDob getINQCUST_DOB()
	{
		return INQCUST_DOB;
	}

	public void setINQCUST_DOB(InqCustDob iNQCUST_DOB)
	{
		INQCUST_DOB = iNQCUST_DOB;
	}

	public String getINQCUST_EYE()
	{
		return INQCUST_EYE;
	}

	public void setINQCUST_EYE(String iNQCUST_EYE)
	{
		INQCUST_EYE = iNQCUST_EYE;
	}

	public String getINQCUST_PCB_POINTER()
	{
		return INQCUST_PCB_POINTER;
	}

	public void setINQCUST_PCB_POINTER(String iNQCUST_PCB_POINTER)
	{
		INQCUST_PCB_POINTER = iNQCUST_PCB_POINTER;
	}

	public int getINQCUST_CUSTNO()
	{
		return INQCUST_CUSTNO;
	}

	public void setINQCUST_CUSTNO(int iNQCUST_CUSTNO)
	{
		INQCUST_CUSTNO = iNQCUST_CUSTNO;
	}

	public InqCustReviewDate getINQCUST_CS_REVIEW_DT()
	{
		return INQCUST_CS_REVIEW_DT;
	}

	public void setINQCUST_CS_REVIEW_DT(InqCustReviewDate iNQCUST_CS_REVIEW_DT)
	{
		INQCUST_CS_REVIEW_DT = iNQCUST_CS_REVIEW_DT;
	}

	@Override
	public String toString()
	{
		return "InqCustZJson [INQCUST_ADDR=" + INQCUST_ADDR + ", INQCUST_CREDIT_SCORE=" + INQCUST_CREDIT_SCORE
				+ ", INQCUST_CS_REVIEW_DT=" + INQCUST_CS_REVIEW_DT + ", INQCUST_CUSTNO=" + INQCUST_CUSTNO
				+ ", INQCUST_DOB=" + INQCUST_DOB + ", INQCUST_EYE=" + INQCUST_EYE + ", INQCUST_INQ_FAIL_CD="
				+ INQCUST_INQ_FAIL_CD + ", INQCUST_INQ_SUCCESS=" + INQCUST_INQ_SUCCESS + ", INQCUST_NAME="
				+ INQCUST_NAME + ", INQCUST_PCB_POINTER=" + INQCUST_PCB_POINTER + ", INQCUST_SCODE=" + INQCUST_SCODE
				+ "]";
	}

}
