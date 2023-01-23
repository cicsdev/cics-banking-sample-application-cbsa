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

	private String inqcustName;
	private int inqcustCreditScore;
	private String inqcustInqSuccess;
	private String inqcustAddress;
	private int inqcustSortcode;
	private int inqcustInqFailCode;
	private InqCustDob inqCustDob;
	private String inqcustEye;
	private String inqcustPcbPointer;
	private int inqcustCustno;
	private InqCustReviewDate inqcustCsReviewDate;

	public InqCustZJson()
	{
		super();
	}

	public String getInqcustName()
	{
		return inqcustName;
	}

	public void setInqcustName(String inqcustNameIn)
	{
		inqcustName = inqcustNameIn;
	}

	public int getInqcustCreditScore()
	{
		return inqcustCreditScore;
	}

	public void setInqcustCreditScore(int inqcustCreditScoreIn)
	{
		inqcustCreditScore = inqcustCreditScoreIn;
	}

	public String getInqcustInqSuccess()
	{
		return inqcustInqSuccess;
	}

	public void setInqcustInqSuccess(String inqcustInqSuccessIn)
	{
		inqcustInqSuccess = inqcustInqSuccessIn;
	}

	public String getInqcustAddress()
	{
		return inqcustAddress;
	}

	public void setInqcustAddress(String inqcustAddressIn)
	{
		inqcustAddress = inqcustAddressIn;
	}

	public int getInqcustSortcode()
	{
		return inqcustSortcode;
	}

	public void setInqcustSortcode(int inqcustSortcodeIn)
	{
		inqcustSortcode = inqcustSortcodeIn;
	}

	public int getInqcustInqFailCode()
	{
		return inqcustInqFailCode;
	}

	public void setInqcustInqFailCode(int inqcustInqFailCodeIn)
	{
		inqcustInqFailCode = inqcustInqFailCodeIn;
	}

	public InqCustDob getInqcustDob()
	{
		return inqCustDob;
	}

	public void setInqcustDob(InqCustDob inqCustDobIn)
	{
		inqCustDob = inqCustDobIn;
	}

	public String getInqcustEye()
	{
		return inqcustEye;
	}

	public void setInqcustEye(String inqcustEyeIn)
	{
		inqcustEye = inqcustEyeIn;
	}

	public String getInqcustPcbPointer()
	{
		return inqcustPcbPointer;
	}

	public void setInqcustPcbPointer(String inqcustPcbPointerIn)
	{
		inqcustPcbPointer = inqcustPcbPointerIn;
	}

	public int getInqcustCustno()
	{
		return inqcustCustno;
	}

	public void setInqcustCustno(int inqcustCustnoIn)
	{
		inqcustCustno = inqcustCustnoIn;
	}

	public InqCustReviewDate getInqcustCsReviewDate()
	{
		return inqcustCsReviewDate;
	}

	public void setInqcustCsReviewDate(InqCustReviewDate inqCustReviewDateIn)
	{
		inqcustCsReviewDate = inqCustReviewDateIn;
	}

	@Override
	public String toString()
	{
		return "InqCustZJson [INQCUST_ADDR=" + inqcustAddress + ", INQCUST_CREDIT_SCORE=" + inqcustCreditScore
				+ ", INQCUST_CS_REVIEW_DT=" + inqcustCsReviewDate + ", INQCUST_CUSTNO=" + inqcustCustno
				+ ", INQCUST_DOB=" + inqCustDob + ", INQCUST_EYE=" + inqcustEye + ", INQCUST_INQ_FAIL_CD="
				+ inqcustInqFailCode + ", INQCUST_INQ_SUCCESS=" + inqcustInqSuccess + ", INQCUST_NAME=" + inqcustName
				+ ", INQCUST_PCB_POINTER=" + inqcustPcbPointer + ", INQCUST_SCODE=" + inqcustSortcode + "]";
	}

}
