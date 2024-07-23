/*                                                                        */
/* Copyright IBM Corp. 2023                                               */
/*                                                                        */
package com.ibm.cics.cip.bank.springboot.customerservices.jsonclasses.customerenquiry;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.annotation.JsonNaming;
import com.ibm.cics.cip.bank.springboot.customerservices.JsonPropertyNamingStrategy;

@JsonNaming(JsonPropertyNamingStrategy.class)
public class InqCustZJson
{



	@JsonProperty("INQCUST_NAME")
	private String inqcustName;

	@JsonProperty("INQCUST_CREDIT_SCORE")
	private int inqcustCreditScore;

	@JsonProperty("INQCUST_INQ_SUCCESS")
	private String inqcustInqSuccess;

	@JsonProperty("INQCUST_ADDR")
	private String inqcustAddress;

	@JsonProperty("INQCUST_SCODE")
	private int inqcustSortcode;

	@JsonProperty("INQCUST_INQ_FAIL_CD")
	private int inqcustInqFailCode;

	@JsonProperty("INQCUST_DOB")
	private InqCustDob inqCustDob;

	@JsonProperty("INQCUST_EYE")
	private String inqcustEye;

	@JsonProperty("INQCUST_PCB_POINTER")
	private String inqcustPcbPointer;

	@JsonProperty("INQCUST_CUSTNO")
	private int inqcustCustno;

	@JsonProperty("INQCUST_CS_REVIEW_DT")
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
		return "INQCUST_ZJson [InqCustAddr=" + inqcustAddress
				+ ", InqCustCreditScore=" + inqcustCreditScore
				+ ", InqCustCsReviewDt=" + inqcustCsReviewDate
				+ ", InqCustCustno=" + inqcustCustno + ", InqCustDob="
				+ inqCustDob + ", InqCustEye=" + inqcustEye
				+ ", InqCustInqFailCd=" + inqcustInqFailCode
				+ ", InqCustInqSuccess=" + inqcustInqSuccess
				+ ", InqCustName=" + inqcustName + ", InqCustPcbPointer="
				+ inqcustPcbPointer + ", InqCustScode=" + inqcustSortcode
				+ "]";
	}

}
