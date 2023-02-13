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



	@JsonProperty("InqCustName")
	private String inqcustName;

	@JsonProperty("InqCustCreditScore")
	private int inqcustCreditScore;

	@JsonProperty("InqCustInqSuccess")
	private String inqcustInqSuccess;

	@JsonProperty("InqCustAddr")
	private String inqcustAddress;

	@JsonProperty("InqCustScode")
	private int inqcustSortcode;

	@JsonProperty("InqCustInqFailCd")
	private int inqcustInqFailCode;

	@JsonProperty("InqCustDob")
	private InqCustDob inqCustDob;

	@JsonProperty("InqCustEye")
	private String inqcustEye;

	@JsonProperty("InqCustPcbPointer")
	private String inqcustPcbPointer;

	@JsonProperty("InqCustCustno")
	private int inqcustCustno;

	@JsonProperty("InqCustCsReviewDt")
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
		return "InqCustZJson [InqCustAddr=" + inqcustAddress
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
