/*                                                                        */
/* Copyright IBM Corp. 2023                                               */
/*                                                                        */
package com.ibm.cics.cip.bank.springboot.customerservices.jsonclasses.deletecustomer;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.annotation.JsonNaming;
import com.ibm.cics.cip.bank.springboot.customerservices.JsonPropertyNamingStrategy;

@JsonNaming(JsonPropertyNamingStrategy.class)
public class DelcusJson
{



	@JsonProperty("COMM_ADDR")
	private String commAddress;

	@JsonProperty("COMM_SCODE")
	private int commSortcode;

	@JsonProperty("COMM_NAME")
	private String commName;

	@JsonProperty("COMM_DEL_SUCCESS")
	private String commDelSuccess;

	@JsonProperty("COMM_EYE")
	private String commEye;

	@JsonProperty("COMM_CS_REVIEW_DATE")
	private String commCsReviewDate;

	@JsonProperty("COMM_CUSTNO")
	private int commCustno;

	@JsonProperty("COMM_DEL_FAIL_CD")
	private int commDelFailCode;

	@JsonProperty("COMM_CREDIT_SCORE")
	private int commCreditScore;

	@JsonProperty("COMM_DOB")
	private String commDateOfBirth;


	public DelcusJson()
	{
		super();
	}


	public String getCommAddress()
	{
		return commAddress;
	}


	public void setCommAddress(String commAddressIn)
	{
		commAddress = commAddressIn;
	}


	public int getCommSortcode()
	{
		return commSortcode;
	}


	public void setCommSortcode(int commSortcodeIn)
	{
		commSortcode = commSortcodeIn;
	}


	public String getCommName()
	{
		return commName;
	}


	public void setCommName(String commNameIn)
	{
		commName = commNameIn;
	}


	public String getCommDelSuccess()
	{
		return commDelSuccess;
	}


	public void setCommDelSuccess(String commDelSuccessIn)
	{
		commDelSuccess = commDelSuccessIn;
	}


	public String getCommEye()
	{
		return commEye;
	}


	public void setCommEye(String commEyeIn)
	{
		commEye = commEyeIn;
	}


	public String getCommCsReviewDate()
	{
		return commCsReviewDate;
	}


	public void setCommCsReviewDate(String commCsReviewDateIn)
	{
		commCsReviewDate = commCsReviewDateIn;
	}


	public int getCommCustno()
	{
		return commCustno;
	}


	public void setCommCustno(int commCustnoIn)
	{
		commCustno = commCustnoIn;
	}


	public int getCommDelFailCode()
	{
		return commDelFailCode;
	}


	public void setCommDelFailCode(int commDelFailCodeIn)
	{
		commDelFailCode = commDelFailCodeIn;
	}


	public int getCommCreditScore()
	{
		return commCreditScore;
	}


	public void setCommCreditScore(int commCreditScoreIn)
	{
		commCreditScore = commCreditScoreIn;
	}


	public String getCommDateOfBirth()
	{
		return commDateOfBirth;
	}


	public void setCommDateOfBirth(String commDateOfBirthIn)
	{
		commDateOfBirth = commDateOfBirthIn;
	}


	@Override
	public String toString()
	{
		return "DelcusJson [CommAddr=" + commAddress + ", CommCreditScore="
				+ commCreditScore + ", CommCsReviewDate=" + commCsReviewDate
				+ ", CommCustno=" + commCustno + ", CommDelFailCd="
				+ commDelFailCode + ", CommDelSuccess=" + commDelSuccess
				+ ", CommDob=" + commDateOfBirth + ", CommEye=" + commEye
				+ ", CommName=" + commName + ", CommScode=" + commSortcode
				+ "]";
	}

}
