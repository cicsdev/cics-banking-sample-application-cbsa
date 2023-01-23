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

	private String commAddress;

	private int commSortcode;

	private String commName;

	private String commDelSuccess;

	private String commEye;

	private String commCsReviewDate;

	private int commCustno;

	private int commDelFailCode;

	private int commCreditScore;

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
		return "DelcusJson [COMM_ADDR=" + commAddress + ", COMM_CREDIT_SCORE="
				+ commCreditScore + ", COMM_CS_REVIEW_DATE=" + commCsReviewDate
				+ ", COMM_CUSTNO=" + commCustno + ", COMM_DEL_FAIL_CD="
				+ commDelFailCode + ", COMM_DEL_SUCCESS=" + commDelSuccess
				+ ", COMM_DOB=" + commDateOfBirth + ", COMM_EYE=" + commEye
				+ ", COMM_NAME=" + commName + ", COMM_SCODE=" + commSortcode
				+ "]";
	}

}
