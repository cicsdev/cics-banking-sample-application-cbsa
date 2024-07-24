/*                                                                        */
/* Copyright IBM Corp. 2023                                               */
/*                                                                        */
package com.ibm.cics.cip.bank.springboot.customerservices.jsonclasses.createcustomer;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.annotation.JsonNaming;
import com.ibm.cics.cip.bank.springboot.customerservices.JsonPropertyNamingStrategy;
import com.ibm.cics.cip.bank.springboot.customerservices.jsonclasses.createaccount.CreaccKeyJson;

@JsonNaming(JsonPropertyNamingStrategy.class)
public class CrecustJson
{



	@JsonProperty("COMM_EYECATCHER")
	private String commEyecatcher = "    ";

	@JsonProperty("COMM_KEY")
	private CreaccKeyJson commKey = new CreaccKeyJson();

	@JsonProperty("COMM_NAME")
	private String commName;

	@JsonProperty("COMM_ADDRESS")
	private String commAddress;

	@JsonProperty("COMM_DATE_OF_BIRTH")
	private String commDateOfBirth;

	@JsonProperty("COMM_CREDIT_SCORE")
	private int commCreditScore = 0;

	@JsonProperty("COMM_CS_REVIEW_DATE")
	private String commCsReviewDate = "0";

	@JsonProperty("COMM_SUCCESS")
	private String commSuccess;

	@JsonProperty("COMM_FAIL_CODE")
	private String commFailCode;




	public CrecustJson(String custName, String custAddress, String custDob)
	{
		commName = custName;
		commAddress = custAddress;
		commDateOfBirth = custDob;
	}


	public CrecustJson()
	{

	}


	public String getCommEyecatcher()
	{
		return commEyecatcher;
	}


	public void setCommEyecatcher(String commEyecatcherIn)
	{
		commEyecatcher = commEyecatcherIn;
	}


	public CreaccKeyJson getCommKey()
	{
		return commKey;
	}


	public void setCommKey(CreaccKeyJson commKeyIn)
	{
		commKey = commKeyIn;
	}


	public String getCommName()
	{
		return commName;
	}


	public void setCommName(String commNameIn)
	{
		commName = commNameIn;
	}


	public String getCommAddress()
	{
		return commAddress;
	}


	public void setCommAddress(String commAddressIn)
	{
		commAddress = commAddressIn;
	}


	public String getCommDateOfBirth()
	{
		return commDateOfBirth;
	}


	public void setCommDateOfBirth(String commDateOfBirthIn)
	{
		commDateOfBirth = commDateOfBirthIn;
	}


	public int getCommCreditScore()
	{
		return commCreditScore;
	}


	public void setCommCreditScore(int commCreditScoreIn)
	{
		commCreditScore = commCreditScoreIn;
	}


	public String getCommCsReviewDate()
	{
		return commCsReviewDate;
	}


	public void setCommCsReviewDate(String commCsReviewDateIn)
	{
		commCsReviewDate = commCsReviewDateIn;
	}


	public String getCommSuccess()
	{
		return commSuccess;
	}


	public void setCommSuccess(String commSuccessIn)
	{
		commSuccess = commSuccessIn;
	}


	public String getCommFailCode()
	{
		return commFailCode;
	}


	public void setCommFailCode(String commFailCodeIn)
	{
		commFailCode = commFailCodeIn;
	}



}
