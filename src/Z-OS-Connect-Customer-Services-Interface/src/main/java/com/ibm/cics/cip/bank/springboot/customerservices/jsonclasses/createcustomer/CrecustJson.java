/*
 *
 *    Copyright IBM Corp. 2022
 *
 *
 */
package com.ibm.cics.cip.bank.springboot.customerservices.jsonclasses.createcustomer;

import com.fasterxml.jackson.databind.annotation.JsonNaming;
import com.ibm.cics.cip.bank.springboot.customerservices.JsonPropertyNamingStrategy;
import com.ibm.cics.cip.bank.springboot.customerservices.jsonclasses.createaccount.CreaccKeyJson;

@JsonNaming(JsonPropertyNamingStrategy.class)
public class CrecustJson
{

	static final String COPYRIGHT = "Copyright IBM Corp. 2022";

	private String commEyecatcher = "    ";
	private CreaccKeyJson commKey = new CreaccKeyJson();
	private String commName;
	private String commAddress;
	private String commDateOfBirth;
	private int commCreditScore = 0;
	private String commCsReviewDate = "0";
	private String commSuccess;
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
