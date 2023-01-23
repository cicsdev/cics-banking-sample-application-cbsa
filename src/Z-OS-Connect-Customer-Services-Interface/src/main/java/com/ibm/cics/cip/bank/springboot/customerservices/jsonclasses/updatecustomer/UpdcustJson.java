/*
 *
 *    Copyright IBM Corp. 2022
 *
 *
 */
package com.ibm.cics.cip.bank.springboot.customerservices.jsonclasses.updatecustomer;

import com.fasterxml.jackson.databind.annotation.JsonNaming;
import com.ibm.cics.cip.bank.springboot.customerservices.JsonPropertyNamingStrategy;

@JsonNaming(JsonPropertyNamingStrategy.class)
public class UpdcustJson
{

	static final String COPYRIGHT = "Copyright IBM Corp. 2022";

	private String commEye = "    ";

	private String commSortcode = "";

	private String commCustno = " ";

	private String commName = " ";

	private String commAddress = " ";

	private int commDateOfBirth = 0;

	private int commCreditScore = 0;

	private int commCreditScoreReviewDate = 0;

	private String commUpdateSuccess = " ";

	private String commUpdateFailCode = " ";


	public UpdcustJson(String commCustnoIn, String commNameIn,
			String commAddressIn, String commDateOfBirthIn,
			int commCreditScoreIn, String commCreditScoreReviewDateIn)
	{
		// Some values need to be padded out when not full
		commCustno = String.format("%10s", commCustnoIn).replace(" ", "0");
		if (!commNameIn.equals(" "))
			commName = String.format("%-60s", commNameIn);
		if (!commAddressIn.equals(" "))
			commAddress = String.format("%-160s", commAddressIn);

		// These convert strings to ints - they use ternary operators to prevent
		// a NumberFormatException on an empty String, as 0 would be okay by
		// default.
		commDateOfBirth = commDateOfBirthIn.equals("") ? 0
				: Integer.parseInt(commDateOfBirthIn);
		commCreditScoreReviewDate = commCreditScoreReviewDateIn.equals("") ? 0
				: Integer.parseInt(commCreditScoreReviewDateIn);

		// Doesn't need conversion as it isn't ever not an int
		commCreditScore = commCreditScoreIn;
	}


	public UpdcustJson()
	{

	}


	public String getCommEye()
	{
		return commEye;
	}


	public void setCommEye(String commEyeIn)
	{
		commEye = commEyeIn;
	}


	public String getCommSortcode()
	{
		return commSortcode;
	}


	public void setCommSortcode(String commSortcodeIn)
	{
		commSortcode = commSortcodeIn;
	}


	public String getCommCustno()
	{
		return commCustno;
	}


	public void setCommCustno(String commCustnoIn)
	{
		commCustno = commCustnoIn;
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


	public int getCommDateOfBirth()
	{
		return commDateOfBirth;
	}


	public void setCommDateOfBirth(int commDateOfBirthIn)
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


	public int getCommCreditScoreReviewDate()
	{
		return commCreditScoreReviewDate;
	}


	public void setCommCreditScoreReviewDate(int commCreditScoreReviewDateIn)
	{
		commCreditScoreReviewDate = commCreditScoreReviewDateIn;
	}


	public String getCommUpdateSuccess()
	{
		return commUpdateSuccess;
	}


	public void setCommUpdateSuccess(String commUpdateSuccessIn)
	{
		commUpdateSuccess = commUpdateSuccessIn;
	}


	public String getCommUpdateFailCode()
	{
		return commUpdateFailCode;
	}


	public void setCommUpdateFailCode(String commUpdateFailCodeIn)
	{
		commUpdateFailCode = commUpdateFailCodeIn;
	}


	@Override
	public String toString()
	{
		return "UpdcustJson [COMM_ADDR=" + commAddress + ", COMM_CREDIT_SCORE="
				+ commCreditScore + ", COMM_CS_REVIEW_DATE="
				+ commCreditScoreReviewDate + ", COMM_CUSTNO=" + commCustno
				+ ", COMM_DOB=" + commDateOfBirth + ", COMM_EYE=" + commEye
				+ ", COMM_NAME=" + commName + ", COMM_SCODE=" + commSortcode
				+ ", COMM_UPD_FAIL_CD=" + commUpdateFailCode
				+ ", COMM_UPD_SUCCESS=" + commUpdateSuccess + "]";
	}

}
