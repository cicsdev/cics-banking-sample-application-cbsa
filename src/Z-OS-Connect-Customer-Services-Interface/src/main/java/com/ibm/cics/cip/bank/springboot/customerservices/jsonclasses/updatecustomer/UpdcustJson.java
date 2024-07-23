/*                                                                        */
/* Copyright IBM Corp. 2023                                               */
/*                                                                        */
package com.ibm.cics.cip.bank.springboot.customerservices.jsonclasses.updatecustomer;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.annotation.JsonNaming;
import com.ibm.cics.cip.bank.springboot.customerservices.JsonPropertyNamingStrategy;

@JsonNaming(JsonPropertyNamingStrategy.class)
public class UpdcustJson
{



	@JsonProperty("COMM_EYE")
	private String commEye = "    ";

	@JsonProperty("COMM_SCODE")
	private String commSortcode = "";

	@JsonProperty("COMM_CUSTNO")
	private String commCustno = " ";

	@JsonProperty("COMM_NAME")
	private String commName = " ";

	@JsonProperty("COMM_ADDR")
	private String commAddress = " ";

	@JsonProperty("COMM_DOB")
	private int commDateOfBirth = 0;

	@JsonProperty("COMM_CREDIT_SCORE")
	private int commCreditScore = 0;

	@JsonProperty("COMM_CS_REVIEW_DATE")
	private int commCreditScoreReviewDate = 0;

	@JsonProperty("COMM_UPD_SUCCESS")
	private String commUpdateSuccess = " ";

	@JsonProperty("COMM_UPD_FAIL_CD")
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
		return "UpdcustJson [CommAddress=" + commAddress + ", CommCreditScore="
				+ commCreditScore + ", CommCsReviewDate="
				+ commCreditScoreReviewDate + ", CommCustno=" + commCustno
				+ ", CommDob=" + commDateOfBirth + ", CommEye=" + commEye
				+ ", CommName=" + commName + ", CommScode=" + commSortcode
				+ ", CommUpdFailCd=" + commUpdateFailCode
				+ ", CommmUpdSuccess=" + commUpdateSuccess + "]";
	}

}
