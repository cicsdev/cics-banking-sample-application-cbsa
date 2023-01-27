/*
 *
 *    Copyright IBM Corp. 2022
 *
 *
 */
package com.ibm.cics.cip.bank.springboot.customerservices.jsonclasses.listaccounts;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.annotation.JsonNaming;
import com.ibm.cics.cip.bank.springboot.customerservices.JsonPropertyNamingStrategy;

@JsonNaming(JsonPropertyNamingStrategy.class)
public class InqAccczJson
{

	static final String COPYRIGHT = "Copyright IBM Corp. 2022";

	@JsonProperty("CommFailCode")
	private int commFailCode;

	@JsonProperty("CustomerNumber")
	private int customerNumber;

	@JsonProperty("AccountDetails")
	private List<AccountDetails> accountDetails;

	@JsonProperty("CommPcbPointer")
	private String commPcbPointer;

	@JsonProperty("CustomerFound")
	private String customerFound;

	@JsonProperty("CommSuccess")
	private String commSuccess;

	
	public int getCommFailCode()
	{
		return commFailCode;
	}


	public void setCommFailCode(int commFailCodeIn)
	{
		commFailCode = commFailCodeIn;
	}


	public int getCustomerNumber()
	{
		return customerNumber;
	}


	public void setCustomerNumber(int customerNumberIn)
	{
		customerNumber = customerNumberIn;
	}


	public List<AccountDetails> getAccountDetails()
	{
		return accountDetails;
	}


	public void setAccountDetails(List<AccountDetails> accountDetailsIn)
	{
		accountDetails = accountDetailsIn;
	}


	public String getCommPcbPointer()
	{
		return commPcbPointer;
	}


	public void setCommPcbPointer(String commPcbPointerIn)
	{
		commPcbPointer = commPcbPointerIn;
	}


	public String getCustomerFound()
	{
		return customerFound;
	}


	public void setCustomerFound(String customerFoundIn)
	{
		customerFound = customerFoundIn;
	}


	public String getCommSuccess()
	{
		return commSuccess;
	}


	public void setCommSuccess(String commSuccessIn)
	{
		commSuccess = commSuccessIn;
	}


	@Override
	public String toString()
	{
		return "InqAccczJson [AccountDetails=" + accountDetails
				+ ", CommFailCode=" + commFailCode + ", CommPcbPointer="
				+ commPcbPointer + ", CommSuccess=" + commSuccess
				+ ", CustomerFound=" + customerFound + ", CustomerNumber="
				+ customerNumber + "]";
	}
}
