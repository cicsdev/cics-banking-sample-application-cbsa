/*                                                                        */
/* Copyright IBM Corp. 2023                                               */
/*                                                                        */
package com.ibm.cics.cip.bank.springboot.customerservices.jsonclasses.listaccounts;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.annotation.JsonNaming;
import com.ibm.cics.cip.bank.springboot.customerservices.JsonPropertyNamingStrategy;

@JsonNaming(JsonPropertyNamingStrategy.class)
public class InqAccczJson
{



	@JsonProperty("COMM_FAIL_CODE")
	private int commFailCode;

	@JsonProperty("CUSTOMER_NUMBER")
	private int customerNumber;

	@JsonProperty("ACCOUNT_DETAILS")
	private List<AccountDetails> accountDetails;

	@JsonProperty("COMM_PCB_POINTER")
	private String commPcbPointer;

	@JsonProperty("CUSTOMER_FOUND")
	private String customerFound;

	@JsonProperty("COMM_SUCCESS")
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
