/*
 *
 *    Copyright IBM Corp. 2022
 *
 *
 */
package com.ibm.cics.cip.bank.springboot.customerservices.jsonclasses.listaccounts;

import java.util.List;

import com.fasterxml.jackson.databind.annotation.JsonNaming;
import com.ibm.cics.cip.bank.springboot.customerservices.JsonPropertyNamingStrategy;

@JsonNaming(JsonPropertyNamingStrategy.class)
public class InqAccczJson
{

	static final String COPYRIGHT = "Copyright IBM Corp. 2022";

	private int commFailCode;
	private int customerNumber;
	private List<AccountDetails> accountDetails;
	private String commPcbPointer;
	private String customerFound;
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
		return "InqAccczJson [ACCOUNT_DETAILS=" + accountDetails + ", COMM_FAIL_CODE=" + commFailCode
				+ ", COMM_PCB_POINTER=" + commPcbPointer + ", COMM_SUCCESS=" + commSuccess + ", CUSTOMER_FOUND="
				+ customerFound + ", CUSTOMER_NUMBER=" + customerNumber + "]";
	}
}
