/*
 *
 *    Copyright IBM Corp. 2022
 *
 *
 */
package com.ibm.cics.cip.bank.springboot.customerservices.jsonclasses.customerenquiry;

import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;

public class CustomerEnquiryForm
{

	static final String COPYRIGHT = "Copyright IBM Corp. 2022";

	@NotNull
	@Size(min = 1, max = 10)
	private String custNumber;

	public CustomerEnquiryForm()
	{

	}

	public CustomerEnquiryForm(@NotNull @Size(min = 1, max = 10) String custNumber)
	{
		this.custNumber = custNumber;
	}

	public String getCustNumber()
	{
		return custNumber;
	}

	public void setCustNumber(String custNumber)
	{
		this.custNumber = custNumber;
	}

	@Override
	public String toString()
	{
		return "TransferForm [custNumber=" + custNumber + "]";
	}
}
