/*
 *
 *    Copyright IBM Corp. 2022
 *
 *
 */
package com.ibm.cics.cip.bank.springboot.customerservices.jsonclasses.accountenquiry;

import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;

public class AccountEnquiryForm
{

	static final String COPYRIGHT = "Copyright IBM Corp. 2022";

	@NotNull
	@Size(max = 8)
	private String acctNumber;

	public AccountEnquiryForm()
	{

	}

	public AccountEnquiryForm(@NotNull @Size(max = 8) String acctNumber)
	{
		this.acctNumber = acctNumber;
	}

	public String getAcctNumber()
	{
		return acctNumber;
	}

	public void setAcctNumber(String acctNumber)
	{
		this.acctNumber = acctNumber;
	}

	@Override
	public String toString()
	{
		return "TransferForm [acctNumber=" + acctNumber + "]";
	}
}
