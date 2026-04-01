/*                                                                        */
/* Copyright IBM Corp. 2023                                               */
/*                                                                        */
package com.ibm.cics.cip.bank.springboot.customerservices.jsonclasses.accountenquiry;

import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;

public class AccountEnquiryForm
{



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
