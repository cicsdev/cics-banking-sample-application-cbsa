/*
 *
 *    Copyright IBM Corp. 2022
 *
 *
 */
package com.ibm.cics.cip.bank.springboot.paymentinterface.jsonclasses.paymentinterface;

import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;

public class TransferForm
{

	static final String COPYRIGHT = "Copyright IBM Corp. 2022";

	// accno
	@NotNull
	@Size(max = 8)
	private String acctNumber;

	@NotNull
	private boolean debit = true;

	@NotNull
	private Float amount;

	@NotNull
	@Size(max = 16)
	private String organisation;


	public TransferForm()
	{

	}


	public TransferForm(@NotNull @Size(max = 8) String acctNumber,
			@NotNull Float amount, @NotNull @Size(max = 16) String organisation)
	{
		this.acctNumber = acctNumber;
		this.amount = amount;
		this.organisation = organisation;
	}


	public String getAcctNumber()
	{
		return acctNumber;
	}


	public void setAcctNumber(String acctNumber)
	{
		this.acctNumber = acctNumber;
	}


	public boolean isDebit()
	{
		return debit;
	}


	public void setDebit(boolean debit)
	{
		this.debit = debit;
	}


	public void setDebit(String type)
	{
		this.debit = type.equals("Debit");
	}


	public Float getAmount()
	{
		return amount;
	}


	public void setAmount(Float amount)
	{
		this.amount = amount;
	}


	public String getOrganisation()
	{
		return organisation;
	}


	public void setOrganisation(String organisation)
	{
		this.organisation = organisation;
	}


	@Override
	public String toString()
	{
		return "TransferForm [acctNumber=" + acctNumber + ", amount=" + amount
				+ ", organisation=" + organisation + "]";
	}
}
