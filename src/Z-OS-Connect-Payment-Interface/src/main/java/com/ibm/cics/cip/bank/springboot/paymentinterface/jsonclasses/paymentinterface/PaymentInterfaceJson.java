/*
 *
 *    Copyright IBM Corp. 2022
 *
 *
 */
package com.ibm.cics.cip.bank.springboot.paymentinterface.jsonclasses.paymentinterface;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.annotation.JsonNaming;
import com.ibm.cics.cip.bank.springboot.paymentinterface.JsonPropertyNamingStrategy;

@JsonNaming(JsonPropertyNamingStrategy.class)
public class PaymentInterfaceJson
{

	static final String COPYRIGHT = "Copyright IBM Corp. 2022";

	@JsonProperty("PAYDBCR")
	private DbcrJson payDbCr;

	public PaymentInterfaceJson()
	{

	}

	public PaymentInterfaceJson(TransferForm transferForm)
	{
		payDbCr = new DbcrJson(transferForm);
	}

	public DbcrJson getPAYDBCR()
	{
		return payDbCr;
	}

	public void setPAYDBCR(DbcrJson payDbCrIn)
	{
		this.payDbCr = payDbCrIn;
	}

	@Override
	public String toString()
	{
		return "PaymentInterfaceJson [PAYDBCR=" + payDbCr.toString() + "]";
	}
}
