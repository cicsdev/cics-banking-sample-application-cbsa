/*
 *
 *    Copyright IBM Corp. 2022
 *
 */
package com.ibm.cics.cip.bankliberty.api.json;

import java.math.BigDecimal;

import javax.ws.rs.FormParam;

/**
 * This class describes the parts of the DebitCredit record, used by
 * TransferLocal, in JSON format
 */

public class DebitCreditAccountJSON
{

	static final String COPYRIGHT = "Copyright IBM Corp. 2022";

	@FormParam("amount")
	BigDecimal amount;


	public BigDecimal getAmount()
	{
		return amount;
	}


	public void setAmount(BigDecimal amount)
	{
		this.amount = amount;
	}

}
