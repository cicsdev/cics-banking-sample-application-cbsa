/*
 *
 *    Copyright IBM Corp. 2023,2026
 *
 */
package com.ibm.cics.cip.bankliberty.api.json;

import java.math.BigDecimal;

import jakarta.ws.rs.FormParam;

/**
 * This class describes the parts of the DebitCredit record, used by
 * TransferLocal, in JSON format
 */

public class DebitCreditAccountJSON
{


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
