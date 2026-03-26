/*
 *
 *    Copyright IBM Corp. 2023,2026
 *
 */
package com.ibm.cics.cip.bankliberty.api.json;

import java.math.BigDecimal;

import javax.validation.constraints.NotNull;
import jakarta.ws.rs.FormParam;

/**
 * This class describes the parts of the ProcessedTransactionDebitCreditJSON
 * record, used by ProcessedTransaction, in JSON format
 */

public class ProcessedTransactionDebitCreditJSON
		extends ProcessedTransactionJSON
{

	@NotNull
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
