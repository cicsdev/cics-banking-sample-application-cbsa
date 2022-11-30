package com.ibm.cics.cip.bankliberty.api.json;

import java.math.BigDecimal;

import javax.validation.constraints.NotNull;
import javax.ws.rs.FormParam;

/**
 * This class describes the parts of the ProcessedTransactionDebitCreditJSON record, used by ProcessedTransaction, in JSON format
 */

/**
 * Copyright contributors to the CICS Banking Sample Application (CBSA) project
 *
 */


public class ProcessedTransactionDebitCreditJSON extends ProcessedTransactionJSON{
    static final String COPYRIGHT =
      "Copyright contributors to the CICS Banking Sample Application (CBSA) project.";
	
	@NotNull
	@FormParam("amount")
	BigDecimal amount;
	
	public BigDecimal getAmount() {
		return amount;
	}

	public void setAmount(BigDecimal amount) {
		this.amount = amount;
	}


}
