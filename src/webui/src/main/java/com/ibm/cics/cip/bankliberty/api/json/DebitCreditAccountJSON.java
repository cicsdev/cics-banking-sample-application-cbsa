package com.ibm.cics.cip.bankliberty.api.json;

import java.math.BigDecimal;



import javax.ws.rs.FormParam;

/**
 * This class describes the parts of the DebitCredit record, used by TransferLocal, in JSON format
 */

/**
 * Copyright contributors to the CICS Banking Sample Application (CBSA) project
 *
 */



public class DebitCreditAccountJSON {
    static final String COPYRIGHT =
      "Copyright contributors to the CICS Banking Sample Application (CBSA) project.";


	@FormParam("amount")
	BigDecimal amount;

	public BigDecimal getAmount() {
		return amount;
	}

	public void setAmount(BigDecimal amount) {
		this.amount = amount;
	}

	








}
