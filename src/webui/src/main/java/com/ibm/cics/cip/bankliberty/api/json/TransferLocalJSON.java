package com.ibm.cics.cip.bankliberty.api.json;





import javax.ws.rs.FormParam;

/**
 * This class describes the parts of the TransferLocal record in JSON format
 */

/**
 * Copyright contributors to the CICS Banking Sample Application (CBSA) project
 *
 */

public class TransferLocalJSON extends DebitCreditAccountJSON{
    static final String COPYRIGHT =
      "Copyright contributors to the CICS Banking Sample Application (CBSA) project.";


	@FormParam("targetAccount")
	Integer targetAccount;

	public Integer getTargetAccount() {
		return targetAccount;
	}

	public void setTargetAccount(Integer targetAccount) {
		this.targetAccount = targetAccount;
	}



}
