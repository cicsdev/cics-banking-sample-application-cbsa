/*
 *
 *    Copyright IBM Corp. 2022
 *
 */
package com.ibm.cics.cip.bankliberty.api.json;

import javax.ws.rs.FormParam;

/**
 * This class describes the parts of the TransferLocal record in JSON format
 */

public class TransferLocalJSON extends DebitCreditAccountJSON
{

	static final String COPYRIGHT = "Copyright IBM Corp. 2022";

	@FormParam("targetAccount")
	Integer targetAccount;


	public Integer getTargetAccount()
	{
		return targetAccount;
	}


	public void setTargetAccount(Integer targetAccount)
	{
		this.targetAccount = targetAccount;
	}

}
