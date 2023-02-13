/*
 *
 *    Copyright IBM Corp. 2023
 *
 */
package com.ibm.cics.cip.bankliberty.api.json;

import javax.ws.rs.FormParam;

/**
 * This class describes the parts of the TransferLocal record in JSON format
 */

public class TransferLocalJSON extends DebitCreditAccountJSON
{

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
