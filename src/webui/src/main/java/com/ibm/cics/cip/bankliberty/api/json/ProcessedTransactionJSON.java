/*
 *
 *    Copyright IBM Corp. 2023,2026
 *
 */
package com.ibm.cics.cip.bankliberty.api.json;

import javax.validation.constraints.NotNull;
import jakarta.ws.rs.FormParam;

/**
 * This class describes the parts of the ProcessedTransactionJSON record, used
 * by ProcessedTransaction, in JSON format
 */

public class ProcessedTransactionJSON
{

	@NotNull
	@FormParam("sortCode")
	String sortCode;

	@NotNull
	@FormParam("accountNumber")
	String accountNumber;


	public String getSortCode()
	{
		return sortCode;
	}


	public void setSortCode(String sortCode)
	{
		this.sortCode = sortCode;
	}


	public String getAccountNumber()
	{
		return accountNumber;
	}


	public void setAccountNumber(String accountNumber)
	{
		this.accountNumber = accountNumber;
	}

}
