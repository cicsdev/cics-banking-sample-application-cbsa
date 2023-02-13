/*
 *
 *    Copyright IBM Corp. 2023
 *
 */
package com.ibm.cics.cip.bankliberty.api.json;

import java.sql.Date;

import javax.validation.constraints.NotNull;
import javax.ws.rs.FormParam;

/**
 * This class describes the parts of the ProcessedTransactionCreateCustomerJSON
 * record, used by ProcessedTransaction, in JSON format
 */

public class ProcessedTransactionCreateCustomerJSON
		extends ProcessedTransactionJSON
{

	@NotNull
	@FormParam("customerName")
	String customerName;

	@NotNull
	@FormParam("customerNumber")
	String customerNumber;

	@NotNull
	@FormParam("customerDOB")
	Date customerDOB;

	@NotNull
	@FormParam("customerCreditScore")
	String customerCreditScore;

	@NotNull
	@FormParam("customerReviewDate")
	Date customerReviewDate;


	public String getCustomerName()
	{
		return customerName;
	}


	public void setCustomerName(String customerName)
	{
		this.customerName = customerName;
	}


	public String getCustomerNumber()
	{
		return customerNumber;
	}


	public void setCustomerNumber(String customerNumber)
	{
		this.customerNumber = customerNumber;
	}


	public Date getCustomerDOB()
	{
		return customerDOB;
	}


	public void setCustomerDOB(Date customerDOB)
	{
		this.customerDOB = customerDOB;
	}


	public String getCreditScore()
	{
		return customerCreditScore;
	}


	public void setCreditScore(String customerCreditScore)
	{
		this.customerCreditScore = customerCreditScore;
	}


	public Date getReviewDate()
	{
		return customerDOB;
	}


	public void setReviewDate(Date customerReviewDate)
	{
		this.customerReviewDate = customerReviewDate;
	}
}
