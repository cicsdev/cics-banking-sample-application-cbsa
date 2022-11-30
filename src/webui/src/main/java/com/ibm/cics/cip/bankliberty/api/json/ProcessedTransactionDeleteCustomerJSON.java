package com.ibm.cics.cip.bankliberty.api.json;

import java.sql.Date;

import javax.validation.constraints.NotNull;
import javax.ws.rs.FormParam;

/**
 * This class describes the parts of the ProcessedTransactionDeleteCustomerJSON record, used by ProcessedTransaction, in JSON format
 */

/**
 * Copyright contributors to the CICS Banking Sample Application (CBSA) project
 *
 */


public class ProcessedTransactionDeleteCustomerJSON extends ProcessedTransactionJSON{
    static final String COPYRIGHT =
      "Copyright contributors to the CICS Banking Sample Application (CBSA) project.";
	
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
	@FormParam("customerCreditScoreReviewDate")
	Date customerCreditScoreReviewDate;

	public String getCustomerName() {
		return customerName;
	}

	public void setCustomerName(String customerName) {
		this.customerName = customerName;
	}

	public String getCustomerNumber() {
		return customerNumber;
	}

	public void setCustomerNumber(String customerNumber) {
		this.customerNumber = customerNumber;
	}

	public Date getCustomerDOB() {
		return customerDOB;
	}

	public void setCustomerDOB(Date customerDOB) {
		this.customerDOB = customerDOB;
	}

	public String getCreditScore() {
		return customerCreditScore;
	}

	public void setCreditScore(String customerCreditScore) {
		this.customerCreditScore = customerCreditScore;
	}
	
	public Date getReviewDate() {
		return customerCreditScoreReviewDate;
	}

	public void setReviewDate(Date customerCreditScoreReviewDate) {
		this.customerCreditScoreReviewDate = customerCreditScoreReviewDate;
	}

}
