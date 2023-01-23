/*
 *
 *    Copyright IBM Corp. 2022
 *
 */
package com.ibm.cics.cip.bankliberty.api.json;

import java.sql.Date;

import javax.validation.constraints.NotNull;
import javax.ws.rs.FormParam;

/**
 * This class describes the parts of the Customer record in JSON format
 */

public class CustomerJSON
{

	static final String COPYRIGHT = "Copyright IBM Corp. 2022";

	@NotNull
	@FormParam("customerAddress")
	String customerAddress;

	@NotNull
	@FormParam("customerName")
	String customerName;

	@NotNull
	@FormParam("dateOfBirth")
	Date dateOfBirth;

	@NotNull
	@FormParam("sortCode")
	String sortCode;

	@NotNull
	@FormParam("customerCreditScore")
	String customerCreditScore;


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
		return customerCreditScoreReviewDate;
	}


	public void setReviewDate(Date customerCreditScoreReviewDate)
	{
		this.customerCreditScoreReviewDate = customerCreditScoreReviewDate;
	}

	@NotNull
	@FormParam("customerCreditScoreReviewDate")
	Date customerCreditScoreReviewDate;

	String id;


	public String getId()
	{
		return id;
	}


	public void setId(String id)
	{
		this.id = id;
	}


	public String getSortCode()
	{
		return sortCode;
	}


	public void setSortCode(String sortCode)
	{
		this.sortCode = sortCode;
	}


	public String getCustomerName()
	{
		return customerName;
	}


	public void setCustomerName(String customerName)
	{
		this.customerName = customerName;
	}


	public String getCustomerAddress()
	{
		return customerAddress;
	}


	public void setCustomerAddress(String customerAddress)
	{
		this.customerAddress = customerAddress;
	}


	public Date getDateOfBirth()
	{
		return dateOfBirth;
	}


	public void setDateOfBirth(Date dateOfBirth)
	{
		this.dateOfBirth = dateOfBirth;
	}


	// Customer title must be one of the following
	public boolean validateTitle(String title)
	{
		if (title.equalsIgnoreCase("Professor"))
		{
			return true;
		}
		if (title.equalsIgnoreCase("Mr"))
		{
			return true;
		}
		if (title.equalsIgnoreCase("Mrs"))
		{
			return true;
		}
		if (title.equalsIgnoreCase("Miss"))
		{
			return true;
		}
		if (title.equalsIgnoreCase("Ms"))
		{
			return true;
		}
		if (title.equalsIgnoreCase("Dr"))
		{
			return true;
		}
		if (title.equalsIgnoreCase("Drs"))
		{
			return true;
		}
		if (title.equalsIgnoreCase("Lord"))
		{
			return true;
		}
		if (title.equalsIgnoreCase("Sir"))
		{
			return true;
		}
		return (title.equalsIgnoreCase("Lady"));
	}

}
