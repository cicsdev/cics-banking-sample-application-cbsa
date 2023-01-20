/*
 *
 *    Copyright IBM Corp. 2022
 *
 *
 */
package com.ibm.cics.cip.bank.springboot.customerservices.jsonclasses.updatecustomer;

import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;

public class UpdateCustomerForm
{

	static final String COPYRIGHT = "Copyright IBM Corp. 2022";

	@NotNull
	@Size(max = 10)
	String custNumber;

	@Size(max = 65)
	String custName = "";

	@Size(max = 165)
	String custAddress = "";

	String custDoB = "";
	int custCreditScore = 0;
	String custReviewDate = "";


	public String getCustNumber()
	{
		return custNumber;
	}

	public void setCustNumber(String custNumber)
	{
		this.custNumber = custNumber;
	}

	public String getCustName()
	{
		return custName;
	}

	public void setCustName(String custName)
	{
		this.custName = custName;
	}

	public String getCustAddress()
	{
		return custAddress;
	}

	public void setCustAddress(String custAddress)
	{
		this.custAddress = custAddress;
	}

	public String getCustDoB()
	{
		return custDoB;
	}

	public void setCustDoB(String custDoB)
	{
		if (custDoB.equals(""))
		{
			return;
		}
		this.custDoB = "";
		this.custDoB += custDoB.substring(8, 10) + custDoB.substring(5, 7) + custDoB.substring(0, 4);
	}

	public int getCustCreditScore()
	{
		return custCreditScore;
	}

	public void setCustCreditScore(int custCreditScore)
	{
		this.custCreditScore = custCreditScore;
	}

	public String getCustReviewDate()
	{
		return custReviewDate;
	}

	public void setCustReviewDate(String custReviewDate)
	{
		this.custReviewDate = custReviewDate;
	}

	@Override
	public String toString()
	{
		return "UpdateCustomerForm [custAddress=" + custAddress + ", custCreditScore=" + custCreditScore + ", custDoB="
				+ custDoB + ", custName=" + custName + ", custNumber=" + custNumber + ", custReviewDate="
				+ custReviewDate + "]";
	}

}
