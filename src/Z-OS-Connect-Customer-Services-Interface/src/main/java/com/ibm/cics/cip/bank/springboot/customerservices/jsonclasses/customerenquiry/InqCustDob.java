/*
 *
 *    Copyright IBM Corp. 2022
 *
 *
 */
package com.ibm.cics.cip.bank.springboot.customerservices.jsonclasses.customerenquiry;

import com.fasterxml.jackson.databind.annotation.JsonNaming;
import com.ibm.cics.cip.bank.springboot.customerservices.JsonPropertyNamingStrategy;

@JsonNaming(JsonPropertyNamingStrategy.class)
public class InqCustDob
{

	static final String COPYRIGHT = "Copyright IBM Corp. 2022";

	private int INQCUST_DOB_YYYY;
	private int INQCUST_DOB_DD;
	private int INQCUST_DOB_MM;

	public InqCustDob()
	{
		super();
	}

	public int getINQCUST_DOB_YYYY()
	{
		return INQCUST_DOB_YYYY;
	}

	public void setINQCUST_DOB_YYYY(int iNQCUST_DOB_YYYY)
	{
		INQCUST_DOB_YYYY = iNQCUST_DOB_YYYY;
	}

	public int getINQCUST_DOB_DD()
	{
		return INQCUST_DOB_DD;
	}

	public void setINQCUST_DOB_DD(int iNQCUST_DOB_DD)
	{
		INQCUST_DOB_DD = iNQCUST_DOB_DD;
	}

	public int getINQCUST_DOB_MM()
	{
		return INQCUST_DOB_MM;
	}

	public void setINQCUST_DOB_MM(int iNQCUST_DOB_MM)
	{
		INQCUST_DOB_MM = iNQCUST_DOB_MM;
	}

	@Override
	public String toString()
	{
		return INQCUST_DOB_DD + "/" + INQCUST_DOB_MM + "/" + INQCUST_DOB_YYYY;
	}

}
