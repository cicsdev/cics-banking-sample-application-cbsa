/*                                                                        */
/* Copyright IBM Corp. 2023                                               */
/*                                                                        */
package com.ibm.cics.cip.bank.springboot.customerservices.jsonclasses.customerenquiry;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.annotation.JsonNaming;
import com.ibm.cics.cip.bank.springboot.customerservices.JsonPropertyNamingStrategy;

@JsonNaming(JsonPropertyNamingStrategy.class)
public class InqCustDob
{


	@JsonProperty("INQCUST_DOB_YYYY")
	private int inqcustDobYyyy;

	@JsonProperty("INQCUST_DOB_DD")
	private int inqcustDobDd;

	@JsonProperty("INQCUST_DOB_MM")
	private int inqcustDobMm;


	public InqCustDob()
	{
		super();
	}


	public int getInqcustDobYyyy()
	{
		return inqcustDobYyyy;
	}


	public void setInqcustDobYyyy(int inqcustDobYyyyIn)
	{
		inqcustDobYyyy = inqcustDobYyyyIn;
	}


	public int getInqcustDobDd()
	{
		return inqcustDobDd;
	}


	public void setInqcustDobDd(int inqcustDobDdIn)
	{
		inqcustDobDd = inqcustDobDdIn;
	}


	public int getInqcustDobMm()
	{
		return inqcustDobMm;
	}


	public void setInqcustDobMm(int inqcustDobMmIn)
	{
		inqcustDobMm = inqcustDobMmIn;
	}


	@Override
	public String toString()
	{
		return inqcustDobDd + "/" + inqcustDobMm + "/" + inqcustDobYyyy;
	}

}
