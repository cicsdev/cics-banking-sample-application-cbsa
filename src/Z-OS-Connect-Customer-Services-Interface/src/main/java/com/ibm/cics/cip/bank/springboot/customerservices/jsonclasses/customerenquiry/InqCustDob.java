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

	private int inqcustDobYyyy;
	private int inqcustDobDd;
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
