/*                                                                        */
/* Copyright IBM Corp. 2023                                               */
/*                                                                        */
package com.ibm.cics.cip.bank.springboot.customerservices.jsonclasses.listaccounts;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.annotation.JsonNaming;
import com.ibm.cics.cip.bank.springboot.customerservices.JsonPropertyNamingStrategy;

@JsonNaming(JsonPropertyNamingStrategy.class)
public class ListAccJson
{


	@JsonProperty("InqAccZ")
	private InqAccczJson inqAcccz;


	public InqAccczJson getInqacccz()
	{
		return inqAcccz;
	}


	public void setInqacccz(InqAccczJson inqAccczIn)
	{
		inqAcccz = inqAccczIn;
	}


	@Override
	public String toString()
	{
		return "ListAccJson [INQACCCZ=" + inqAcccz + "]";
	}
}
