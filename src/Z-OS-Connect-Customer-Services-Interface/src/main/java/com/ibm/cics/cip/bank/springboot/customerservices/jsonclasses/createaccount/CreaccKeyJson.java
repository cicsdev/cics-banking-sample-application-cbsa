/*                                                                        */
/* Copyright IBM Corp. 2023                                               */
/*                                                                        */
package com.ibm.cics.cip.bank.springboot.customerservices.jsonclasses.createaccount;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.annotation.JsonNaming;
import com.ibm.cics.cip.bank.springboot.customerservices.JsonPropertyNamingStrategy;

@JsonNaming(JsonPropertyNamingStrategy.class)
public class CreaccKeyJson
{


	@JsonProperty("CommSortcode")
	private int commSortcode = 0;

	@JsonProperty("CommNumber")
	private int commNumber = 0;


	public CreaccKeyJson()
	{
		super();
	}


	public int getCommSortcode()
	{
		return commSortcode;
	}


	public void setCommSortcode(int commSortcodeIn)
	{
		commSortcode = commSortcodeIn;
	}


	public int getCommNumber()
	{
		return commNumber;
	}


	public void setCommNumber(int commNumberIn)
	{
		commNumber = commNumberIn;
	}


	@Override
	public String toString()
	{
		return "CreaccKeyJson [COMM_NUMBER=" + commNumber + ", COMM_SORTCODE="
				+ commSortcode + "]";
	}

}
