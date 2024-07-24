/*                                                                        */
/* Copyright IBM Corp. 2023                                               */
/*                                                                        */
package com.ibm.cics.cip.bank.springboot.customerservices.jsonclasses.customerenquiry;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.annotation.JsonNaming;
import com.ibm.cics.cip.bank.springboot.customerservices.JsonPropertyNamingStrategy;

@JsonNaming(JsonPropertyNamingStrategy.class)
public class InqCustReviewDate
{



	@JsonProperty("INQCUST_CS_REVIEW_YYYY")
	private int inqcustCsReviewYyyy;

	@JsonProperty("INQCUST_CS_REVIEW_MM")
	private int inqcustCsReviewMm;

	@JsonProperty("INQCUST_CS_REVIEW_DD")
	private int inqcustCsReviewDd;


	public InqCustReviewDate()
	{
		super();
	}


	public int getInqcustCsReviewYyyy()
	{
		return inqcustCsReviewYyyy;
	}


	public void setInqcustCsReviewYyyy(int inqcustCsReviewYyyyIn)
	{
		inqcustCsReviewYyyy = inqcustCsReviewYyyyIn;
	}


	public int getInqcustCsReviewMm()
	{
		return inqcustCsReviewMm;
	}


	public void setInqcustCsReviewMm(int inqcustCsReviewMmIn)
	{
		inqcustCsReviewMm = inqcustCsReviewMmIn;
	}


	public int getInqcustCsReviewDd()
	{
		return inqcustCsReviewDd;
	}


	public void setInqcustCsReviewDd(int inqcustCsReviewDdIn)
	{
		inqcustCsReviewDd = inqcustCsReviewDdIn;
	}


	@Override
	public String toString()
	{
		return inqcustCsReviewDd + "/" + inqcustCsReviewMm + "/"
				+ inqcustCsReviewYyyy;
	}

}
