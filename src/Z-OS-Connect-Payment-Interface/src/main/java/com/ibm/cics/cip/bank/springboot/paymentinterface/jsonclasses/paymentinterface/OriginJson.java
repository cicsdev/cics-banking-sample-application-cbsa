/*                                                                        */
/* Copyright IBM Corp. 2023                                               */
/*                                                                        */
package com.ibm.cics.cip.bank.springboot.paymentinterface.jsonclasses.paymentinterface;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.annotation.JsonNaming;
import com.ibm.cics.cip.bank.springboot.paymentinterface.JsonPropertyNamingStrategy;

@JsonNaming(JsonPropertyNamingStrategy.class)
public class OriginJson
{


	@JsonProperty("CommApplid")
	private String commApplid;

	@JsonProperty("CommUserid")
	private String commUserid;

	@JsonProperty("CommFacilityName")
	private String commFacilityName = "        ";

	@JsonProperty("CommNetwrkId")
	private String commNetwrkId = "        ";

	@JsonProperty("CommFaciltype")
	private String commFaciltype = "0496";

	@JsonProperty("Fill0")
	private String fill0 = "    ";


	public OriginJson()
	{

	}


	public OriginJson(String organisation)
	{
		// APPLID and USERID are both used to contain the organisation data, so
		// it's split into two 8 char strings
		String paddedOrg = String.format("%-16s", organisation);
		commApplid = paddedOrg.substring(0, 8);
		commUserid = paddedOrg.substring(8);
	}


	public void setOrganisation(String organisation)
	{
		String paddedOrg = String.format("%-16s", organisation);
		commApplid = paddedOrg.substring(0, 8);
		commUserid = paddedOrg.substring(8);
	}


	public String getCommApplid()
	{
		return commApplid;
	}


	public void setCommApplid(String commApplidIn)
	{
		commApplid = commApplidIn;
	}


	public String getCommUserid()
	{
		return commUserid;
	}


	public void setCommUserid(String commUseridIn)
	{
		commUserid = commUseridIn;
	}


	public String getCommFacilityName()
	{
		return commFacilityName;
	}


	public String getCommNetwrkId()
	{
		return commNetwrkId;
	}


	public void setCommNetwrkId(String commNetwrkIdIn)
	{
		commNetwrkId = commNetwrkIdIn;
	}


	public String getCommFacilType()
	{
		return commFaciltype;
	}


	public void setCommFacilType(String commFacilType)
	{
		commFaciltype = commFacilType;
	}


	public String getFill0()
	{
		return fill0;
	}


	public void setFill0(String fill0In)
	{
		fill0 = fill0In;
	}


	@Override
	public String toString()
	{
		return "OriginJson [CommApplid=" + commApplid + ", CommFacilityName="
				+ commFacilityName + ", CommFaciltype=" + commFaciltype
				+ ", CommNetwrkId=" + commNetwrkId + ", CommUserid="
				+ commUserid + ", Fill0=" + fill0 + "]";
	}
}
