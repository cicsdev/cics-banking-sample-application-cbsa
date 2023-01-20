/*
 *
 *    Copyright IBM Corp. 2022
 *
 *
 */
package com.ibm.cics.cip.bank.springboot.paymentinterface.jsonclasses.paymentinterface;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.annotation.JsonNaming;
import com.ibm.cics.cip.bank.springboot.paymentinterface.JsonPropertyNamingStrategy;

@JsonNaming(JsonPropertyNamingStrategy.class)
public class OriginJson
{

	static final String COPYRIGHT = "Copyright IBM Corp. 2022";

	@JsonProperty("COMM_APPLID")
	private String commApplid;
	@JsonProperty("COMM_USERID")
	private String commUserid;

	@JsonProperty("COMM_FACILITY_NAME")
	private final String commFacilityName = "        ";

	@JsonProperty("COMM_NETWRK_ID")
	private String commNetwrkId = "        ";

	@JsonProperty("COMM_FACILTYPE")
	private String commFaciltype = "0496";

	@JsonProperty("FILL_0")
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
		return "OriginJson [COMM_APPLID=" + commApplid + ", COMM_FACILITY_NAME=" + commFacilityName
				+ ", COMM_FACILTYPE=" + commFaciltype + ", COMM_NETWRK_ID=" + commNetwrkId + ", COMM_USERID="
				+ commUserid + ", FILL_0=" + fill0 + "]";
	}
}
