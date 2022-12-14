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
public class OriginJson {

    static final String COPYRIGHT =
      "Copyright IBM Corp. 2022";


    @JsonProperty("COMM_APPLID")
    private String COMM_APPLID;
    @JsonProperty("COMM_USERID")
    private String COMM_USERID;

    @JsonProperty("COMM_FACILITY_NAME")
    private final String COMM_FACILITY_NAME = "        ";

    @JsonProperty("COMM_NETWRK_ID")
    private String COMM_NETWRK_ID = "        ";

    @JsonProperty("COMM_FACILTYPE")
    private String COMM_FACILTYPE = "0496";

    @JsonProperty("FILL_0")
    private String FILL_0 = "    ";

    public OriginJson() {

    }

    public OriginJson(String organisation) {
        // APPLID and USERID are both used to contain the organisation data, so it's split into two 8 char strings
        String paddedOrg = String.format("%-16s", organisation);
        COMM_APPLID = paddedOrg.substring(0, 8);
        COMM_USERID = paddedOrg.substring(8);
    }

    public void setOrganisation(String organisation) {
        String paddedOrg = String.format("%-16s", organisation);
        COMM_APPLID = paddedOrg.substring(0, 8);
        COMM_USERID = paddedOrg.substring(8);
    }

    public String getCOMM_APPLID() {
        return COMM_APPLID;
    }

    public void setCOMM_APPLID(String cOMM_APPLID) {
        COMM_APPLID = cOMM_APPLID;
    }

    public String getCOMM_USERID() {
        return COMM_USERID;
    }

    public void setCOMM_USERID(String cOMM_USERID) {
        COMM_USERID = cOMM_USERID;
    }

    public String getCOMM_FACILITY_NAME() {
        return COMM_FACILITY_NAME;
    }

    public String getCOMM_NETWRK_ID() {
        return COMM_NETWRK_ID;
    }

    public void setCOMM_NETWRK_ID(String cOMM_NETWRK_ID) {
        COMM_NETWRK_ID = cOMM_NETWRK_ID;
    }

    public String getCOMM_FACILTYPE() {
        return COMM_FACILTYPE;
    }

    public void setCOMM_FACILTYPE(String cOMM_FACILTYPE) {
        COMM_FACILTYPE = cOMM_FACILTYPE;
    }

    public String getFILL_0() {
        return FILL_0;
    }

    public void setFILL_0(String fILL_0) {
        FILL_0 = fILL_0;
    }

    @Override
    public String toString() {
        return "OriginJson [COMM_APPLID=" + COMM_APPLID + ", COMM_FACILITY_NAME=" + COMM_FACILITY_NAME
                + ", COMM_FACILTYPE=" + COMM_FACILTYPE + ", COMM_NETWRK_ID=" + COMM_NETWRK_ID + ", COMM_USERID="
                + COMM_USERID + ", FILL_0=" + FILL_0 + "]";
    }
}
