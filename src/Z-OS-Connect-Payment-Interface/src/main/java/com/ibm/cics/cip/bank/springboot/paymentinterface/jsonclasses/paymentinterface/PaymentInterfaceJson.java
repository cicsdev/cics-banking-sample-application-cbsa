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
public class PaymentInterfaceJson {

    static final String COPYRIGHT =
      "Copyright IBM Corp. 2022";


    @JsonProperty("PAYDBCR")
    private DbcrJson PAYDBCR;

    public PaymentInterfaceJson() {

    }

    public PaymentInterfaceJson(TransferForm transferForm) {
        PAYDBCR = new DbcrJson(transferForm);
    }

    public DbcrJson getPAYDBCR() {
        return PAYDBCR;
    }

    public void setPAYDBCR(DbcrJson PAYDBCR) {
        this.PAYDBCR = PAYDBCR;
    }

    @Override
    public String toString() {
        return "PaymentInterfaceJson [PAYDBCR=" + PAYDBCR.toString() + "]";
    }
}
