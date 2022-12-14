/*
 *
 *    Copyright IBM Corp. 2022
 *
 *
 */
package com.ibm.cics.cip.bank.springboot.paymentinterface.controllers;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.ibm.cics.cip.bank.springboot.paymentinterface.ConnectionInfo;
import com.ibm.cics.cip.bank.springboot.paymentinterface.PaymentInterface;
import com.ibm.cics.cip.bank.springboot.paymentinterface.jsonclasses.paymentinterface.PaymentInterfaceJson;
import com.ibm.cics.cip.bank.springboot.paymentinterface.jsonclasses.paymentinterface.TransferForm;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.reactive.function.BodyInserters;
import org.springframework.web.reactive.function.client.WebClient;
import org.springframework.web.reactive.function.client.WebClient.ResponseSpec;

@RestController
public class ParamsController {

    static final String COPYRIGHT =
      "Copyright IBM Corp. 2022";

    private static final Logger log = LoggerFactory.getLogger(PaymentInterface.class);

    // This follows a very similar format to the form submitting equivalents in WebController.java
    // Instead of a form object, parameters required in the url
    @PostMapping("/submit")
	public PaymentInterfaceJson submit(@RequestParam(name = "acctnum", required = true) String acctNumber, @RequestParam(name = "amount", required = true) float amount, @RequestParam(name = "organisation", required = true) String organisation) throws JsonProcessingException {
        log.info("AcctNumber: " + acctNumber + " Amount: " + amount + " Organisation: " + organisation);
        TransferForm transferForm = new TransferForm(acctNumber, amount, organisation);

        PaymentInterfaceJson transferJson = new PaymentInterfaceJson(transferForm);

        String jsonString = new ObjectMapper().writeValueAsString(transferJson);
        log.info(jsonString);

        WebClient client = WebClient.create(ConnectionInfo.getAddressAndPort() + "/makepayment/dbcr");
        PaymentInterfaceJson responseObj;

        try {
            ResponseSpec response = client.put().header("content-type", "application/json").accept(MediaType.APPLICATION_JSON).body(BodyInserters.fromValue(jsonString)).retrieve();
            String responseBody = response.bodyToMono(String.class).block();
            log.info(responseBody);
            responseObj = new ObjectMapper().readValue(responseBody, PaymentInterfaceJson.class);
            log.info(responseObj.toString());
            return responseObj;
        } catch (Exception e) {
            log.info(e.toString());
        }

        return null;
	}
}
