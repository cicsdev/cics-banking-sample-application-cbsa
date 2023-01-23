/*
 *
 *    Copyright IBM Corp. 2022
 *
 *
 */
package com.ibm.cics.cip.bank.springboot.paymentinterface.controllers;

import javax.validation.Valid;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.springframework.http.MediaType;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.reactive.function.BodyInserters;
import org.springframework.web.reactive.function.client.WebClient;
import org.springframework.web.reactive.function.client.WebClientRequestException;
import org.springframework.web.reactive.function.client.WebClient.ResponseSpec;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.ibm.cics.cip.bank.springboot.paymentinterface.ConnectionInfo;
import com.ibm.cics.cip.bank.springboot.paymentinterface.jsonclasses.paymentinterface.PaymentInterfaceJson;
import com.ibm.cics.cip.bank.springboot.paymentinterface.jsonclasses.paymentinterface.TransferForm;

// The code in this file is quite repetitive, however a try/catch block would've required too much over-engineering to do
// Ideally I'd only need to send off one class and I'd only get either an account or customer object back to deserialise,
// but all of the objects returned have slightly different formats and/or fields.

@Controller
public class WebController implements WebMvcConfigurer
{

	static final String COPYRIGHT = "Copyright IBM Corp. 2022";

	private static final Logger log = LoggerFactory
			.getLogger(WebController.class);

	private static final String FORM_NAME = "paymentInterfaceForm";

	private static final String LARGE_TEXT = "largeText";

	private static final String SMALL_TEXT = "smallText";

	private static final String PAYMENT_ERROR = "Payment Error";


	// Payment Interface
	@GetMapping("/")
	public String showForm(TransferForm personForm)
	{
		return FORM_NAME;
	}


	@PostMapping("/paydbcr")
	public String checkPersonInfo(@Valid TransferForm transferForm,
			BindingResult bindingResult, Model model)
			throws JsonProcessingException
	{

		// The same page is returned, this time with the errors given as an
		// object.
		if (bindingResult.hasErrors())
		{
			return FORM_NAME;
		}

		PaymentInterfaceJson transferjson = new PaymentInterfaceJson(
				transferForm);

		// Serialise the object to JSON
		log.info("{}", transferjson);
		String jsonString = new ObjectMapper().writeValueAsString(transferjson);
		log.info(jsonString);

		// The port is set elsewhere as it changes frequently
		WebClient client = WebClient.create(
				ConnectionInfo.getAddressAndPort() + "/makepayment/dbcr");

		try
		{
			// Create a response object - body of json, accept json back, and
			// insert the
			// request body created a couple lines up
			ResponseSpec response = client.put()
					.header("content-type", "application/json")
					.accept(MediaType.APPLICATION_JSON)
					.body(BodyInserters.fromValue(jsonString)).retrieve();
			String responseBody = response.bodyToMono(String.class).block();
			log.info(responseBody);

			// Deserialise into a POJO
			PaymentInterfaceJson responseObj = new ObjectMapper()
					.readValue(responseBody, PaymentInterfaceJson.class);
			log.info("{}", responseObj);

			// Throws out different exceptions depending on the contents
			checkIfResponseValidDbcr(responseObj);

			// If successful...

			if (transferForm.isDebit())
			{
				model.addAttribute(LARGE_TEXT, "Payment Successful");
			}
			else
			{
				model.addAttribute(LARGE_TEXT, "Credit Successful");
			}
			model.addAttribute(SMALL_TEXT,
					("Value: " + responseObj.getPAYDBCR().getCommAmt()));

			// Otherwise...
		}
		catch (InsufficientFundsException | InvalidAccountTypeException e)
		{
			log.info(e.toString());
			model.addAttribute(LARGE_TEXT, PAYMENT_ERROR);
			model.addAttribute(SMALL_TEXT, e.getMessage());
		}
		catch (WebClientRequestException e)
		{
			log.info(e.toString());
			model.addAttribute(LARGE_TEXT, PAYMENT_ERROR);
			model.addAttribute(SMALL_TEXT,
					"Connection refused or failed to resolve; Are you using the right address and port? Is the server running?");
		}
		catch (Exception e)
		{
			log.info(e.toString());
			model.addAttribute(LARGE_TEXT, PAYMENT_ERROR);
			model.addAttribute(SMALL_TEXT,
					"There was an error processing the request; Please try again later or check logs for more info.");
		}

		// The HTML template includes a clause to show the box for the results
		// if this is set to true(the page is otherwise the same)
		model.addAttribute("results", true);

		return FORM_NAME;
	}


	public static void checkIfResponseValidDbcr(PaymentInterfaceJson response)
			throws InsufficientFundsException, InvalidAccountTypeException
	{
		switch (Integer.parseInt(response.getPAYDBCR().getCommFailCode()))
		{
		case 3:
			throw new InsufficientFundsException();
		case 4:
			throw new InvalidAccountTypeException();
		default:
			break;
		}
	}
}

class InsufficientFundsException extends Exception
{

	static final String COPYRIGHT = "Copyright IBM Corp. 2022";

	/**
	 *
	 */
	private static final long serialVersionUID = 1L;


	public InsufficientFundsException()
	{
		super("Payment rejected: Insufficient funds.");
	}
}

class InvalidAccountTypeException extends Exception
{

	static final String COPYRIGHT = "Copyright IBM Corp. 2022";

	/**
	 *
	 */
	private static final long serialVersionUID = 1L;


	public InvalidAccountTypeException()
	{
		super("Payment rejected: Invalid account type.");
	}
}

class TooManyAccountsException extends Exception
{

	static final String COPYRIGHT = "Copyright IBM Corp. 2022";

	/**
	 *
	 */
	private static final long serialVersionUID = 1L;


	public TooManyAccountsException(int customerNumber)
	{
		super("Too many accounts for customer number " + customerNumber
				+ "; Try deleting an account first.");
	}
}

class ItemNotFoundException extends Exception
{

	static final String COPYRIGHT = "Copyright IBM Corp. 2022";

	/**
	 *
	 */
	private static final long serialVersionUID = 1L;


	public ItemNotFoundException(String item)
	{
		super("The " + item
				+ " you searched for could not be found; Try a different "
				+ item + " number.");
	}
}