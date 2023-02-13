/*
 *
 *    Copyright IBM Corp. 2023
 *
 */

package com.ibm.cics.cip.bankliberty.api.json;

import java.util.logging.Logger;

import javax.ws.rs.DELETE;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;

import javax.ws.rs.Produces;

import javax.ws.rs.core.Response;

import com.ibm.cics.cip.bankliberty.datainterfaces.NewAccountNumber;
import com.ibm.cics.cip.bankliberty.datainterfaces.NewCustomerNumber;
import com.ibm.cics.server.InvalidProgramIdException;
import com.ibm.cics.server.InvalidRequestException;
import com.ibm.cics.server.InvalidSystemIdException;
import com.ibm.cics.server.LengthErrorException;
import com.ibm.cics.server.NotAuthorisedException;
import com.ibm.cics.server.Program;
import com.ibm.cics.server.RolledBackException;

import com.ibm.cics.server.TerminalException;

import com.ibm.json.java.JSONObject;

@Path("/counter")
public class CounterResource extends HBankDataAccess
{


	/**
	 * This class describes the methods of the CounterResource. NamedCounters
	 * are not supported in jCICS, so we jCICS LINK to COBOL programs
	 * 
	 */

	private static final String NEWACCNO = "NEWACCNO";

	private static final String NEWCUSNO = "NEWCUSNO";

	private static final String JSON_ERROR_MSG = "errorMessage";

	private static final String JSON_SUCCESS = "success";

	private static final String JSON_ACCOUNT_NUMBER = "accountNumber";

	private static final String JSON_CUSTOMER_NUMBER = "customerNumber";

	private static final String GET_ACCOUNT_COUNTER = "getAccountCounter()";

	private static final String GET_CUSTOMER_COUNTER = "getCustomerCounter()";

	private static final String INCREMENT_CUSTOMER_COUNTER = "incrementCustomerCounter()";

	private static final String INCREMENT_ACCOUNT_COUNTER = "incrementAccountCounter()";

	private static final String DECREMENT_CUSTOMER_COUNTER = "decrementCustomerCounter()";

	private static final String DECREMENT_ACCOUNT_COUNTER = "decrementAccountCounter()";

	private static final String NEW_ACCNO_PREFIX = "Account number from NEWACCNO is ";

	private static final String NEW_CUSNO_PREFIX = "Customer number from NEWCUSNO is ";

	private static Logger logger = Logger
			.getLogger("com.ibm.cics.cip.bankliberty.api.json");
	// </copyright>


	public CounterResource()
	{
		sortOutLogging();
	}


	@GET
	@Path("/account")
	@Produces("application/json")
	public Response getAccountCounter()
	{
		logger.entering(this.getClass().getName(), GET_ACCOUNT_COUNTER);
		JSONObject response = new JSONObject();
		Response myResponse = null;

		Program newaccnoProgam = new Program();
		newaccnoProgam.setName(NEWACCNO);

		NewAccountNumber myNEWACCNO = new NewAccountNumber();

		myNEWACCNO.setNewaccnoFunction("C");
		byte[] data = myNEWACCNO.getByteBuffer();
		try
		{
			newaccnoProgam.link(data);
			myNEWACCNO = new NewAccountNumber(data);
			logger.fine(NEW_ACCNO_PREFIX + myNEWACCNO.getAccountNumber());
			response.put(JSON_ACCOUNT_NUMBER, myNEWACCNO.getAccountNumber());
			response.put(JSON_SUCCESS, "Y");
			myResponse = Response.status(200).entity(response.toString())
					.build();
			logger.exiting(this.getClass().getName(), GET_ACCOUNT_COUNTER,
					myResponse);
			return myResponse;
		}
		catch (InvalidRequestException | LengthErrorException
				| InvalidSystemIdException | NotAuthorisedException
				| InvalidProgramIdException | RolledBackException
				| TerminalException e)
		{
			logger.severe(e.getLocalizedMessage());
			response.put(JSON_ERROR_MSG, e.toString());
			response.put(JSON_SUCCESS, "N");
			myResponse = Response.status(500).entity(response.toString())
					.build();
			logger.exiting(this.getClass().getName(), GET_ACCOUNT_COUNTER,
					myResponse);
			return myResponse;
		}
	}


	@GET
	@Path("/customer")
	@Produces("application/json")
	public Response getCustomerCounter()
	{
		logger.entering(this.getClass().getName(), GET_CUSTOMER_COUNTER);

		JSONObject response = new JSONObject();
		Response myResponse = null;

		Program newcusnoProgram = new Program();
		newcusnoProgram.setName(NEWCUSNO);

		NewCustomerNumber myNEWCUSNO = new NewCustomerNumber();

		myNEWCUSNO.setNewcusnoFunction("C");
		byte[] data = myNEWCUSNO.getByteBuffer();
		try
		{
			newcusnoProgram.link(data);
			myNEWCUSNO = new NewCustomerNumber(data);
			logger.fine(NEW_CUSNO_PREFIX + myNEWCUSNO.getCustomerNumber());
			response.put(JSON_CUSTOMER_NUMBER, myNEWCUSNO.getCustomerNumber());
			response.put(JSON_SUCCESS, "Y");
			myResponse = Response.status(200).entity(response.toString())
					.build();
			logger.exiting(this.getClass().getName(), GET_CUSTOMER_COUNTER,
					myResponse);
			return myResponse;
		}
		catch (InvalidRequestException | LengthErrorException
				| InvalidSystemIdException | NotAuthorisedException
				| InvalidProgramIdException | RolledBackException
				| TerminalException e)
		{
			logger.severe(e.getLocalizedMessage());
			response.put(JSON_ERROR_MSG, e.toString());
			response.put(JSON_SUCCESS, "N");
			myResponse = Response.status(500).entity(response.toString())
					.build();
			logger.exiting(this.getClass().getName(), GET_CUSTOMER_COUNTER,
					myResponse);
			return myResponse;
		}
	}


	@POST
	@Path("/customer")
	@Produces("application/json")
	public Response incrementCustomerCounter()
	{
		logger.entering(this.getClass().getName(), INCREMENT_CUSTOMER_COUNTER);
		Response myResponse = null;

		JSONObject response = new JSONObject();

		Program newcusnoProgram = new Program();
		newcusnoProgram.setName(NEWCUSNO);

		NewCustomerNumber myNEWCUSNO = new NewCustomerNumber();

		myNEWCUSNO.setNewcusnoFunction("G");
		byte[] data = myNEWCUSNO.getByteBuffer();
		try
		{
			newcusnoProgram.link(data);
			myNEWCUSNO = new NewCustomerNumber(data);

			logger.fine(NEW_CUSNO_PREFIX + myNEWCUSNO.getCustomerNumber());
			response.put(JSON_CUSTOMER_NUMBER, myNEWCUSNO.getCustomerNumber());
			response.put(JSON_SUCCESS, "Y");
			myResponse = Response.status(200).entity(response.toString())
					.build();
			logger.exiting(this.getClass().getName(),
					INCREMENT_CUSTOMER_COUNTER, myResponse);
			return myResponse;
		}
		catch (InvalidRequestException | LengthErrorException
				| InvalidSystemIdException | NotAuthorisedException
				| InvalidProgramIdException | RolledBackException
				| TerminalException e)
		{
			logger.severe(e.getLocalizedMessage());
			response.put(JSON_ERROR_MSG, e.toString());
			response.put(JSON_SUCCESS, "N");
			myResponse = Response.status(500).entity(response.toString())
					.build();
			logger.exiting(this.getClass().getName(),
					INCREMENT_CUSTOMER_COUNTER, myResponse);
			return myResponse;
		}
	}


	@DELETE
	@Path("/customer")
	@Produces("application/json")
	public Response decrementCustomerCounter()
	{
		logger.entering(this.getClass().getName(), DECREMENT_CUSTOMER_COUNTER);
		Response myResponse = null;

		JSONObject response = new JSONObject();

		Program newcusnoProgram = new Program();
		newcusnoProgram.setName(NEWCUSNO);

		NewCustomerNumber myNEWCUSNO = new NewCustomerNumber();

		myNEWCUSNO.setNewcusnoFunction("C");
		byte[] data = myNEWCUSNO.getByteBuffer();
		try
		{
			newcusnoProgram.link(data);
			myNEWCUSNO = new NewCustomerNumber(data);
			myNEWCUSNO.setCustomerNumber(myNEWCUSNO.getCustomerNumber() - 1);
			myNEWCUSNO.setNewcusnoFunction("R");
			newcusnoProgram.link(data);

			logger.fine(NEW_CUSNO_PREFIX + myNEWCUSNO.getCustomerNumber());
			response.put(JSON_CUSTOMER_NUMBER, myNEWCUSNO.getCustomerNumber());
			response.put(JSON_SUCCESS, "Y");
			myResponse = Response.status(200).entity(response.toString())
					.build();
			logger.exiting(this.getClass().getName(),
					DECREMENT_CUSTOMER_COUNTER, myResponse);
			return myResponse;
		}
		catch (InvalidRequestException | LengthErrorException
				| InvalidSystemIdException | NotAuthorisedException
				| InvalidProgramIdException | RolledBackException
				| TerminalException e)
		{
			logger.severe(e.getLocalizedMessage());
			response.put(JSON_ERROR_MSG, e.toString());
			response.put(JSON_SUCCESS, "N");
			myResponse = Response.status(500).entity(response.toString())
					.build();
			logger.exiting(this.getClass().getName(),
					DECREMENT_CUSTOMER_COUNTER, myResponse);
			return myResponse;
		}

	}


	@POST
	@Path("/account")
	@Produces("application/json")
	public Response incrementAccountCounter()
	{

		logger.entering(this.getClass().getName(), INCREMENT_ACCOUNT_COUNTER);
		Response myResponse = null;
		JSONObject response = new JSONObject();

		Program newaccnoProgram = new Program();
		newaccnoProgram.setName(NEWACCNO);

		NewAccountNumber myNEWACCNO = new NewAccountNumber();

		myNEWACCNO.setNewaccnoFunction("G");
		byte[] data = myNEWACCNO.getByteBuffer();
		try
		{
			newaccnoProgram.link(data);
			myNEWACCNO = new NewAccountNumber(data);
			logger.fine(NEW_ACCNO_PREFIX + myNEWACCNO.getAccountNumber());
			response.put(JSON_ACCOUNT_NUMBER, myNEWACCNO.getAccountNumber());
			response.put(JSON_SUCCESS, "Y");
			myResponse = Response.status(200).entity(response.toString())
					.build();
			logger.exiting(this.getClass().getName(), INCREMENT_ACCOUNT_COUNTER,
					myResponse);
			return myResponse;
		}
		catch (InvalidRequestException | LengthErrorException
				| InvalidSystemIdException | NotAuthorisedException
				| InvalidProgramIdException | RolledBackException
				| TerminalException e)
		{
			logger.severe(e.getLocalizedMessage());
			response.put(JSON_ERROR_MSG, e.toString());
			response.put(JSON_SUCCESS, "N");
			myResponse = Response.status(500).entity(response.toString())
					.build();
			logger.exiting(this.getClass().getName(), INCREMENT_ACCOUNT_COUNTER,
					myResponse);
			return myResponse;
		}
	}


	@DELETE
	@Path("/account")
	@Produces("application/json")
	public Response decrementAccountCounter()
	{
		logger.entering(this.getClass().getName(), DECREMENT_ACCOUNT_COUNTER);
		Response myResponse = null;
		JSONObject response = new JSONObject();

		Program newaccnoProgram = new Program();
		newaccnoProgram.setName(NEWACCNO);

		NewAccountNumber myNEWACCNO = new NewAccountNumber();

		myNEWACCNO.setNewaccnoFunction("C");
		byte[] data = myNEWACCNO.getByteBuffer();
		try
		{
			newaccnoProgram.link(data);
			myNEWACCNO = new NewAccountNumber(data);
			myNEWACCNO.setAccountNumber(myNEWACCNO.getAccountNumber() - 1);
			myNEWACCNO.setNewaccnoFunction("R");
			newaccnoProgram.link(data);

			logger.fine(NEW_ACCNO_PREFIX + myNEWACCNO.getAccountNumber());
			response.put(JSON_ACCOUNT_NUMBER, myNEWACCNO.getAccountNumber());
			response.put(JSON_SUCCESS, "Y");
			myResponse = Response.status(200).entity(response.toString())
					.build();
			logger.exiting(this.getClass().getName(), DECREMENT_ACCOUNT_COUNTER,
					myResponse);
			return myResponse;
		}
		catch (InvalidRequestException | LengthErrorException
				| InvalidSystemIdException | NotAuthorisedException
				| InvalidProgramIdException | RolledBackException
				| TerminalException e)
		{
			logger.severe(e.getLocalizedMessage());
			response.put(JSON_ERROR_MSG, e.toString());
			response.put(JSON_SUCCESS, "N");
			myResponse = Response.status(500).entity(response.toString())
					.build();
			logger.exiting(this.getClass().getName(), DECREMENT_ACCOUNT_COUNTER,
					myResponse);
			return myResponse;
		}

	}

}
