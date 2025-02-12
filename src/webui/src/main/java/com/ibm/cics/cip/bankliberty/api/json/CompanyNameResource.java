/*
 *
 *    Copyright IBM Corp. 2023
 *
 */

package com.ibm.cics.cip.bankliberty.api.json;

import java.io.IOException;
import java.util.logging.LogManager;
import java.util.logging.Logger;

import jakarta.ws.rs.GET;

import jakarta.ws.rs.Path;

import jakarta.ws.rs.Produces;
import jakarta.ws.rs.core.Response;

import com.ibm.cics.cip.bankliberty.datainterfaces.GetCompany;
import com.ibm.cics.server.AbendException;
import com.ibm.cics.server.InvalidProgramIdException;
import com.ibm.cics.server.InvalidRequestException;
import com.ibm.cics.server.InvalidSystemIdException;
import com.ibm.cics.server.LengthErrorException;
import com.ibm.cics.server.NotAuthorisedException;
import com.ibm.cics.server.Program;
import com.ibm.cics.server.RolledBackException;

import com.ibm.cics.server.TerminalException;

import com.ibm.json.java.JSONObject;

/**
 * This class is used to get the Company Name
 *
 */

@Path("/companyName")
public class CompanyNameResource
{

	static String companyNameString = null;

	private static Logger logger = Logger.getLogger(
			"com.ibm.cics.cip.bankliberty.api.json.CompanyNameResource");
	// </copyright>

	private static final String GET_COMPANY_NAME = "getCompanyName()";

	private static final String ERROR_MSG_PREFIX = "CompanyNameResource.getCompanyName() has experienced error ";

	private static final String ERROR_MSG_SUFFIX = " linking to program GETCOMPY";


	public CompanyNameResource()
	{
		sortOutLogging();
	}


	@GET
	@Produces("application/json")
	public Response getCompanyName()
	{
		logger.entering(this.getClass().getName(), GET_COMPANY_NAME);
		// We cache the company name as a static variable. If not set, we jCICS
		// LINK to a COBOL program to go get it
		if (companyNameString == null)
		{
			Program getCompy = new Program();
			getCompy.setName("GETCOMPY");

			byte[] companyNameBytes = new byte[40];

			try
			{
				getCompy.link(companyNameBytes);
				GetCompany myGetCompanyData = new GetCompany(companyNameBytes);
				CompanyNameResource.setCompanyName(
						myGetCompanyData.getCompanyName().trim());
			}
			catch (InvalidRequestException | LengthErrorException
					| InvalidSystemIdException | NotAuthorisedException
					| InvalidProgramIdException | RolledBackException
					| TerminalException e)
			{
				Response myResponse = Response.status(500).entity(
						ERROR_MSG_PREFIX + e.toString() + ERROR_MSG_SUFFIX)
						.build();
				logger.warning(
						ERROR_MSG_PREFIX + e.toString() + ERROR_MSG_SUFFIX);
				logger.exiting(this.getClass().getName(), GET_COMPANY_NAME,
						myResponse);
				return myResponse;
			}
			catch (AbendException e)
			{
				logger.severe(
						"CompanyNameResource.getCompanyName() has experienced abend "
								+ e.toString() + ERROR_MSG_SUFFIX);
				Response myResponse = Response.status(500).entity(
						ERROR_MSG_PREFIX + e.toString() + ERROR_MSG_SUFFIX)
						.build();
				logger.exiting(this.getClass().getName(), GET_COMPANY_NAME,
						myResponse);
				return myResponse;
			}

		}

		JSONObject response = new JSONObject();
		response.put("companyName", companyNameString);

		Response myResponse = Response.status(200).entity(response.toString())
				.build();
		logger.exiting(this.getClass().getName(), GET_COMPANY_NAME, myResponse);

		return myResponse;
	}


	private void sortOutLogging()
	{
		try
		{
			LogManager.getLogManager().readConfiguration();
		}
		catch (SecurityException | IOException e)
		{
			logger.severe(e.toString());
		}
	}


	private static void setCompanyName(String companyName)
	{
		companyNameString = companyName;
	}

}
