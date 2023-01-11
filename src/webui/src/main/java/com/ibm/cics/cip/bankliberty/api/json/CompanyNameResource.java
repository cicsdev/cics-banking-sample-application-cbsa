/*
 *
 *    Copyright IBM Corp. 2022
 *
 */


package com.ibm.cics.cip.bankliberty.api.json;

import java.io.IOException;
import java.util.logging.LogManager;
import java.util.logging.Logger;

import javax.ws.rs.GET;

import javax.ws.rs.Path;

import javax.ws.rs.Produces;
import javax.ws.rs.core.Response;

import com.ibm.cics.cip.bankliberty.dataInterfaces.GetCompany;
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
public class CompanyNameResource{

    static final String COPYRIGHT =
      "Copyright IBM Corp. 2022";

	static String companyNameString = null;
	

	private static Logger logger = Logger.getLogger("com.ibm.cics.cip.bankliberty.api.json.CompanyNameResource");
	// </copyright>
	
	private static final String getCompanyName = "getCompanyName()";
	private static final String errorMsgPrefix = "CompanyNameResource.getCompanyName() has experienced error ";
	private static final String errorMsgSuffix = " linking to program GETCOMPY";


	public CompanyNameResource()
	{
		sortOutLogging();
	}
	@GET
	@Produces("application/json")
	public Response getCompanyName() {
		logger.entering(this.getClass().getName(), getCompanyName);
// We cache the company name as a static variable. If not set, we jCICS LINK to a COBOL program to go get it
		if(companyNameString == null)
		{
			Program GETCOMPY = new Program();
			GETCOMPY.setName("GETCOMPY");

			byte[] companyNameBytes = new byte[40];



			try {
				GETCOMPY.link(companyNameBytes);
				GetCompany myGetCompanyData = new GetCompany(companyNameBytes);
				CompanyNameResource.setCompanyName(myGetCompanyData.getCompanyName().trim());
			} catch (InvalidRequestException | LengthErrorException
					| InvalidSystemIdException | NotAuthorisedException
					| InvalidProgramIdException | RolledBackException
					| TerminalException e) {
				Response myResponse = Response.status(500)
						.entity(errorMsgPrefix + e.toString() + errorMsgSuffix)
						.build();
				logger.warning(errorMsgPrefix + e.toString() + errorMsgSuffix);
				logger.exiting(this.getClass().getName(),  getCompanyName,myResponse);
				return myResponse;
			}
			catch (AbendException e) {
				logger.severe("CompanyNameResource.getCompanyName() has experienced abend " + e.toString() + errorMsgSuffix);
				Response myResponse = Response.status(500).entity(errorMsgPrefix + e.toString() + errorMsgSuffix).build();
				logger.exiting(this.getClass().getName(),  getCompanyName,myResponse);
				return myResponse;
			}

		}

		JSONObject response = new JSONObject();
		response.put("companyName", companyNameString);
		
		Response myResponse = Response.status(200).entity(response.toString()).build();
		logger.exiting(this.getClass().getName(), getCompanyName,myResponse);

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
