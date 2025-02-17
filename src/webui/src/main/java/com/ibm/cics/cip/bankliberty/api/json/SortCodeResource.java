/*
 *
 *    Copyright IBM Corp. 2023
 *
 */
/**
 * This class describes the methods of the SortCode Resource
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

import com.ibm.cics.cip.bankliberty.datainterfaces.GetSortCode;
import com.ibm.cics.server.InvalidProgramIdException;
import com.ibm.cics.server.InvalidRequestException;
import com.ibm.cics.server.InvalidSystemIdException;
import com.ibm.cics.server.LengthErrorException;
import com.ibm.cics.server.NotAuthorisedException;
import com.ibm.cics.server.Program;
import com.ibm.cics.server.RolledBackException;

import com.ibm.cics.server.TerminalException;

import com.ibm.json.java.JSONObject;

@Path("/sortCode")
public class SortCodeResource
{


	private static Logger logger = Logger
			.getLogger("com.ibm.cics.cip.bankliberty.api.json");


	public SortCodeResource()
	{
		sortOutLogging();
	}

	static String sortCodeString = null;


	@GET
	@Produces("application/json")
	public Response getSortCode()
	{

		if (sortCodeString == null)
		{
			Program getscode = new Program();
			getscode.setName("GETSCODE");

			byte[] sortCodeBytes = new byte[6];

			try
			{
				getscode.link(sortCodeBytes);
			}
			catch (InvalidRequestException | LengthErrorException
					| InvalidSystemIdException | NotAuthorisedException
					| InvalidProgramIdException | RolledBackException
					| TerminalException e)
			{
				logger.severe(e.toString());
			}

			GetSortCode myGetSortCodeData = new GetSortCode(sortCodeBytes);

			SortCodeResource.setSortCode(myGetSortCodeData.getSortcode());
		}

		JSONObject response = new JSONObject();
		response.put("sortCode", sortCodeString);

		return Response.status(200).entity(response.toString()).build();
	}


	private static void setSortCode(String sortcode)
	{
		sortCodeString = sortcode;
	}


	private static void sortOutLogging()
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
}
