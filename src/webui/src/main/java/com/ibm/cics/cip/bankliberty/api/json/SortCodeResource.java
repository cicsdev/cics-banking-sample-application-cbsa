/*
 *
 *    Copyright IBM Corp. 2022
 *
 */
/**
 * This class describes the methods of the SortCode Resource
 * 
 */


package com.ibm.cics.cip.bankliberty.api.json;

import javax.ws.rs.GET;

import javax.ws.rs.Path;

import javax.ws.rs.Produces;
import javax.ws.rs.core.Response;


import com.ibm.cics.cip.bankliberty.dataInterfaces.GetSortCode;





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
public class SortCodeResource{

    static final String COPYRIGHT =
      "Copyright IBM Corp. 2022";


	static String sortCodeString = null;

	@GET
	@Produces("application/json")
	public Response getSortCode() {

		if(sortCodeString == null)
		{
			Program GETSCODE = new Program();
			GETSCODE.setName("GETSCODE");

			byte[] sortCodeBytes = new byte[6];



			try {
				GETSCODE.link(sortCodeBytes);
			} catch (InvalidRequestException | LengthErrorException
					| InvalidSystemIdException | NotAuthorisedException
					| InvalidProgramIdException | RolledBackException
					| TerminalException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}

			GetSortCode myGetSortCodeData = new GetSortCode(sortCodeBytes);


			sortCodeString = myGetSortCodeData.getSortcode();
		}

		JSONObject response = new JSONObject();
		response.put("sortCode", sortCodeString);

		return Response.status(200)
				.entity(response.toString())
				.build();
	}

}
