/*
 *
 *    Copyright IBM Corp. 2022
 *
 */


package com.ibm.cics.cip.bankliberty.api.json;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.text.DateFormat;

import javax.ws.rs.Consumes;

import javax.ws.rs.GET;
import javax.ws.rs.POST;

import javax.ws.rs.Path;

import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import com.ibm.json.java.JSONArray;
import com.ibm.json.java.JSONObject;

/**
 * This class describes the methods of the ProcessedTransaction Resource
 * 
 */



@Path("/processedTransaction")
public class ProcessedTransactionResource{

    static final String COPYRIGHT =
      "Copyright IBM Corp. 2022";


	@GET
	@Produces(MediaType.APPLICATION_JSON)
	public Response getProcessedTransactionExternal(@QueryParam("limit") Integer limit, @QueryParam("offset") Integer offset) {
		Response myResponse = getProcessedTransactionInternal(limit, offset); 
		HBankDataAccess myHBankDataAccess = new HBankDataAccess();
		myHBankDataAccess.terminate();
		return myResponse;
	}


	public Response getProcessedTransactionInternal(@QueryParam("limit") Integer limit, @QueryParam("offset") Integer offset) {


		if(offset == null)
		{
			offset = 0;
		}
		if(limit == null)
		{
			limit = 250000;
		}
		JSONObject response = new JSONObject();
		JSONArray processedTransactionsJSON = null;
		int numberOfProcessedTransactions = 0;
		Integer sortCode = this.getSortCode();

		com.ibm.cics.cip.bankliberty.web.db2.ProcessedTransaction myProcessedTransaction = new com.ibm.cics.cip.bankliberty.web.db2.ProcessedTransaction();
		com.ibm.cics.cip.bankliberty.web.db2.ProcessedTransaction[] processedTransactions = null;
		processedTransactions= myProcessedTransaction.getProcessedTransactions(sortCode.intValue(),limit,offset);
		if(processedTransactions == null)
		{
			JSONObject error = new JSONObject();
			error.put("errorMessage", "Proctran DB2 table not accessible. Please contact your system administrator.");
			return Response.status(500).entity(error.toString()).build();
		}
		numberOfProcessedTransactions = processedTransactions.length;
		processedTransactionsJSON = new JSONArray(processedTransactions.length);


		for(int i = 0;i < processedTransactions.length;i++)
		{
			JSONObject proctran = new JSONObject();
			proctran.put("sortCode", processedTransactions[i].getSortcode());
			proctran.put("accountNumber", processedTransactions[i].getAccount_number());
			proctran.put("amount", BigDecimal.valueOf(processedTransactions[i].getAmount()).setScale(2,RoundingMode.HALF_UP));
			DateFormat myDateFormat = DateFormat.getDateInstance();
			proctran.put("timestamp", myDateFormat.format(processedTransactions[i].getTransactionDate()));
			proctran.put("description", processedTransactions[i].getDescription().trim());
			proctran.put("type", processedTransactions[i].getType());
			proctran.put("reference", processedTransactions[i].getReference());
			if(processedTransactions[i].getType().compareTo(com.ibm.cics.cip.bankliberty.web.db2.ProcessedTransaction.BRANCH_DELETE_ACCOUNT) == 0 || processedTransactions[i].getType().compareTo(com.ibm.cics.cip.bankliberty.web.db2.ProcessedTransaction.WEB_DELETE_ACCOUNT) ==0)
			{
				proctran.put("accountType", processedTransactions[i].getAccount_type());
				proctran.put("lastStatement", myDateFormat.format(processedTransactions[i].getLast_statement()));
				proctran.put("nextStatement", myDateFormat.format(processedTransactions[i].getNext_statement()));
				proctran.put("customer", processedTransactions[i].getCustomer());
			}


			if(processedTransactions[i].getType().compareTo(com.ibm.cics.cip.bankliberty.web.db2.ProcessedTransaction.BRANCH_DELETE_CUSTOMER) == 0 || processedTransactions[i].getType().compareTo(com.ibm.cics.cip.bankliberty.web.db2.ProcessedTransaction.WEB_DELETE_CUSTOMER) ==0)
			{
				proctran.put("dateOfBirth", myDateFormat.format(processedTransactions[i].getDateOfBirth()));
				proctran.put("customerName", processedTransactions[i].getCustomerName());
				proctran.put("customer", processedTransactions[i].getCustomer());
			}

			if(processedTransactions[i].getType().compareTo(com.ibm.cics.cip.bankliberty.web.db2.ProcessedTransaction.BRANCH_CREATE_CUSTOMER) == 0 || processedTransactions[i].getType().compareTo(com.ibm.cics.cip.bankliberty.web.db2.ProcessedTransaction.WEB_CREATE_CUSTOMER) ==0)
			{	
				proctran.put("dateOfBirth", myDateFormat.format(processedTransactions[i].getDateOfBirth()));
				proctran.put("customerName", processedTransactions[i].getCustomerName());
				proctran.put("customer", processedTransactions[i].getCustomer());
			}

			if(processedTransactions[i].getType().compareTo(com.ibm.cics.cip.bankliberty.web.db2.ProcessedTransaction.TRANSFER) == 0)
			{
				proctran.put("targetAccount",processedTransactions[i].getTarget_account_number());
				proctran.put("targetSortcode",processedTransactions[i].getTarget_sortcode());
			}
			if(processedTransactions[i].getType().compareTo(com.ibm.cics.cip.bankliberty.web.db2.ProcessedTransaction.BRANCH_CREATE_ACCOUNT) == 0 || processedTransactions[i].getType().compareTo(com.ibm.cics.cip.bankliberty.web.db2.ProcessedTransaction.WEB_CREATE_ACCOUNT) ==0)
			{
				proctran.put("accountType", processedTransactions[i].getAccount_type());
				proctran.put("lastStatement", myDateFormat.format(processedTransactions[i].getLast_statement()));
				proctran.put("nextStatement", myDateFormat.format(processedTransactions[i].getNext_statement()));
				proctran.put("customer", processedTransactions[i].getCustomer());
			}

			processedTransactionsJSON.add(proctran);
			numberOfProcessedTransactions = processedTransactionsJSON.size();
		}

		numberOfProcessedTransactions = processedTransactions.length;
		processedTransactionsJSON = new JSONArray(processedTransactions.length);


		for(int i = 0;i < processedTransactions.length;i++)
		{
			JSONObject proctran = new JSONObject();
			DateFormat myDateFormat = DateFormat.getDateInstance();
			proctran.put("sortCode", processedTransactions[i].getSortcode());
			proctran.put("accountNumber", processedTransactions[i].getAccount_number());
			proctran.put("amount", BigDecimal.valueOf(processedTransactions[i].getAmount()).setScale(2,RoundingMode.HALF_UP));
			proctran.put("timestamp", myDateFormat.format(processedTransactions[i].getTransactionDate()));
			proctran.put("description", processedTransactions[i].getDescription().trim());
			proctran.put("type", processedTransactions[i].getType());
			if(processedTransactions[i].getType().compareTo(com.ibm.cics.cip.bankliberty.web.db2.ProcessedTransaction.BRANCH_DELETE_ACCOUNT) == 0)
			{
				proctran.put("accountType", processedTransactions[i].getAccount_type());
				proctran.put("lastStatement", myDateFormat.format(processedTransactions[i].getLast_statement()));
				proctran.put("nextStatement", myDateFormat.format(processedTransactions[i].getNext_statement()));
				proctran.put("customer", processedTransactions[i].getCustomer());
			}
			if(processedTransactions[i].getType().compareTo(com.ibm.cics.cip.bankliberty.web.db2.ProcessedTransaction.WEB_DELETE_ACCOUNT) == 0)
			{
				proctran.put("accountType", processedTransactions[i].getAccount_type());
				proctran.put("lastStatement", myDateFormat.format(processedTransactions[i].getLast_statement()));
				proctran.put("nextStatement", myDateFormat.format(processedTransactions[i].getNext_statement()));
				proctran.put("customer", processedTransactions[i].getCustomer());
			}
			if(processedTransactions[i].isTransfer())
			{
				proctran.put("targetAccount",processedTransactions[i].getTarget_account_number());
				proctran.put("targetSortcode",processedTransactions[i].getTarget_sortcode());
			}
if(processedTransactions[i].getType().compareTo(com.ibm.cics.cip.bankliberty.web.db2.ProcessedTransaction.BRANCH_CREATE_CUSTOMER) == 0  )
{
	proctran.put("name",processedTransactions[i].getCustomerName());
	proctran.put("D.O.B.",myDateFormat.format(processedTransactions[i].getDateOfBirth()));
}
if(processedTransactions[i].getType().compareTo(com.ibm.cics.cip.bankliberty.web.db2.ProcessedTransaction.BRANCH_DELETE_CUSTOMER) == 0  )
{
	proctran.put("name",processedTransactions[i].getCustomerName());
	proctran.put("D.O.B.",myDateFormat.format(processedTransactions[i].getDateOfBirth()));
}
if(processedTransactions[i].getType().compareTo(com.ibm.cics.cip.bankliberty.web.db2.ProcessedTransaction.WEB_CREATE_CUSTOMER) == 0  )
{
	proctran.put("name",processedTransactions[i].getCustomerName());
	proctran.put("D.O.B.",myDateFormat.format(processedTransactions[i].getDateOfBirth()));
}
if(processedTransactions[i].getType().compareTo(com.ibm.cics.cip.bankliberty.web.db2.ProcessedTransaction.WEB_DELETE_CUSTOMER) == 0  )
{
	proctran.put("name",processedTransactions[i].getCustomerName());
	proctran.put("D.O.B.",myDateFormat.format(processedTransactions[i].getDateOfBirth()));
}

			processedTransactionsJSON.add(proctran);
			numberOfProcessedTransactions = processedTransactionsJSON.size();
		}






		/*
		 * Parse returned data and return to calling method
		 */

		response.put("numberOfProcessedTransactionRecords", numberOfProcessedTransactions);
		response.put("processedTransactions", processedTransactionsJSON);
		response.put("success", "Y");




		return Response.status(200)
				.entity(response.toString())
				.build();
	}




	private Integer getSortCode() {
		SortCodeResource mySortCodeResource = new SortCodeResource();
		Response mySortCodeJSON = mySortCodeResource.getSortCode();
		String mySortCode = ((String) mySortCodeJSON.getEntity()).substring(13, 19);
		return new Integer(mySortCode);
	}


	@POST
	@Produces("application/json")
	@Path("/debitCreditAccount")
	public Response writeExternal(ProcessedTransactionDebitCreditJSON proctranDbCr) 
	{
		Response myResponse = writeInternal(proctranDbCr); 
		HBankDataAccess myHBankDataAccess = new HBankDataAccess();
		myHBankDataAccess.terminate();
		return myResponse;
	}



	public Response writeInternal(ProcessedTransactionDebitCreditJSON proctranDbCr) 
	{
		com.ibm.cics.cip.bankliberty.web.db2.ProcessedTransaction myProcessedTransactionDB2 = new com.ibm.cics.cip.bankliberty.web.db2.ProcessedTransaction();

		if(proctranDbCr.getAmount().compareTo(new BigDecimal(0)) < 0)
		{
			if(myProcessedTransactionDB2.writeDebit(proctranDbCr.getAccountNumber(), proctranDbCr.getSortCode(), proctranDbCr.getAmount()))
			{
				Response myResponse = Response.ok().build();
				return myResponse;
			}
			else
			{
				System.err.println("PROCTRAN Insert debit didn't work");
				Response myResponse = Response.serverError().build();
				return myResponse;
			}	
		}
		else
		{
			if(myProcessedTransactionDB2.writeCredit(proctranDbCr.getAccountNumber(), proctranDbCr.getSortCode(), proctranDbCr.getAmount()))
			{
				Response myResponse = Response.ok().build();
				return myResponse;
			}
			else
			{
				System.err.println("PROCTRAN Insert credit didn't work");
				Response myResponse = Response.serverError().build();
				return myResponse;
			}	
		}
	}

	@POST
	@Produces("application/json")
	@Consumes(MediaType.APPLICATION_JSON)
	@Path("/transferLocal")
	public Response writeTransferLocalExternal(ProcessedTransactionTransferLocalJSON proctranLocal) {
		Response myResponse = writeTransferLocalInternal(proctranLocal); 
		HBankDataAccess myHBankDataAccess = new HBankDataAccess();
		myHBankDataAccess.terminate();
		return myResponse;
	}


	public Response writeTransferLocalInternal(ProcessedTransactionTransferLocalJSON proctranLocal) {
		com.ibm.cics.cip.bankliberty.web.db2.ProcessedTransaction myProcessedTransactionDB2 = new com.ibm.cics.cip.bankliberty.web.db2.ProcessedTransaction();


		if(myProcessedTransactionDB2.writeTransferLocal(proctranLocal.getSortCode(), proctranLocal.getAccountNumber(), proctranLocal.getAmount(), proctranLocal.getTargetAccountNumber()))
		{
			Response myResponse = Response.ok().build();
			return myResponse;
		}
		else
		{
			Response myResponse = Response.serverError().build();
			return myResponse;
		}	
	}


	@POST
	@Produces("application/json")
	@Consumes(MediaType.APPLICATION_JSON)
	@Path("/deleteCustomer")
	public Response writeDeleteCustomerExternal(ProcessedTransactionDeleteCustomerJSON myDeletedCustomer) {
		Response myResponse = writeDeleteCustomerInternal(myDeletedCustomer); 
		HBankDataAccess myHBankDataAccess = new HBankDataAccess();
		myHBankDataAccess.terminate();
		return myResponse;
	}



	public Response writeDeleteCustomerInternal(ProcessedTransactionDeleteCustomerJSON myDeletedCustomer) {
		com.ibm.cics.cip.bankliberty.web.db2.ProcessedTransaction myProcessedTransactionDB2 = new com.ibm.cics.cip.bankliberty.web.db2.ProcessedTransaction();


		if(myProcessedTransactionDB2.writeDeleteCustomer(myDeletedCustomer.getSortCode(), myDeletedCustomer.getAccountNumber(), 0.00, myDeletedCustomer.getCustomerDOB(), myDeletedCustomer.getCustomerName(), myDeletedCustomer.getCustomerNumber()))
		{
			Response myResponse = Response.ok().build();
			return myResponse;
		}
		else
		{
			Response myResponse = Response.serverError().build();
			return myResponse;
		}	


	}


	@POST
	@Produces("application/json")
	@Consumes(MediaType.APPLICATION_JSON)
	@Path("/createCustomer")
	public Response writeCreateCustomerExternal(ProcessedTransactionCreateCustomerJSON myCreatedCustomer) {
		Response myResponse = writeCreateCustomerInternal(myCreatedCustomer); 
		HBankDataAccess myHBankDataAccess = new HBankDataAccess();
		myHBankDataAccess.terminate();
		return myResponse;
	}



	public Response writeCreateCustomerInternal(ProcessedTransactionCreateCustomerJSON myCreatedCustomer) {
		com.ibm.cics.cip.bankliberty.web.db2.ProcessedTransaction myProcessedTransactionDB2 = new com.ibm.cics.cip.bankliberty.web.db2.ProcessedTransaction();


		if(myProcessedTransactionDB2.writeCreateCustomer(myCreatedCustomer.getSortCode(), myCreatedCustomer.getAccountNumber(), 0.00, myCreatedCustomer.getCustomerDOB(), myCreatedCustomer.getCustomerName(), myCreatedCustomer.getCustomerNumber()))
		{
			Response myResponse = Response.ok().build();
			return myResponse;
		}
		else
		{
			Response myResponse = Response.serverError().build();
			return myResponse;
		}	

	}

	@POST
	@Produces("application/json")
	@Consumes(MediaType.APPLICATION_JSON)
	@Path("/deleteAccount")
	public Response writeDeleteAccountExternal(ProcessedTransactionAccountJSON myDeletedAccount) {
		Response myResponse = writeDeleteAccountInternal(myDeletedAccount); 
		HBankDataAccess myHBankDataAccess = new HBankDataAccess();
		myHBankDataAccess.terminate();
		return myResponse;
	}

	public Response writeDeleteAccountInternal(ProcessedTransactionAccountJSON myDeletedAccount) {
		com.ibm.cics.cip.bankliberty.web.db2.ProcessedTransaction myProcessedTransactionDB2 = new com.ibm.cics.cip.bankliberty.web.db2.ProcessedTransaction();
		if(myProcessedTransactionDB2.writeDeleteAccount(myDeletedAccount.getSortCode(), myDeletedAccount.getAccountNumber(), myDeletedAccount.getActualBalance(), myDeletedAccount.getLastStatement(), myDeletedAccount.getNextStatement(), myDeletedAccount.getCustomerNumber(),myDeletedAccount.getType()))
		{
			Response myResponse = Response.ok().build();
			return myResponse;
		}
		else
		{
			Response myResponse = Response.serverError().build();
			return myResponse;
		}	

	}

	@POST
	@Produces("application/json")
	@Consumes(MediaType.APPLICATION_JSON)
	@Path("/createAccount")

	public Response writeCreateAccountExternal(ProcessedTransactionAccountJSON myCreatedAccount) {
		Response myResponse = writeCreateAccountInternal(myCreatedAccount); 
		HBankDataAccess myHBankDataAccess = new HBankDataAccess();
		myHBankDataAccess.terminate();
		return myResponse;
	}



	public Response writeCreateAccountInternal(ProcessedTransactionAccountJSON myCreatedAccount) {
		com.ibm.cics.cip.bankliberty.web.db2.ProcessedTransaction myProcessedTransactionDB2 = new com.ibm.cics.cip.bankliberty.web.db2.ProcessedTransaction();
		if(myProcessedTransactionDB2.writeCreateAccount(myCreatedAccount.getSortCode(), myCreatedAccount.getAccountNumber(), myCreatedAccount.getActualBalance(), myCreatedAccount.getLastStatement(), myCreatedAccount.getNextStatement(), myCreatedAccount.getCustomerNumber(),myCreatedAccount.getType()))
		{
			Response myResponse = Response.ok().build();
			return myResponse;
		}
		else
		{
			Response myResponse = Response.serverError().build();
			return myResponse;
		}	

	}
}











