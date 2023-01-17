/*
 *
 *    Copyright IBM Corp. 2022
 *
 */


package com.ibm.cics.cip.bankliberty.api.json;

import java.io.IOException;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.text.DateFormat;
import java.util.logging.LogManager;
import java.util.logging.Logger;

import javax.ws.rs.Consumes;

import javax.ws.rs.GET;
import javax.ws.rs.POST;

import javax.ws.rs.Path;

import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import com.ibm.cics.cip.bankliberty.datainterfaces.PROCTRAN;
import com.ibm.cics.cip.bankliberty.web.db2.ProcessedTransaction;
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

	private static Logger logger = Logger.getLogger("com.ibm.cics.cip.bankliberty.api.json");

	private static final String JSON_NUMBER_OF_RECORDS = "numberOfProcessedTransactionRecords";
	private static final String JSON_PROCESSED_TRANSACTIONS = "processedTransactions";
	private static final String JSON_SORT_CODE = "sortCode";
	private static final String JSON_TARGET_SORT_CODE = "targetSortcode";
	private static final String JSON_TARGET_ACCOUNT = "targetAccount";
	private static final String JSON_ACCOUNT_NUMBER = "accountNumber";
	private static final String JSON_AMOUNT = "amount";
	private static final String JSON_TIMESTAMP = "timestamp";
	private static final String JSON_DESCRIPTION = "description";
	private static final String JSON_TYPE = "type";
	private static final String JSON_REFERENCE = "reference";
	private static final String JSON_ACCOUNT_TYPE = "accountType";
	private static final String JSON_LAST_STATEMENT = "lastStatement";
	private static final String JSON_NEXT_STATEMENT = "nextStatement";
	private static final String JSON_CUSTOMER_NAME = "customerName";
	private static final String JSON_DATE_OF_BIRTH = "dateOfBirth";
	private static final String JSON_ERROR_MSG = "errorMessage";
	private static final String JSON_SUCCESS = "success";
	private static final String JSON_CUSTOMER = "customer";
	private static final String LIMIT = "limit";
	private static final String OFFSET = "offset";

	public ProcessedTransactionResource()
	{
		sortOutLogging();
	}

	@GET
	@Produces(MediaType.APPLICATION_JSON)
	public Response getProcessedTransactionExternal(@QueryParam(LIMIT) Integer limit, @QueryParam(OFFSET) Integer offset) {
		Response myResponse = getProcessedTransactionInternal(limit, offset); 
		HBankDataAccess myHBankDataAccess = new HBankDataAccess();
		myHBankDataAccess.terminate();
		return myResponse;
	}


	public Response getProcessedTransactionInternal(@QueryParam(LIMIT) Integer limit, @QueryParam(OFFSET) Integer offset) {


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
			error.put(JSON_ERROR_MSG, "Proctran DB2 table not accessible. Please contact your system administrator.");
			return Response.status(500).entity(error.toString()).build();
		}

		processedTransactionsJSON = new JSONArray(processedTransactions.length);


		for(int i = 0;i < processedTransactions.length;i++)
		{
			JSONObject proctran = new JSONObject();
			proctran.put(JSON_SORT_CODE, processedTransactions[i].getSortcode());
			proctran.put(JSON_ACCOUNT_NUMBER, processedTransactions[i].getAccount_number());
			proctran.put(JSON_AMOUNT, BigDecimal.valueOf(processedTransactions[i].getAmount()).setScale(2,RoundingMode.HALF_UP));
			DateFormat myDateFormat = DateFormat.getDateInstance();
			proctran.put(JSON_TIMESTAMP, myDateFormat.format(processedTransactions[i].getTransactionDate()));
			proctran.put(JSON_DESCRIPTION, processedTransactions[i].getDescription().trim());
			proctran.put(JSON_TYPE, processedTransactions[i].getType());
			proctran.put(JSON_REFERENCE, processedTransactions[i].getReference());

			proctran = processDeleteCreateAccount(proctran,processedTransactions[i],myDateFormat);
			proctran = processDeleteCreateCustomer(proctran,processedTransactions[i],myDateFormat);
			proctran = processTransfer(proctran,processedTransactions[i],myDateFormat);

			processedTransactionsJSON.add(proctran);
		}
		numberOfProcessedTransactions = processedTransactionsJSON.size();


		processedTransactionsJSON = new JSONArray(processedTransactions.length);


		for(int i = 0;i < processedTransactions.length;i++)
		{
			JSONObject proctran = new JSONObject();
			DateFormat myDateFormat = DateFormat.getDateInstance();
			proctran.put(JSON_SORT_CODE, processedTransactions[i].getSortcode());
			proctran.put(JSON_ACCOUNT_NUMBER, processedTransactions[i].getAccount_number());
			proctran.put(JSON_AMOUNT, BigDecimal.valueOf(processedTransactions[i].getAmount()).setScale(2,RoundingMode.HALF_UP));
			proctran.put(JSON_TIMESTAMP, myDateFormat.format(processedTransactions[i].getTransactionDate()));
			proctran.put(JSON_DESCRIPTION, processedTransactions[i].getDescription().trim());
			proctran.put(JSON_TYPE, processedTransactions[i].getType());
			if(processedTransactions[i].getType().compareTo(PROCTRAN.PROC_TY_BRANCH_DELETE_ACCOUNT) == 0)
			{
				proctran.put(JSON_ACCOUNT_TYPE, processedTransactions[i].getAccountType());
				proctran.put(JSON_LAST_STATEMENT, myDateFormat.format(processedTransactions[i].getLastStatement()));
				proctran.put(JSON_NEXT_STATEMENT, myDateFormat.format(processedTransactions[i].getNextStatement()));
				proctran.put(JSON_CUSTOMER, processedTransactions[i].getCustomer());
			}
			if(processedTransactions[i].getType().compareTo(PROCTRAN.PROC_TY_WEB_DELETE_ACCOUNT) == 0)
			{
				proctran.put(JSON_ACCOUNT_TYPE, processedTransactions[i].getAccountType());
				proctran.put(JSON_LAST_STATEMENT, myDateFormat.format(processedTransactions[i].getLastStatement()));
				proctran.put(JSON_NEXT_STATEMENT, myDateFormat.format(processedTransactions[i].getNextStatement()));
				proctran.put(JSON_CUSTOMER, processedTransactions[i].getCustomer());
			}
			if(processedTransactions[i].isTransfer())
			{
				proctran.put(JSON_TARGET_ACCOUNT,processedTransactions[i].getTarget_account_number());
				proctran.put(JSON_TARGET_SORT_CODE,processedTransactions[i].getTargetSortcode());
			}
			if(processedTransactions[i].getType().compareTo(PROCTRAN.PROC_TY_BRANCH_CREATE_CUSTOMER) == 0  )
			{
				proctran.put(JSON_CUSTOMER_NAME,processedTransactions[i].getCustomerName());
				proctran.put(JSON_DATE_OF_BIRTH,myDateFormat.format(processedTransactions[i].getDateOfBirth()));
			}
			if(processedTransactions[i].getType().compareTo(PROCTRAN.PROC_TY_BRANCH_DELETE_CUSTOMER) == 0  )
			{
				proctran.put(JSON_CUSTOMER_NAME,processedTransactions[i].getCustomerName());
				proctran.put(JSON_DATE_OF_BIRTH,myDateFormat.format(processedTransactions[i].getDateOfBirth()));
			}
			if(processedTransactions[i].getType().compareTo(PROCTRAN.PROC_TY_WEB_CREATE_CUSTOMER) == 0  )
			{
				proctran.put(JSON_CUSTOMER_NAME,processedTransactions[i].getCustomerName());
				proctran.put(JSON_DATE_OF_BIRTH,myDateFormat.format(processedTransactions[i].getDateOfBirth()));
			}
			if(processedTransactions[i].getType().compareTo(PROCTRAN.PROC_TY_WEB_DELETE_CUSTOMER) == 0  )
			{
				proctran.put(JSON_CUSTOMER_NAME,processedTransactions[i].getCustomerName());
				proctran.put(JSON_DATE_OF_BIRTH,myDateFormat.format(processedTransactions[i].getDateOfBirth()));
			}

			processedTransactionsJSON.add(proctran);
			numberOfProcessedTransactions = processedTransactionsJSON.size();
		}






		/*
		 * Parse returned data and return to calling method
		 */

		response.put(JSON_NUMBER_OF_RECORDS, numberOfProcessedTransactions);
		response.put(JSON_PROCESSED_TRANSACTIONS, processedTransactionsJSON);
		response.put(JSON_SUCCESS, "Y");




		return Response.status(200)
				.entity(response.toString())
				.build();
	}




	private JSONObject processTransfer(JSONObject proctran, ProcessedTransaction processedTransaction,
			DateFormat myDateFormat) {
		// Process bank to bank transfer records
		if(processedTransaction.getType().compareTo(PROCTRAN.PROC_TY_TRANSFER) == 0)
		{
			proctran.put(JSON_TARGET_ACCOUNT,processedTransaction.getTarget_account_number());
			proctran.put(JSON_TARGET_SORT_CODE,processedTransaction.getTargetSortcode());
		}
		return proctran;
	}

	private JSONObject processDeleteCreateCustomer(JSONObject proctran, ProcessedTransaction processedTransaction,
			DateFormat myDateFormat) {
		// Deal with create account and delete customer
		if(processedTransaction.getType().compareTo(PROCTRAN.PROC_TY_BRANCH_DELETE_CUSTOMER) == 0 || processedTransaction.getType().compareTo(PROCTRAN.PROC_TY_WEB_DELETE_CUSTOMER) ==0 || (processedTransaction.getType().compareTo(PROCTRAN.PROC_TY_BRANCH_CREATE_CUSTOMER) == 0 || processedTransaction.getType().compareTo(PROCTRAN.PROC_TY_WEB_CREATE_CUSTOMER) ==0))
		{
			proctran.put(JSON_DATE_OF_BIRTH, myDateFormat.format(processedTransaction.getDateOfBirth()));
			proctran.put(JSON_CUSTOMER_NAME, processedTransaction.getCustomerName());
			proctran.put(JSON_CUSTOMER, processedTransaction.getCustomer());
		}
		return proctran;
	}

	private JSONObject processDeleteCreateAccount(JSONObject proctran, ProcessedTransaction processedTransaction, DateFormat myDateFormat) {
		// Deal with create account and delete account
		if(processedTransaction.getType().compareTo(PROCTRAN.PROC_TY_BRANCH_DELETE_ACCOUNT) == 0 || processedTransaction.getType().compareTo(PROCTRAN.PROC_TY_WEB_DELETE_ACCOUNT) ==0 || processedTransaction.getType().compareTo(PROCTRAN.PROC_TY_BRANCH_CREATE_ACCOUNT) == 0 || processedTransaction.getType().compareTo(PROCTRAN.PROC_TY_WEB_CREATE_ACCOUNT) ==0)
		{
			proctran.put(JSON_ACCOUNT_TYPE, processedTransaction.getAccountType());
			proctran.put(JSON_LAST_STATEMENT, myDateFormat.format(processedTransaction.getLastStatement()));
			proctran.put(JSON_NEXT_STATEMENT, myDateFormat.format(processedTransaction.getNextStatement()));
			proctran.put(JSON_CUSTOMER, processedTransaction.getCustomer());
		}
		return proctran;
	}

	private Integer getSortCode() {
		SortCodeResource mySortCodeResource = new SortCodeResource();
		Response mySortCodeJSON = mySortCodeResource.getSortCode();
		String mySortCode = ((String) mySortCodeJSON.getEntity()).substring(13, 19);
		return Integer.getInteger(mySortCode);
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
				return Response.ok().build();
			}
			else
			{
				logger.severe("PROCTRAN Insert debit didn't work");
				return Response.serverError().build();
			}	
		}
		else
		{
			if(myProcessedTransactionDB2.writeCredit(proctranDbCr.getAccountNumber(), proctranDbCr.getSortCode(), proctranDbCr.getAmount()))
			{
				return Response.ok().build();
			}
			else
			{
				logger.severe("PROCTRAN Insert credit didn't work");
				return Response.serverError().build();
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
			return Response.ok().build();
		}
		else
		{
			return Response.serverError().build();
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
			return Response.ok().build();
		}
		else
		{
			return Response.serverError().build();
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
			return Response.ok().build();
		}
		else
		{
			return Response.serverError().build();
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
			return Response.ok().build();
		}
		else
		{
			return Response.serverError().build();
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
			return  Response.ok().build();
		}
		else
		{
			return  Response.serverError().build();
		}	

	}

	protected void sortOutLogging()
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











