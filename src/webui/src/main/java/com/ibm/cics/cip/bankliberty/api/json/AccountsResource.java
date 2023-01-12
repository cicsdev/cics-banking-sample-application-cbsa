/*
 *
 *    Copyright IBM Corp. 2022
 *
 */


package com.ibm.cics.cip.bankliberty.api.json;

import java.io.IOException;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.logging.LogManager;
import java.util.logging.Logger;

import javax.ws.rs.Consumes;
import javax.ws.rs.DELETE;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import com.ibm.cics.cip.bankliberty.web.db2.Account;
import com.ibm.cics.server.InvalidRequestException;
import com.ibm.cics.server.Task;
import com.ibm.json.java.JSONArray;
import com.ibm.json.java.JSONObject;

/**
 * This class describes the methods of the AccountsResource
 * 
 */

@Path("/account")
public class AccountsResource extends HBankDataAccess{

    static final String COPYRIGHT =
      "Copyright IBM Corp. 2022";

	private static Logger logger = Logger.getLogger("com.ibm.cics.cip.bankliberty.api.json");
	
	private static final String NOT_SUPPORTED = " is not supported.";
	private static final String ERROR_MSG = "errorMessage";
	private static final String ACC_TYPE_STRING = "Account type ";
	private static final String CREATE_ACCOUNT_INTERNAL = "createAccountInternal(AccountJSON account)";
	private static final String CREATE_ACCOUNT_EXTERNAL = "createAccountExternal(AccountJSON account)";
	private static final String GET_ACCOUNT_INTERNAL = "getAccountInternal(Long accountNumber)";
	private static final String GET_ACCOUNTS_BY_CUSTOMER_INTERNAL = "getAccountsByCustomerInternal(Long customerNumber, boolean countOnly)";
	private static final String UPDATE_ACCOUNT_INTERNAL = "updateAccountInternal(Long id, AccountJSON account)";
	private static final String DEBIT_ACCOUNT_INTERNAL = "debitAccountInternal(String accountNumber, DebitCreditAccountJSON dbcr)";
	private static final String CREDIT_ACCOUNT_INTERNAL = "creditAccountInternal(String accountNumber, DebitCreditAccountJSON dbcr)";
	private static final String TRANSFER_LOCAL_INTERNAL = "transferLocalInternal(String accountNumber, TransferLocalJSON transferLocal)";
	private static final String DEBIT_CREDIT_ACCOUNT = "debitCreditAccount(Long sortCode, String accountNumber, BigDecimal apiAmount, boolean debitAccount)";
	private static final String DELETE_ACCOUNT = "deleteAccountInternal(Long accountNumber)";
	private static final String GET_ACCOUNTS_BY_BALANCE_WITH_OFFSET_AND_LIMIT_INTERNAL = "getAccountsByBalanceWithOffsetAndLimitInternal(BigDecimal balance, String operator, Integer offset, Integer limit, Boolean countOnly";
	
	private static final String CLASS_NAME_MSG = " in com.ibm.cics.cip.bankliberty.web.db2.Account";
	
	private static final String INTEREST_RATE_LESS_THAN_ZERO = "Interest rate cannot be greater than 9999.99%.";
	private static final String INTEREST_RATE_TOO_HIGH = "Interest rate cannot be greater than 9999.99%.";
	private static final String GET_ACCOUNTS_EXTERNAL = "getAccountsExternal(Boolean countOnly)";
	private static final String GET_ACCOUNTS_INTERNAL = "getAccountsInternal(Boolean countOnly)";
	private static final String NOT_VALID_FOR_THIS_BANK =  "not valid for this bank (";
	private static final String SORT_CODE_LITERAL = "Sortcode ";
	private static final String ACCOUNT_LITERAL = "Account ";
	private static final String CANNOT_BE_FOUND = " cannot be found.";
	private static final String CANNOT_BE_ACCESSED = " cannot be accessed.";
	private static final String CUSTOMER_NUMBER_LITERAL = "Customer number ";
	private static final String FAILED_TO_READ = "Failed to read account ";
	private static final String PROCTRAN_WRITE_FAILURE = "Failed to write to PROCTRAN data store";
	private static final String DB2_READ_FAILURE = "Unable to access Db2 account store";
	private static final String ACCOUNT_CREATE_FAILURE = "Failed to create account in com.ibm.cics.cip.bankliberty.web.db2.Account";
	private static final String IN_DEBIT_ACCOUNT = " in debitAccount ";
	private static final String IN_CREDIT_ACCOUNT = " in creditAccount ";
	private static final String NEED_DIFFERENT_ACCOUNTS = "Source and target accounts must be different";
	private static final String SOURCE_ACCOUNT_NUMBER = "Source account number ";
	private static final String TARGET_ACCOUNT_NUMBER = "Target account number ";
	
	private static final String JSON_NUMBER_OF_ACCOUNTS = "numberOfAccounts";
	private static final String JSON_SORT_CODE = "sortCode";
	private static final String JSON_CUSTOMER_NUMBER = "customerNumber";
	private static final String JSON_ACCOUNT_TYPE = "accountType";
	private static final String JSON_AVAILABLE_BALANCE = "availableBalance";
	private static final String JSON_ACTUAL_BALANCE = "actualBalance";
	private static final String JSON_INTEREST_RATE = "interestRate";
	private static final String JSON_OVERDRAFT = "overdraft";
	private static final String JSON_LAST_STATEMENT_DATE = "lastStatementDate";
	private static final String JSON_NEXT_STATEMENT_DATE = "nextStatementDate";
	private static final String JSON_DATE_OPENED = "dateOpened";
	private static final String JSON_ACCOUNTS = "accounts";
	



	public AccountsResource()
	{
		/** 
		 * Constructor
		 */
		sortOutLogging();
	}

	@POST
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)

	public Response createAccountExternal(
			AccountJSON account) {
		/** This method is called from OUTSIDE Liberty. We need to know in order to keep track of DB2 connections */
		logger.entering(this.getClass().getName(),CREATE_ACCOUNT_EXTERNAL + " for account " + account.toString());

		Response myResponse = createAccountInternal(account);

		HBankDataAccess myHBankDataAccess = new HBankDataAccess();
		myHBankDataAccess.terminate();
		logger.exiting(this.getClass().getName(),CREATE_ACCOUNT_EXTERNAL,myResponse);
		return myResponse;

	}

	public Response createAccountInternal(
			/** Internal methods can be called by either the external methods, or another part of Liberty */
			AccountJSON account) {
		logger.entering(this.getClass().getName(),CREATE_ACCOUNT_INTERNAL + " for account " + account.toString());
		Response myResponse = null;
		/* Only ISA, LOAN, CURRENT, MORTGAGE and SAVING are valid account types */
		if(!account.validateType(account.getAccountType().trim()))
		{
			JSONObject error = new JSONObject();
			error.put(ERROR_MSG, ACC_TYPE_STRING + account.getAccountType() + NOT_SUPPORTED);
			logger.warning(ACC_TYPE_STRING + account.getAccountType() + NOT_SUPPORTED);
			myResponse = Response.status(400).entity(error.toString()).build();
			logger.exiting(this.getClass().getName(),CREATE_ACCOUNT_INTERNAL ,myResponse);
			return myResponse;

		}
		//Interest rate cannot be < 0
		if(account.getInterestRate().doubleValue() < 0.00)
		{
			JSONObject error = new JSONObject();
			error.put(ERROR_MSG, INTEREST_RATE_LESS_THAN_ZERO);
			logger.warning(INTEREST_RATE_LESS_THAN_ZERO);
			myResponse = Response.status(400).entity(error.toString()).build();
			logger.exiting(this.getClass().getName(),CREATE_ACCOUNT_INTERNAL ,myResponse);
			return myResponse;
		}

		//Interest rate cannot be > 9999.99%
		if(account.getInterestRate().doubleValue() > 9999.99)
		{
			JSONObject error = new JSONObject();
			error.put(ERROR_MSG, INTEREST_RATE_TOO_HIGH);
			logger.warning(INTEREST_RATE_TOO_HIGH);
			myResponse = Response.status(400).entity(error.toString()).build();
			logger.exiting(this.getClass().getName(),CREATE_ACCOUNT_INTERNAL ,myResponse);
			return myResponse;
		}

		//Interest rate cannot have more than 2dp
		BigDecimal myInterestRate = account.getInterestRate();
		if(myInterestRate.scale() > 2)
		{
			JSONObject error = new JSONObject();
			error.put(ERROR_MSG, "Interest rate cannot have more than 2 decimal places. " + myInterestRate.toPlainString());
			logger.warning("Interest rate cannot have more than 2 decimal places."  + myInterestRate.toPlainString());
			myResponse = Response.status(400).entity(error.toString()).build();
			logger.exiting(this.getClass().getName(),CREATE_ACCOUNT_INTERNAL ,myResponse);
			return myResponse;			
		}

		//Overdraft limit cannot be < 0
		if(account.getOverdraft().intValue() < 0)
		{
			JSONObject error = new JSONObject();
			error.put(ERROR_MSG, "Overdraft limit cannot be less than zero.");
			logger.warning("Overdraft limit cannot be less than zero.");
			myResponse = Response.status(400).entity(error.toString()).build();
			logger.exiting(this.getClass().getName(),CREATE_ACCOUNT_INTERNAL ,myResponse);
			return myResponse;			
		}

		//Customer number cannot be < 1
		Long customerNumberLong = new Long(account.getCustomerNumber());
		if(customerNumberLong.longValue() < 1)
		{
			JSONObject error = new JSONObject();
			error.put(ERROR_MSG, "Customer number cannot be less than one.");
			logger.warning("Customer number cannot be less than one.");
			myResponse = Response.status(400).entity(error.toString()).build();
			logger.exiting(this.getClass().getName(),CREATE_ACCOUNT_INTERNAL ,myResponse);
			return myResponse;
		}
		
		//Customer number cannot be 9999999999
		if(customerNumberLong.longValue() == 9999999999L)
		{
			JSONObject error = new JSONObject();
			error.put(ERROR_MSG, "Customer number cannot be 9,999,999,999.");
			logger.warning("Customer number cannot be 9,999,999,999.");
			myResponse = Response.status(400).entity(error.toString()).build();
			logger.exiting(this.getClass().getName(),CREATE_ACCOUNT_INTERNAL ,myResponse);
			return myResponse;
		}
		
		//Sortcode is not valid for this bank
		Integer inputSortCode = new Integer(account.getSortCode());
		Integer thisSortCode = this.getSortCode();

		if(inputSortCode.intValue() != thisSortCode.intValue())
		{
			JSONObject error = new JSONObject();
			error.put(ERROR_MSG, SORT_CODE_LITERAL+  inputSortCode + NOT_VALID_FOR_THIS_BANK + thisSortCode + ")");
			logger.warning(SORT_CODE_LITERAL+  inputSortCode + NOT_VALID_FOR_THIS_BANK + thisSortCode + ")");
			myResponse = Response.status(400).entity(error.toString()).build();
			logger.exiting(this.getClass().getName(),CREATE_ACCOUNT_INTERNAL ,myResponse);
			return myResponse;
		}





		CustomerResource myCustomer = new CustomerResource();
		Response customerResponse = myCustomer.getCustomerInternal(customerNumberLong);
		// Customer number cannot be found
		if(customerResponse.getStatus() != 200)
		{
			JSONObject error = new JSONObject();
			error.put(ERROR_MSG, CUSTOMER_NUMBER_LITERAL + customerNumberLong.longValue() + CANNOT_BE_FOUND);
			logger.warning(CUSTOMER_NUMBER_LITERAL + customerNumberLong.longValue() + CANNOT_BE_FOUND);
			myResponse = Response.status(404).entity(error.toString()).build();
			logger.exiting(this.getClass().getName(),CREATE_ACCOUNT_INTERNAL ,myResponse);
			return myResponse;

		}

		JSONObject response = new JSONObject();
		AccountsResource thisAccountsResource = new AccountsResource();

		JSONObject myAccountsJSON = null;
		try {
			Response accountsOfThisCustomer = thisAccountsResource.getAccountsByCustomerInternal(customerNumberLong,false);
			if(accountsOfThisCustomer.getStatus() != 200)
			{
				//If accountsOfThisCustomer returns status 404, create new JSONObject containing the error message
				if(accountsOfThisCustomer.getStatus() == 404)
				{
					JSONObject error = new JSONObject();
					error.put(ERROR_MSG, CUSTOMER_NUMBER_LITERAL + customerNumberLong.longValue() + CANNOT_BE_FOUND);
					logger.warning(CUSTOMER_NUMBER_LITERAL + customerNumberLong.longValue() + CANNOT_BE_FOUND);
					myResponse = Response.status(404).entity(error.toString()).build();
					logger.exiting(this.getClass().getName(),CREATE_ACCOUNT_INTERNAL ,myResponse);
					return myResponse;
				}
				JSONObject error = new JSONObject();
				error.put(ERROR_MSG, CUSTOMER_NUMBER_LITERAL + customerNumberLong.longValue() + CANNOT_BE_ACCESSED);
				logger.severe(CUSTOMER_NUMBER_LITERAL + customerNumberLong.longValue() + CANNOT_BE_ACCESSED);
				myResponse = Response.status(accountsOfThisCustomer.getStatus()).entity(error.toString()).build();
				logger.exiting(this.getClass().getName(),CREATE_ACCOUNT_INTERNAL ,myResponse);
				return myResponse;

			}
			String accountsOfThisCustomerString = accountsOfThisCustomer.getEntity().toString();
			myAccountsJSON = JSONObject.parse(accountsOfThisCustomerString);
		} catch (IOException e) {
			JSONObject error = new JSONObject();
			error.put(ERROR_MSG,"Failed to retrieve customer number " + customerNumberLong + " " + e.getLocalizedMessage());
			logger.severe("Failed to retrieve customer number " + customerNumberLong + " " + e.getLocalizedMessage());
			myResponse = Response.status(500).entity(error.toString()).build();
			logger.exiting(this.getClass().getName(),CREATE_ACCOUNT_INTERNAL ,myResponse);
			return myResponse;
		}
		long accountCount = (Long) myAccountsJSON.get(JSON_NUMBER_OF_ACCOUNTS);

		// Does the customer have ten or more accounts?

		if(accountCount >= 10)
		{
			JSONObject error = new JSONObject();
			error.put(ERROR_MSG, CUSTOMER_NUMBER_LITERAL + customerNumberLong.longValue() + " cannot have more than ten accounts.");
			logger.warning(CUSTOMER_NUMBER_LITERAL + customerNumberLong.longValue() + " cannot have more than ten accounts.");
			myResponse = Response.status(400).entity(error.toString()).build();
			logger.exiting(this.getClass().getName(),CREATE_ACCOUNT_INTERNAL ,myResponse);
			return myResponse;
		}

		com.ibm.cics.cip.bankliberty.web.db2.Account db2Account = new com.ibm.cics.cip.bankliberty.web.db2.Account();
		db2Account.setSortcode(this.getSortCode().toString());
		db2Account = db2Account.createAccount(account, this.getSortCode(),true);
		//Add data to JSONObject
		if(db2Account != null)
		{
			response.put(JSON_SORT_CODE, db2Account.getSortcode().trim());
			response.put("id", db2Account.getAccountNumber());
			response.put(JSON_CUSTOMER_NUMBER, db2Account.getCustomerNumber());
			response.put(JSON_ACCOUNT_TYPE, db2Account.getType().trim());
			response.put(JSON_AVAILABLE_BALANCE,  BigDecimal.valueOf(db2Account.getAvailableBalance()));
			response.put(JSON_ACTUAL_BALANCE, BigDecimal.valueOf(db2Account.getActualBalance()));
			response.put(JSON_INTEREST_RATE, BigDecimal.valueOf(db2Account.getInterestRate()));
			response.put(JSON_OVERDRAFT, db2Account.getOverdraftLimit());
			response.put(JSON_LAST_STATEMENT_DATE, db2Account.getLastStatement().toString().trim());
			response.put(JSON_NEXT_STATEMENT_DATE, db2Account.getNextStatement().toString().trim());
			response.put(JSON_DATE_OPENED, db2Account.getOpened().toString().trim());

			//Create a new ProcessedTransactionAccount and set credentials
			ProcessedTransactionResource myProcessedTransactionResource = new ProcessedTransactionResource();
			ProcessedTransactionAccountJSON myProctranAccount = new ProcessedTransactionAccountJSON();
			myProctranAccount.setSortCode(db2Account.getSortcode());
			myProctranAccount.setAccountNumber(db2Account.getAccountNumber());
			myProctranAccount.setCustomerNumber(new Long(db2Account.getCustomerNumber()).toString());
			myProctranAccount.setLastStatement(db2Account.getLastStatement());
			myProctranAccount.setNextStatement(db2Account.getNextStatement());
			myProctranAccount.setType(db2Account.getType());
			myProctranAccount.setActualBalance(BigDecimal.valueOf(db2Account.getActualBalance()));



			Response writeCreateAccountResponse = myProcessedTransactionResource.writeCreateAccountInternal(myProctranAccount);
			if(writeCreateAccountResponse == null || writeCreateAccountResponse.getStatus() != 200)
			{
				JSONObject error = new JSONObject();
				error.put(ERROR_MSG, PROCTRAN_WRITE_FAILURE);
				try {
					logger.severe("Accounts: createAccount: " +PROCTRAN_WRITE_FAILURE);
					Task.getTask().rollback();
				} catch (InvalidRequestException e) {
				}
				logger.severe(ACCOUNT_CREATE_FAILURE);
				myResponse = Response.status(500).entity(error.toString()).build();
				logger.exiting(this.getClass().getName(),CREATE_ACCOUNT_INTERNAL ,myResponse);
				return myResponse;
			}

		}
		else
		{
			JSONObject error = new JSONObject();
			error.put(ERROR_MSG, ACCOUNT_CREATE_FAILURE);
			logger.severe(ACCOUNT_CREATE_FAILURE);
			myResponse = Response.status(500).entity(error.toString()).build();
			logger.exiting(this.getClass().getName(),CREATE_ACCOUNT_INTERNAL ,myResponse);
			return myResponse;
		}

		myResponse = Response.status(201).entity(response.toString()).build();
		logger.exiting(this.getClass().getName(),CREATE_ACCOUNT_INTERNAL ,myResponse);
		return myResponse;

	}

	@GET
	@Produces("application/json")
	public Response getAccountsExternal(@QueryParam("countOnly") Boolean countOnly)
	{
		/** This will list all accounts. The countOnly boolean indicates that we just want the number of accounts, not every  single account */
		logger.entering(this.getClass().getName(),GET_ACCOUNTS_EXTERNAL);
		boolean countOnlyReal;
		if(countOnly != null)
		{
			countOnlyReal = countOnly.booleanValue();
		}
		else
		{
			countOnlyReal = false;
		}
		Response myResponse = getAccountsInternal(countOnlyReal); 
		HBankDataAccess myHBankDataAccess = new HBankDataAccess();
		myHBankDataAccess.terminate();
		logger.exiting(this.getClass().getName(),GET_ACCOUNTS_EXTERNAL,myResponse);
		return myResponse;
	}



	public Response getAccountsInternal(boolean countOnly) {
		/** This will list all accounts. The countOnly boolean indicates that we just want the number of accounts, not every  single account */
		logger.entering(this.getClass().getName(),GET_ACCOUNTS_INTERNAL);
		com.ibm.cics.cip.bankliberty.web.db2.Account db2Account = null;
		JSONObject response = new JSONObject();
		Response myResponse = null;
		JSONArray accounts = null;
		int numberOfAccounts = 0;
		Integer sortCode = this.getSortCode();

		Account[] myAccounts = null;
		db2Account = new Account();

		if(countOnly)
		{
			numberOfAccounts = db2Account.getAccountsCountOnly(sortCode);
		}
		else
		{
			myAccounts = db2Account.getAccounts(sortCode);
			//If cannot find any accounts
			if(myAccounts == null)
			{
				response.put(ERROR_MSG,"Accounts cannot be accessed in com.ibm.cics.cip.bankliberty.web.db2.Account");
				logger.severe("Accounts cannot be accessed in com.ibm.cics.cip.bankliberty.web.db2.Account");
				myResponse = Response.status(500).entity(response.toString()).build();
				logger.exiting(this.getClass().getName(),GET_ACCOUNTS_INTERNAL,myResponse);
				return myResponse;
			}
			accounts = new JSONArray(myAccounts.length);

			for(int i = 0;i < myAccounts.length;i++)
			{
				JSONObject account = new JSONObject();
				account.put(JSON_SORT_CODE, myAccounts[i].getSortcode().trim());
				account.put("id", myAccounts[i].getAccountNumber());
				account.put(JSON_CUSTOMER_NUMBER, myAccounts[i].getCustomerNumber());
				account.put(JSON_ACCOUNT_TYPE, myAccounts[i].getType().trim());
				account.put(JSON_AVAILABLE_BALANCE, BigDecimal.valueOf(myAccounts[i].getAvailableBalance()).setScale(2,RoundingMode.HALF_UP));
				account.put(JSON_ACTUAL_BALANCE, BigDecimal.valueOf(myAccounts[i].getActualBalance()).setScale(2,RoundingMode.HALF_UP));
				account.put(JSON_INTEREST_RATE, BigDecimal.valueOf(myAccounts[i].getInterestRate()).setScale(2,RoundingMode.HALF_UP));
				account.put(JSON_OVERDRAFT, myAccounts[i].getOverdraftLimit());
				account.put(JSON_LAST_STATEMENT_DATE, myAccounts[i].getLastStatement().toString().trim());
				account.put(JSON_NEXT_STATEMENT_DATE, myAccounts[i].getNextStatement().toString().trim());
				account.put(JSON_DATE_OPENED, myAccounts[i].getOpened().toString());

				accounts.add(account);
			}
			numberOfAccounts = accounts.size();
		}

		/*
		 * Parse returned data and return to calling method
		 */

		response.put(JSON_NUMBER_OF_ACCOUNTS, numberOfAccounts);
		if(accounts != null)
		{
			response.put(JSON_ACCOUNTS, accounts);
		}
		myResponse = Response.status(200).entity(response.toString()).build();
		logger.exiting(this.getClass().getName(),GET_ACCOUNTS_INTERNAL,myResponse);
		return myResponse;
	}



	@GET
	@Path("/{accountNumber}")
	@Produces("application/json")
	public Response getAccountExternal(@PathParam("accountNumber") Long accountNumber) 
	{
		/** This will list one single account of the specified number. */
		logger.entering(this.getClass().getName(),"getAccountExternal(Long accountNumber)");
		Response myResponse = getAccountInternal(accountNumber); 
		HBankDataAccess myHBankDataAccess = new HBankDataAccess();
		myHBankDataAccess.terminate();
		logger.exiting(this.getClass().getName(),"getAccountExternal(Long accountNumber)",myResponse);
		return myResponse;
	}


	public Response getAccountInternal(Long accountNumber) {
		/** This will list one single account of the specified number. */
		logger.entering(this.getClass().getName(),GET_ACCOUNT_INTERNAL);
		Response myResponse = null;
		JSONObject response = new JSONObject();

		Integer sortCode = this.getSortCode();
		Long id_safe = accountNumber;

		com.ibm.cics.cip.bankliberty.web.db2.Account db2Account = new Account();

		db2Account = db2Account.getAccount(accountNumber.intValue(), sortCode);
		if(db2Account != null)
		{
			
			response.put(JSON_SORT_CODE, db2Account.getSortcode().trim());
			response.put("id", db2Account.getAccountNumber());
			response.put(JSON_CUSTOMER_NUMBER, db2Account.getCustomerNumber());
			response.put(JSON_ACCOUNT_TYPE, db2Account.getType().trim());
			response.put(JSON_AVAILABLE_BALANCE, BigDecimal.valueOf(db2Account.getAvailableBalance()));
			response.put(JSON_ACTUAL_BALANCE, BigDecimal.valueOf(db2Account.getActualBalance()));
			response.put(JSON_INTEREST_RATE, BigDecimal.valueOf(db2Account.getInterestRate()));
			response.put(JSON_OVERDRAFT, db2Account.getOverdraftLimit());
			response.put(JSON_LAST_STATEMENT_DATE, db2Account.getLastStatement().toString().trim());
			response.put(JSON_NEXT_STATEMENT_DATE, db2Account.getNextStatement().toString().trim());
			response.put(JSON_DATE_OPENED, db2Account.getOpened().toString().trim());
		}
		else
		{
			// 99999999L is a special account number, meaning "get me the last account number that exists"
			if(db2Account == null && id_safe == 99999999L)
			{
				db2Account = new Account();
				db2Account = db2Account.getAccount(id_safe.intValue(), sortCode);
				if(db2Account != null)
				{
					response.put(JSON_SORT_CODE, db2Account.getSortcode().trim());
					response.put("id", db2Account.getAccountNumber());
					response.put(JSON_CUSTOMER_NUMBER, db2Account.getCustomerNumber());
					response.put(JSON_ACCOUNT_TYPE, db2Account.getType().trim());
					response.put(JSON_AVAILABLE_BALANCE, BigDecimal.valueOf(db2Account.getAvailableBalance()));
					response.put(JSON_ACTUAL_BALANCE, BigDecimal.valueOf(db2Account.getActualBalance()));
					response.put(JSON_INTEREST_RATE, BigDecimal.valueOf(db2Account.getInterestRate()));
					response.put(JSON_OVERDRAFT, db2Account.getOverdraftLimit());
					response.put(JSON_LAST_STATEMENT_DATE, db2Account.getLastStatement().toString().trim());
					response.put(JSON_NEXT_STATEMENT_DATE, db2Account.getNextStatement().toString().trim());
					response.put(JSON_DATE_OPENED, db2Account.getOpened().toString().trim());
				}
				//* There exists a possibility that the last account has been deleted. In which case we try once the old fashioned way
			}
			if(db2Account == null)
			{
				response.put(ERROR_MSG,ACCOUNT_LITERAL + accountNumber + " not found in com.ibm.cics.cip.bankliberty.web.db2.Account");
				logger.warning(ACCOUNT_LITERAL + accountNumber + " not found in com.ibm.cics.cip.bankliberty.web.db2.Account");
				myResponse = Response.status(404).entity(response.toString()).build();
				logger.exiting(this.getClass().getName(),GET_ACCOUNT_INTERNAL,myResponse);
				return myResponse;
			}
		}

		myResponse = Response.status(200).entity(response.toString()).build();
		logger.exiting(this.getClass().getName(),GET_ACCOUNT_INTERNAL,myResponse);

		return myResponse;
	}


	@GET
	@Path("/retrieveByCustomerNumber/{customerNumber}")
	@Produces("application/json")
	public Response getAccountsByCustomerExternal(@PathParam(JSON_CUSTOMER_NUMBER) Long customerNumber, @QueryParam("countOnly") Boolean countOnly) {
		/** This will list accounts owned by a specified customer */
		logger.entering(this.getClass().getName(),"getAccountsByCustomerExternal(Long customerNumber, Boolean countOnly)");
		boolean countOnlyReal;
		if(countOnly != null)
		{
			countOnlyReal = countOnly.booleanValue();
		}
		else
		{
			countOnlyReal = false;
		}
		Response myResponse = getAccountsByCustomerInternal(customerNumber,countOnlyReal); 
		HBankDataAccess myHBankDataAccess = new HBankDataAccess();
		myHBankDataAccess.terminate();
		logger.exiting(this.getClass().getName(),"getAccountsByCustomerExternal(Long customerNumber, Boolean countOnly)",myResponse);
		return myResponse;
	}

	public Response getAccountsByCustomerInternal(@PathParam(JSON_CUSTOMER_NUMBER) Long customerNumber,boolean countOnly) {
		logger.entering(this.getClass().getName(),GET_ACCOUNTS_BY_CUSTOMER_INTERNAL);

		JSONArray accounts = null;
		Response myResponse = null;

		JSONObject response = new JSONObject();
		Integer sortCode =  this.getSortCode();
		int numberOfAccounts = 0;


		CustomerResource myCustomer = new CustomerResource();
		Response customerResponse = myCustomer.getCustomerInternal(customerNumber);

		if(customerResponse.getStatus() != 200)
		{
			if(customerResponse.getStatus() == 404)
			{
				//If cannot find response "CustomerResponse" then error 404 returned
				JSONObject error = new JSONObject();
				error.put(ERROR_MSG, CUSTOMER_NUMBER_LITERAL + customerNumber.longValue() + CANNOT_BE_FOUND);
				logger.severe(CUSTOMER_NUMBER_LITERAL + customerNumber.longValue() + CANNOT_BE_FOUND);
				myResponse = Response.status(404).entity(error.toString()).build(); 
				logger.exiting(this.getClass().getName(),GET_ACCOUNTS_BY_CUSTOMER_INTERNAL,myResponse);
				return myResponse;
			}
			else
			{
				JSONObject error = new JSONObject();
				error.put(ERROR_MSG, CUSTOMER_NUMBER_LITERAL + customerNumber.longValue() + " cannot be accessed. " + customerResponse.toString());
				logger.severe(CUSTOMER_NUMBER_LITERAL + customerNumber.longValue() + " cannot be accessed. " + customerResponse.toString());
				myResponse = Response.status(customerResponse.getStatus()).entity(error.toString()).build(); 
				logger.exiting(this.getClass().getName(),GET_ACCOUNTS_BY_CUSTOMER_INTERNAL,myResponse);
				return myResponse;
			}
		}


		com.ibm.cics.cip.bankliberty.web.db2.Account db2Account = new Account();
		Account[] myAccounts = db2Account.getAccounts(customerNumber.intValue(), sortCode);
		if(myAccounts == null)
		{
			JSONObject error = new JSONObject();
			error.put(ERROR_MSG, "Accounts cannot be accessed for customer " + customerNumber.longValue() + CLASS_NAME_MSG);
			logger.severe("Accounts cannot be accessed for customer " + customerNumber.longValue() + CLASS_NAME_MSG);
			myResponse = Response.status(500).entity(error.toString()).build(); 
			logger.exiting(this.getClass().getName(),GET_ACCOUNTS_BY_CUSTOMER_INTERNAL,myResponse);
			return myResponse;	
		}

		numberOfAccounts = myAccounts.length;
		accounts = new JSONArray(numberOfAccounts);
		for (int i = 0; i < numberOfAccounts; i++) {


			JSONObject account = new JSONObject();
			account.put(JSON_SORT_CODE, myAccounts[i].getSortcode());
			account.put("id", myAccounts[i].getAccountNumber());
			account.put(JSON_CUSTOMER_NUMBER, myAccounts[i].getCustomerNumber());
			account.put(JSON_ACCOUNT_TYPE, myAccounts[i].getType());
			account.put(JSON_AVAILABLE_BALANCE, BigDecimal.valueOf(myAccounts[i].getAvailableBalance()));
			account.put(JSON_ACTUAL_BALANCE, BigDecimal.valueOf(myAccounts[i].getActualBalance()));
			account.put(JSON_INTEREST_RATE, BigDecimal.valueOf(myAccounts[i].getInterestRate()));
			account.put(JSON_OVERDRAFT, myAccounts[i].getOverdraftLimit());
			account.put(JSON_LAST_STATEMENT_DATE, myAccounts[i].getLastStatement().toString());
			account.put(JSON_NEXT_STATEMENT_DATE, myAccounts[i].getNextStatement().toString());
			account.put(JSON_DATE_OPENED, myAccounts[i].getOpened().toString());

			accounts.add(account);
		}
		String customerNumberString = new String(customerNumber.toString());
		for(int i=10;customerNumberString.length()<10;i--)
		{
			customerNumberString = "0" + customerNumberString;
		}
		response.put(JSON_CUSTOMER_NUMBER, customerNumberString);
		response.put(JSON_NUMBER_OF_ACCOUNTS, numberOfAccounts);
		response.put(JSON_ACCOUNTS, accounts);


		myResponse = Response.status(200).entity(response.toString()).build(); 
		logger.exiting(this.getClass().getName(),GET_ACCOUNTS_BY_CUSTOMER_INTERNAL,myResponse);
		return myResponse;


	}


	private Integer getSortCode() {
		/** This will get the Sort Code from the SortCode Resource */
		SortCodeResource mySortCodeResource = new SortCodeResource();
		Response mySortCodeJSON = mySortCodeResource.getSortCode();
		String mySortCode = ((String) mySortCodeJSON.getEntity()).substring(13, 19);
		return new Integer(mySortCode);
	}

	@PUT
	@Path("/{id}")
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)
	public Response updateAccountExternal(
			@PathParam("id") Long id,
			AccountJSON account) {
		/** Update the account specified by "id" with the JSON in AccountJSON. This is for interest rates, types and overdraft limits, not balances */		
		logger.entering(this.getClass().getName(),"updateAccountExternal(Long id, AccountJSON account)");
		Response myResponse = updateAccountInternal(id,account); 
		HBankDataAccess myHBankDataAccess = new HBankDataAccess();
		myHBankDataAccess.terminate();
		logger.exiting(this.getClass().getName(),"updateAccountExternal(Long id, AccountJSON account)",myResponse);
		return myResponse;
	}

	public Response updateAccountInternal(
			Long id,
			AccountJSON account) {
		/** Update the account specified by "id" with the JSON in AccountJSON. This is for interest rates, types and overdraft limits, not balances */
		logger.entering(this.getClass().getName(),UPDATE_ACCOUNT_INTERNAL);
		JSONObject response = new JSONObject();
		Response myResponse = null;


		if(!(account.validateType(account.getAccountType().trim())))
			//If account type invalid
		{
			JSONObject error = new JSONObject();
			error.put(ERROR_MSG, ACC_TYPE_STRING + account.getAccountType() + NOT_SUPPORTED);
			logger.warning(ACC_TYPE_STRING + account.getAccountType() + NOT_SUPPORTED);
			myResponse = Response.status(400).entity(error.toString()).build(); 
			logger.exiting(this.getClass().getName(),UPDATE_ACCOUNT_INTERNAL,myResponse);
			return myResponse;
		}

		if(account.getInterestRate().doubleValue() < 0.00)
		{
			//If interest rate < 0
			JSONObject error = new JSONObject();
			error.put(ERROR_MSG, INTEREST_RATE_LESS_THAN_ZERO);
			logger.warning(INTEREST_RATE_LESS_THAN_ZERO);
			myResponse = Response.status(400).entity(error.toString()).build(); 
			logger.exiting(this.getClass().getName(),UPDATE_ACCOUNT_INTERNAL,myResponse);
			return myResponse;
		}

		if(account.getInterestRate().doubleValue() > 9999.99)
		{
			//If interest rate > 9999.99
			JSONObject error = new JSONObject();
			error.put(ERROR_MSG, INTEREST_RATE_TOO_HIGH);
			logger.warning(INTEREST_RATE_TOO_HIGH);
			myResponse = Response.status(400).entity(error.toString()).build(); 
			logger.exiting(this.getClass().getName(),UPDATE_ACCOUNT_INTERNAL,myResponse);
			return myResponse;

		}

		BigDecimal myInterestRate = account.getInterestRate();
		if(myInterestRate.scale() > 2)
			//Interest rate more than 2dp
		{
			JSONObject error = new JSONObject();
			error.put(ERROR_MSG, "Interest rate cannot have more than 2 decimal places. " + myInterestRate.toPlainString());
			logger.warning("Interest rate cannot have more than 2 decimal places."  + myInterestRate.toPlainString());
			myResponse = Response.status(400).entity(error.toString()).build(); 
			logger.exiting(this.getClass().getName(),UPDATE_ACCOUNT_INTERNAL,myResponse);
			return myResponse;
		}

		Integer inputSortCode = new Integer(account.getSortCode());
		Integer thisSortCode = this.getSortCode();

		if(inputSortCode.intValue() != thisSortCode.intValue())
			//Invalid sortcode
		{
			JSONObject error = new JSONObject();
			error.put(ERROR_MSG, SORT_CODE_LITERAL+  inputSortCode + NOT_VALID_FOR_THIS_BANK + thisSortCode + ")");
			logger.warning(SORT_CODE_LITERAL+  inputSortCode + NOT_VALID_FOR_THIS_BANK + thisSortCode + ")");
			myResponse = Response.status(400).entity(error.toString()).build(); 
			logger.exiting(this.getClass().getName(),UPDATE_ACCOUNT_INTERNAL,myResponse);
			return myResponse;
		}


		com.ibm.cics.cip.bankliberty.web.db2.Account db2Account = new com.ibm.cics.cip.bankliberty.web.db2.Account();
		account.setId(id.toString());
		db2Account = db2Account.getAccount(new Integer(account.getId()).intValue(),this.getSortCode().intValue());

		if(db2Account != null)
		{
			db2Account = db2Account.updateAccount(account);
			if(db2Account != null)
			{
				response.put(JSON_SORT_CODE, db2Account.getSortcode().trim());
				response.put("id", db2Account.getAccountNumber());
				response.put(JSON_CUSTOMER_NUMBER,db2Account.getCustomerNumber());
				response.put(JSON_ACCOUNT_TYPE, db2Account.getType().trim());
				response.put(JSON_AVAILABLE_BALANCE, BigDecimal.valueOf(db2Account.getAvailableBalance()));
				response.put(JSON_ACTUAL_BALANCE, BigDecimal.valueOf(db2Account.getActualBalance()));
				response.put(JSON_INTEREST_RATE, BigDecimal.valueOf(db2Account.getInterestRate()));				
				response.put(JSON_OVERDRAFT, db2Account.getOverdraftLimit());
				response.put(JSON_LAST_STATEMENT_DATE, db2Account.getLastStatement().toString());
				response.put(JSON_NEXT_STATEMENT_DATE, db2Account.getNextStatement().toString());
				response.put(JSON_DATE_OPENED, db2Account.getOpened().toString().trim());
			}
			else
			{
				JSONObject error = new JSONObject();
				error.put(ERROR_MSG, "Failed to update account in com.ibm.cics.cip.bankliberty.web.db2.Account");
				logger.severe("Failed to update account in com.ibm.cics.cip.bankliberty.web.db2.Account");
				myResponse = Response.status(500).entity(error.toString()).build(); 
				logger.exiting(this.getClass().getName(),UPDATE_ACCOUNT_INTERNAL,myResponse);
				return myResponse;
			}
		}
		else
		{
			JSONObject error = new JSONObject();
			error.put(ERROR_MSG, FAILED_TO_READ + account.getId() + " in " + this.getClass().toString());

			logger.warning(FAILED_TO_READ + account.getId() + CLASS_NAME_MSG);
			myResponse = Response.status(404).entity(error.toString()).build(); 
			logger.exiting(this.getClass().getName(),UPDATE_ACCOUNT_INTERNAL,myResponse);
			return myResponse;
		}
		myResponse = Response.status(200).entity(response.toString()).build(); 
		logger.exiting(this.getClass().getName(),UPDATE_ACCOUNT_INTERNAL,myResponse);

		return myResponse;
	}




	@PUT
	@Path("/debit/{id}")
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)
	public Response debitAccountExternal(@PathParam("id") String accountNumber,
			DebitCreditAccountJSON dbcr) {
		// we use this to subtract money from an account
		logger.entering(this.getClass().getName(),"debitAccountExternal(String accountNumber, DebitCreditAccountJSON dbcr)");
		Response myResponse = debitAccountInternal(accountNumber,dbcr); 
		HBankDataAccess myHBankDataAccess = new HBankDataAccess();
		myHBankDataAccess.terminate();
		logger.exiting(this.getClass().getName(),"debitAccountExternal(String accountNumber, DebitCreditAccountJSON dbcr)",myResponse);
		return myResponse;
	}


	public Response debitAccountInternal(String accountNumber,
			DebitCreditAccountJSON dbcr) {
		// we use this to subtract money from an account		
		logger.entering(this.getClass().getName(),DEBIT_ACCOUNT_INTERNAL);
		Response myResponse = null;

		AccountsResource checkAccount = new AccountsResource();
		Response checkAccountResponse = checkAccount.getAccountInternal(new Long(accountNumber));

		if(checkAccountResponse.getStatus() != 200)
		{
			if(checkAccountResponse.getStatus() == 404)
			{
				JSONObject error = new JSONObject();
				error.put(ERROR_MSG, FAILED_TO_READ + accountNumber + IN_DEBIT_ACCOUNT + this.getClass().toString());
				logger.warning(FAILED_TO_READ + accountNumber + IN_DEBIT_ACCOUNT + this.getClass().toString());
				myResponse = Response.status(404).entity(error.toString()).build();
				logger.exiting(this.getClass().getName(),DEBIT_ACCOUNT_INTERNAL,myResponse);
				return myResponse;
			}
			JSONObject error = new JSONObject();
			error.put(ERROR_MSG, FAILED_TO_READ + accountNumber + IN_DEBIT_ACCOUNT + this.getClass().toString());
			logger.severe(FAILED_TO_READ + accountNumber + IN_DEBIT_ACCOUNT + this.getClass().toString());
			myResponse = Response.status(checkAccountResponse.getStatus()).entity(error.toString()).build();
			logger.exiting(this.getClass().getName(),DEBIT_ACCOUNT_INTERNAL,myResponse);
			return myResponse;
		}
		Long sortcode = new Long(this.getSortCode().longValue());
		myResponse = debitCreditAccount(sortcode, accountNumber, dbcr.getAmount(), true);
		logger.exiting(this.getClass().getName(),DEBIT_ACCOUNT_INTERNAL,myResponse);
		return myResponse;
	}

	@PUT
	@Path("/credit/{id}")
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)
	public Response creditAccountExternal(@PathParam("id") String accountNumber,
			DebitCreditAccountJSON dbcr) {
		// we use this to add money to an account		
		logger.entering(this.getClass().getName(),"creditAccountExternal(String accountNumber, DebitCreditAccountJSON dbcr)");
		Response myResponse = creditAccountInternal(accountNumber,dbcr); 
		HBankDataAccess myHBankDataAccess = new HBankDataAccess();
		myHBankDataAccess.terminate();
		logger.exiting(this.getClass().getName(),"creditAccountExternal(String accountNumber, DebitCreditAccountJSON dbcr)",myResponse);
		return myResponse;
	}

	public Response creditAccountInternal(@PathParam("id") String accountNumber,
			DebitCreditAccountJSON dbcr) {
		// we use this to add money to an account		
		logger.entering(this.getClass().getName(),CREDIT_ACCOUNT_INTERNAL);
		Response myResponse = null;

		AccountsResource checkAccount = new AccountsResource();
		Response checkAccountResponse = checkAccount.getAccountInternal(new Long(accountNumber));

		if(checkAccountResponse.getStatus() != 200)
		{
			if(checkAccountResponse.getStatus() == 404)
			{
				JSONObject error = new JSONObject();
				error.put(ERROR_MSG, FAILED_TO_READ + accountNumber + IN_CREDIT_ACCOUNT + this.getClass().toString());
				logger.warning(FAILED_TO_READ + accountNumber + IN_CREDIT_ACCOUNT + this.getClass().toString());
				myResponse = Response.status(404).entity(error.toString()).build(); 
				logger.exiting(this.getClass().getName(),CREDIT_ACCOUNT_INTERNAL,myResponse);
				return myResponse;
			}
			JSONObject error = new JSONObject();
			error.put(ERROR_MSG, FAILED_TO_READ + accountNumber + IN_CREDIT_ACCOUNT + this.getClass().toString());
			logger.severe(FAILED_TO_READ + accountNumber + IN_CREDIT_ACCOUNT + this.getClass().toString());
			myResponse = Response.status(checkAccountResponse.getStatus()).entity(error.toString()).build(); 
			logger.exiting(this.getClass().getName(),CREDIT_ACCOUNT_INTERNAL,myResponse);
			return myResponse;
		}

		Long sortcode = new Long(this.getSortCode().longValue());
		myResponse = debitCreditAccount(sortcode, accountNumber, dbcr.getAmount(), false); 
		logger.exiting(this.getClass().getName(),CREDIT_ACCOUNT_INTERNAL,myResponse);
		return myResponse;
	}

	@PUT
	@Path("/transfer/{id}")
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)
	public Response transferLocalExternal(@PathParam("id") String accountNumber,
			TransferLocalJSON transferLocal) {
		// we use this to move money between two accounts at the same bank		
		logger.entering(this.getClass().getName(),"transferLocalExternal(String accountNumber, TransferLocalJSON transferLocal)");
		Integer accountNumberInteger;
		try
		{
			accountNumberInteger = new Integer(accountNumber);
			if(accountNumberInteger.intValue() < 1 || accountNumberInteger.intValue() == 99999999)
			{
				return null;
			}
		}
		catch(NumberFormatException e)
		{
			return null;
		}
		TransferLocalJSON transferLocalValid = new TransferLocalJSON();
		if(transferLocal.getAmount().doubleValue() < 0.00)
		{
			return null;
		}
		else 
		{
			transferLocalValid.setAmount(transferLocal.getAmount());
		}
		if(transferLocal.getTargetAccount()<1 || transferLocal.getTargetAccount() == 99999999)
		{
			return null;
		}
		else
		{
			transferLocalValid.setTargetAccount(transferLocal.getTargetAccount());
		}
		
		Response myResponse = transferLocalInternal(accountNumberInteger.toString(),transferLocalValid); 
		HBankDataAccess myHBankDataAccess = new HBankDataAccess();
		myHBankDataAccess.terminate();
		logger.exiting(this.getClass().getName(),"transferLocalExternal(String accountNumber, TransferLocalJSON transferLocal)",myResponse);
		return myResponse;
	}

	public Response transferLocalInternal(@PathParam("id") String accountNumber,
			TransferLocalJSON transferLocal) {
		// we use this to move money between two accounts at the same bank		
		logger.entering(this.getClass().getName(),TRANSFER_LOCAL_INTERNAL);
		Response myResponse = null;

		//* We are transferring money from account "id" at this bank, to another account at this bank
		//* The amount MUST be positive
		JSONObject response = new JSONObject();

		if(new Integer(accountNumber).equals(transferLocal.getTargetAccount()))
		{
			JSONObject error = new JSONObject();
			error.put(ERROR_MSG, NEED_DIFFERENT_ACCOUNTS);
			logger.warning(NEED_DIFFERENT_ACCOUNTS);
			myResponse = Response.status(400).entity(error.toString()).build(); 
			logger.exiting(this.getClass().getName(),TRANSFER_LOCAL_INTERNAL,myResponse);
			return myResponse;
		}

		if(transferLocal.getAmount().doubleValue() <= 0.00)
		{
			JSONObject error = new JSONObject();
			error.put(ERROR_MSG, "Amount to transfer must be positive");
			logger.warning(NEED_DIFFERENT_ACCOUNTS);
			myResponse = Response.status(400).entity(error.toString()).build(); 
			logger.exiting(this.getClass().getName(),TRANSFER_LOCAL_INTERNAL,myResponse);
			return myResponse;
		}

		BigDecimal amount = transferLocal.getAmount();
		amount = amount.setScale(2,RoundingMode.HALF_UP);
		BigDecimal negativeAmount = amount;
		negativeAmount = negativeAmount.multiply(BigDecimal.valueOf(-1));
		negativeAmount = negativeAmount.setScale(2,RoundingMode.HALF_UP);

		Long sortCode = new Long(this.getSortCode().longValue());


		//		Let's make sure that from account and to account exist
		AccountsResource checkAccount = new AccountsResource();
		Response checkAccountResponse = checkAccount.getAccountInternal(new Long(accountNumber));

		if(checkAccountResponse.getStatus() == 404)
		{
			JSONObject error = new JSONObject();
			error.put(ERROR_MSG, SOURCE_ACCOUNT_NUMBER + accountNumber + CANNOT_BE_FOUND);
			logger.warning(SOURCE_ACCOUNT_NUMBER + accountNumber + CANNOT_BE_FOUND);
			myResponse = Response.status(404).entity(error.toString()).build(); 
			logger.exiting(this.getClass().getName(),TRANSFER_LOCAL_INTERNAL,myResponse);
			return myResponse;
		}

		if(checkAccountResponse.getStatus() != 200)
		{
			JSONObject error = new JSONObject();
			error.put(ERROR_MSG, SOURCE_ACCOUNT_NUMBER + accountNumber + CANNOT_BE_ACCESSED);
			logger.warning(SOURCE_ACCOUNT_NUMBER + accountNumber + CANNOT_BE_ACCESSED);
			myResponse = Response.status(checkAccountResponse.getStatus()).entity(error.toString()).build(); 
			logger.exiting(this.getClass().getName(),TRANSFER_LOCAL_INTERNAL,myResponse);
			return myResponse;
		}
		checkAccountResponse = checkAccount.getAccountInternal(new Long(transferLocal.getTargetAccount()));

		if(checkAccountResponse.getStatus() == 404)
		{
			JSONObject error = new JSONObject();
			error.put(ERROR_MSG, TARGET_ACCOUNT_NUMBER + transferLocal.getTargetAccount() + CANNOT_BE_FOUND);
			logger.warning(TARGET_ACCOUNT_NUMBER + accountNumber + CANNOT_BE_FOUND);
			myResponse = Response.status(404).entity(error.toString()).build(); 
			logger.exiting(this.getClass().getName(),TRANSFER_LOCAL_INTERNAL,myResponse);
			return myResponse;
		}
		if(checkAccountResponse.getStatus() != 200)
		{
			JSONObject error = new JSONObject();
			error.put(ERROR_MSG, TARGET_ACCOUNT_NUMBER + transferLocal.getTargetAccount() + CANNOT_BE_ACCESSED);
			logger.severe(TARGET_ACCOUNT_NUMBER + accountNumber + CANNOT_BE_ACCESSED);
			myResponse = Response.status(404).entity(error.toString()).build(); 
			logger.exiting(this.getClass().getName(),TRANSFER_LOCAL_INTERNAL,myResponse);
			return myResponse;
		}

		com.ibm.cics.cip.bankliberty.web.db2.Account db2Account  = new  com.ibm.cics.cip.bankliberty.web.db2.Account();
		db2Account.setAccountNumber(accountNumber);
		db2Account.setSortcode(sortCode.toString());
		db2Account.debitCredit(negativeAmount);

		db2Account.setAccountNumber(transferLocal.targetAccount.toString());
		db2Account.setSortcode(sortCode.toString());
		db2Account.debitCredit(amount);

		ProcessedTransactionResource myProcessedTransactionResource = new ProcessedTransactionResource();

		ProcessedTransactionTransferLocalJSON myProctranTransferLocal = new ProcessedTransactionTransferLocalJSON();
		myProctranTransferLocal.setSortCode(db2Account.getSortcode());
		myProctranTransferLocal.setAccountNumber(db2Account.getAccountNumber());
		myProctranTransferLocal.setAmount(amount);
		myProctranTransferLocal.setTargetAccountNumber(transferLocal.getTargetAccount().toString());

		Response writeTransferResponse = myProcessedTransactionResource.writeTransferLocalInternal(myProctranTransferLocal);
		if(writeTransferResponse == null || writeTransferResponse.getStatus() != 200)
		{
			JSONObject error = new JSONObject();
			error.put(ERROR_MSG, PROCTRAN_WRITE_FAILURE);
			logger.severe("Accounts: transferLocal: " +PROCTRAN_WRITE_FAILURE);
			try {
				Task.getTask().rollback();
			} catch (InvalidRequestException e) {
			}
			myResponse = Response.status(500).entity(error.toString()).build();
			logger.exiting(this.getClass().getName(),TRANSFER_LOCAL_INTERNAL,myResponse);
			return myResponse;
		}


		response.put(JSON_SORT_CODE, db2Account.getSortcode().trim());
		response.put("id", db2Account.getAccountNumber());
		response.put(JSON_AVAILABLE_BALANCE, BigDecimal.valueOf(db2Account.getAvailableBalance()));
		response.put(JSON_ACTUAL_BALANCE, BigDecimal.valueOf(db2Account.getActualBalance()));
		response.put(JSON_INTEREST_RATE, BigDecimal.valueOf(db2Account.getInterestRate()));

		myResponse = Response.status(200).entity(response.toString()).build();
		logger.exiting(this.getClass().getName(),TRANSFER_LOCAL_INTERNAL,myResponse);
		return myResponse;
	}


	private Response debitCreditAccount(Long sortCode, String accountNumber, BigDecimal apiAmount, boolean debitAccount) {
		// This method does both debit AND credit, controlled by the boolean
		logger.entering(this.getClass().getName(),DEBIT_CREDIT_ACCOUNT);
		Response myResponse = null;
		JSONObject response = new JSONObject();

		if (debitAccount) apiAmount = apiAmount.multiply(BigDecimal.valueOf(-1));


		com.ibm.cics.cip.bankliberty.web.db2.Account db2Account  = new  com.ibm.cics.cip.bankliberty.web.db2.Account();
		db2Account.setAccountNumber(accountNumber);
		db2Account.setSortcode(sortCode.toString());
		if(!db2Account.debitCredit(apiAmount))
		{
			JSONObject error = new JSONObject();
			if(apiAmount.doubleValue() < 0)
			{
				error.put(ERROR_MSG, "Failed to debit account " + accountNumber + CLASS_NAME_MSG);
				logger.severe("Failed to debit account " + accountNumber + CLASS_NAME_MSG);
				myResponse = Response.status(500).entity(error.toString()).build(); 
				logger.exiting(this.getClass().getName(),DEBIT_CREDIT_ACCOUNT,myResponse);
				return myResponse;
			}
			else
			{
				error.put(ERROR_MSG, "Failed to credit account " + accountNumber + CLASS_NAME_MSG);
				logger.severe("Failed to credit account " + accountNumber + CLASS_NAME_MSG);
				myResponse = Response.status(500).entity(error.toString()).build(); 
				logger.exiting(this.getClass().getName(),DEBIT_CREDIT_ACCOUNT,myResponse);
				return myResponse;
			}
		}

		ProcessedTransactionResource myProcessedTransactionResource = new ProcessedTransactionResource();
		ProcessedTransactionDebitCreditJSON myProctranDbCr = new ProcessedTransactionDebitCreditJSON();
		myProctranDbCr.setSortCode(db2Account.getSortcode());
		myProctranDbCr.setAccountNumber(db2Account.getAccountNumber());
		myProctranDbCr.setAmount(apiAmount);


		Response debitCreditResponse = myProcessedTransactionResource.writeInternal(myProctranDbCr);

		if(debitCreditResponse == null || debitCreditResponse.getStatus() != 200)
		{

			JSONObject error = new JSONObject();
			error.put(ERROR_MSG, PROCTRAN_WRITE_FAILURE);
			logger.severe(PROCTRAN_WRITE_FAILURE);
			try {
				Task.getTask().rollback();
			} catch (InvalidRequestException e) {
			}
			myResponse = Response.status(500).entity(error.toString()).build(); 
			logger.exiting(this.getClass().getName(),DEBIT_CREDIT_ACCOUNT,myResponse);
			return myResponse;
		}

		response.put(JSON_SORT_CODE, db2Account.getSortcode().trim());
		response.put("id", db2Account.getAccountNumber());
		response.put(JSON_AVAILABLE_BALANCE, BigDecimal.valueOf(db2Account.getAvailableBalance()));
		response.put(JSON_ACTUAL_BALANCE, BigDecimal.valueOf(db2Account.getActualBalance()));
		response.put(JSON_INTEREST_RATE, BigDecimal.valueOf(db2Account.getInterestRate()));
		myResponse = Response.status(200).entity(response.toString()).build();
		logger.exiting(this.getClass().getName(),DEBIT_CREDIT_ACCOUNT,myResponse);
		return myResponse;
	}


	@DELETE
	@Path("/{accountNumber}")
	@Produces("application/json")
	public Response deleteAccountExternal(@PathParam("accountNumber") Long accountNumber) {
		logger.entering(this.getClass().getName(),"deleteAccountExternal(Long accountNumber)");
		Response myResponse = deleteAccountInternal(accountNumber); 
		HBankDataAccess myHBankDataAccess = new HBankDataAccess();
		myHBankDataAccess.terminate();
		logger.exiting(this.getClass().getName(),"deleteAccountExternal(Long accountNumber)",myResponse);
		return myResponse;
	}


	public Response deleteAccountInternal(Long accountNumber) {
		logger.entering(this.getClass().getName(),DELETE_ACCOUNT);
		Response myResponse = null;


		JSONObject response = new JSONObject();

		Integer sortCode = this.getSortCode();

		com.ibm.cics.cip.bankliberty.web.db2.Account db2Account = new Account();

		db2Account = db2Account.deleteAccount(accountNumber.intValue(), sortCode.intValue());
		if(db2Account != null)
		{
			response.put(JSON_SORT_CODE, db2Account.getSortcode().trim());
			response.put("id",db2Account.getAccountNumber());
			response.put(JSON_CUSTOMER_NUMBER, db2Account.getCustomerNumber());
			response.put(JSON_ACCOUNT_TYPE, db2Account.getType().trim());
			response.put(JSON_AVAILABLE_BALANCE, BigDecimal.valueOf(db2Account.getAvailableBalance()));
			response.put(JSON_ACTUAL_BALANCE, BigDecimal.valueOf(db2Account.getActualBalance()));
			response.put(JSON_INTEREST_RATE, BigDecimal.valueOf(db2Account.getInterestRate()));
			response.put(JSON_OVERDRAFT, db2Account.getOverdraftLimit());
			response.put(JSON_LAST_STATEMENT_DATE, db2Account.getLastStatement().toString().trim());
			response.put(JSON_NEXT_STATEMENT_DATE, db2Account.getNextStatement().toString().trim());
			response.put(JSON_DATE_OPENED, db2Account.getOpened().toString().trim());

			ProcessedTransactionResource myProcessedTransactionResource = new ProcessedTransactionResource();

			ProcessedTransactionAccountJSON myDeletedAccount = new ProcessedTransactionAccountJSON();
			myDeletedAccount.setAccountNumber(db2Account.getAccountNumber());
			myDeletedAccount.setType(db2Account.getType());
			myDeletedAccount.setCustomerNumber(db2Account.getCustomerNumber());
			myDeletedAccount.setSortCode(db2Account.getSortcode());
			myDeletedAccount.setNextStatement(db2Account.getNextStatement());
			myDeletedAccount.setLastStatement(db2Account.getLastStatement());
			myDeletedAccount.setActualBalance(BigDecimal.valueOf(db2Account.getActualBalance()));


			Response deletedAccountResponse = myProcessedTransactionResource.writeDeleteAccountInternal(myDeletedAccount);
			if(deletedAccountResponse == null || deletedAccountResponse.getStatus() != 200)
			{
				JSONObject error = new JSONObject();
				error.put(ERROR_MSG, PROCTRAN_WRITE_FAILURE);
				logger.severe(PROCTRAN_WRITE_FAILURE);
				try {
					Task.getTask().rollback();
				} catch (InvalidRequestException e) {
				}
				myResponse = Response.status(500).entity(error.toString()).build();
				logger.exiting(this.getClass().getName(),DELETE_ACCOUNT,myResponse);
				return myResponse;
			}
		}
		else
		{
			try 
			{
				logger.warning("Accounts: deleteAccount: Failed to find account " + accountNumber);
				Task.getTask().rollback();
			} 
			catch (InvalidRequestException e) 
			{
				logger.severe(e.toString());
			}
			response.put(ERROR_MSG,ACCOUNT_LITERAL + accountNumber + " not found");
			myResponse = Response.status(404).entity(response.toString()).build();
			logger.exiting(this.getClass().getName(),DELETE_ACCOUNT,myResponse);
			return myResponse;
		}
		/*
		 * Parse returned data and return to calling method
		 */



		myResponse = Response.status(200).entity(response.toString()).build();
		logger.exiting(this.getClass().getName(),DELETE_ACCOUNT,myResponse);
		return myResponse;

	}

	@GET
	@Produces("application/json")
	public Response getAccountsExternal(@QueryParam("limit") Integer limit, @QueryParam("offset") Integer offset,@QueryParam("countOnly") Boolean countOnly)
	{
		// This method returns a fixed number of accounts, up to limit "limit", starting at offset "offset"
		logger.entering(this.getClass().getName(), "getAccountsExternal(Integer limit, Integer offset,Boolean countOnly)");
		boolean countOnlyReal = false;
		if(countOnly != null)
		{
			countOnlyReal = countOnly.booleanValue();
		}
		Response myResponse = getAccountsInternal(limit,offset,countOnlyReal); 
		HBankDataAccess myHBankDataAccess = new HBankDataAccess();
		myHBankDataAccess.terminate();
		logger.exiting(this.getClass().getName(), "getAccountsExternal(Integer limit, Integer offset,Boolean countOnly)",myResponse);
		return myResponse;
	}

	public Response getAccountsInternal(Integer limit, Integer offset,boolean countOnly)
	{
		logger.entering(this.getClass().getName(), "getAccountsInternal(Integer limit, Integer offset,boolean countOnly)");
		Response myResponse = null;

		com.ibm.cics.cip.bankliberty.web.db2.Account db2Account = null;
		JSONObject response = new JSONObject();
		JSONArray accounts = null;
		int numberOfAccounts = 0;
		Integer sortCode = this.getSortCode();
// We want to set a limit to try to avoid OutOfMemory Exceptions. 250,000 seems a bit large
		if(limit == null)
		{
			limit = new Integer(250000);
		}
		if(limit == 0)
		{
			limit = new Integer(250000);
		}
		if(offset == null)
		{
			offset = new Integer(0);
		}



		if(countOnly)
		{
			db2Account = new com.ibm.cics.cip.bankliberty.web.db2.Account();
			numberOfAccounts = db2Account.getAccountsCountOnly(sortCode.intValue());
		}
		else
		{
			Account[] myAccounts = null;

			db2Account = new Account();

			myAccounts = db2Account.getAccounts(sortCode,limit,offset);
			if(myAccounts == null)
			{
				response.put(ERROR_MSG,"Accounts cannot be accessed");
				logger.severe("Accounts cannot be accessed");
				myResponse = Response.status(500).entity(response.toString()).build();
				logger.exiting(this.getClass().getName(), "getAccountsInternal(Integer limit, Integer offset,boolean countOnly)",myResponse);
				return myResponse;	
			}
			accounts = new JSONArray(myAccounts.length);

			for(int i = 0;i < myAccounts.length;i++)
			{
				JSONObject account = new JSONObject();
				account.put(JSON_SORT_CODE, myAccounts[i].getSortcode().trim());
				account.put("id", myAccounts[i].getAccountNumber());
				account.put(JSON_CUSTOMER_NUMBER, myAccounts[i].getCustomerNumber());
				account.put(JSON_ACCOUNT_TYPE, myAccounts[i].getType().trim());
				account.put(JSON_AVAILABLE_BALANCE, BigDecimal.valueOf(myAccounts[i].getAvailableBalance()));
				account.put(JSON_ACTUAL_BALANCE, BigDecimal.valueOf(myAccounts[i].getActualBalance()));
				account.put(JSON_INTEREST_RATE, BigDecimal.valueOf(myAccounts[i].getInterestRate()));
				account.put(JSON_OVERDRAFT, myAccounts[i].getOverdraftLimit());
				account.put(JSON_LAST_STATEMENT_DATE, myAccounts[i].getLastStatement().toString().trim());
				account.put(JSON_NEXT_STATEMENT_DATE, myAccounts[i].getNextStatement().toString().trim());
				account.put(JSON_DATE_OPENED, myAccounts[i].getOpened().toString());

				accounts.add(account);

			}
			numberOfAccounts = accounts.size();
		}
		/*
		 * Parse returned data and return to calling method
		 */

		response.put(JSON_NUMBER_OF_ACCOUNTS, numberOfAccounts);
		if(accounts != null)
		{
			response.put(JSON_ACCOUNTS, accounts);
		}
		myResponse = Response.status(200).entity(response.toString()).build(); 
		logger.exiting(this.getClass().getName(),DELETE_ACCOUNT,myResponse);
		return myResponse;
	}

	@GET
	@Path("/balance")
	@Produces("application/json")
	public Response getAccountsByBalanceWithOffsetAndLimitExternal(@QueryParam("balance") BigDecimal balance, @QueryParam("operator") String operator, @QueryParam("offset") Integer offset, @QueryParam("limit") Integer limit,@QueryParam("countOnly") Boolean countOnly)
	{
		// return only accounts with a certain balance
		logger.entering(this.getClass().getName(),"getAccountsByBalanceWithOffsetAndLimitExternal(BigDecimal balance, String operator, Integer offset, Integer limit, Boolean countOnly");
		boolean countOnlyReal = false;
		if(countOnly != null)
		{
			countOnlyReal = countOnly.booleanValue();
		}

		Response myResponse = getAccountsByBalanceWithOffsetAndLimitInternal(balance,operator,offset,limit,countOnlyReal); 
		HBankDataAccess myHBankDataAccess = new HBankDataAccess();
		myHBankDataAccess.terminate();
		logger.exiting(this.getClass().getName(),"getAccountsByBalanceWithOffsetAndLimitExternal(BigDecimal balance, String operator, Integer offset, Integer limit, Boolean countOnly",myResponse);
		return myResponse;
	}

	public Response getAccountsByBalanceWithOffsetAndLimitInternal(@QueryParam("balance") BigDecimal balance, @QueryParam("operator") String operator, @QueryParam("offset") Integer offset, @QueryParam("limit") Integer limit,boolean countOnly)	
	{
		// return only accounts with a certain balance
		logger.entering(this.getClass().getName(),GET_ACCOUNTS_BY_BALANCE_WITH_OFFSET_AND_LIMIT_INTERNAL);
		Response myResponse = null;
		boolean lessThan;
		if(operator.startsWith("<"))
		{
			lessThan = true;
		}
		else
		{
			if(operator.startsWith(">"))
			{
				lessThan = false;
			}
			else
			{
				JSONObject error = new JSONObject();
				error.put(ERROR_MSG, "Invalid operator, '" + operator + "' only <= or >= allowed");
				logger.warning("Invalid operator, '" + operator + "' only <= or >= allowed");
				logger.exiting(this.getClass().getName(),GET_ACCOUNTS_BY_BALANCE_WITH_OFFSET_AND_LIMIT_INTERNAL,myResponse);
				myResponse = Response.status(400).entity(error.toString()).build();
				return myResponse;
			}
		}

		
		
		if(offset == null)
		{
			offset = new Integer(0);
		}
		// We want to set a limit to try to avoid OutOfMemory Exceptions. 250,000 seems a bit large

		if(limit == null)
		{
			limit = new Integer(250000);
		}
		if(limit.intValue() == 0)
		{
			limit = new Integer(250000);
		}



		com.ibm.cics.cip.bankliberty.web.db2.Account db2Account = null;
		JSONObject response = new JSONObject();
		JSONArray accounts = null;
		int numberOfAccounts = 0;
		Integer sortCode = this.getSortCode();

		if(countOnly)
		{
			db2Account = new Account();
			numberOfAccounts = db2Account.getAccountsByBalanceCountOnly(sortCode,balance,lessThan,offset,limit);
			if(numberOfAccounts == -1)
			{
				JSONObject error = new JSONObject();
				error.put(ERROR_MSG, DB2_READ_FAILURE);
				logger.severe(DB2_READ_FAILURE);
				logger.exiting(this.getClass().getName(),GET_ACCOUNTS_BY_BALANCE_WITH_OFFSET_AND_LIMIT_INTERNAL,myResponse);
				myResponse = Response.status(500).entity(error.toString()).build();
				return myResponse;
			}
		}
		else
		{
			Account[] myAccounts = null;

			db2Account = new Account();

			myAccounts = db2Account.getAccountsByBalance(sortCode,balance,lessThan,offset,limit);

			if(myAccounts == null)
			{
				JSONObject error = new JSONObject();
				error.put(ERROR_MSG, DB2_READ_FAILURE);
				logger.severe(DB2_READ_FAILURE);
				logger.exiting(this.getClass().getName(),GET_ACCOUNTS_BY_BALANCE_WITH_OFFSET_AND_LIMIT_INTERNAL,myResponse);
				myResponse = Response.status(500).entity(error.toString()).build();
				return myResponse;
			}
			accounts = new JSONArray(myAccounts.length);



			for(int i = 0;i < myAccounts.length;i++)
			{
				JSONObject account = new JSONObject();
				account.put(JSON_SORT_CODE, myAccounts[i].getSortcode().trim());
				account.put("id", myAccounts[i].getAccountNumber());
				account.put(JSON_CUSTOMER_NUMBER, myAccounts[i].getCustomerNumber());
				account.put(JSON_ACCOUNT_TYPE, myAccounts[i].getType().trim());
				account.put(JSON_AVAILABLE_BALANCE, BigDecimal.valueOf(myAccounts[i].getAvailableBalance()));
				account.put(JSON_ACTUAL_BALANCE, BigDecimal.valueOf(myAccounts[i].getActualBalance()));
				account.put(JSON_INTEREST_RATE, BigDecimal.valueOf(myAccounts[i].getInterestRate()));
				account.put(JSON_OVERDRAFT, myAccounts[i].getOverdraftLimit());
				account.put(JSON_LAST_STATEMENT_DATE, myAccounts[i].getLastStatement().toString().trim());
				account.put(JSON_NEXT_STATEMENT_DATE, myAccounts[i].getNextStatement().toString().trim());
				account.put(JSON_DATE_OPENED, myAccounts[i].getOpened().toString());

				accounts.add(account);

			}
			numberOfAccounts = accounts.size();
		}


		/*
		 * Parse returned data and return to calling method
		 */

		response.put(JSON_NUMBER_OF_ACCOUNTS, numberOfAccounts);
		response.put(JSON_ACCOUNTS, accounts);
		myResponse = Response.status(200).entity(response.toString()).build(); 
		logger.exiting(this.getClass().getName(),GET_ACCOUNTS_BY_BALANCE_WITH_OFFSET_AND_LIMIT_INTERNAL,myResponse);
		return myResponse;


	}





}



