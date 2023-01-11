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
	
	private static final String notSupported = " is not supported.";
	private static final String errorMsg = "errorMessage";
	private static final String accTypeString = "Account type ";
	private static final String createAccountInternal = "createAccountInternal(AccountJSON account)";
	private static final String createAccountExternal = "createAccountExternal(AccountJSON account)";
	private static final String getAccountInternal = "getAccountInternal(Long accountNumber)";
	private static final String getAccountsByCustomerNumberInternal = "getAccountsByCustomerInternal(Long customerNumber, boolean countOnly)";
	private static final String updateAccountInternal = "updateAccountInternal(Long id, AccountJSON account)";
	private static final String debitAccountInternal = "debitAccountInternal(String accountNumber, DebitCreditAccountJSON dbcr)";
	private static final String creditAccountInternal = "creditAccountInternal(String accountNumber, DebitCreditAccountJSON dbcr)";
	private static final String transferLocalInternal = "transferLocalInternal(String accountNumber, TransferLocalJSON transferLocal)";
	private static final String debitCreditAccount = "debitCreditAccount(Long sortCode, String accountNumber, BigDecimal apiAmount, boolean debitAccount)";
	private static final String deleteAccount = "deleteAccountInternal(Long accountNumber)";
	private static final String getAccountsByBalanceWithOffsetAndLimitInternal = "getAccountsByBalanceWithOffsetAndLimitInternal(BigDecimal balance, String operator, Integer offset, Integer limit, Boolean countOnly";
	
	private static final String classNameMsg = " in com.ibm.cics.cip.bankliberty.web.db2.Account";
	
	private static final String interestRateLessThanZero = "Interest rate cannot be greater than 9999.99%.";
	private static final String interestRateTooHigh = "Interest rate cannot be greater than 9999.99%.";
	private static final String getAccountsExternal = "getAccountsExternal(Boolean countOnly)";
	private static final String getAccountsInternal = "getAccountsInternal(Boolean countOnly)";
	private static final String notValidForThisBank =  "not valid for this bank (";
	private static final String sortCodeLiteral = "Sortcode ";
	private static final String accountLiteral = "Account ";
	private static final String cannotBeFound = " cannot be found.";
	private static final String cannotBeAccessed = " cannot be accessed.";
	private static final String customerNumberLiteral = "Customer number ";
	private static final String failedToRead = "Failed to read account ";
	private static final String proctranWriteFailure = "Failed to write to PROCTRAN data store";
	private static final String db2ReadFailure = "Unable to access Db2 account store";
	private static final String accountCreateFailure = "Failed to create account in com.ibm.cics.cip.bankliberty.web.db2.Account";
	private static final String inDebitAccount = " in debitAccount ";
	private static final String inCreditAccount = " in creditAccount ";
	private static final String needDifferentAccounts = "Source and target accounts must be different";
	private static final String sourceAccountNumber = "Source account number ";
	private static final String targetAccountNumber = "Target account number ";
	
	private static final String jsonNumberOfAccounts = "numberOfAccounts";
	private static final String jsonSortCode = "sortCode";
	private static final String jsonCustomerNumber = "customerNumber";
	private static final String jsonAccountType = "accountType";
	private static final String jsonAvailableBalance = "availableBalance";
	private static final String jsonActualBalance = "actualBalance";
	private static final String jsonInterestRate = "interestRate";
	private static final String jsonOverdraft = "overdraft";
	private static final String jsonLastStatementDate = "lastStatementDate";
	private static final String jsonNextStatementDate = "nextStatementDate";
	private static final String jsonDateOpened = "dateOpened";
	private static final String jsonAccounts = "accounts";
	



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
		logger.entering(this.getClass().getName(),createAccountExternal + " for account " + account.toString());

		Response myResponse = createAccountInternal(account);

		HBankDataAccess myHBankDataAccess = new HBankDataAccess();
		myHBankDataAccess.terminate();
		logger.exiting(this.getClass().getName(),createAccountExternal,myResponse);
		return myResponse;

	}

	public Response createAccountInternal(
			/** Internal methods can be called by either the external methods, or another part of Liberty */
			AccountJSON account) {
		logger.entering(this.getClass().getName(),createAccountInternal + " for account " + account.toString());
		Response myResponse = null;
		/* Only ISA, LOAN, CURRENT, MORTGAGE and SAVING are valid account types */
		if(!account.validateType(account.getAccountType().trim()))
		{
			JSONObject error = new JSONObject();
			error.put(errorMsg, accTypeString + account.getAccountType() + notSupported);
			logger.warning(accTypeString + account.getAccountType() + notSupported);
			myResponse = Response.status(400).entity(error.toString()).build();
			logger.exiting(this.getClass().getName(),createAccountInternal ,myResponse);
			return myResponse;

		}
		//Interest rate cannot be < 0
		if(account.getInterestRate().doubleValue() < 0.00)
		{
			JSONObject error = new JSONObject();
			error.put(errorMsg, interestRateLessThanZero);
			logger.warning(interestRateLessThanZero);
			myResponse = Response.status(400).entity(error.toString()).build();
			logger.exiting(this.getClass().getName(),createAccountInternal ,myResponse);
			return myResponse;
		}

		//Interest rate cannot be > 9999.99%
		if(account.getInterestRate().doubleValue() > 9999.99)
		{
			JSONObject error = new JSONObject();
			error.put(errorMsg, interestRateTooHigh);
			logger.warning(interestRateTooHigh);
			myResponse = Response.status(400).entity(error.toString()).build();
			logger.exiting(this.getClass().getName(),createAccountInternal ,myResponse);
			return myResponse;
		}

		//Interest rate cannot have more than 2dp
		BigDecimal myInterestRate = account.getInterestRate();
		if(myInterestRate.scale() > 2)
		{
			JSONObject error = new JSONObject();
			error.put(errorMsg, "Interest rate cannot have more than 2 decimal places. " + myInterestRate.toPlainString());
			logger.warning("Interest rate cannot have more than 2 decimal places."  + myInterestRate.toPlainString());
			myResponse = Response.status(400).entity(error.toString()).build();
			logger.exiting(this.getClass().getName(),createAccountInternal ,myResponse);
			return myResponse;			
		}

		//Overdraft limit cannot be < 0
		if(account.getOverdraft().intValue() < 0)
		{
			JSONObject error = new JSONObject();
			error.put(errorMsg, "Overdraft limit cannot be less than zero.");
			logger.warning("Overdraft limit cannot be less than zero.");
			myResponse = Response.status(400).entity(error.toString()).build();
			logger.exiting(this.getClass().getName(),createAccountInternal ,myResponse);
			return myResponse;			
		}

		//Customer number cannot be < 1
		Long customerNumberLong = new Long(account.getCustomerNumber());
		if(customerNumberLong.longValue() < 1)
		{
			JSONObject error = new JSONObject();
			error.put(errorMsg, "Customer number cannot be less than one.");
			logger.warning("Customer number cannot be less than one.");
			myResponse = Response.status(400).entity(error.toString()).build();
			logger.exiting(this.getClass().getName(),createAccountInternal ,myResponse);
			return myResponse;
		}
		
		//Customer number cannot be 9999999999
		if(customerNumberLong.longValue() == 9999999999L)
		{
			JSONObject error = new JSONObject();
			error.put(errorMsg, "Customer number cannot be 9,999,999,999.");
			logger.warning("Customer number cannot be 9,999,999,999.");
			myResponse = Response.status(400).entity(error.toString()).build();
			logger.exiting(this.getClass().getName(),createAccountInternal ,myResponse);
			return myResponse;
		}
		
		//Sortcode is not valid for this bank
		Integer inputSortCode = new Integer(account.getSortCode());
		Integer thisSortCode = this.getSortCode();

		if(inputSortCode.intValue() != thisSortCode.intValue())
		{
			JSONObject error = new JSONObject();
			error.put(errorMsg, sortCodeLiteral+  inputSortCode + notValidForThisBank + thisSortCode + ")");
			logger.warning(sortCodeLiteral+  inputSortCode + notValidForThisBank + thisSortCode + ")");
			myResponse = Response.status(400).entity(error.toString()).build();
			logger.exiting(this.getClass().getName(),createAccountInternal ,myResponse);
			return myResponse;
		}





		CustomerResource myCustomer = new CustomerResource();
		Response customerResponse = myCustomer.getCustomerInternal(customerNumberLong);
		// Customer number cannot be found
		if(customerResponse.getStatus() != 200)
		{
			JSONObject error = new JSONObject();
			error.put(errorMsg, customerNumberLiteral + customerNumberLong.longValue() + cannotBeFound);
			logger.warning(customerNumberLiteral + customerNumberLong.longValue() + cannotBeFound);
			myResponse = Response.status(404).entity(error.toString()).build();
			logger.exiting(this.getClass().getName(),createAccountInternal ,myResponse);
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
					error.put(errorMsg, customerNumberLiteral + customerNumberLong.longValue() + cannotBeFound);
					logger.warning(customerNumberLiteral + customerNumberLong.longValue() + cannotBeFound);
					myResponse = Response.status(404).entity(error.toString()).build();
					logger.exiting(this.getClass().getName(),createAccountInternal ,myResponse);
					return myResponse;
				}
				JSONObject error = new JSONObject();
				error.put(errorMsg, customerNumberLiteral + customerNumberLong.longValue() + cannotBeAccessed);
				logger.severe(customerNumberLiteral + customerNumberLong.longValue() + cannotBeAccessed);
				myResponse = Response.status(accountsOfThisCustomer.getStatus()).entity(error.toString()).build();
				logger.exiting(this.getClass().getName(),createAccountInternal ,myResponse);
				return myResponse;

			}
			String accountsOfThisCustomerString = accountsOfThisCustomer.getEntity().toString();
			myAccountsJSON = JSONObject.parse(accountsOfThisCustomerString);
		} catch (IOException e) {
			JSONObject error = new JSONObject();
			error.put(errorMsg,"Failed to retrieve customer number " + customerNumberLong + " " + e.getLocalizedMessage());
			logger.severe("Failed to retrieve customer number " + customerNumberLong + " " + e.getLocalizedMessage());
			myResponse = Response.status(500).entity(error.toString()).build();
			logger.exiting(this.getClass().getName(),createAccountInternal ,myResponse);
			return myResponse;
		}
		long accountCount = (Long) myAccountsJSON.get(jsonNumberOfAccounts);

		// Does the customer have ten or more accounts?

		if(accountCount >= 10)
		{
			JSONObject error = new JSONObject();
			error.put(errorMsg, customerNumberLiteral + customerNumberLong.longValue() + " cannot have more than ten accounts.");
			logger.warning(customerNumberLiteral + customerNumberLong.longValue() + " cannot have more than ten accounts.");
			myResponse = Response.status(400).entity(error.toString()).build();
			logger.exiting(this.getClass().getName(),createAccountInternal ,myResponse);
			return myResponse;
		}

		com.ibm.cics.cip.bankliberty.web.db2.Account db2Account = new com.ibm.cics.cip.bankliberty.web.db2.Account();
		db2Account.setSortcode(this.getSortCode().toString());
		db2Account = db2Account.createAccount(account, this.getSortCode(),true);
		//Add data to JSONObject
		if(db2Account != null)
		{
			response.put(jsonSortCode, db2Account.getSortcode().trim());
			response.put("id", db2Account.getAccount_number());
			response.put(jsonCustomerNumber, db2Account.getCustomer_number());
			response.put(jsonAccountType, db2Account.getType().trim());
			response.put(jsonAvailableBalance,  BigDecimal.valueOf(db2Account.getAvailable_balance()));
			response.put(jsonActualBalance, BigDecimal.valueOf(db2Account.getActual_balance()));
			response.put(jsonInterestRate, BigDecimal.valueOf(db2Account.getInterest_rate()));
			response.put(jsonOverdraft, db2Account.getOverdraft_limit());
			response.put(jsonLastStatementDate, db2Account.getLast_statement().toString().trim());
			response.put(jsonNextStatementDate, db2Account.getNext_statement().toString().trim());
			response.put(jsonDateOpened, db2Account.getOpened().toString().trim());

			//Create a new ProcessedTransactionAccount and set credentials
			ProcessedTransactionResource myProcessedTransactionResource = new ProcessedTransactionResource();
			ProcessedTransactionAccountJSON myProctranAccount = new ProcessedTransactionAccountJSON();
			myProctranAccount.setSortCode(db2Account.getSortcode());
			myProctranAccount.setAccountNumber(db2Account.getAccount_number());
			myProctranAccount.setCustomerNumber(new Long(db2Account.getCustomer_number()).toString());
			myProctranAccount.setLastStatement(db2Account.getLast_statement());
			myProctranAccount.setNextStatement(db2Account.getNext_statement());
			myProctranAccount.setType(db2Account.getType());
			myProctranAccount.setActualBalance(BigDecimal.valueOf(db2Account.getActual_balance()));



			Response writeCreateAccountResponse = myProcessedTransactionResource.writeCreateAccountInternal(myProctranAccount);
			if(writeCreateAccountResponse == null || writeCreateAccountResponse.getStatus() != 200)
			{
				JSONObject error = new JSONObject();
				error.put(errorMsg, proctranWriteFailure);
				try {
					logger.severe("Accounts: createAccount: " +proctranWriteFailure);
					Task.getTask().rollback();
				} catch (InvalidRequestException e) {
				}
				logger.severe(accountCreateFailure);
				myResponse = Response.status(500).entity(error.toString()).build();
				logger.exiting(this.getClass().getName(),createAccountInternal ,myResponse);
				return myResponse;
			}

		}
		else
		{
			JSONObject error = new JSONObject();
			error.put(errorMsg, accountCreateFailure);
			logger.severe(accountCreateFailure);
			myResponse = Response.status(500).entity(error.toString()).build();
			logger.exiting(this.getClass().getName(),createAccountInternal ,myResponse);
			return myResponse;
		}

		myResponse = Response.status(201).entity(response.toString()).build();
		logger.exiting(this.getClass().getName(),createAccountInternal ,myResponse);
		return myResponse;

	}

	@GET
	@Produces("application/json")
	public Response getAccountsExternal(@QueryParam("countOnly") Boolean countOnly)
	{
		/** This will list all accounts. The countOnly boolean indicates that we just want the number of accounts, not every  single account */
		logger.entering(this.getClass().getName(),getAccountsExternal);
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
		logger.exiting(this.getClass().getName(),getAccountsExternal,myResponse);
		return myResponse;
	}



	public Response getAccountsInternal(boolean countOnly) {
		/** This will list all accounts. The countOnly boolean indicates that we just want the number of accounts, not every  single account */
		logger.entering(this.getClass().getName(),getAccountsInternal);
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
				response.put(errorMsg,"Accounts cannot be accessed in com.ibm.cics.cip.bankliberty.web.db2.Account");
				logger.severe("Accounts cannot be accessed in com.ibm.cics.cip.bankliberty.web.db2.Account");
				myResponse = Response.status(500).entity(response.toString()).build();
				logger.exiting(this.getClass().getName(),getAccountsInternal,myResponse);
				return myResponse;
			}
			accounts = new JSONArray(myAccounts.length);

			for(int i = 0;i < myAccounts.length;i++)
			{
				JSONObject account = new JSONObject();
				account.put(jsonSortCode, myAccounts[i].getSortcode().trim());
				account.put("id", myAccounts[i].getAccount_number());
				account.put(jsonCustomerNumber, myAccounts[i].getCustomer_number());
				account.put(jsonAccountType, myAccounts[i].getType().trim());
				account.put(jsonAvailableBalance, BigDecimal.valueOf(myAccounts[i].getAvailable_balance()).setScale(2,RoundingMode.HALF_UP));
				account.put(jsonActualBalance, BigDecimal.valueOf(myAccounts[i].getActual_balance()).setScale(2,RoundingMode.HALF_UP));
				account.put(jsonInterestRate, BigDecimal.valueOf(myAccounts[i].getInterest_rate()).setScale(2,RoundingMode.HALF_UP));
				account.put(jsonOverdraft, myAccounts[i].getOverdraft_limit());
				account.put(jsonLastStatementDate, myAccounts[i].getLast_statement().toString().trim());
				account.put(jsonNextStatementDate, myAccounts[i].getNext_statement().toString().trim());
				account.put(jsonDateOpened, myAccounts[i].getOpened().toString());

				accounts.add(account);
			}
			numberOfAccounts = accounts.size();
		}

		/*
		 * Parse returned data and return to calling method
		 */

		response.put(jsonNumberOfAccounts, numberOfAccounts);
		if(accounts != null)
		{
			response.put(jsonAccounts, accounts);
		}
		myResponse = Response.status(200).entity(response.toString()).build();
		logger.exiting(this.getClass().getName(),getAccountsInternal,myResponse);
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
		logger.entering(this.getClass().getName(),getAccountInternal);
		Response myResponse = null;
		JSONObject response = new JSONObject();

		Integer sortCode = this.getSortCode();
		Long id_safe = accountNumber;

		com.ibm.cics.cip.bankliberty.web.db2.Account db2Account = new Account();

		db2Account = db2Account.getAccount(accountNumber.intValue(), sortCode);
		if(db2Account != null)
		{
			
			response.put(jsonSortCode, db2Account.getSortcode().trim());
			response.put("id", db2Account.getAccount_number());
			response.put(jsonCustomerNumber, db2Account.getCustomer_number());
			response.put(jsonAccountType, db2Account.getType().trim());
			response.put(jsonAvailableBalance, BigDecimal.valueOf(db2Account.getAvailable_balance()));
			response.put(jsonActualBalance, BigDecimal.valueOf(db2Account.getActual_balance()));
			response.put(jsonInterestRate, BigDecimal.valueOf(db2Account.getInterest_rate()));
			response.put(jsonOverdraft, db2Account.getOverdraft_limit());
			response.put(jsonLastStatementDate, db2Account.getLast_statement().toString().trim());
			response.put(jsonNextStatementDate, db2Account.getNext_statement().toString().trim());
			response.put(jsonDateOpened, db2Account.getOpened().toString().trim());
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
					response.put(jsonSortCode, db2Account.getSortcode().trim());
					response.put("id", db2Account.getAccount_number());
					response.put(jsonCustomerNumber, db2Account.getCustomer_number());
					response.put(jsonAccountType, db2Account.getType().trim());
					response.put(jsonAvailableBalance, BigDecimal.valueOf(db2Account.getAvailable_balance()));
					response.put(jsonActualBalance, BigDecimal.valueOf(db2Account.getActual_balance()));
					response.put(jsonInterestRate, BigDecimal.valueOf(db2Account.getInterest_rate()));
					response.put(jsonOverdraft, db2Account.getOverdraft_limit());
					response.put(jsonLastStatementDate, db2Account.getLast_statement().toString().trim());
					response.put(jsonNextStatementDate, db2Account.getNext_statement().toString().trim());
					response.put(jsonDateOpened, db2Account.getOpened().toString().trim());
				}
				//* There exists a possibility that the last account has been deleted. In which case we try once the old fashioned way
			}
			if(db2Account == null)
			{
				response.put(errorMsg,accountLiteral + accountNumber + " not found in com.ibm.cics.cip.bankliberty.web.db2.Account");
				logger.warning(accountLiteral + accountNumber + " not found in com.ibm.cics.cip.bankliberty.web.db2.Account");
				myResponse = Response.status(404).entity(response.toString()).build();
				logger.exiting(this.getClass().getName(),getAccountInternal,myResponse);
				return myResponse;
			}
		}

		myResponse = Response.status(200).entity(response.toString()).build();
		logger.exiting(this.getClass().getName(),getAccountInternal,myResponse);

		return myResponse;
	}


	@GET
	@Path("/retrieveByCustomerNumber/{customerNumber}")
	@Produces("application/json")
	public Response getAccountsByCustomerExternal(@PathParam(jsonCustomerNumber) Long customerNumber, @QueryParam("countOnly") Boolean countOnly) {
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

	public Response getAccountsByCustomerInternal(@PathParam(jsonCustomerNumber) Long customerNumber,boolean countOnly) {
		logger.entering(this.getClass().getName(),getAccountsByCustomerNumberInternal);

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
				error.put(errorMsg, customerNumberLiteral + customerNumber.longValue() + cannotBeFound);
				logger.severe(customerNumberLiteral + customerNumber.longValue() + cannotBeFound);
				myResponse = Response.status(404).entity(error.toString()).build(); 
				logger.exiting(this.getClass().getName(),getAccountsByCustomerNumberInternal,myResponse);
				return myResponse;
			}
			else
			{
				JSONObject error = new JSONObject();
				error.put(errorMsg, customerNumberLiteral + customerNumber.longValue() + " cannot be accessed. " + customerResponse.toString());
				logger.severe(customerNumberLiteral + customerNumber.longValue() + " cannot be accessed. " + customerResponse.toString());
				myResponse = Response.status(customerResponse.getStatus()).entity(error.toString()).build(); 
				logger.exiting(this.getClass().getName(),getAccountsByCustomerNumberInternal,myResponse);
				return myResponse;
			}
		}


		com.ibm.cics.cip.bankliberty.web.db2.Account db2Account = new Account();
		Account[] myAccounts = db2Account.getAccounts(customerNumber.intValue(), sortCode);
		if(myAccounts == null)
		{
			JSONObject error = new JSONObject();
			error.put(errorMsg, "Accounts cannot be accessed for customer " + customerNumber.longValue() + classNameMsg);
			logger.severe("Accounts cannot be accessed for customer " + customerNumber.longValue() + classNameMsg);
			myResponse = Response.status(500).entity(error.toString()).build(); 
			logger.exiting(this.getClass().getName(),getAccountsByCustomerNumberInternal,myResponse);
			return myResponse;	
		}

		numberOfAccounts = myAccounts.length;
		accounts = new JSONArray(numberOfAccounts);
		for (int i = 0; i < numberOfAccounts; i++) {


			JSONObject account = new JSONObject();
			account.put(jsonSortCode, myAccounts[i].getSortcode());
			account.put("id", myAccounts[i].getAccount_number());
			account.put(jsonCustomerNumber, myAccounts[i].getCustomer_number());
			account.put(jsonAccountType, myAccounts[i].getType());
			account.put(jsonAvailableBalance, BigDecimal.valueOf(myAccounts[i].getAvailable_balance()));
			account.put(jsonActualBalance, BigDecimal.valueOf(myAccounts[i].getActual_balance()));
			account.put(jsonInterestRate, BigDecimal.valueOf(myAccounts[i].getInterest_rate()));
			account.put(jsonOverdraft, myAccounts[i].getOverdraft_limit());
			account.put(jsonLastStatementDate, myAccounts[i].getLast_statement().toString());
			account.put(jsonNextStatementDate, myAccounts[i].getNext_statement().toString());
			account.put(jsonDateOpened, myAccounts[i].getOpened().toString());

			accounts.add(account);
		}
		String customerNumberString = new String(customerNumber.toString());
		for(int i=10;customerNumberString.length()<10;i--)
		{
			customerNumberString = "0" + customerNumberString;
		}
		response.put(jsonCustomerNumber, customerNumberString);
		response.put(jsonNumberOfAccounts, numberOfAccounts);
		response.put(jsonAccounts, accounts);


		myResponse = Response.status(200).entity(response.toString()).build(); 
		logger.exiting(this.getClass().getName(),getAccountsByCustomerNumberInternal,myResponse);
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
		logger.entering(this.getClass().getName(),updateAccountInternal);
		JSONObject response = new JSONObject();
		Response myResponse = null;


		if(!(account.validateType(account.getAccountType().trim())))
			//If account type invalid
		{
			JSONObject error = new JSONObject();
			error.put(errorMsg, accTypeString + account.getAccountType() + notSupported);
			logger.warning(accTypeString + account.getAccountType() + notSupported);
			myResponse = Response.status(400).entity(error.toString()).build(); 
			logger.exiting(this.getClass().getName(),updateAccountInternal,myResponse);
			return myResponse;
		}

		if(account.getInterestRate().doubleValue() < 0.00)
		{
			//If interest rate < 0
			JSONObject error = new JSONObject();
			error.put(errorMsg, interestRateLessThanZero);
			logger.warning(interestRateLessThanZero);
			myResponse = Response.status(400).entity(error.toString()).build(); 
			logger.exiting(this.getClass().getName(),updateAccountInternal,myResponse);
			return myResponse;
		}

		if(account.getInterestRate().doubleValue() > 9999.99)
		{
			//If interest rate > 9999.99
			JSONObject error = new JSONObject();
			error.put(errorMsg, interestRateTooHigh);
			logger.warning(interestRateTooHigh);
			myResponse = Response.status(400).entity(error.toString()).build(); 
			logger.exiting(this.getClass().getName(),updateAccountInternal,myResponse);
			return myResponse;

		}

		BigDecimal myInterestRate = account.getInterestRate();
		if(myInterestRate.scale() > 2)
			//Interest rate more than 2dp
		{
			JSONObject error = new JSONObject();
			error.put(errorMsg, "Interest rate cannot have more than 2 decimal places. " + myInterestRate.toPlainString());
			logger.warning("Interest rate cannot have more than 2 decimal places."  + myInterestRate.toPlainString());
			myResponse = Response.status(400).entity(error.toString()).build(); 
			logger.exiting(this.getClass().getName(),updateAccountInternal,myResponse);
			return myResponse;
		}

		Integer inputSortCode = new Integer(account.getSortCode());
		Integer thisSortCode = this.getSortCode();

		if(inputSortCode.intValue() != thisSortCode.intValue())
			//Invalid sortcode
		{
			JSONObject error = new JSONObject();
			error.put(errorMsg, sortCodeLiteral+  inputSortCode + notValidForThisBank + thisSortCode + ")");
			logger.warning(sortCodeLiteral+  inputSortCode + notValidForThisBank + thisSortCode + ")");
			myResponse = Response.status(400).entity(error.toString()).build(); 
			logger.exiting(this.getClass().getName(),updateAccountInternal,myResponse);
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
				response.put(jsonSortCode, db2Account.getSortcode().trim());
				response.put("id", db2Account.getAccount_number());
				response.put(jsonCustomerNumber,db2Account.getCustomer_number());
				response.put(jsonAccountType, db2Account.getType().trim());
				response.put(jsonAvailableBalance, BigDecimal.valueOf(db2Account.getAvailable_balance()));
				response.put(jsonActualBalance, BigDecimal.valueOf(db2Account.getActual_balance()));
				response.put(jsonInterestRate, BigDecimal.valueOf(db2Account.getInterest_rate()));				
				response.put(jsonOverdraft, db2Account.getOverdraft_limit());
				response.put(jsonLastStatementDate, db2Account.getLast_statement().toString());
				response.put(jsonNextStatementDate, db2Account.getNext_statement().toString());
				response.put(jsonDateOpened, db2Account.getOpened().toString().trim());
			}
			else
			{
				JSONObject error = new JSONObject();
				error.put(errorMsg, "Failed to update account in com.ibm.cics.cip.bankliberty.web.db2.Account");
				logger.severe("Failed to update account in com.ibm.cics.cip.bankliberty.web.db2.Account");
				myResponse = Response.status(500).entity(error.toString()).build(); 
				logger.exiting(this.getClass().getName(),updateAccountInternal,myResponse);
				return myResponse;
			}
		}
		else
		{
			JSONObject error = new JSONObject();
			error.put(errorMsg, failedToRead + account.getId() + " in " + this.getClass().toString());

			logger.warning(failedToRead + account.getId() + classNameMsg);
			myResponse = Response.status(404).entity(error.toString()).build(); 
			logger.exiting(this.getClass().getName(),updateAccountInternal,myResponse);
			return myResponse;
		}
		myResponse = Response.status(200).entity(response.toString()).build(); 
		logger.exiting(this.getClass().getName(),updateAccountInternal,myResponse);

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
		logger.entering(this.getClass().getName(),debitAccountInternal);
		Response myResponse = null;

		AccountsResource checkAccount = new AccountsResource();
		Response checkAccountResponse = checkAccount.getAccountInternal(new Long(accountNumber));

		if(checkAccountResponse.getStatus() != 200)
		{
			if(checkAccountResponse.getStatus() == 404)
			{
				JSONObject error = new JSONObject();
				error.put(errorMsg, failedToRead + accountNumber + inDebitAccount + this.getClass().toString());
				logger.warning(failedToRead + accountNumber + inDebitAccount + this.getClass().toString());
				myResponse = Response.status(404).entity(error.toString()).build();
				logger.exiting(this.getClass().getName(),debitAccountInternal,myResponse);
				return myResponse;
			}
			JSONObject error = new JSONObject();
			error.put(errorMsg, failedToRead + accountNumber + inDebitAccount + this.getClass().toString());
			logger.severe(failedToRead + accountNumber + inDebitAccount + this.getClass().toString());
			myResponse = Response.status(checkAccountResponse.getStatus()).entity(error.toString()).build();
			logger.exiting(this.getClass().getName(),debitAccountInternal,myResponse);
			return myResponse;
		}
		Long sortcode = new Long(this.getSortCode().longValue());
		myResponse = debitCreditAccount(sortcode, accountNumber, dbcr.getAmount(), true);
		logger.exiting(this.getClass().getName(),debitAccountInternal,myResponse);
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
		logger.entering(this.getClass().getName(),creditAccountInternal);
		Response myResponse = null;

		AccountsResource checkAccount = new AccountsResource();
		Response checkAccountResponse = checkAccount.getAccountInternal(new Long(accountNumber));

		if(checkAccountResponse.getStatus() != 200)
		{
			if(checkAccountResponse.getStatus() == 404)
			{
				JSONObject error = new JSONObject();
				error.put(errorMsg, failedToRead + accountNumber + inCreditAccount + this.getClass().toString());
				logger.warning(failedToRead + accountNumber + inCreditAccount + this.getClass().toString());
				myResponse = Response.status(404).entity(error.toString()).build(); 
				logger.exiting(this.getClass().getName(),creditAccountInternal,myResponse);
				return myResponse;
			}
			JSONObject error = new JSONObject();
			error.put(errorMsg, failedToRead + accountNumber + inCreditAccount + this.getClass().toString());
			logger.severe(failedToRead + accountNumber + inCreditAccount + this.getClass().toString());
			myResponse = Response.status(checkAccountResponse.getStatus()).entity(error.toString()).build(); 
			logger.exiting(this.getClass().getName(),creditAccountInternal,myResponse);
			return myResponse;
		}

		Long sortcode = new Long(this.getSortCode().longValue());
		myResponse = debitCreditAccount(sortcode, accountNumber, dbcr.getAmount(), false); 
		logger.exiting(this.getClass().getName(),creditAccountInternal,myResponse);
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
		logger.entering(this.getClass().getName(),transferLocalInternal);
		Response myResponse = null;

		//* We are transferring money from account "id" at this bank, to another account at this bank
		//* The amount MUST be positive
		JSONObject response = new JSONObject();

		if(new Integer(accountNumber).equals(transferLocal.getTargetAccount()))
		{
			JSONObject error = new JSONObject();
			error.put(errorMsg, needDifferentAccounts);
			logger.warning(needDifferentAccounts);
			myResponse = Response.status(400).entity(error.toString()).build(); 
			logger.exiting(this.getClass().getName(),transferLocalInternal,myResponse);
			return myResponse;
		}

		if(transferLocal.getAmount().doubleValue() <= 0.00)
		{
			JSONObject error = new JSONObject();
			error.put(errorMsg, "Amount to transfer must be positive");
			logger.warning(needDifferentAccounts);
			myResponse = Response.status(400).entity(error.toString()).build(); 
			logger.exiting(this.getClass().getName(),transferLocalInternal,myResponse);
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
			error.put(errorMsg, sourceAccountNumber + accountNumber + cannotBeFound);
			logger.warning(sourceAccountNumber + accountNumber + cannotBeFound);
			myResponse = Response.status(404).entity(error.toString()).build(); 
			logger.exiting(this.getClass().getName(),transferLocalInternal,myResponse);
			return myResponse;
		}

		if(checkAccountResponse.getStatus() != 200)
		{
			JSONObject error = new JSONObject();
			error.put(errorMsg, sourceAccountNumber + accountNumber + cannotBeAccessed);
			logger.warning(sourceAccountNumber + accountNumber + cannotBeAccessed);
			myResponse = Response.status(checkAccountResponse.getStatus()).entity(error.toString()).build(); 
			logger.exiting(this.getClass().getName(),transferLocalInternal,myResponse);
			return myResponse;
		}
		checkAccountResponse = checkAccount.getAccountInternal(new Long(transferLocal.getTargetAccount()));

		if(checkAccountResponse.getStatus() == 404)
		{
			JSONObject error = new JSONObject();
			error.put(errorMsg, targetAccountNumber + transferLocal.getTargetAccount() + cannotBeFound);
			logger.warning(targetAccountNumber + accountNumber + cannotBeFound);
			myResponse = Response.status(404).entity(error.toString()).build(); 
			logger.exiting(this.getClass().getName(),transferLocalInternal,myResponse);
			return myResponse;
		}
		if(checkAccountResponse.getStatus() != 200)
		{
			JSONObject error = new JSONObject();
			error.put(errorMsg, targetAccountNumber + transferLocal.getTargetAccount() + cannotBeAccessed);
			logger.severe(targetAccountNumber + accountNumber + cannotBeAccessed);
			myResponse = Response.status(404).entity(error.toString()).build(); 
			logger.exiting(this.getClass().getName(),transferLocalInternal,myResponse);
			return myResponse;
		}

		com.ibm.cics.cip.bankliberty.web.db2.Account db2Account  = new  com.ibm.cics.cip.bankliberty.web.db2.Account();
		db2Account.setAccount_number(accountNumber);
		db2Account.setSortcode(sortCode.toString());
		db2Account.debitCredit(negativeAmount);

		db2Account.setAccount_number(transferLocal.targetAccount.toString());
		db2Account.setSortcode(sortCode.toString());
		db2Account.debitCredit(amount);

		ProcessedTransactionResource myProcessedTransactionResource = new ProcessedTransactionResource();

		ProcessedTransactionTransferLocalJSON myProctranTransferLocal = new ProcessedTransactionTransferLocalJSON();
		myProctranTransferLocal.setSortCode(db2Account.getSortcode());
		myProctranTransferLocal.setAccountNumber(db2Account.getAccount_number());
		myProctranTransferLocal.setAmount(amount);
		myProctranTransferLocal.setTargetAccountNumber(transferLocal.getTargetAccount().toString());

		Response writeTransferResponse = myProcessedTransactionResource.writeTransferLocalInternal(myProctranTransferLocal);
		if(writeTransferResponse == null || writeTransferResponse.getStatus() != 200)
		{
			JSONObject error = new JSONObject();
			error.put(errorMsg, proctranWriteFailure);
			logger.severe("Accounts: transferLocal: " +proctranWriteFailure);
			try {
				Task.getTask().rollback();
			} catch (InvalidRequestException e) {
			}
			myResponse = Response.status(500).entity(error.toString()).build();
			logger.exiting(this.getClass().getName(),transferLocalInternal,myResponse);
			return myResponse;
		}


		response.put(jsonSortCode, db2Account.getSortcode().trim());
		response.put("id", db2Account.getAccount_number());
		response.put(jsonAvailableBalance, BigDecimal.valueOf(db2Account.getAvailable_balance()));
		response.put(jsonActualBalance, BigDecimal.valueOf(db2Account.getActual_balance()));
		response.put(jsonInterestRate, BigDecimal.valueOf(db2Account.getInterest_rate()));

		myResponse = Response.status(200).entity(response.toString()).build();
		logger.exiting(this.getClass().getName(),transferLocalInternal,myResponse);
		return myResponse;
	}


	private Response debitCreditAccount(Long sortCode, String accountNumber, BigDecimal apiAmount, boolean debitAccount) {
		// This method does both debit AND credit, controlled by the boolean
		logger.entering(this.getClass().getName(),debitCreditAccount);
		Response myResponse = null;
		JSONObject response = new JSONObject();

		if (debitAccount) apiAmount = apiAmount.multiply(BigDecimal.valueOf(-1));


		com.ibm.cics.cip.bankliberty.web.db2.Account db2Account  = new  com.ibm.cics.cip.bankliberty.web.db2.Account();
		db2Account.setAccount_number(accountNumber);
		db2Account.setSortcode(sortCode.toString());
		if(!db2Account.debitCredit(apiAmount))
		{
			JSONObject error = new JSONObject();
			if(apiAmount.doubleValue() < 0)
			{
				error.put(errorMsg, "Failed to debit account " + accountNumber + classNameMsg);
				logger.severe("Failed to debit account " + accountNumber + classNameMsg);
				myResponse = Response.status(500).entity(error.toString()).build(); 
				logger.exiting(this.getClass().getName(),debitCreditAccount,myResponse);
				return myResponse;
			}
			else
			{
				error.put(errorMsg, "Failed to credit account " + accountNumber + classNameMsg);
				logger.severe("Failed to credit account " + accountNumber + classNameMsg);
				myResponse = Response.status(500).entity(error.toString()).build(); 
				logger.exiting(this.getClass().getName(),debitCreditAccount,myResponse);
				return myResponse;
			}
		}

		ProcessedTransactionResource myProcessedTransactionResource = new ProcessedTransactionResource();
		ProcessedTransactionDebitCreditJSON myProctranDbCr = new ProcessedTransactionDebitCreditJSON();
		myProctranDbCr.setSortCode(db2Account.getSortcode());
		myProctranDbCr.setAccountNumber(db2Account.getAccount_number());
		myProctranDbCr.setAmount(apiAmount);


		Response debitCreditResponse = myProcessedTransactionResource.writeInternal(myProctranDbCr);

		if(debitCreditResponse == null || debitCreditResponse.getStatus() != 200)
		{

			JSONObject error = new JSONObject();
			error.put(errorMsg, proctranWriteFailure);
			logger.severe(proctranWriteFailure);
			try {
				Task.getTask().rollback();
			} catch (InvalidRequestException e) {
			}
			myResponse = Response.status(500).entity(error.toString()).build(); 
			logger.exiting(this.getClass().getName(),debitCreditAccount,myResponse);
			return myResponse;
		}

		response.put(jsonSortCode, db2Account.getSortcode().trim());
		response.put("id", db2Account.getAccount_number());
		response.put(jsonAvailableBalance, BigDecimal.valueOf(db2Account.getAvailable_balance()));
		response.put(jsonActualBalance, BigDecimal.valueOf(db2Account.getActual_balance()));
		response.put(jsonInterestRate, BigDecimal.valueOf(db2Account.getInterest_rate()));
		myResponse = Response.status(200).entity(response.toString()).build();
		logger.exiting(this.getClass().getName(),debitCreditAccount,myResponse);
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
		logger.entering(this.getClass().getName(),deleteAccount);
		Response myResponse = null;


		JSONObject response = new JSONObject();

		Integer sortCode = this.getSortCode();

		com.ibm.cics.cip.bankliberty.web.db2.Account db2Account = new Account();

		db2Account = db2Account.deleteAccount(accountNumber.intValue(), sortCode.intValue());
		if(db2Account != null)
		{
			response.put(jsonSortCode, db2Account.getSortcode().trim());
			response.put("id",db2Account.getAccount_number());
			response.put(jsonCustomerNumber, db2Account.getCustomer_number());
			response.put(jsonAccountType, db2Account.getType().trim());
			response.put(jsonAvailableBalance, BigDecimal.valueOf(db2Account.getAvailable_balance()));
			response.put(jsonActualBalance, BigDecimal.valueOf(db2Account.getActual_balance()));
			response.put(jsonInterestRate, BigDecimal.valueOf(db2Account.getInterest_rate()));
			response.put(jsonOverdraft, db2Account.getOverdraft_limit());
			response.put(jsonLastStatementDate, db2Account.getLast_statement().toString().trim());
			response.put(jsonNextStatementDate, db2Account.getNext_statement().toString().trim());
			response.put(jsonDateOpened, db2Account.getOpened().toString().trim());

			ProcessedTransactionResource myProcessedTransactionResource = new ProcessedTransactionResource();

			ProcessedTransactionAccountJSON myDeletedAccount = new ProcessedTransactionAccountJSON();
			myDeletedAccount.setAccountNumber(db2Account.getAccount_number());
			myDeletedAccount.setType(db2Account.getType());
			myDeletedAccount.setCustomerNumber(db2Account.getCustomer_number());
			myDeletedAccount.setSortCode(db2Account.getSortcode());
			myDeletedAccount.setNextStatement(db2Account.getNext_statement());
			myDeletedAccount.setLastStatement(db2Account.getLast_statement());
			myDeletedAccount.setActualBalance(BigDecimal.valueOf(db2Account.getActual_balance()));


			Response deletedAccountResponse = myProcessedTransactionResource.writeDeleteAccountInternal(myDeletedAccount);
			if(deletedAccountResponse == null || deletedAccountResponse.getStatus() != 200)
			{
				JSONObject error = new JSONObject();
				error.put(errorMsg, proctranWriteFailure);
				logger.severe(proctranWriteFailure);
				try {
					Task.getTask().rollback();
				} catch (InvalidRequestException e) {
				}
				myResponse = Response.status(500).entity(error.toString()).build();
				logger.exiting(this.getClass().getName(),deleteAccount,myResponse);
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
			response.put(errorMsg,accountLiteral + accountNumber + " not found");
			myResponse = Response.status(404).entity(response.toString()).build();
			logger.exiting(this.getClass().getName(),deleteAccount,myResponse);
			return myResponse;
		}
		/*
		 * Parse returned data and return to calling method
		 */



		myResponse = Response.status(200).entity(response.toString()).build();
		logger.exiting(this.getClass().getName(),deleteAccount,myResponse);
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
				response.put(errorMsg,"Accounts cannot be accessed");
				logger.severe("Accounts cannot be accessed");
				myResponse = Response.status(500).entity(response.toString()).build();
				logger.exiting(this.getClass().getName(), "getAccountsInternal(Integer limit, Integer offset,boolean countOnly)",myResponse);
				return myResponse;	
			}
			accounts = new JSONArray(myAccounts.length);

			for(int i = 0;i < myAccounts.length;i++)
			{
				JSONObject account = new JSONObject();
				account.put(jsonSortCode, myAccounts[i].getSortcode().trim());
				account.put("id", myAccounts[i].getAccount_number());
				account.put(jsonCustomerNumber, myAccounts[i].getCustomer_number());
				account.put(jsonAccountType, myAccounts[i].getType().trim());
				account.put(jsonAvailableBalance, BigDecimal.valueOf(myAccounts[i].getAvailable_balance()));
				account.put(jsonActualBalance, BigDecimal.valueOf(myAccounts[i].getActual_balance()));
				account.put(jsonInterestRate, BigDecimal.valueOf(myAccounts[i].getInterest_rate()));
				account.put(jsonOverdraft, myAccounts[i].getOverdraft_limit());
				account.put(jsonLastStatementDate, myAccounts[i].getLast_statement().toString().trim());
				account.put(jsonNextStatementDate, myAccounts[i].getNext_statement().toString().trim());
				account.put(jsonDateOpened, myAccounts[i].getOpened().toString());

				accounts.add(account);

			}
			numberOfAccounts = accounts.size();
		}
		/*
		 * Parse returned data and return to calling method
		 */

		response.put(jsonNumberOfAccounts, numberOfAccounts);
		if(accounts != null)
		{
			response.put(jsonAccounts, accounts);
		}
		myResponse = Response.status(200).entity(response.toString()).build(); 
		logger.exiting(this.getClass().getName(),deleteAccount,myResponse);
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
		logger.entering(this.getClass().getName(),getAccountsByBalanceWithOffsetAndLimitInternal);
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
				error.put(errorMsg, "Invalid operator, '" + operator + "' only <= or >= allowed");
				logger.warning("Invalid operator, '" + operator + "' only <= or >= allowed");
				logger.exiting(this.getClass().getName(),getAccountsByBalanceWithOffsetAndLimitInternal,myResponse);
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
				error.put(errorMsg, db2ReadFailure);
				logger.severe(db2ReadFailure);
				logger.exiting(this.getClass().getName(),getAccountsByBalanceWithOffsetAndLimitInternal,myResponse);
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
				error.put(errorMsg, db2ReadFailure);
				logger.severe(db2ReadFailure);
				logger.exiting(this.getClass().getName(),getAccountsByBalanceWithOffsetAndLimitInternal,myResponse);
				myResponse = Response.status(500).entity(error.toString()).build();
				return myResponse;
			}
			accounts = new JSONArray(myAccounts.length);



			for(int i = 0;i < myAccounts.length;i++)
			{
				JSONObject account = new JSONObject();
				account.put(jsonSortCode, myAccounts[i].getSortcode().trim());
				account.put("id", myAccounts[i].getAccount_number());
				account.put(jsonCustomerNumber, myAccounts[i].getCustomer_number());
				account.put(jsonAccountType, myAccounts[i].getType().trim());
				account.put(jsonAvailableBalance, BigDecimal.valueOf(myAccounts[i].getAvailable_balance()));
				account.put(jsonActualBalance, BigDecimal.valueOf(myAccounts[i].getActual_balance()));
				account.put(jsonInterestRate, BigDecimal.valueOf(myAccounts[i].getInterest_rate()));
				account.put(jsonOverdraft, myAccounts[i].getOverdraft_limit());
				account.put(jsonLastStatementDate, myAccounts[i].getLast_statement().toString().trim());
				account.put(jsonNextStatementDate, myAccounts[i].getNext_statement().toString().trim());
				account.put(jsonDateOpened, myAccounts[i].getOpened().toString());

				accounts.add(account);

			}
			numberOfAccounts = accounts.size();
		}


		/*
		 * Parse returned data and return to calling method
		 */

		response.put(jsonNumberOfAccounts, numberOfAccounts);
		response.put(jsonAccounts, accounts);
		myResponse = Response.status(200).entity(response.toString()).build(); 
		logger.exiting(this.getClass().getName(),getAccountsByBalanceWithOffsetAndLimitInternal,myResponse);
		return myResponse;


	}





}



