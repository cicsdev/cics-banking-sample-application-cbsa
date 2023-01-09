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
		logger.entering(this.getClass().getName(),"createAccountExternal(AccountJSON account) for account " + account.toString());

		Response myResponse = createAccountInternal(account);

		HBankDataAccess myHBankDataAccess = new HBankDataAccess();
		myHBankDataAccess.terminate();
		logger.exiting(this.getClass().getName(),"createAccountExternal(AccountJSON account)",myResponse);
		return myResponse;

	}

	public Response createAccountInternal(
			/** Internal methods can be called by either the external methods, or another part of Liberty */
			AccountJSON account) {
		logger.entering(this.getClass().getName(),"createAccountInternal(AccountJSON account) for account " + account.toString());
		Response myResponse = null;
		/* Only ISA, LOAN, CURRENT, MORTGAGE and SAVING are valid account types */
		if(!account.validateType(account.getAccountType().trim()))
		{
			JSONObject error = new JSONObject();
			error.put("errorMessage", "Account type " + account.getAccountType() + " is not supported.");
			logger.warning("Account type " + account.getAccountType() + " is not supported.");
			myResponse = Response.status(400).entity(error.toString()).build();
			logger.exiting(this.getClass().getName(),"createAccountInternal(AccountJSON account)",myResponse);
			return myResponse;

		}

		//Interest rate cannot be < 0
		if(account.getInterestRate().doubleValue() < 0.00)
		{
			JSONObject error = new JSONObject();
			error.put("errorMessage", "Interest rate cannot be less than zero.");
			logger.warning("Interest rate cannot be less than zero.");
			myResponse = Response.status(400).entity(error.toString()).build();
			logger.exiting(this.getClass().getName(),"createAccountInternal(AccountJSON account)",myResponse);
			return myResponse;
		}

		//Interest rate cannot be > 9999.99%
		if(account.getInterestRate().doubleValue() > 9999.99)
		{
			JSONObject error = new JSONObject();
			error.put("errorMessage", "Interest rate cannot be greater than 9999.99%.");
			logger.warning("Interest rate cannot be greater than 9999.99%.");
			myResponse = Response.status(400).entity(error.toString()).build();
			logger.exiting(this.getClass().getName(),"createAccountInternal(AccountJSON account)",myResponse);
			return myResponse;
		}

		//Interest rate cannot have more than 2dp
		BigDecimal myInterestRate = account.getInterestRate();
		if(myInterestRate.scale() > 2)
		{
			JSONObject error = new JSONObject();
			error.put("errorMessage", "Interest rate cannot have more than 2 decimal places. " + myInterestRate.toPlainString());
			logger.warning("Interest rate cannot have more than 2 decimal places."  + myInterestRate.toPlainString());
			myResponse = Response.status(400).entity(error.toString()).build();
			logger.exiting(this.getClass().getName(),"createAccountInternal(AccountJSON account)",myResponse);
			return myResponse;			
		}

		//Overdraft limit cannot be < 0
		if(account.getOverdraft().intValue() < 0)
		{
			JSONObject error = new JSONObject();
			error.put("errorMessage", "Overdraft limit cannot be less than zero.");
			logger.warning("Overdraft limit cannot be less than zero.");
			myResponse = Response.status(400).entity(error.toString()).build();
			logger.exiting(this.getClass().getName(),"createAccountInternal(AccountJSON account)",myResponse);
			return myResponse;			
		}

		//Customer number cannot be < 1
		Long customerNumberLong = new Long(account.getCustomerNumber());
		if(customerNumberLong.longValue() < 1)
		{
			JSONObject error = new JSONObject();
			error.put("errorMessage", "Customer number cannot be less than one.");
			logger.warning("Customer number cannot be less than one.");
			myResponse = Response.status(400).entity(error.toString()).build();
			logger.exiting(this.getClass().getName(),"createAccountInternal(AccountJSON account)",myResponse);
			return myResponse;
		}
		
		//Customer number cannot be 9999999999
		if(customerNumberLong.longValue() == 9999999999L)
		{
			JSONObject error = new JSONObject();
			error.put("errorMessage", "Customer number cannot be 9,999,999,999.");
			logger.warning("Customer number cannot be 9,999,999,999.");
			myResponse = Response.status(400).entity(error.toString()).build();
			logger.exiting(this.getClass().getName(),"createAccountInternal(AccountJSON account)",myResponse);
			return myResponse;
		}
		
		//Sortcode is not valid for this bank
		Integer inputSortCode = new Integer(account.getSortCode());
		Integer thisSortCode = this.getSortCode();

		if(inputSortCode.intValue() != thisSortCode.intValue())
		{
			JSONObject error = new JSONObject();
			error.put("errorMessage", "Sortcode " +  inputSortCode + " not valid for this bank (" + thisSortCode + ")");
			logger.warning("Sortcode " +  inputSortCode + " not valid for this bank (" + thisSortCode + ")");
			myResponse = Response.status(400).entity(error.toString()).build();
			logger.exiting(this.getClass().getName(),"createAccountInternal(AccountJSON account)",myResponse);
			return myResponse;
		}





		CustomerResource myCustomer = new CustomerResource();
		Response customerResponse = myCustomer.getCustomerInternal(customerNumberLong);
		// Customer number cannot be found
		if(customerResponse.getStatus() != 200)
		{
			JSONObject error = new JSONObject();
			error.put("errorMessage", "Customer number " + customerNumberLong.longValue() + " cannot be found.");
			logger.warning("Customer number " + customerNumberLong.longValue() + " cannot be found.");
			myResponse = Response.status(404).entity(error.toString()).build();
			logger.exiting(this.getClass().getName(),"createAccountInternal(AccountJSON account)",myResponse);
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
					error.put("errorMessage", "Customer number " + customerNumberLong.longValue() + " cannot be found.");
					logger.warning("Customer number " + customerNumberLong.longValue() + " cannot be found.");
					myResponse = Response.status(404).entity(error.toString()).build();
					logger.exiting(this.getClass().getName(),"createAccountInternal(AccountJSON account)",myResponse);
					return myResponse;
				}
				JSONObject error = new JSONObject();
				error.put("errorMessage", "Customer number " + customerNumberLong.longValue() + " cannot be accessed.");
				logger.severe("Customer number " + customerNumberLong.longValue() + " cannot be accessed.");
				myResponse = Response.status(accountsOfThisCustomer.getStatus()).entity(error.toString()).build();
				logger.exiting(this.getClass().getName(),"createAccountInternal(AccountJSON account)",myResponse);
				return myResponse;

			}
			String accountsOfThisCustomerString = accountsOfThisCustomer.getEntity().toString();
			myAccountsJSON = JSONObject.parse(accountsOfThisCustomerString);
		} catch (IOException e) {
			JSONObject error = new JSONObject();
			error.put("errorMessage","Failed to retrieve customer number " + customerNumberLong + " " + e.getLocalizedMessage());
			logger.severe("Failed to retrieve customer number " + customerNumberLong + " " + e.getLocalizedMessage());
			myResponse = Response.status(500).entity(error.toString()).build();
			logger.exiting(this.getClass().getName(),"createAccountInternal(AccountJSON account)",myResponse);
			return myResponse;
		}
		long accountCount = (Long) myAccountsJSON.get("numberOfAccounts");

		// Does the customer have ten or more accounts?

		if(accountCount >= 10)
		{
			JSONObject error = new JSONObject();
			error.put("errorMessage", "Customer number " + customerNumberLong.longValue() + " cannot have more than ten accounts.");
			logger.warning("Customer number " + customerNumberLong.longValue() + " cannot have more than ten accounts.");
			myResponse = Response.status(400).entity(error.toString()).build();
			logger.exiting(this.getClass().getName(),"createAccountInternal(AccountJSON account)",myResponse);
			return myResponse;
		}

		com.ibm.cics.cip.bankliberty.web.db2.Account db2Account = new com.ibm.cics.cip.bankliberty.web.db2.Account();
		db2Account.setSortcode(this.getSortCode().toString());
		db2Account = db2Account.createAccount(account, this.getSortCode(),true);
		//Add data to JSONObject
		if(db2Account != null)
		{
			response.put("sortCode", db2Account.getSortcode().trim());
			response.put("id", db2Account.getAccount_number());
			response.put("customerNumber", db2Account.getCustomer_number());
			response.put("accountType", db2Account.getType().trim());
			response.put("availableBalance",  BigDecimal.valueOf(db2Account.getAvailable_balance()));
			response.put("actualBalance", BigDecimal.valueOf(db2Account.getActual_balance()));
			response.put("interestRate", BigDecimal.valueOf(db2Account.getInterest_rate()));
			response.put("overdraft", db2Account.getOverdraft_limit());
			response.put("lastStatementDate", db2Account.getLast_statement().toString().trim());
			response.put("nextStatementDate", db2Account.getNext_statement().toString().trim());
			response.put("dateOpened", db2Account.getOpened().toString().trim());

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
				error.put("errorMessage", "Failed to write to PROCTRAN data store");
				try {
					logger.severe("Accounts: createAccount: Failed to write to PROCTRAN data store");
					Task.getTask().rollback();
				} catch (InvalidRequestException e) {
				}
				logger.severe("Failed to create account in com.ibm.cics.cip.bankliberty.web.db2.Account");
				myResponse = Response.status(500).entity(error.toString()).build();
				logger.exiting(this.getClass().getName(),"createAccountInternal(AccountJSON account)",myResponse);
				return myResponse;
			}

		}
		else
		{
			JSONObject error = new JSONObject();
			error.put("errorMessage", "Failed to create account in com.ibm.cics.cip.bankliberty.web.db2.Account");
			logger.severe("Failed to create account in com.ibm.cics.cip.bankliberty.web.db2.Account");
			myResponse = Response.status(500).entity(error.toString()).build();
			logger.exiting(this.getClass().getName(),"createAccountInternal(AccountJSON account)",myResponse);
			return myResponse;
		}

		myResponse = Response.status(201).entity(response.toString()).build();
		logger.exiting(this.getClass().getName(),"createAccountInternal(AccountJSON account)",myResponse);
		return myResponse;

	}

	@GET
	@Produces("application/json")
	public Response getAccountsExternal(@QueryParam("countOnly") Boolean countOnly)
	{
		/** This will list all accounts. The countOnly boolean indicates that we just want the number of accounts, not every  single account */
		logger.entering(this.getClass().getName(),"getAccountsExternal(Boolean countOnly)");
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
		logger.exiting(this.getClass().getName(),"getAccountsExternal(Boolean countOnly)",myResponse);
		return myResponse;
	}



	public Response getAccountsInternal(boolean countOnly) {
		/** This will list all accounts. The countOnly boolean indicates that we just want the number of accounts, not every  single account */
		logger.entering(this.getClass().getName(),"getAccountsInternal(boolean countOnly)");
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
				response.put("errorMessage","Accounts cannot be accessed in com.ibm.cics.cip.bankliberty.web.db2.Account");
				logger.severe("Accounts cannot be accessed in com.ibm.cics.cip.bankliberty.web.db2.Account");
				myResponse = Response.status(500).entity(response.toString()).build();
				logger.exiting(this.getClass().getName(),"getAccountsInternal(boolean countOnly)",myResponse);
				return myResponse;
			}
			accounts = new JSONArray(myAccounts.length);

			for(int i = 0;i < myAccounts.length;i++)
			{
				JSONObject account = new JSONObject();
				account.put("sortCode", myAccounts[i].getSortcode().trim());
				account.put("id", myAccounts[i].getAccount_number());
				account.put("customerNumber", myAccounts[i].getCustomer_number());
				account.put("accountType", myAccounts[i].getType().trim());
				account.put("availableBalance", BigDecimal.valueOf(myAccounts[i].getAvailable_balance()).setScale(2,RoundingMode.HALF_UP));
				account.put("actualBalance", BigDecimal.valueOf(myAccounts[i].getActual_balance()).setScale(2,RoundingMode.HALF_UP));
				account.put("interestRate", BigDecimal.valueOf(myAccounts[i].getInterest_rate()).setScale(2,RoundingMode.HALF_UP));
				account.put("overdraft", myAccounts[i].getOverdraft_limit());
				account.put("lastStatementDate", myAccounts[i].getLast_statement().toString().trim());
				account.put("nextStatementDate", myAccounts[i].getNext_statement().toString().trim());
				account.put("dateOpened", myAccounts[i].getOpened().toString());

				accounts.add(account);
			}
			numberOfAccounts = accounts.size();
		}

		/*
		 * Parse returned data and return to calling method
		 */

		response.put("numberOfAccounts", numberOfAccounts);
		if(accounts != null)
		{
			response.put("accounts", accounts);
		}
		myResponse = Response.status(200).entity(response.toString()).build();
		logger.exiting(this.getClass().getName(),"getAccountsInternal(boolean countOnly)",myResponse);
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
		logger.entering(this.getClass().getName(),"getAccountInternal(Long accountNumber)");
		Response myResponse = null;
		JSONObject response = new JSONObject();

		Integer sortCode = this.getSortCode();
		Long id_safe = accountNumber;

		com.ibm.cics.cip.bankliberty.web.db2.Account db2Account = new Account();

		db2Account = db2Account.getAccount(accountNumber.intValue(), sortCode);
		if(db2Account != null)
		{
			
			response.put("sortCode", db2Account.getSortcode().trim());
			response.put("id", db2Account.getAccount_number());
			response.put("customerNumber", db2Account.getCustomer_number());
			response.put("accountType", db2Account.getType().trim());
			response.put("availableBalance", BigDecimal.valueOf(db2Account.getAvailable_balance()));
			response.put("actualBalance", BigDecimal.valueOf(db2Account.getActual_balance()));
			response.put("interestRate", BigDecimal.valueOf(db2Account.getInterest_rate()));
			response.put("overdraft", db2Account.getOverdraft_limit());
			response.put("lastStatementDate", db2Account.getLast_statement().toString().trim());
			response.put("nextStatementDate", db2Account.getNext_statement().toString().trim());
			response.put("dateOpened", db2Account.getOpened().toString().trim());
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
					response.put("sortCode", db2Account.getSortcode().trim());
					response.put("id", db2Account.getAccount_number());
					response.put("customerNumber", db2Account.getCustomer_number());
					response.put("accountType", db2Account.getType().trim());
					response.put("availableBalance", BigDecimal.valueOf(db2Account.getAvailable_balance()));
					response.put("actualBalance", BigDecimal.valueOf(db2Account.getActual_balance()));
					response.put("interestRate", BigDecimal.valueOf(db2Account.getInterest_rate()));
					response.put("overdraft", db2Account.getOverdraft_limit());
					response.put("lastStatementDate", db2Account.getLast_statement().toString().trim());
					response.put("nextStatementDate", db2Account.getNext_statement().toString().trim());
					response.put("dateOpened", db2Account.getOpened().toString().trim());
				}
				//* There exists a possibility that the last account has been deleted. In which case we try once the old fashioned way
			}
			if(db2Account == null)
			{
				response.put("errorMessage","Account " + accountNumber + " not found in com.ibm.cics.cip.bankliberty.web.db2.Account");
				logger.warning("Account " + accountNumber + " not found in com.ibm.cics.cip.bankliberty.web.db2.Account");
				myResponse = Response.status(404).entity(response.toString()).build();
				logger.exiting(this.getClass().getName(),"getAccountInternal(Long accountNumber)",myResponse);
				return myResponse;
			}
		}

		myResponse = Response.status(200).entity(response.toString()).build();
		logger.exiting(this.getClass().getName(),"getAccountInternal(Long accountNumber)",myResponse);

		return myResponse;
	}


	@GET
	@Path("/retrieveByCustomerNumber/{customerNumber}")
	@Produces("application/json")
	public Response getAccountsByCustomerExternal(@PathParam("customerNumber") Long customerNumber, @QueryParam("countOnly") Boolean countOnly) {
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

	public Response getAccountsByCustomerInternal(@PathParam("customerNumber") Long customerNumber,boolean countOnly) {
		logger.entering(this.getClass().getName(),"getAccountsByCustomerInternal(Long customerNumber, boolean countOnly)");

		JSONArray accounts = null;
		Response myResponse = null;

		JSONObject response = new JSONObject();
		Integer sortCode =  this.getSortCode();
		int numberOfAccounts = 0;


		CustomerResource myCustomer = new CustomerResource();
		Response customerResponse = myCustomer.getCustomerInternal(customerNumber.longValue());

		if(customerResponse.getStatus() != 200)
		{
			if(customerResponse.getStatus() == 404)
			{
				//If cannot find response "CustomerResponse" then error 404 returned
				JSONObject error = new JSONObject();
				error.put("errorMessage", "Customer number " + customerNumber.longValue() + " cannot be found.");
				logger.severe("Customer number " + customerNumber.longValue() + " cannot be found.");
				myResponse = Response.status(404).entity(error.toString()).build(); 
				logger.exiting(this.getClass().getName(),"getAccountsByCustomerInternal(Long customerNumber, boolean countOnly)",myResponse);
				return myResponse;
			}
			else
			{
				JSONObject error = new JSONObject();
				error.put("errorMessage", "Customer number " + customerNumber.longValue() + " cannot be accessed. " + customerResponse.toString());
				logger.severe("Customer number " + customerNumber.longValue() + " cannot be accessed. " + customerResponse.toString());
				myResponse = Response.status(customerResponse.getStatus()).entity(error.toString()).build(); 
				logger.exiting(this.getClass().getName(),"getAccountsByCustomerInternal(Long customerNumber, boolean countOnly)",myResponse);
				return myResponse;
			}
		}


		com.ibm.cics.cip.bankliberty.web.db2.Account db2Account = new Account();
		Account[] myAccounts = db2Account.getAccounts(customerNumber.intValue(), sortCode);
		if(myAccounts == null)
		{
			JSONObject error = new JSONObject();
			error.put("errorMessage", "Accounts cannot be accessed for customer " + customerNumber.longValue() + " in com.ibm.cics.cip.bankliberty.web.db2.Account");
			logger.severe("Accounts cannot be accessed for customer " + customerNumber.longValue() + " in com.ibm.cics.cip.bankliberty.web.db2.Account");
			myResponse = Response.status(500).entity(error.toString()).build(); 
			logger.exiting(this.getClass().getName(),"getAccountsByCustomerInternal(Long customerNumber, boolean countOnly)",myResponse);
			return myResponse;	
		}

		numberOfAccounts = myAccounts.length;
		accounts = new JSONArray(numberOfAccounts);
		for (int i = 0; i < numberOfAccounts; i++) {


			JSONObject account = new JSONObject();
			account.put("sortCode", myAccounts[i].getSortcode());
			account.put("id", myAccounts[i].getAccount_number());
			account.put("customerNumber", myAccounts[i].getCustomer_number());
			account.put("accountType", myAccounts[i].getType());
			account.put("availableBalance", BigDecimal.valueOf(myAccounts[i].getAvailable_balance()));
			account.put("actualBalance", BigDecimal.valueOf(myAccounts[i].getActual_balance()));
			account.put("interestRate", BigDecimal.valueOf(myAccounts[i].getInterest_rate()));
			account.put("overdraft", myAccounts[i].getOverdraft_limit());
			account.put("lastStatementDate", myAccounts[i].getLast_statement().toString());
			account.put("nextStatementDate", myAccounts[i].getNext_statement().toString());
			account.put("dateOpened", myAccounts[i].getOpened().toString());

			accounts.add(account);
		}
		String customerNumberString = new String(customerNumber.toString());
		for(int i=10;customerNumberString.length()<10;i--)
		{
			customerNumberString = "0" + customerNumberString;
		}
		response.put("customerNumber", customerNumberString);
		response.put("numberOfAccounts", numberOfAccounts);
		response.put("accounts", accounts);


		myResponse = Response.status(200).entity(response.toString()).build(); 
		logger.exiting(this.getClass().getName(),"getAccountsByCustomerInternal(Long customerNumber, boolean countOnly)",myResponse);
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
		logger.entering(this.getClass().getName(),"updateAccountInternal(Long id, AccountJSON account)");
		JSONObject response = new JSONObject();
		Response myResponse = null;


		if(!(account.validateType(account.getAccountType().trim())))
			//If account type invalid
		{
			JSONObject error = new JSONObject();
			error.put("errorMessage", "Account type " + account.getAccountType() + " is not supported.");
			logger.warning("Account type " + account.getAccountType() + " is not supported.");
			myResponse = Response.status(400).entity(error.toString()).build(); 
			logger.exiting(this.getClass().getName(),"updateAccountInternal(Long id, AccountJSON account)",myResponse);
			return myResponse;
		}

		if(account.getInterestRate().doubleValue() < 0.00)
		{
			//If interest rate < 0
			JSONObject error = new JSONObject();
			error.put("errorMessage", "Interest rate cannot be less than zero.");
			logger.warning("Interest rate cannot be less than zero.");
			myResponse = Response.status(400).entity(error.toString()).build(); 
			logger.exiting(this.getClass().getName(),"updateAccountInternal(Long id, AccountJSON account)",myResponse);
			return myResponse;
		}

		if(account.getInterestRate().doubleValue() > 9999.99)
		{
			//If interest rate > 9999.99
			JSONObject error = new JSONObject();
			error.put("errorMessage", "Interest rate cannot be greater than 9999.99%.");
			logger.warning("Interest rate cannot be greater than 9999.99%.");
			myResponse = Response.status(400).entity(error.toString()).build(); 
			logger.exiting(this.getClass().getName(),"updateAccountInternal(Long id, AccountJSON account)",myResponse);
			return myResponse;

		}

		BigDecimal myInterestRate = account.getInterestRate();
		if(myInterestRate.scale() > 2)
			//Interest rate more than 2dp
		{
			JSONObject error = new JSONObject();
			error.put("errorMessage", "Interest rate cannot have more than 2 decimal places. " + myInterestRate.toPlainString());
			logger.warning("Interest rate cannot have more than 2 decimal places."  + myInterestRate.toPlainString());
			myResponse = Response.status(400).entity(error.toString()).build(); 
			logger.exiting(this.getClass().getName(),"updateAccountInternal(Long id, AccountJSON account)",myResponse);
			return myResponse;
		}

		Integer inputSortCode = new Integer(account.getSortCode());
		Integer thisSortCode = this.getSortCode();

		if(inputSortCode.intValue() != thisSortCode.intValue())
			//Invalid sortcode
		{
			JSONObject error = new JSONObject();
			error.put("errorMessage", "Sortcode " +  inputSortCode + " not valid for this bank (" + thisSortCode + ")");
			logger.warning("Sortcode " +  inputSortCode + " not valid for this bank (" + thisSortCode + ")");
			myResponse = Response.status(400).entity(error.toString()).build(); 
			logger.exiting(this.getClass().getName(),"updateAccountInternal(Long id, AccountJSON account)",myResponse);
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
				response.put("sortCode", db2Account.getSortcode().trim());
				response.put("id", db2Account.getAccount_number());
				response.put("customerNumber",db2Account.getCustomer_number());
				response.put("accountType", db2Account.getType().trim());
				response.put("availableBalance", BigDecimal.valueOf(db2Account.getAvailable_balance()));
				response.put("actualBalance", BigDecimal.valueOf(db2Account.getActual_balance()));
				response.put("interestRate", BigDecimal.valueOf(db2Account.getInterest_rate()));				
				response.put("overdraft", db2Account.getOverdraft_limit());
				response.put("lastStatementDate", db2Account.getLast_statement().toString());
				response.put("nextStatementDate", db2Account.getNext_statement().toString());
				response.put("dateOpened", db2Account.getOpened().toString().trim());
			}
			else
			{
				JSONObject error = new JSONObject();
				error.put("errorMessage", "Failed to update account in com.ibm.cics.cip.bankliberty.web.db2.Account");
				logger.severe("Failed to update account in com.ibm.cics.cip.bankliberty.web.db2.Account");
				myResponse = Response.status(500).entity(error.toString()).build(); 
				logger.exiting(this.getClass().getName(),"updateAccountInternal(Long id, AccountJSON account)",myResponse);
				return myResponse;
			}
		}
		else
		{
			JSONObject error = new JSONObject();
			error.put("errorMessage", "Failed to read account " + account.getId() + " in " + this.getClass().toString());

			logger.warning("Failed to read account " + account.getId() + " in com.ibm.cics.cip.bankliberty.web.db2.Account");
			myResponse = Response.status(404).entity(error.toString()).build(); 
			logger.exiting(this.getClass().getName(),"updateAccountInternal(Long id, AccountJSON account)",myResponse);
			return myResponse;
		}
		myResponse = Response.status(200).entity(response.toString()).build(); 
		logger.exiting(this.getClass().getName(),"updateAccountInternal(Long id, AccountJSON account)",myResponse);

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
		logger.entering(this.getClass().getName(),"debitAccountInternal(String accountNumber, DebitCreditAccountJSON dbcr)");
		Response myResponse = null;

		AccountsResource checkAccount = new AccountsResource();
		Response checkAccountResponse = checkAccount.getAccountInternal(new Long(accountNumber));

		if(checkAccountResponse.getStatus() != 200)
		{
			if(checkAccountResponse.getStatus() == 404)
			{
				JSONObject error = new JSONObject();
				error.put("errorMessage", "Failed to read account " + accountNumber + " in debitAccount " + this.getClass().toString());
				logger.warning("Failed to read account " + accountNumber + " in debitAccount " + this.getClass().toString());
				myResponse = Response.status(404).entity(error.toString()).build();
				logger.exiting(this.getClass().getName(),"debitAccountInternal(String accountNumber, DebitCreditAccountJSON dbcr)",myResponse);
				return myResponse;
			}
			JSONObject error = new JSONObject();
			error.put("errorMessage", "Failed to read account " + accountNumber + " in debitAccount " + this.getClass().toString());
			logger.severe("Failed to read account " + accountNumber + " in debitAccount " + this.getClass().toString());
			myResponse = Response.status(checkAccountResponse.getStatus()).entity(error.toString()).build();
			logger.exiting(this.getClass().getName(),"debitAccountInternal(String accountNumber, DebitCreditAccountJSON dbcr)",myResponse);
			return myResponse;
		}
		Long sortcode = new Long(this.getSortCode().longValue());
		myResponse = debitCreditAccount(sortcode, accountNumber, dbcr.getAmount(), true);
		logger.exiting(this.getClass().getName(),"debitAccountInternal(String accountNumber, DebitCreditAccountJSON dbcr)",myResponse);
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
		logger.entering(this.getClass().getName(),"creditAccountInternal(String accountNumber, DebitCreditAccountJSON dbcr)");
		Response myResponse = null;

		AccountsResource checkAccount = new AccountsResource();
		Response checkAccountResponse = checkAccount.getAccountInternal(new Long(accountNumber));

		if(checkAccountResponse.getStatus() != 200)
		{
			if(checkAccountResponse.getStatus() == 404)
			{
				JSONObject error = new JSONObject();
				error.put("errorMessage", "Failed to read account " + accountNumber + " in creditAccount " + this.getClass().toString());
				logger.warning("Failed to read account " + accountNumber + " in creditAccount " + this.getClass().toString());
				myResponse = Response.status(404).entity(error.toString()).build(); 
				logger.exiting(this.getClass().getName(),"creditAccountInternal(String accountNumber, DebitCreditAccountJSON dbcr)",myResponse);
				return myResponse;
			}
			JSONObject error = new JSONObject();
			error.put("errorMessage", "Failed to read account " + accountNumber + " in creditAccount " + this.getClass().toString());
			logger.severe("Failed to read account " + accountNumber + " in creditAccount " + this.getClass().toString());
			myResponse = Response.status(checkAccountResponse.getStatus()).entity(error.toString()).build(); 
			logger.exiting(this.getClass().getName(),"creditAccountInternal(String accountNumber, DebitCreditAccountJSON dbcr)",myResponse);
			return myResponse;
		}

		Long sortcode = new Long(this.getSortCode().longValue());
		myResponse = debitCreditAccount(sortcode, accountNumber, dbcr.getAmount(), false); 
		logger.exiting(this.getClass().getName(),"creditAccountInternal(String accountNumber, DebitCreditAccountJSON dbcr)",myResponse);
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
		logger.entering(this.getClass().getName(),"transferLocalInternal(String accountNumber, TransferLocalJSON transferLocal)");
		Response myResponse = null;

		//* We are transferring money from account "id" at this bank, to another account at this bank
		//* The amount MUST be positive
		JSONObject response = new JSONObject();

		if(new Integer(accountNumber).intValue() == new Integer(transferLocal.getTargetAccount().intValue()))
		{
			JSONObject error = new JSONObject();
			error.put("errorMessage", "Source and target accounts must be different");
			logger.warning("Source and target accounts must be different");
			myResponse = Response.status(400).entity(error.toString()).build(); 
			logger.exiting(this.getClass().getName(),"transferLocalInternal(String accountNumber, TransferLocalJSON transferLocal)",myResponse);
			return myResponse;
		}

		if(transferLocal.getAmount().doubleValue() <= 0.00)
		{
			JSONObject error = new JSONObject();
			error.put("errorMessage", "Amount to transfer must be positive");
			logger.warning("Source and target accounts must be different");
			myResponse = Response.status(400).entity(error.toString()).build(); 
			logger.exiting(this.getClass().getName(),"transferLocalInternal(String accountNumber, TransferLocalJSON transferLocal)",myResponse);
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
			error.put("errorMessage", "Source account number " + accountNumber + " cannot be found.");
			logger.warning("Source account number " + accountNumber + " cannot be found.");
			myResponse = Response.status(404).entity(error.toString()).build(); 
			logger.exiting(this.getClass().getName(),"transferLocalInternal(String accountNumber, TransferLocalJSON transferLocal)",myResponse);
			return myResponse;
		}

		if(checkAccountResponse.getStatus() != 200)
		{
			JSONObject error = new JSONObject();
			error.put("errorMessage", "Source account number " + accountNumber + " cannot be accessed.");
			logger.warning("Source account number " + accountNumber + " cannot be accessed.");
			myResponse = Response.status(checkAccountResponse.getStatus()).entity(error.toString()).build(); 
			logger.exiting(this.getClass().getName(),"transferLocalInternal(String accountNumber, TransferLocalJSON transferLocal)",myResponse);
			return myResponse;
		}
		checkAccountResponse = checkAccount.getAccountInternal(new Long(transferLocal.getTargetAccount()));

		if(checkAccountResponse.getStatus() == 404)
		{
			JSONObject error = new JSONObject();
			error.put("errorMessage", "Destination account number " + transferLocal.getTargetAccount() + " cannot be found.");
			logger.warning("Destination account number " + accountNumber + " cannot be found.");
			myResponse = Response.status(404).entity(error.toString()).build(); 
			logger.exiting(this.getClass().getName(),"transferLocalInternal(String accountNumber, TransferLocalJSON transferLocal)",myResponse);
			return myResponse;
		}
		if(checkAccountResponse.getStatus() != 200)
		{
			JSONObject error = new JSONObject();
			error.put("errorMessage", "Destination account number " + transferLocal.getTargetAccount() + " cannot be accessed.");
			logger.severe("Destination account number " + accountNumber + " cannot be accessed.");
			myResponse = Response.status(404).entity(error.toString()).build(); 
			logger.exiting(this.getClass().getName(),"transferLocalInternal(String accountNumber, TransferLocalJSON transferLocal)",myResponse);
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
			error.put("errorMessage", "Failed to write to PROCTRAN data store");
			logger.severe("Accounts: transferLocal: Failed to write to PROCTRAN data store");
			try {
				Task.getTask().rollback();
			} catch (InvalidRequestException e) {
			}
			myResponse = Response.status(500).entity(error.toString()).build();
			logger.exiting(this.getClass().getName(),"transferLocalInternal(String accountNumber, TransferLocalJSON transferLocal)",myResponse);
			return myResponse;
		}


		response.put("sortCode", db2Account.getSortcode().trim());
		response.put("id", db2Account.getAccount_number());
		response.put("availableBalance", BigDecimal.valueOf(db2Account.getAvailable_balance()));
		response.put("actualBalance", BigDecimal.valueOf(db2Account.getActual_balance()));
		response.put("interestRate", BigDecimal.valueOf(db2Account.getInterest_rate()));

		myResponse = Response.status(200).entity(response.toString()).build();
		logger.exiting(this.getClass().getName(),"transferLocalInternal(String accountNumber, TransferLocalJSON transferLocal)",myResponse);
		return myResponse;
	}


	private Response debitCreditAccount(Long sortCode, String accountNumber, BigDecimal apiAmount, boolean debitAccount) {
		// This method does both debit AND credit, controlled by the boolean
		logger.entering(this.getClass().getName(),"debitCreditAccount(Long sortCode, String accountNumber, BigDecimal apiAmount, boolean debitAccount)");
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
				error.put("errorMessage", "Failed to debit account " + accountNumber + " in com.ibm.cics.cip.bankliberty.web.db2.Account");
				logger.severe("Failed to debit account " + accountNumber + " in com.ibm.cics.cip.bankliberty.web.db2.Account");
				myResponse = Response.status(500).entity(error.toString()).build(); 
				logger.exiting(this.getClass().getName(),"debitCreditAccount(Long sortCode, String accountNumber, BigDecimal apiAmount, boolean debitAccount)",myResponse);
				return myResponse;
			}
			else
			{
				error.put("errorMessage", "Failed to credit account " + accountNumber + " in com.ibm.cics.cip.bankliberty.web.db2.Account");
				logger.severe("Failed to credit account " + accountNumber + " in com.ibm.cics.cip.bankliberty.web.db2.Account");
				myResponse = Response.status(500).entity(error.toString()).build(); 
				logger.exiting(this.getClass().getName(),"debitCreditAccount(Long sortCode, String accountNumber, BigDecimal apiAmount, boolean debitAccount)",myResponse);
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
			error.put("errorMessage", "Failed to write to PROCTRAN data store");
			logger.severe("Failed to write to PROCTRAN data store");
			try {
				Task.getTask().rollback();
			} catch (InvalidRequestException e) {
			}
			myResponse = Response.status(500).entity(error.toString()).build(); 
			logger.exiting(this.getClass().getName(),"debitCreditAccount(Long sortCode, String accountNumber, BigDecimal apiAmount, boolean debitAccount)",myResponse);
			return myResponse;
		}

		response.put("sortCode", db2Account.getSortcode().trim());
		response.put("id", db2Account.getAccount_number());
		response.put("availableBalance", BigDecimal.valueOf(db2Account.getAvailable_balance()));
		response.put("actualBalance", BigDecimal.valueOf(db2Account.getActual_balance()));
		response.put("interestRate", BigDecimal.valueOf(db2Account.getInterest_rate()));
		myResponse = Response.status(200).entity(response.toString()).build();
		logger.exiting(this.getClass().getName(),"debitCreditAccount(Long sortCode, String accountNumber, BigDecimal apiAmount, boolean debitAccount)",myResponse);
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
		logger.entering(this.getClass().getName(),"deleteAccountInternal(Long accountNumber)");
		Response myResponse = null;


		JSONObject response = new JSONObject();

		Integer sortCode = this.getSortCode();

		com.ibm.cics.cip.bankliberty.web.db2.Account db2Account = new Account();

		db2Account = db2Account.deleteAccount(accountNumber.intValue(), sortCode.intValue());
		if(db2Account != null)
		{
			response.put("sortCode", db2Account.getSortcode().trim());
			response.put("id",db2Account.getAccount_number());
			response.put("customerNumber", db2Account.getCustomer_number());
			response.put("accountType", db2Account.getType().trim());
			response.put("availableBalance", BigDecimal.valueOf(db2Account.getAvailable_balance()));
			response.put("actualBalance", BigDecimal.valueOf(db2Account.getActual_balance()));
			response.put("interestRate", BigDecimal.valueOf(db2Account.getInterest_rate()));
			response.put("overdraft", db2Account.getOverdraft_limit());
			response.put("lastStatementDate", db2Account.getLast_statement().toString().trim());
			response.put("nextStatementDate", db2Account.getNext_statement().toString().trim());
			response.put("dateOpened", db2Account.getOpened().toString().trim());

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
				error.put("errorMessage", "Failed to write to PROCTRAN data store");
				logger.severe("Failed to write to PROCTRAN data store");
				try {
					Task.getTask().rollback();
				} catch (InvalidRequestException e) {
				}
				myResponse = Response.status(500).entity(error.toString()).build();
				logger.exiting(this.getClass().getName(),"deleteAccountInternal(Long accountNumber)",myResponse);
				return myResponse;
			}
		}
		else
		{
			try {
				logger.warning("Accounts: deleteAccount: Failed to find account " + accountNumber);
				Task.getTask().rollback();
			} catch (InvalidRequestException e) {
				e.printStackTrace();
			}
			response.put("errorMessage","Account " + accountNumber + " not found");
			myResponse = Response.status(404).entity(response.toString()).build();
			logger.exiting(this.getClass().getName(),"deleteAccountInternal(Long accountNumber)",myResponse);
			return myResponse;
		}
		/*
		 * Parse returned data and return to calling method
		 */



		myResponse = Response.status(200).entity(response.toString()).build();
		logger.exiting(this.getClass().getName(),"deleteAccountInternal(Long accountNumber)",myResponse);
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
				response.put("errorMessage","Accounts cannot be accessed");
				logger.severe("Accounts cannot be accessed");
				myResponse = Response.status(500).entity(response.toString()).build();
				logger.exiting(this.getClass().getName(), "getAccountsInternal(Integer limit, Integer offset,boolean countOnly)",myResponse);
				return myResponse;	
			}
			accounts = new JSONArray(myAccounts.length);

			for(int i = 0;i < myAccounts.length;i++)
			{
				JSONObject account = new JSONObject();
				account.put("sortCode", myAccounts[i].getSortcode().trim());
				account.put("id", myAccounts[i].getAccount_number());
				account.put("customerNumber", myAccounts[i].getCustomer_number());
				account.put("accountType", myAccounts[i].getType().trim());
				account.put("availableBalance", BigDecimal.valueOf(myAccounts[i].getAvailable_balance()));
				account.put("actualBalance", BigDecimal.valueOf(myAccounts[i].getActual_balance()));
				account.put("interestRate", BigDecimal.valueOf(myAccounts[i].getInterest_rate()));
				account.put("overdraft", myAccounts[i].getOverdraft_limit());
				account.put("lastStatementDate", myAccounts[i].getLast_statement().toString().trim());
				account.put("nextStatementDate", myAccounts[i].getNext_statement().toString().trim());
				account.put("dateOpened", myAccounts[i].getOpened().toString());

				accounts.add(account);

			}
			numberOfAccounts = accounts.size();
		}
		/*
		 * Parse returned data and return to calling method
		 */

		response.put("numberOfAccounts", numberOfAccounts);
		if(accounts != null)
		{
			response.put("accounts", accounts);
		}
		myResponse = Response.status(200).entity(response.toString()).build(); 
		logger.exiting(this.getClass().getName(),"deleteAccountInternal(Long accountNumber)",myResponse);
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
		logger.entering(this.getClass().getName(),"getAccountsByBalanceWithOffsetAndLimitInternal(BigDecimal balance, String operator, Integer offset, Integer limit, Boolean countOnly");
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
				error.put("errorMessage", "Invalid operator, '" + operator + "' only <= or >= allowed");
				logger.warning("Invalid operator, '" + operator + "' only <= or >= allowed");
				logger.exiting(this.getClass().getName(),"getAccountsByBalanceWithOffsetAndLimitInternal(BigDecimal balance, String operator, Integer offset, Integer limit, Boolean countOnly",myResponse);
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
				error.put("errorMessage", "Unable to access DB2 account store");
				logger.severe("Unable to access DB2 account store");
				logger.exiting(this.getClass().getName(),"getAccountsByBalanceWithOffsetAndLimitInternal(BigDecimal balance, String operator, Integer offset, Integer limit, Boolean countOnly",myResponse);
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
				error.put("errorMessage", "Unable to access DB2 account store");
				logger.severe("Unable to access DB2 account store");
				logger.exiting(this.getClass().getName(),"getAccountsByBalanceWithOffsetAndLimitInternal(BigDecimal balance, String operator, Integer offset, Integer limit, Boolean countOnly",myResponse);
				myResponse = Response.status(500).entity(error.toString()).build();
				return myResponse;
			}
			accounts = new JSONArray(myAccounts.length);



			for(int i = 0;i < myAccounts.length;i++)
			{
				JSONObject account = new JSONObject();
				account.put("sortCode", myAccounts[i].getSortcode().trim());
				account.put("id", myAccounts[i].getAccount_number());
				account.put("customerNumber", myAccounts[i].getCustomer_number());
				account.put("accountType", myAccounts[i].getType().trim());
				account.put("availableBalance", BigDecimal.valueOf(myAccounts[i].getAvailable_balance()));
				account.put("actualBalance", BigDecimal.valueOf(myAccounts[i].getActual_balance()));
				account.put("interestRate", BigDecimal.valueOf(myAccounts[i].getInterest_rate()));
				account.put("overdraft", myAccounts[i].getOverdraft_limit());
				account.put("lastStatementDate", myAccounts[i].getLast_statement().toString().trim());
				account.put("nextStatementDate", myAccounts[i].getNext_statement().toString().trim());
				account.put("dateOpened", myAccounts[i].getOpened().toString());

				accounts.add(account);

			}
			numberOfAccounts = accounts.size();
		}


		/*
		 * Parse returned data and return to calling method
		 */

		response.put("numberOfAccounts", numberOfAccounts);
		response.put("accounts", accounts);
		myResponse = Response.status(200).entity(response.toString()).build(); 
		logger.exiting(this.getClass().getName(),"getAccountsByBalanceWithOffsetAndLimitInternal(BigDecimal balance, String operator, Integer offset, Integer limit, Boolean countOnly",myResponse);
		return myResponse;


	}





}



