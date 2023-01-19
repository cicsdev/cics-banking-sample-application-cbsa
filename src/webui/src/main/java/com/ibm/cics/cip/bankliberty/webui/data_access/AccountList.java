/*
 *
 *    Copyright IBM Corp. 2022
 *
 */
package com.ibm.cics.cip.bankliberty.webui.data_access;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;
import java.util.logging.LogManager;
import java.util.logging.Logger;
import java.io.IOException;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.sql.Date;

import javax.ws.rs.core.Response;

import com.ibm.cics.cip.bankliberty.api.json.AccountsResource;
import com.ibm.cics.cip.bankliberty.api.json.SortCodeResource;
import com.ibm.json.java.JSONArray;
import com.ibm.json.java.JSONObject;

public class AccountList
{

	static final String COPYRIGHT = "Copyright IBM Corp. 2022";

	private static Logger logger = Logger.getLogger("com.ibm.cics.cip.bankliberty.webui.dataAccess");

	private List<Account> accountList = new ArrayList<>();
	private static String sortcode;
	private int count;
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
	private static final String JSON_ID = "id";

	public int getCount(String filter)
	{
		if (this.accountList.isEmpty())
		{
			howMany(filter);
		}
		return this.count;
	}

	public int howMany(String filter)
	{

		// AND ACCOUNT_NUMBER = 0000000024
		// AND ACCOUNT_CUSTOMER_NUMBER

		AccountsResource myAccountsResource = new AccountsResource();
		Response myAccountsResponse = null;

		try
		{
			if (filter.contains("AND ACCOUNT_AVAILABLE_BALANCE"))
			{
				// 01234567890123456789012345678901234567890
				// AND ACCOUNT_AVAILABLE_BALANCE <= 33558.0

				String operator = filter.substring(31, 32);
				BigDecimal balance = BigDecimal.valueOf(Double.parseDouble(filter.substring(34)));

				myAccountsResponse = myAccountsResource.getAccountsByBalanceWithOffsetAndLimitExternal(balance,
						operator, null, null, true);
			}

			if (filter.contains("AND ACCOUNT_NUMBER"))
			{
				// ("We are filtering by account");
				String accountNumberFilter = filter.substring(22);
				if (accountNumberFilter.indexOf(' ') >= 0)
				{
					accountNumberFilter = accountNumberFilter.substring(0, accountNumberFilter.indexOf(' '));
				}
				Long accountNumberFilterLong = Long.parseLong(accountNumberFilter);
				myAccountsResponse = myAccountsResource.getAccountExternal(accountNumberFilterLong);
			}

			if (filter.contains("AND ACCOUNT_CUSTOMER_NUMBER"))
			{
				// ("We are filtering by account_customer_number!");
				String customerNumberFilter = filter.substring(31);
				Long customerNumber = Long.parseLong(customerNumberFilter);

				myAccountsResponse = myAccountsResource.getAccountsByCustomerExternal(customerNumber, true);

			}

			if (filter.length() == 0)
			{
				// ("No filter so get the lot please!");
				myAccountsResponse = myAccountsResource.getAccountsExternal(true);
				this.count = 0;
			}
			if (myAccountsResponse != null && myAccountsResponse.getStatus() == 200)
			{

				String myAccountsString = myAccountsResponse.getEntity().toString();

				JSONObject myAccountsJSON = JSONObject.parse(myAccountsString);
				long accountCount = (Long) myAccountsJSON.get(JSON_NUMBER_OF_ACCOUNTS);
				this.count = (int) accountCount;
			}
		}
		catch (IOException e)
		{
			logger.severe(e.toString());
		}
		return this.count;

	}

	public void doGet(int limit, int offset, String filter) throws IOException
	{

		AccountsResource myAccountsResource = new AccountsResource();

		Response myAccountsResponse = null;
		String myAccountsString = null;
		JSONObject myAccountsJSON = null;

		try
		{

			if (filter.contains(" AND ACCOUNT_AVAILABLE_BALANCE"))
			{
				this.accountList.clear();
				String operator = filter.substring(31, 32);
				BigDecimal balance = BigDecimal.valueOf(Double.parseDouble(filter.substring(34)));

				myAccountsResponse = myAccountsResource.getAccountsByBalanceWithOffsetAndLimitExternal(balance,
						operator, offset, limit, false);
			}

			if (filter.contains(" AND ACCOUNT_NUMBER"))
			{
				this.accountList.clear();

				String accountNumberFilter = filter.substring(22);
				Long accountNumberFilterLong = Long.parseLong(accountNumberFilter);

				myAccountsResponse = myAccountsResource.getAccountExternal(accountNumberFilterLong);
			}

			// 0123456789012345678901234567890
			if (filter.contains(" AND ACCOUNT_CUSTOMER_NUMBER = "))
			{
				this.accountList.clear();
				String customerNumberFilter = filter.substring(31);
				Long customerNumber = Long.parseLong(customerNumberFilter);

				myAccountsResponse = myAccountsResource.getAccountsByCustomerExternal(customerNumber, false);
			}

			if (filter.length() == 0)
			{
				this.accountList.clear();
				myAccountsResponse = myAccountsResource.getAccountsExternal(limit, offset, false);
			}

			if (offset == 0)
			{
				// ("Offset is zero to doing howMany for filter " + filter);
				howMany(filter);
			}
			myAccountsString = myAccountsResponse.getEntity().toString();

			if (myAccountsResponse.getStatus() == 200)
			{

				myAccountsJSON = JSONObject.parse(myAccountsString);
				long accountCount = (Long) myAccountsJSON.get(JSON_NUMBER_OF_ACCOUNTS);
				this.count = (int) accountCount;
				JSONArray myAccountsArrayJSON = (JSONArray) myAccountsJSON.get(JSON_ACCOUNTS);

				for (int i = 0; i < accountCount; i++)
				{
					JSONObject myAccount = (JSONObject) myAccountsArrayJSON.get(i);

					Date lastStatement = sortOutDate((String) myAccount.get(JSON_LAST_STATEMENT_DATE));
					Date nextStatement = sortOutDate((String) myAccount.get(JSON_NEXT_STATEMENT_DATE));
					Date dateOpened = sortOutDate((String) myAccount.get(JSON_DATE_OPENED));
					String id = (String) myAccount.get(JSON_ID);
					String customerNumberString = (String) myAccount.get(JSON_CUSTOMER_NUMBER);

					BigDecimal actualBalance = BigDecimal.valueOf((Double) myAccount.get(JSON_ACTUAL_BALANCE))
							.setScale(2, RoundingMode.HALF_UP);
					BigDecimal availableBalance = BigDecimal.valueOf((Double) myAccount.get(JSON_AVAILABLE_BALANCE))
							.setScale(2, RoundingMode.HALF_UP);
					BigDecimal interestRate = BigDecimal.valueOf((Double) myAccount.get(JSON_INTEREST_RATE))
							.setScale(2, RoundingMode.HALF_UP);
					Long overdraft = (Long) myAccount.get(JSON_OVERDRAFT);
					String sortCode = (String) myAccount.get(JSON_SORT_CODE);
					String type = (String) myAccount.get(JSON_ACCOUNT_TYPE);

					Account myListAccount = new Account();
					myListAccount.setAccountNumber(id);
					myListAccount.setActualBalance(actualBalance.setScale(2, RoundingMode.HALF_UP));
					myListAccount.setAvailableBalance(availableBalance.setScale(2, RoundingMode.HALF_UP));
					myListAccount.setCustomerNumber(customerNumberString);
					myListAccount.setInterestRate(interestRate.setScale(2, RoundingMode.HALF_UP));
					myListAccount.setLastStatement(lastStatement);
					myListAccount.setNextStatement(nextStatement);
					myListAccount.setOpened(dateOpened);
					myListAccount.setOverdraftLimit(overdraft.intValue());
					myListAccount.setSortcode(sortCode);
					myListAccount.setType(type);

					this.accountList.add(myListAccount);
				}

			}

		}
		catch (IOException e)
		{
			logger.severe(e.toString());
		}
	}

	public Account getAccount(int i)
	{
		return this.accountList.get(i);
	}

	public int size()
	{
		return this.accountList.size();
	}

	public AccountList()
	{
		sortOutLogging();
		AccountList.setSortcode();
	}

	private static void setSortcode()
	{
		if (sortcode == null)
		{
			SortCodeResource mySortCodeResource = new SortCodeResource();
			Response mySortCodeJSON = mySortCodeResource.getSortCode();
			sortcode = ((String) mySortCodeJSON.getEntity()).substring(13, 19);
		}

	}

	public String getSortCode()
	{
		return sortcode;
	}

	public List<Account> getList()
	{
		return accountList;
	}

	
	private Date sortOutDate(String dateString)
	{
		String[] dateArray = dateString.split("-");

		Integer year = Integer.parseInt(dateArray[0]);
		Integer month = Integer.parseInt(dateArray[1]);
		Integer day = Integer.parseInt(dateArray[2]);

		Calendar myCalendar = Calendar.getInstance();
		myCalendar.set(Calendar.YEAR, year);
		myCalendar.set(Calendar.MONTH, month-1);
		myCalendar.set(Calendar.DATE, day);
				
		return new Date(myCalendar.getTimeInMillis());

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
