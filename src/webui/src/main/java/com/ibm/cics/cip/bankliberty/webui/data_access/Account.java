/*
 *
 *    Copyright IBM Corp. 2022
 *
 */

package com.ibm.cics.cip.bankliberty.webui.data_access;

import java.io.IOException;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.sql.Date;

import java.util.Calendar;
import java.util.logging.Level;
import java.util.logging.LogManager;
import java.util.logging.Logger;

import javax.ws.rs.core.Response;

import com.ibm.cics.cip.bankliberty.api.json.AccountJSON;
import com.ibm.cics.cip.bankliberty.api.json.AccountsResource;
import com.ibm.json.java.JSONObject;

public class Account
{

	static final String COPYRIGHT = "Copyright IBM Corp. 2022";

	private static Logger logger = Logger
			.getLogger("com.ibm.cics.cip.bankliberty.webui.data_access");

	private String customerNumber;

	private String sortcode;

	private String accountNumber;

	private String type;

	private BigDecimal interestRate;

	private Date opened;

	private int overdraftLimit;

	private Date lastStatement;

	private Date nextStatement;

	private BigDecimal availableBalance;

	private BigDecimal actualBalance;

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


	public Account()
	{
		sortOutLogging();
	}


	public Account(String custNo, String sortCo, String accNo, String type,
			BigDecimal intRate, Date opened, int overdraftL, Date lastStatement,
			Date nextStatement, BigDecimal avBal, BigDecimal acBal)
	{
		setCustomerNumber(custNo);
		setSortcode(sortCo);
		setAccountNumber(accNo);
		setType(type);
		setInterestRate(intRate);
		setOpened(opened);
		setOverdraftLimit(overdraftL);
		setLastStatement(lastStatement);
		setNextStatement(nextStatement);
		setAvailableBalance(avBal);
		setActualBalance(acBal);
	}


	public Account(String custNo, String sortCo, String accNo, String type,
			BigDecimal intRate, java.util.Date opened, int overdraftL,
			BigDecimal avBal, BigDecimal acBal)
	{
		setCustomerNumber(custNo);
		setSortcode(sortCo);
		setAccountNumber(accNo);
		setType(type);
		setInterestRate(intRate.setScale(2, RoundingMode.HALF_UP));
		setOpened(new Date(opened.getTime()));
		setOverdraftLimit(overdraftL);
		setLastStatement(null);
		setNextStatement(null);
		setAvailableBalance(avBal.setScale(2, RoundingMode.HALF_UP));
		setActualBalance(acBal.setScale(2, RoundingMode.HALF_UP));
	}


	public void showInfo()
	{
		logger.log(Level.INFO, () -> "------------" + this.accountNumber + ":"
				+ this.sortcode + "------------");
		logger.log(Level.INFO,
				() -> "Customer number - " + this.customerNumber);
		logger.log(Level.INFO, () -> "Type - " + this.type);
		logger.log(Level.INFO, () -> "Interest rate - " + this.interestRate);
		logger.log(Level.INFO, () -> "Opened - " + this.opened.toString());
		logger.log(Level.INFO,
				() -> "Overdraft Limit - " + this.overdraftLimit);
		logger.log(Level.INFO,
				() -> "Last Statement - " + this.lastStatement.toString());
		logger.log(Level.INFO,
				() -> "Next Statement - " + this.nextStatement.toString());
		logger.log(Level.INFO,
				() -> ("Available Balance - " + this.availableBalance));
		logger.log(Level.INFO, () -> "Actual Balance - " + this.actualBalance);
	}


	public String getCustomerNumber()
	{
		return customerNumber;
	}


	public void setCustomerNumber(String custNo)
	{
		this.customerNumber = custNo;
	}


	public String getSortcode()
	{
		return sortcode;
	}


	public void setSortcode(String sortcode)
	{
		this.sortcode = sortcode;
	}


	public String getAccountNumber()
	{
		return accountNumber;
	}


	public void setAccountNumber(String accNo)
	{
		this.accountNumber = accNo;
	}


	public String getType()
	{
		return type;
	}


	public void setType(String type)
	{
		this.type = type;
	}


	public BigDecimal getInterestRate()
	{
		return interestRate;
	}


	public void setInterestRate(BigDecimal intRate)
	{
		this.interestRate = intRate;
	}


	public Date getOpened()
	{
		return opened;
	}


	public void setOpened(Date opened)
	{
		this.opened = opened;
	}


	public int getOverdraftLimit()
	{
		return overdraftLimit;
	}


	public void setOverdraftLimit(int overdraft)
	{
		this.overdraftLimit = overdraft;
	}


	public Date getLastStatement()
	{
		return lastStatement;
	}


	public void setLastStatement(Date last)
	{
		if (last == null)
		{
			this.lastStatement = this.opened;
		}
		else
		{
			this.lastStatement = last;
		}
	}


	public Date getNextStatement()
	{
		return nextStatement;
	}


	public void setNextStatement(Date next)
	{
		if (next == null)
		{
			Calendar cal = Calendar.getInstance();
			cal.setTime(opened);
			cal.add(Calendar.DATE, +7);
			this.nextStatement = new Date(cal.getTime().getTime());
		}
		else
		{
			this.nextStatement = next;
		}
	}


	public BigDecimal getAvailableBalance()
	{
		return availableBalance;
	}


	public void setAvailableBalance(BigDecimal availableBalance)
	{
		this.availableBalance = availableBalance;
	}


	public BigDecimal getActualBalance()
	{
		return actualBalance;
	}


	public void setActualBalance(BigDecimal actualBalance)
	{
		this.actualBalance = actualBalance;
	}


	public boolean updateThis()
	{
		AccountsResource myAccountsResource = new AccountsResource();

		AccountJSON myAccountJSON = new AccountJSON();

		myAccountJSON.setAccountType(this.getType());
		myAccountJSON.setInterestRate(this.getInterestRate());
		myAccountJSON.setOverdraft(this.getOverdraftLimit());
		myAccountJSON.setSortCode(this.getSortcode());

		Response myAccountsResponse = myAccountsResource.updateAccountInternal(
				Long.parseLong(this.getAccountNumber()), myAccountJSON);

		String myAccountsString = null;
		JSONObject myAccountsJSON = null;

		if (myAccountsResponse.getStatus() == 200)
		{
			myAccountsString = myAccountsResponse.getEntity().toString();
			try
			{
				myAccountsJSON = JSONObject.parse(myAccountsString);
			}
			catch (IOException e)
			{
				logger.severe(e.toString());
				return false;
			}

			JSONObject myAccount = myAccountsJSON;

			this.lastStatement = sortOutDate(
					(String) myAccount.get(JSON_LAST_STATEMENT_DATE));
			this.nextStatement = sortOutDate(
					(String) myAccount.get(JSON_NEXT_STATEMENT_DATE));
			this.opened = sortOutDate((String) myAccount.get(JSON_DATE_OPENED));

			String accountNoString = (String) myAccount.get("id");

			String customerNoString = (String) myAccount
					.get(JSON_CUSTOMER_NUMBER);

			this.accountNumber = accountNoString;
			this.customerNumber = customerNoString;

			this.actualBalance = BigDecimal
					.valueOf((Double) myAccount.get(JSON_ACTUAL_BALANCE))
					.setScale(2, RoundingMode.HALF_UP);
			this.availableBalance = BigDecimal
					.valueOf((Double) myAccount.get(JSON_AVAILABLE_BALANCE))
					.setScale(2, RoundingMode.HALF_UP);
			this.interestRate = BigDecimal
					.valueOf((Double) myAccount.get(JSON_INTEREST_RATE))
					.setScale(2, RoundingMode.HALF_UP);

			Long myLong = (Long) myAccount.get(JSON_OVERDRAFT);

			this.overdraftLimit = myLong.intValue();

			this.sortcode = (String) myAccount.get(JSON_SORT_CODE);
			this.type = (String) myAccount.get(JSON_ACCOUNT_TYPE);
		}
		else
		{
			return false;
		}
		return true;
	}


	public boolean deleteFromDB()
	{
		AccountsResource myAccountsResource = new AccountsResource();

		Response myAccountsResponse = null;
		String myAccountsString = null;
		JSONObject myAccountsJSON = null;

		myAccountsResponse = myAccountsResource
				.deleteAccountInternal(Long.parseLong(this.accountNumber));
		if (myAccountsResponse.getStatus() == 200)
		{
			myAccountsString = myAccountsResponse.getEntity().toString();
			try
			{
				myAccountsJSON = JSONObject.parse(myAccountsString);
			}
			catch (IOException e)
			{

				logger.severe(e.toString());
				return false;
			}

			JSONObject myAccount = myAccountsJSON;

			this.lastStatement = sortOutDate(
					(String) myAccount.get(JSON_LAST_STATEMENT_DATE));
			this.nextStatement = sortOutDate(
					(String) myAccount.get(JSON_NEXT_STATEMENT_DATE));
			this.opened = sortOutDate((String) myAccount.get(JSON_DATE_OPENED));

			String accountNoString = (String) myAccount.get("id");

			String customerNoString = (String) myAccount
					.get(JSON_CUSTOMER_NUMBER);

			this.accountNumber = accountNoString;
			this.customerNumber = customerNoString;

			this.actualBalance = BigDecimal
					.valueOf((Double) myAccount.get(JSON_ACTUAL_BALANCE))
					.setScale(2, RoundingMode.HALF_UP);
			this.availableBalance = BigDecimal
					.valueOf((Double) myAccount.get(JSON_AVAILABLE_BALANCE))
					.setScale(2, RoundingMode.HALF_UP);
			this.interestRate = BigDecimal
					.valueOf((Double) myAccount.get(JSON_INTEREST_RATE))
					.setScale(2, RoundingMode.HALF_UP);

			Long myLong = (Long) myAccount.get(JSON_OVERDRAFT);

			this.overdraftLimit = myLong.intValue();
			
			this.sortcode = (String) myAccount.get(JSON_SORT_CODE);
			this.type = (String) myAccount.get(JSON_ACCOUNT_TYPE);
			return true;
		}
		return false;
	}


	public int addToDB()
	{
		AccountsResource myAccountsResource = new AccountsResource();

		Response myAccountsResponse = null;
		String myAccountsString = null;
		JSONObject myAccountsJSON = null;

		AccountJSON myAccountJSON = new AccountJSON();

		myAccountJSON.setAccountType(this.getType());
		myAccountJSON.setInterestRate(
				this.getInterestRate().setScale(2, RoundingMode.HALF_UP));
		myAccountJSON.setOverdraft(Integer.valueOf(this.getOverdraftLimit()));
		myAccountJSON.setCustomerNumber(this.getCustomerNumber());
		myAccountJSON.setSortCode(this.getSortcode());

		myAccountsResponse = myAccountsResource
				.createAccountInternal(myAccountJSON);

		if (myAccountsResponse.getStatus() == 201)
		{
			myAccountsString = myAccountsResponse.getEntity().toString();
			try
			{
				myAccountsJSON = JSONObject.parse(myAccountsString);
			}
			catch (IOException e)
			{
				logger.severe(e.toString());
				return -1;
			}

			JSONObject myAccount = myAccountsJSON;

			this.lastStatement = sortOutDate(
					(String) myAccount.get(JSON_LAST_STATEMENT_DATE));
			this.nextStatement = sortOutDate(
					(String) myAccount.get(JSON_NEXT_STATEMENT_DATE));
			this.opened = sortOutDate((String) myAccount.get(JSON_DATE_OPENED));

			String accountNoString = (String) myAccount.get("id");

			String customerNoString = (String) myAccount
					.get(JSON_CUSTOMER_NUMBER);

			this.accountNumber = accountNoString;
			this.customerNumber = customerNoString;

			this.actualBalance = BigDecimal
					.valueOf((Double) myAccount.get(JSON_ACTUAL_BALANCE))
					.setScale(2, RoundingMode.HALF_UP);
			this.availableBalance = BigDecimal
					.valueOf((Double) myAccount.get(JSON_AVAILABLE_BALANCE))
					.setScale(2, RoundingMode.HALF_UP);
			this.interestRate = BigDecimal
					.valueOf((Double) myAccount.get(JSON_INTEREST_RATE))
					.setScale(2, RoundingMode.HALF_UP);

			Long myLong = (Long) myAccount.get(JSON_OVERDRAFT);

			this.overdraftLimit = myLong.intValue();

			this.sortcode = (String) myAccount.get(JSON_SORT_CODE);
			this.type = (String) myAccount.get(JSON_ACCOUNT_TYPE);
			return Integer.parseInt(this.accountNumber);
		}
		return -1;
	}


	public boolean inDB()
	{
		AccountsResource myAccountsResource = new AccountsResource();

		Response myAccountsResponse = null;
		String myAccountsString = null;
		JSONObject myAccountsJSON = null;

		myAccountsResponse = myAccountsResource
				.getAccountInternal(Long.parseLong(this.accountNumber));
		if (myAccountsResponse.getStatus() == 200)
		{
			myAccountsString = myAccountsResponse.getEntity().toString();
			try
			{
				myAccountsJSON = JSONObject.parse(myAccountsString);
			}
			catch (IOException e)
			{
				logger.severe(e.toString());
				return false;
			}

			JSONObject myAccount = myAccountsJSON;

			this.lastStatement = sortOutDate(
					(String) myAccount.get(JSON_LAST_STATEMENT_DATE));
			this.nextStatement = sortOutDate(
					(String) myAccount.get(JSON_NEXT_STATEMENT_DATE));
			this.opened = sortOutDate((String) myAccount.get(JSON_DATE_OPENED));

			String accountNoString = (String) myAccount.get("id");

			String customerNoString = (String) myAccount
					.get(JSON_CUSTOMER_NUMBER);

			this.accountNumber = accountNoString;
			this.customerNumber = customerNoString;

			this.actualBalance = BigDecimal
					.valueOf((Double) myAccount.get(JSON_ACTUAL_BALANCE))
					.setScale(2, RoundingMode.HALF_UP);
			this.availableBalance = BigDecimal
					.valueOf((Double) myAccount.get(JSON_AVAILABLE_BALANCE))
					.setScale(2, RoundingMode.HALF_UP);
			this.interestRate = BigDecimal
					.valueOf((Double) myAccount.get(JSON_INTEREST_RATE))
					.setScale(2, RoundingMode.HALF_UP);

			Long myLong = (Long) myAccount.get(JSON_OVERDRAFT);
			this.overdraftLimit = myLong.intValue();
			
			this.sortcode = (String) myAccount.get(JSON_SORT_CODE);
			this.type = (String) myAccount.get(JSON_ACCOUNT_TYPE);
			return true;
		}
		return false;

	}


	private Date sortOutDate(String dateString)
	{
		String[] dateArray = dateString.split("-");

		Integer year = Integer.parseInt(dateArray[0]);
		Integer month = Integer.parseInt(dateArray[1]);
		Integer day = Integer.parseInt(dateArray[2]);

		Calendar myCalendar = Calendar.getInstance();
		myCalendar.set(Calendar.YEAR, year);
		myCalendar.set(Calendar.MONTH, month - 1);
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
