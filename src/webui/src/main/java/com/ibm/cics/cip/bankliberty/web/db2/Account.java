/*
 *
 *    Copyright IBM Corp. 2022
 *
 */


package com.ibm.cics.cip.bankliberty.web.db2;



import java.math.BigDecimal;
import java.sql.Date;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Calendar;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.ibm.cics.cip.bankliberty.api.json.AccountJSON;
import com.ibm.cics.cip.bankliberty.api.json.HBankDataAccess;
import com.ibm.cics.server.InvalidRequestException;
import com.ibm.cics.server.Task;

public class Account extends HBankDataAccess{

	static final String COPYRIGHT =
			"Copyright IBM Corp. 2022";




	private static Logger logger = Logger.getLogger("com.ibm.cics.cip.bankliberty.web.db2");
	
	private static final String GET_ACCOUNT = "getAccount(int accountNumber, int sortCode) for account ";
	private static final String GET_ACCOUNTS_CUSTNO = "getAccounts(long l, int sortCode) for customer ";
	private static final String GET_ACCOUNTS = "getAccounts(int sortCode)";
	private static final String GET_ACCOUNTS_COUNT_ONLY = "getAccountsCountOnly(int sortCode)";
	private static final String DELETE_ACCOUNT = "deleteAccount(int account, int sortCode)";
	private static final String CREATE_ACCOUNT = "createAccount(AccountJSON account, Integer sortcode, boolean use NamedCounter)";
	private static final String UPDATE_ACCOUNT = "updateAccount(AccountJSON account)";
	private static final String DEBIT_CREDIT_ACCOUNT = "debitCredit(BigDecimal apiAmount)";
	private static final String GET_ACCOUNTS_BY_BALANCE = "getAccountsByBalance(Integer sortCode2, BigDecimal balance, boolean lessThan)";
	private static final String GET_ACCOUNTS_WITH_LIMIT_AND_OFFSET = "getAccounts(Integer sortCode, int limit, int offset)";
	private static final String GET_ACCOUNTS_BY_BALANCE_WITH_LIMIT_AND_OFFSET = "getAccountsByBalance(Integer sortCode2, BigDecimal balance, boolean lessThan, int offset, int limit)";
	private static final String GET_ACCOUNTS_COUNT_ONLY2 = "getAccountsCountOnly(Integer sortCode2)";
	private static final String GET_ACCOUNTS_BY_BALANCE_COUNT_ONLY = "getAccountsByBalanceCountOnly(Integer sortCode2, BigDecimal balance, boolean lessThan, Integer offset, Integer limit)";
	
	private static final String PRE_SELECT_MSG = "About to do SELECT <";
	private static final String SQL_WAS = "SQL was <";
	
	private static final String ACCOUNT_SORTCODE = "ACCOUNT_SORTCODE";
	private static final String ACCOUNT_CUSTOMER_NUMBER = "ACCOUNT_CUSTOMER_NUMBER";
	private static final String ACCOUNT_NUMBER = "ACCOUNT_NUMBER";
	private static final String ACCOUNT_TYPE = "ACCOUNT_TYPE";
	private static final String ACCOUNT_INTEREST_RATE = "ACCOUNT_INTEREST_RATE";
	private static final String ACCOUNT_LAST_STATEMENT = "ACCOUNT_LAST_STATEMENT";
	private static final String ACCOUNT_NEXT_STATEMENT = "ACCOUNT_NEXT_STATEMENT";
	private static final String ACCOUNT_OVERDRAFT_LIMIT = "ACCOUNT_OVERDRAFT_LIMIT";
	private static final String ACCOUNT_OPENED = "ACCOUNT_OPENED";
	private static final String ACCOUNT_AVAILABLE_BALANCE = "ACCOUNT_AVAILABLE_BALANCE";
	private static final String ACCOUNT_ACTUAL_BALANCE = "ACCOUNT_ACTUAL_BALANCE";
	private static final String ACCOUNT_COUNT = "ACCOUNT_COUNT";
	
	private static final String SQL_SELECT = "SELECT * from ACCOUNT where ACCOUNT_EYECATCHER LIKE 'ACCT' AND ACCOUNT_NUMBER like ? and ACCOUNT_SORTCODE like ?";
	private static final String SQL_LESS_THAN = " AND ACCOUNT_ACTUAL_BALANCE <= ?";
	private static final String SQL_MORE_THAN = " AND ACCOUNT_ACTUAL_BALANCE >= ?";
	
	// </copyright>

	// String ACCOUNT_EYECATCHER             CHAR(4),
	private 	String 		customerNumber;
	private 	String 		sortcode;              
	private 	String 		accountNumber;
	private 	String 		type;
	private 	double 		interestRate;
	private 	Date 		opened;
	private 	int 		overdraftLimit;
	private 	Date 		last_statement;
	private 	Date 		next_statement;
	private	 	double 		availableBalance;    
	private 	double 		actualBalance;

	public Account()
	{
		sortOutLogging();
	}



	public Account (String custNo, String sortCo, String accNo, String type, double intRate, Date opened, int overdraft, Date lastStatement, Date nextStatement, double avBal, double acBal) {
		setCustomerNumber(custNo);
		setSortcode(sortCo);
		setAccountNumber(accNo);
		setType(type);
		setInterestRate(intRate);
		setOpened(opened);
		setOverdraftLimit(overdraft);
		setLastStatement(lastStatement);
		setNextStatement(nextStatement);
		setAvailableBalance(avBal);
		setActualBalance(acBal);
		sortOutLogging();
	}

	public Account(String custNo, String sortCo, String accNo, String type, double intRate, java.util.Date opened,
			int overdraft, Date lastStatement, Date nextStatement, double avBal, double acBal) {
		setCustomerNumber(custNo);
		setSortcode(sortCo);
		setAccountNumber(accNo);
		setType(type);
		setInterestRate(intRate);
		setOpened(new Date(opened.getTime()));
		setOverdraftLimit(overdraft);
		setLastStatement(lastStatement);
		setNextStatement(nextStatement);
		setAvailableBalance(avBal);
		setActualBalance(acBal);
		sortOutLogging();
	}

	public void showInfo(){
		logger.log(Level.FINE, () -> "------------"+this.accountNumber+":"+this.sortcode+"------------");
		logger.log(Level.FINE, () -> "Customer number - "+this.customerNumber);
		logger.log(Level.FINE, () -> "Type - "+this.type);
		logger.log(Level.FINE, () -> "Interest rate - "+this.interestRate);
		logger.log(Level.FINE, () -> "Opened - "+this.opened.toString());
		logger.log(Level.FINE, () -> "Overdraft Limit - "+this.overdraftLimit);
		logger.log(Level.FINE, () -> "Last Statement - "+this.last_statement.toString());
		logger.log(Level.FINE, () -> "Next Statement - "+this.next_statement.toString());
		logger.log(Level.FINE, () -> "Available Balance - "+this.availableBalance);
		logger.log(Level.FINE, () -> "Actual Balance - "+this.actualBalance);
	}

	public String getCustomerNumber() {
		if(this.customerNumber.length()<10)
		{
			StringBuilder myStringBuilder = new StringBuilder();
			

			for (int i=this.customerNumber.length();i<10;i++)
			{
				myStringBuilder.append('0');
			}
			myStringBuilder.append(this.customerNumber);
			this.customerNumber = myStringBuilder.toString();
		}
		return this.customerNumber;
	}

	public void setCustomerNumber(String custNo) {

		if(custNo.length()<10)
		{
			StringBuilder myStringBuilder = new StringBuilder();
			

			for (int i=custNo.length();i<10;i++)
			{
				myStringBuilder.append('0');
			}
			myStringBuilder.append(custNo);
			custNo = myStringBuilder.toString();
		}

		this.customerNumber = custNo;
	}

	public String getSortcode() {
		return sortcode;
	}

	public void setSortcode(String sortcode) {
		this.sortcode = sortcode;
	}

	public String getAccountNumber() {
		if(this.accountNumber.length()<8)
		{
			for (int i=8;i>0 || this.accountNumber.length()<8;i--)
			{
				this.accountNumber = "0" + this.accountNumber;
			}
		}
		return this.accountNumber;
	}

	public void setAccountNumber(String accNo) {
		if(accNo.length()<8)
		{
			for (int i=8;i>0 || accNo.length()<8;i--)
			{
				accNo = "0" + accNo;
			}
		}
		this.accountNumber = accNo;
	}

	public String getType() {
		return type;
	}

	public void setType(String type) {
		this.type = type;
	}

	public double getInterestRate() {
		return interestRate;
	}

	public void setInterestRate(double interestRate) {
		this.interestRate = interestRate;
	}

	public Date getOpened() {
		return opened;
	}

	public void setOpened(Date opened) {
		this.opened = opened;
	}

	public int getOverdraftLimit() {
		return overdraftLimit;
	}

	public void setOverdraftLimit(int overdraftLimit) {
		this.overdraftLimit = overdraftLimit;
	}

	public Date getLastStatement() {
		return last_statement;
	}

	public void setLastStatement(Date lastStatement) {
		if(lastStatement == null){
			this.last_statement = this.opened;
		}
		else{
			this.last_statement = lastStatement;
		}
	}

	public Date getNextStatement() {
		return next_statement;
	}

	
	public void setNextStatement(Date nextStatement) {
		if(nextStatement == null){
			Calendar cal = Calendar.getInstance();
			cal.setTime(opened);
			cal.add(Calendar.DATE, +7);
			this.next_statement =  new Date(cal.getTime().getTime());
		}
		else{
			this.next_statement = nextStatement;
		}
	}



	public double getAvailableBalance() {
		return availableBalance;
	}

	public void setAvailableBalance(double availableBalance) {
		this.availableBalance = availableBalance;
	}

	public double getActualBalance() {
		return actualBalance;
	}

	public void setActualBalance(double actualBalance) {
		this.actualBalance = actualBalance;
	}

	public void updateThis(){
		openConnection();

		String sql = "UPDATE ACCOUNT SET ACCOUNT_TYPE = ? ,ACCOUNT_INTEREST_RATE = ? ,ACCOUNT_OVERDRAFT_LIMIT = ? ,ACCOUNT_LAST_STATEMENT = ? ,ACCOUNT_NEXT_STATEMENT = ? ,ACCOUNT_AVAILABLE_BALANCE = ? ,ACCOUNT_ACTUAL_BALANCE = ? WHERE ACCOUNT_NUMBER like ? AND ACCOUNT_SORTCODE like ?";
		try(PreparedStatement stmt = conn.prepareStatement(sql);){
			stmt.setString(1,this.type);
			stmt.setDouble(2, this.interestRate);
			stmt.setInt(3, this.overdraftLimit);
			stmt.setString(4,this.last_statement.toString());
			stmt.setString(5,this.next_statement.toString());
			stmt.setDouble(6, this.availableBalance);
			stmt.setDouble(7, this.actualBalance);
			stmt.setString(8, this.accountNumber);
			stmt.setString(9,this.sortcode);
			stmt.executeUpdate();
		} catch (SQLException e) 
		{
			logger.severe(e.toString());
		}
	}


	public Account getAccount(int accountNumber, int sortCode){
		logger.entering(this.getClass().getName(),GET_ACCOUNT + accountNumber);
		openConnection();
		Account temp = null;
		StringBuffer myStringBuffer = new StringBuffer(new Integer(sortCode).toString());
		for(int z = myStringBuffer.length(); z < 6;z++)
		{
			myStringBuffer = myStringBuffer.insert(0, "0");	
		}
		String sortCodeString = myStringBuffer.toString();
		if(accountNumber == 99999999)
		{
			String sql = "SELECT * from ACCOUNT where ACCOUNT_EYECATCHER LIKE 'ACCT' AND ACCOUNT_SORTCODE like ? order by ACCOUNT_NUMBER DESC";
			logger.fine(PRE_SELECT_MSG + sql + ">");
			try (PreparedStatement stmt = conn.prepareStatement(sql);)
			{
				stmt.setString(1, sortCodeString);
				ResultSet rs = stmt.executeQuery();
				if(rs.next())
				{
					temp = new Account(rs.getString(ACCOUNT_CUSTOMER_NUMBER), rs.getString(ACCOUNT_SORTCODE),
							rs.getString(ACCOUNT_NUMBER), rs.getString(ACCOUNT_TYPE), rs.getDouble(ACCOUNT_INTEREST_RATE),
							rs.getDate(ACCOUNT_OPENED), rs.getInt(ACCOUNT_OVERDRAFT_LIMIT), rs.getDate(ACCOUNT_LAST_STATEMENT),
							rs.getDate(ACCOUNT_NEXT_STATEMENT), rs.getDouble(ACCOUNT_AVAILABLE_BALANCE),
							rs.getDouble(ACCOUNT_ACTUAL_BALANCE));
					rs.close();
					logger.exiting(this.getClass().getName(),GET_ACCOUNT + accountNumber,temp);
					return temp;
				}
				else
				{
					logger.warning("No results found");
					logger.exiting(this.getClass().getName(),GET_ACCOUNT + accountNumber,null);
					return null;
				}

			} catch (SQLException e) {
				logger.severe(e.getLocalizedMessage());
				logger.exiting(this.getClass().getName(),GET_ACCOUNT + accountNumber,null);
				return null;
			}
		}
		else
		{
			String sql = SQL_SELECT;
			try (PreparedStatement stmt = conn.prepareStatement(sql);)
			{
				logger.fine(PRE_SELECT_MSG + sql + ">");
				myStringBuffer = new StringBuffer(new Integer(accountNumber).toString());
				for(int z = myStringBuffer.length(); z < 8;z++)
				{
					myStringBuffer = myStringBuffer.insert(0, "0");	
				}
				String accountNumberString = myStringBuffer.toString();
				stmt.setString(1,accountNumberString);
				stmt.setString(2,sortCodeString);

				ResultSet rs = stmt.executeQuery();
				if(rs.isClosed())
				{
					logger.warning("Result set is closed so returning 'temp' which is " + temp);
					logger.exiting(this.getClass().getName(),GET_ACCOUNT + accountNumber,temp);
					return temp;
				}
				else
				{
					while(rs.next())
					{
						temp = new Account(rs.getString(ACCOUNT_CUSTOMER_NUMBER), rs.getString(ACCOUNT_SORTCODE),
								rs.getString(ACCOUNT_NUMBER), rs.getString(ACCOUNT_TYPE), rs.getDouble(ACCOUNT_INTEREST_RATE),
								rs.getDate(ACCOUNT_OPENED), rs.getInt(ACCOUNT_OVERDRAFT_LIMIT), rs.getDate(ACCOUNT_LAST_STATEMENT),
								rs.getDate(ACCOUNT_NEXT_STATEMENT), rs.getDouble(ACCOUNT_AVAILABLE_BALANCE),
								rs.getDouble(ACCOUNT_ACTUAL_BALANCE));
						rs.close();
						logger.exiting(this.getClass().getName(),GET_ACCOUNT + accountNumber,temp);
						return temp;
					}

				}

			} catch (SQLException e) {
				logger.severe(e.getLocalizedMessage());
				logger.exiting(this.getClass().getName(),GET_ACCOUNT + accountNumber,null);
				return null;
			}
		}
		logger.exiting(this.getClass().getName(),GET_ACCOUNT + accountNumber,temp);
		return temp;
	}

	public Account[] getAccounts(long l, int sortCode){
		logger.entering(this.getClass().getName(),GET_ACCOUNTS_CUSTNO + l);
		openConnection();
		Account[] temp = new Account[10];
		int i = 0;
		StringBuffer myStringBuffer = new StringBuffer(new Long(l).toString());
		for(int z = myStringBuffer.length(); z < 10;z++)
		{
			myStringBuffer = myStringBuffer.insert(0, "0");	
		}
		String customerNumberString = myStringBuffer.toString();

		myStringBuffer = new StringBuffer(new Integer(sortCode).toString());
		for(int z = myStringBuffer.length(); z < 6;z++)
		{
			myStringBuffer = myStringBuffer.insert(0, "0");	
		}
		String sortCodeString = myStringBuffer.toString();

		String sql = "SELECT * from ACCOUNT where ACCOUNT_EYECATCHER LIKE 'ACCT' AND ACCOUNT_CUSTOMER_NUMBER like ? and ACCOUNT_SORTCODE like ? ORDER BY ACCOUNT_NUMBER";
		logger.fine(PRE_SELECT_MSG + sql + ">");
		try (PreparedStatement stmt = conn.prepareStatement(sql);)
		{
			stmt.setString(1, customerNumberString);
			stmt.setString(2, sortCodeString);

			ResultSet rs = stmt.executeQuery();
			while (rs.next()) {
				temp[i] = new Account(rs.getString(ACCOUNT_CUSTOMER_NUMBER), rs.getString(ACCOUNT_SORTCODE),
						rs.getString(ACCOUNT_NUMBER), rs.getString(ACCOUNT_TYPE), rs.getDouble(ACCOUNT_INTEREST_RATE),
						rs.getDate(ACCOUNT_OPENED), rs.getInt(ACCOUNT_OVERDRAFT_LIMIT), rs.getDate(ACCOUNT_LAST_STATEMENT),
						rs.getDate(ACCOUNT_NEXT_STATEMENT), rs.getDouble(ACCOUNT_AVAILABLE_BALANCE),
						rs.getDouble(ACCOUNT_ACTUAL_BALANCE));
				i++;
			}
		} catch (SQLException e) {
			logger.severe(e.getLocalizedMessage());
			logger.exiting(this.getClass().getName(),GET_ACCOUNTS_CUSTNO + l,null);
			return null;
		}
		Account[] real = new Account[i];
		for(int j=0;j<i;j++)
		{
			real[j] = temp[j];
		}
		logger.exiting(this.getClass().getName(),GET_ACCOUNTS_CUSTNO + l,real);
		return real;
	}

	public Account[] getAccounts(int sortCode){
		logger.entering(this.getClass().getName(),GET_ACCOUNTS);
		openConnection();
		Account[] temp = new Account[250000];
		int i = 0;
		StringBuffer myStringBuffer = new StringBuffer(new Integer(sortCode).toString());
		for(int z = myStringBuffer.length(); z < 6;z++)
		{
			myStringBuffer = myStringBuffer.insert(0, "0");	
		}
		String sortCodeString = myStringBuffer.toString();
		String sql = "SELECT * from ACCOUNT where ACCOUNT_EYECATCHER LIKE 'ACCT' AND ACCOUNT_SORTCODE like ? ORDER BY ACCOUNT_NUMBER";
		logger.fine("About is issue query SQL <" + sql + ">");
		try (PreparedStatement stmt = conn.prepareStatement(sql);)
		{
			stmt.setString(1, sortCodeString);
			ResultSet rs = stmt.executeQuery();
			while (rs.next()) {
				temp[i] = new Account(rs.getString(ACCOUNT_CUSTOMER_NUMBER), rs.getString(ACCOUNT_SORTCODE),
						rs.getString(ACCOUNT_NUMBER), rs.getString(ACCOUNT_TYPE), rs.getDouble(ACCOUNT_INTEREST_RATE),
						rs.getDate(ACCOUNT_OPENED), rs.getInt(ACCOUNT_OVERDRAFT_LIMIT), rs.getDate(ACCOUNT_LAST_STATEMENT),
						rs.getDate(ACCOUNT_NEXT_STATEMENT), rs.getDouble(ACCOUNT_AVAILABLE_BALANCE),
						rs.getDouble(ACCOUNT_ACTUAL_BALANCE));
				i++;
			}
		} catch (SQLException e) {
			logger.severe(e.getLocalizedMessage());
			logger.exiting(this.getClass().getName(),GET_ACCOUNTS,null);
			return null;
		}
		Account[] real = new Account[i];
		for(int j=0;j<i;j++)
		{
			real[j] = temp[j];
		}
		logger.exiting(this.getClass().getName(),GET_ACCOUNTS,real);
		return real;
	}

	public int getAccountsCountOnly(int sortCode){
		logger.entering(this.getClass().getName(),GET_ACCOUNTS_COUNT_ONLY);
		openConnection();
		StringBuffer myStringBuffer = new StringBuffer(new Integer(sortCode).toString());
		for(int z = myStringBuffer.length(); z < 6;z++)
		{
			myStringBuffer = myStringBuffer.insert(0, "0");	
		}
		String sortCodeString = myStringBuffer.toString();
		String sql = "SELECT COUNT(*) as ACCOUNT_COUNT from ACCOUNT where ACCOUNT_EYECATCHER LIKE 'ACCT' AND ACCOUNT_SORTCODE like ?";
		logger.fine(PRE_SELECT_MSG + sql + ">");
		try (PreparedStatement stmt = conn.prepareStatement(sql);)
		{
			stmt.setString(1, sortCodeString);
			ResultSet rs = stmt.executeQuery();
			if(rs.next())
			{
				logger.exiting(this.getClass().getName(),GET_ACCOUNTS_COUNT_ONLY,rs.getInt(ACCOUNT_COUNT));
				return rs.getInt(ACCOUNT_COUNT);
			}
			else 
			{
				logger.exiting(this.getClass().getName(),GET_ACCOUNTS_COUNT_ONLY,-1);
				return -1;
			}
		} catch (SQLException e) {
			logger.severe(e.getLocalizedMessage());
			logger.exiting(this.getClass().getName(),GET_ACCOUNTS_COUNT_ONLY,-1);
			return -1;
		}
	}

	public Account deleteAccount(int account, int sortcode) {
		logger.entering(this.getClass().getName(),DELETE_ACCOUNT);
		Account db2Account = this.getAccount(account, sortcode);
		if(db2Account == null)
		{
			logger.exiting(this.getClass().getName(),DELETE_ACCOUNT,null);
			return null;
		}
		Account temp = null;
		openConnection();
		StringBuffer myStringBuffer = new StringBuffer(new Integer(account).toString());
		for(int z = myStringBuffer.length(); z < 8;z++)
		{
			myStringBuffer = myStringBuffer.insert(0, "0");	
		}
		String accountNumberString = myStringBuffer.toString();

		myStringBuffer = new StringBuffer(new Integer(sortcode).toString());
		for(int z = myStringBuffer.length(); z < 6;z++)
		{
			myStringBuffer = myStringBuffer.insert(0, "0");	
		}
		String sortCodeString = myStringBuffer.toString();

		String sql1 = SQL_SELECT;
		String sql2 = "DELETE from ACCOUNT where ACCOUNT_EYECATCHER LIKE 'ACCT' AND ACCOUNT_NUMBER like ? and ACCOUNT_SORTCODE like ?";
		try (PreparedStatement stmt = conn.prepareStatement(sql1);
			PreparedStatement stmt2 = conn.prepareStatement(sql2);)
		{
			logger.fine(PRE_SELECT_MSG + sql1 + ">");

			stmt.setString(1, accountNumberString);
			stmt.setString(2,sortCodeString);
			ResultSet rs = stmt.executeQuery();
			while (rs.next()) 
			{
				temp = new Account(rs.getString(ACCOUNT_CUSTOMER_NUMBER), rs.getString(ACCOUNT_SORTCODE),
						rs.getString(ACCOUNT_NUMBER), rs.getString(ACCOUNT_TYPE), rs.getDouble(ACCOUNT_INTEREST_RATE),
						rs.getDate(ACCOUNT_OPENED), rs.getInt(ACCOUNT_OVERDRAFT_LIMIT), rs.getDate(ACCOUNT_LAST_STATEMENT),
						rs.getDate(ACCOUNT_NEXT_STATEMENT), rs.getDouble(ACCOUNT_AVAILABLE_BALANCE),
						rs.getDouble(ACCOUNT_ACTUAL_BALANCE));
				db2Account = temp;
				logger.fine("About to issue delete SQL <" + sql2 + ">");
				stmt2.setString(1, accountNumberString);
				stmt2.setString(2,sortCodeString);
				stmt2.execute();
				logger.exiting(this.getClass().getName(),DELETE_ACCOUNT,temp);
				return temp;
			}
		} 
		catch (SQLException e) 
		{
			logger.severe(e.getLocalizedMessage());
			logger.exiting(this.getClass().getName(),DELETE_ACCOUNT,null);
			return null;
		}
		logger.exiting(this.getClass().getName(),DELETE_ACCOUNT,db2Account);
		return db2Account;
	}



	@SuppressWarnings("deprecation")
	public Account createAccount(AccountJSON account, Integer sortcode, boolean useNamedCounter) 
	{
		logger.entering(this.getClass().getName(),CREATE_ACCOUNT);

		Account temp = null;
		openConnection();
		String accountNumberString = null;
		StringBuffer myStringBuffer = null;

		temp = new Account();
		myStringBuffer = new StringBuffer(sortcode.toString());
		for(int z = myStringBuffer.length(); z < 6;z++)
		{
			myStringBuffer = myStringBuffer.insert(0, "0");	
		}
		String sortCodeString = myStringBuffer.toString();

		Long accountNumber =0L;
		String controlString = sortcode.toString() + "-" + "ACCOUNT-LAST";
		String sqlControl = "SELECT * from CONTROL where CONTROL_NAME = ?";
		logger.fine(PRE_SELECT_MSG + sqlControl + ">" + " " + controlString);

		try(PreparedStatement stmt = conn.prepareStatement(sqlControl);) 
		{
			stmt.setString(1,controlString);
			ResultSet rs = stmt.executeQuery();
			if(rs.next())
			{
				accountNumber = (Long) rs.getLong("CONTROL_VALUE_NUM");
				rs.close();
			}
		} 
		catch (SQLException e) 
		{
			logger.severe("Error accessing Control Table for SELECT " + e.getLocalizedMessage());
			logger.exiting(this.getClass().getName(),CREATE_ACCOUNT,null);
			try {
				Task.getTask().rollback();
			} catch (InvalidRequestException e1) {
				logger.severe(e1.toString());
			}
			return null;
		}
		accountNumber++;

		myStringBuffer = new StringBuffer(accountNumber.toString());
		for(int z = myStringBuffer.length(); z < 8;z++)
		{
			myStringBuffer = myStringBuffer.insert(0, "0");	
		}
		accountNumberString = myStringBuffer.toString();


		//
		// Store today's date as the ACCOUNT-OPENED date and calculate
		// the LAST-STMT-DATE (which should be today) and the
		// NEXT-STMT-DATE (which should be today + 30 days).

		Calendar myCalendar = Calendar.getInstance();
		Date today = new Date(myCalendar.getTimeInMillis());

		Date dateOpened = today;
		Date lastStatement = dateOpened;

		temp.setOpened(dateOpened);
		temp.setLastStatement(lastStatement);

		long timeNow = myCalendar.getTimeInMillis();
		// You must specify the values here as longs otherwise it ends up negative due to overflow
		long nextMonthInMs = 0L;
		switch(today.getMonth())
		{
		case 8:
		case 3:
		case 5:
		case 10:
			nextMonthInMs = 1000L * 60L * 60L * 24L * 30L;
			break;
		case 1:
			if((today.getYear() + 1900) % 4 > 0)
			{
				nextMonthInMs = 1000L * 60L * 60L * 24L * 28L;
			}
			else
			{
				if((today.getYear() + 1900) % 100 > 0)
				{
					nextMonthInMs = 1000L * 60L * 60L * 24L * 29L;
				}
				else
				{
					if((today.getYear() + 1900) % 400 == 0)
					{
						nextMonthInMs = 1000L * 60L * 60L * 24L * 29L;
					}
					else
					{
						nextMonthInMs = 1000L * 60L * 60L * 24L * 28L;
					}
				}
			}
			break;
		default:
			nextMonthInMs = 1000L * 60L * 60L * 24L * 31L;
			break;
		}

		long nextStatementLong = timeNow + nextMonthInMs;
		Date nextStatement =  new Date(nextStatementLong);

		temp.setNextStatement(nextStatement);



		myStringBuffer = new StringBuffer(new Integer(account.getCustomerNumber()).toString());
		for(int z = myStringBuffer.length(); z < 10;z++)
		{
			myStringBuffer = myStringBuffer.insert(0, "0");	
		}
		String customerNumberString = myStringBuffer.toString();

		String sqlInsert = "INSERT INTO ACCOUNT (ACCOUNT_EYECATCHER, ACCOUNT_CUSTOMER_NUMBER, ACCOUNT_SORTCODE, ACCOUNT_NUMBER, ACCOUNT_TYPE, ACCOUNT_INTEREST_RATE, ACCOUNT_OPENED, ACCOUNT_OVERDRAFT_LIMIT, ACCOUNT_LAST_STATEMENT, ACCOUNT_NEXT_STATEMENT, ACCOUNT_AVAILABLE_BALANCE, ACCOUNT_ACTUAL_BALANCE) VALUES ('ACCT',?,?,?,?,?,?,?,?,?,0.00,0.00)";
		logger.fine("About to insert record SQL <" + sqlInsert + ">");
		controlString = sortcode.toString() + "-" + "ACCOUNT-LAST";
		sqlControl = "SELECT * from CONTROL where CONTROL_NAME LIKE '" + controlString + "'";
		String sqlUpdate = "UPDATE CONTROL "+
				"SET"+
				" CONTROL_VALUE_NUM = ?"+
				" WHERE CONTROL_NAME = ?";

		try(PreparedStatement stmt = conn.prepareStatement(sqlInsert);
			PreparedStatement stmt2 = conn.prepareStatement(sqlControl);
			PreparedStatement stmt3 = conn.prepareStatement(sqlUpdate);) 
		{

			stmt.setString(1, customerNumberString);
			stmt.setString(2, sortCodeString);
			stmt.setString(3,accountNumberString);
			stmt.setString(4, account.getAccountType());
			stmt.setBigDecimal(5, account.getInterestRate());
			stmt.setDate(6, dateOpened);
			stmt.setInt(7, account.getOverdraft());
			stmt.setDate(8, lastStatement);
			stmt.setDate(9, nextStatement);
			stmt.execute();
			temp.setAccountNumber(accountNumberString);
			temp.setActualBalance(0.00);
			temp.setAvailableBalance(0.00);
			temp.setCustomerNumber(customerNumberString);
			temp.setInterestRate(account.getInterestRate().doubleValue());
			temp.setLastStatement(lastStatement);
			temp.setNextStatement(nextStatement);
			temp.setOverdraftLimit(account.getOverdraft().intValue());
			temp.setSortcode(sortCodeString);
			temp.setType(account.getAccountType());
			temp.setOpened(dateOpened);

			logger.fine(PRE_SELECT_MSG + sqlControl + ">");
			try {
				ResultSet rs = stmt2.executeQuery();
				rs.next();

				logger.fine("About to execute update SQL <" + sqlUpdate + ">");
				stmt3.setLong(1, accountNumber);
				stmt3.setString(2, controlString);
				stmt3.execute();
			} catch (SQLException e) {
				logger.severe("Error accessing Control Table for UPDATE" + e.getLocalizedMessage());
				logger.exiting(this.getClass().getName(),CREATE_ACCOUNT,null);
				try {
					Task.getTask().rollback();
				} catch (InvalidRequestException e1) {
					logger.severe(e1.toString());
				}
				return null;
			}			
			return temp;
		}
		catch (SQLException e) 
		{

			if(e.getErrorCode() == -803)
			{
				logger.severe("SQLException -803 duplicate value. Have you combined named counter and non-named counter with the same data? com.ibm.cics.cip.bankliberty.web.db2.Account " + e.getErrorCode() + "," + e.getSQLState() + ","  + e.getMessage());
			}
			try {
				Task.getTask().rollback();
			} catch (InvalidRequestException e1) {
				logger.severe(e1.toString());
			}
			logger.severe("SQL statement #" + sqlInsert + "# had error " + e.getErrorCode());
			logger.severe(e.getLocalizedMessage());
			logger.exiting(this.getClass().getName(),CREATE_ACCOUNT,null);
			return null;
		}

	}




	public Account updateAccount(AccountJSON account) 
	{
		logger.entering(this.getClass().getName(),UPDATE_ACCOUNT);
		Account db2Account = this.getAccount(new Integer(account.getId()).intValue(), new Integer(sortcode).intValue());
		if(db2Account == null)
		{
			logger.warning("Unable to access DB2 account " + account.getId());
			logger.exiting(this.getClass().getName(),UPDATE_ACCOUNT,null);
			return null;
		}
		Account temp = null;
		openConnection();
		String accountNumberString = db2Account.getAccountNumber();
		Integer accountNumber = new Integer(db2Account.getAccountNumber());
		StringBuffer myStringBuffer = new StringBuffer(accountNumber.toString());
		for(int z = myStringBuffer.length(); z < 8;z++)
		{
			myStringBuffer = myStringBuffer.insert(0, "0");	
		}
		accountNumberString = myStringBuffer.toString();
		myStringBuffer = new StringBuffer(new Integer(sortcode).toString());
		for(int z = myStringBuffer.length(); z < 6;z++)
		{
			myStringBuffer = myStringBuffer.insert(0, "0");	
		}
		String sortCodeString = myStringBuffer.toString();
		String sql1 = SQL_SELECT;
		logger.fine("About to perform query SQL <" + sql1 + ">");
		String sqlUpdateSafe = "UPDATE ACCOUNT SET ACCOUNT_TYPE = ?,ACCOUNT_INTEREST_RATE = ? ,ACCOUNT_OVERDRAFT_LIMIT = ? WHERE ACCOUNT_NUMBER like ? AND ACCOUNT_SORTCODE like ?";
		try(PreparedStatement stmt = conn.prepareStatement(sql1);
		PreparedStatement myPreparedStatement = conn.prepareStatement(sqlUpdateSafe);) 
		{
			stmt.setString(1, accountNumberString);
			stmt.setString(2, sortCodeString);
			ResultSet rs = stmt.executeQuery();
			while (rs.next()) {
				temp = new Account(rs.getString(ACCOUNT_CUSTOMER_NUMBER), rs.getString(ACCOUNT_SORTCODE),
						rs.getString(ACCOUNT_NUMBER), rs.getString(ACCOUNT_TYPE), rs.getDouble(ACCOUNT_INTEREST_RATE),
						rs.getDate(ACCOUNT_OPENED), rs.getInt(ACCOUNT_OVERDRAFT_LIMIT), rs.getDate(ACCOUNT_LAST_STATEMENT),
						rs.getDate(ACCOUNT_NEXT_STATEMENT), rs.getDouble(ACCOUNT_AVAILABLE_BALANCE),
						rs.getDouble(ACCOUNT_ACTUAL_BALANCE));
				db2Account = temp;
				
				myPreparedStatement.setString(1,account.getAccountType());
				myPreparedStatement.setBigDecimal(2,account.getInterestRate());
				myPreparedStatement.setInt(3, account.getOverdraft());
				myPreparedStatement.setString(4, accountNumberString);
				myPreparedStatement.setString(5, account.getSortCode());
				logger.fine("About to execute update SQL <" + sqlUpdateSafe + ">");
				myPreparedStatement.execute();

				logger.exiting(this.getClass().getName(),UPDATE_ACCOUNT,temp);
				return temp;
			}
		} catch (SQLException e) {
			logger.severe(e.toString());
			logger.exiting(this.getClass().getName(),UPDATE_ACCOUNT,null);
			return null;
		}
		logger.exiting(this.getClass().getName(),UPDATE_ACCOUNT,temp);
		return temp;
	}



	public boolean debitCredit(BigDecimal apiAmount) 
	{
		logger.entering(this.getClass().getName(),DEBIT_CREDIT_ACCOUNT);
		Account temp = this.getAccount(new Integer(this.getAccountNumber()).intValue(), new Integer(this.getSortcode()).intValue());
		if(temp == null)
		{
			logger.warning("Unable to find account " + this.getAccountNumber());
			logger.exiting(this.getClass().getName(),DEBIT_CREDIT_ACCOUNT,false);
			return false;
		}

		openConnection();
		String accountNumberString = temp.getAccountNumber();
		StringBuffer myStringBuffer = new StringBuffer(this.getSortcode());
		for(int z = myStringBuffer.length(); z < 6;z++)
		{
			myStringBuffer = myStringBuffer.insert(0, "0");	
		}
		String sortCodeString = myStringBuffer.toString();
		String sql1 = SQL_SELECT;
		logger.fine("About to issue QUERY <" + sql1 + ">");
		String sqlUpdate = "UPDATE ACCOUNT SET ACCOUNT_ACTUAL_BALANCE = ? ,ACCOUNT_AVAILABLE_BALANCE = ? WHERE ACCOUNT_NUMBER like ? AND ACCOUNT_SORTCODE like ?";
		try (PreparedStatement stmt = conn.prepareStatement(sql1);
			PreparedStatement stmt2 = conn.prepareStatement(sqlUpdate);)
		{
			stmt.setString(1, accountNumberString);
			stmt.setString(2, sortCodeString);
			ResultSet rs = stmt.executeQuery();
			if(rs.next())
			{
				temp = new Account(rs.getString(ACCOUNT_CUSTOMER_NUMBER), rs.getString(ACCOUNT_SORTCODE),
						rs.getString(ACCOUNT_NUMBER), rs.getString(ACCOUNT_TYPE), rs.getDouble(ACCOUNT_INTEREST_RATE),
						rs.getDate(ACCOUNT_OPENED), rs.getInt(ACCOUNT_OVERDRAFT_LIMIT), rs.getDate(ACCOUNT_LAST_STATEMENT),
						rs.getDate(ACCOUNT_NEXT_STATEMENT), rs.getDouble(ACCOUNT_AVAILABLE_BALANCE),
						rs.getDouble(ACCOUNT_ACTUAL_BALANCE));

			}
			else 
			{
				logger.warning("Result set had no results");
				logger.exiting(this.getClass().getName(),DEBIT_CREDIT_ACCOUNT,false);
				return false;
			}

			double newActualBalance = temp.getActualBalance();
			double newAvailableBalance = temp.getAvailableBalance();


			newActualBalance = newActualBalance + apiAmount.doubleValue();
			newAvailableBalance = newAvailableBalance + apiAmount.doubleValue();

			this.setActualBalance(newActualBalance);
			this.setAvailableBalance(newAvailableBalance);


			

			stmt2.setDouble(1, newActualBalance);
			stmt2.setDouble(2, newAvailableBalance);
			stmt2.setString(3, accountNumberString);
			stmt2.setString(4, sortCodeString);
			logger.fine("About to issue update SQL <" + sqlUpdate + ">");
			stmt2.execute();

			logger.exiting(this.getClass().getName(),DEBIT_CREDIT_ACCOUNT,true);
			return true;

		} 
		catch (SQLException e) 
		{
			logger.severe(e.getLocalizedMessage());
			logger.exiting(this.getClass().getName(),DEBIT_CREDIT_ACCOUNT,false);
			return false;
		}
	}



	public Account[] getAccountsByBalance(Integer sortCode2, BigDecimal balance, boolean lessThan) 
	{
		logger.entering(this.getClass().getName(), GET_ACCOUNTS_BY_BALANCE);
		openConnection();
		Account[] temp = new Account[250000];
		int i = 0;
		StringBuffer myStringBuffer = new StringBuffer(sortCode2);
		for(int z = myStringBuffer.length(); z < 6;z++)
		{
			myStringBuffer = myStringBuffer.insert(0, "0");	
		}
		String sortCodeString = myStringBuffer.toString();
		String sql = "SELECT * from ACCOUNT where ACCOUNT_EYECATCHER LIKE 'ACCT' AND ACCOUNT_SORTCODE like ? ";
		if(lessThan)
		{
			sql=sql.concat(SQL_LESS_THAN);
		}
		else
		{
			sql=sql.concat(SQL_MORE_THAN);
		}
		sql = sql.concat(" order by ACCOUNT_NUMBER");

		sql = sql.concat(" , ACCOUNT_ACTUAL_BALANCE DESC");


		logger.fine(PRE_SELECT_MSG + sql + ">");
		try (PreparedStatement stmt = conn.prepareStatement(sql);)
		{
			stmt.setString(1,sortCodeString);
			stmt.setBigDecimal(2, balance);
			ResultSet rs = stmt.executeQuery();
			while (rs.next()) 
			{
				temp[i] = new Account(rs.getString(ACCOUNT_CUSTOMER_NUMBER), rs.getString(ACCOUNT_SORTCODE),
						rs.getString(ACCOUNT_NUMBER), rs.getString(ACCOUNT_TYPE), rs.getDouble(ACCOUNT_INTEREST_RATE),
						rs.getDate(ACCOUNT_OPENED), rs.getInt(ACCOUNT_OVERDRAFT_LIMIT), rs.getDate(ACCOUNT_LAST_STATEMENT),
						rs.getDate(ACCOUNT_NEXT_STATEMENT), rs.getDouble(ACCOUNT_AVAILABLE_BALANCE),
						rs.getDouble(ACCOUNT_ACTUAL_BALANCE));
				i++;
			}
		} 
		catch (SQLException e) 
		{
			System.err.println(SQL_WAS + sql + ">");
			logger.severe(e.getLocalizedMessage());
			logger.exiting(this.getClass().getName(), GET_ACCOUNTS_BY_BALANCE,null);
			return null;
		}
		Account[] real = new Account[i];
		for(int j=0;j<i;j++)
		{
			real[j] = temp[j];
		}
		logger.exiting(this.getClass().getName(), GET_ACCOUNTS_BY_BALANCE,real);
		return real;

	}



	public Account[] getAccounts(Integer sortCode, int limit, int offset) 
	{
		logger.entering(this.getClass().getName(), GET_ACCOUNTS_WITH_LIMIT_AND_OFFSET);
		openConnection();
		Account[] temp = new Account[limit];
		int i = 0, retrieved = 0;
		StringBuffer myStringBuffer = new StringBuffer(sortCode);
		for(int z = myStringBuffer.length(); z < 6;z++)
		{
			myStringBuffer = myStringBuffer.insert(0, "0");	
		}
		String sortCodeString = myStringBuffer.toString();
		String sql = "SELECT * from ACCOUNT where ACCOUNT_EYECATCHER LIKE 'ACCT' AND ACCOUNT_SORTCODE like ? ORDER BY ACCOUNT_NUMBER";
		logger.fine(PRE_SELECT_MSG + sql + ">");
		try(PreparedStatement stmt = conn.prepareStatement(sql);) 
		{
			stmt.setString(1, sortCodeString);
			ResultSet rs = stmt.executeQuery();
			while (rs.next() && i < limit) 
			{
				if(retrieved >= offset)
				{
					temp[i] = new Account(rs.getString(ACCOUNT_CUSTOMER_NUMBER), rs.getString(ACCOUNT_SORTCODE),
							rs.getString(ACCOUNT_NUMBER), rs.getString(ACCOUNT_TYPE), rs.getDouble(ACCOUNT_INTEREST_RATE),
							rs.getDate(ACCOUNT_OPENED), rs.getInt(ACCOUNT_OVERDRAFT_LIMIT), rs.getDate(ACCOUNT_LAST_STATEMENT),
							rs.getDate(ACCOUNT_NEXT_STATEMENT), rs.getDouble(ACCOUNT_AVAILABLE_BALANCE),
							rs.getDouble(ACCOUNT_ACTUAL_BALANCE));
					i++;
				}
				retrieved++;
			}
		} 
		catch (SQLException e) 
		{
			logger.severe(e.getLocalizedMessage());
			logger.exiting(this.getClass().getName(), GET_ACCOUNTS_WITH_LIMIT_AND_OFFSET,null);
			return null;
		}
		Account[] real = new Account[i];
		for(int j=0;j<i;j++)
		{
			real[j] = temp[j];
		}
		logger.exiting(this.getClass().getName(), GET_ACCOUNTS_WITH_LIMIT_AND_OFFSET,real);
		return real;

	}



	public Account[] getAccountsByBalance(Integer sortCode2, BigDecimal balance, boolean lessThan, int offset,
			int limit) 
	{
		logger.entering(this.getClass().getName(), GET_ACCOUNTS_BY_BALANCE_WITH_LIMIT_AND_OFFSET);
		openConnection();
		Account[] temp = new Account[250000];
		int i = 0;
		StringBuffer myStringBuffer = new StringBuffer(sortCode2);
		for(int z = myStringBuffer.length(); z < 6;z++)
		{
			myStringBuffer = myStringBuffer.insert(0, "0");	
		}
		String sortCodeString = myStringBuffer.toString();
		String sql = "SELECT * from (SELECT p.*,row_number() over() as rn from ACCOUNT as p where ACCOUNT_EYECATCHER LIKE 'ACCT' AND ACCOUNT_SORTCODE like ?";
		if(lessThan)
		{
			sql=sql.concat(SQL_LESS_THAN);
		}
		else
		{
			sql=sql.concat(SQL_MORE_THAN);
		}

		sql = sql.concat(" order by ACCOUNT_NUMBER");

		sql = sql.concat(" , ACCOUNT_ACTUAL_BALANCE DESC)");

		sql = sql.concat(" as col where rn between ? and ?");



		try(PreparedStatement stmt = conn.prepareStatement(sql);) 
		{
			logger.fine(PRE_SELECT_MSG + sql + ">");
			stmt.setString(1, sortCodeString);
			stmt.setBigDecimal(2, balance);
			stmt.setInt(3, offset);
			stmt.setInt(4, ((limit+offset) -1));
			ResultSet rs = stmt.executeQuery();
			while (rs.next()) 
			{
				temp[i] = new Account(rs.getString(ACCOUNT_CUSTOMER_NUMBER), rs.getString(ACCOUNT_SORTCODE),
						rs.getString(ACCOUNT_NUMBER), rs.getString(ACCOUNT_TYPE), rs.getDouble(ACCOUNT_INTEREST_RATE),
						rs.getDate(ACCOUNT_OPENED), rs.getInt(ACCOUNT_OVERDRAFT_LIMIT), rs.getDate(ACCOUNT_LAST_STATEMENT),
						rs.getDate(ACCOUNT_NEXT_STATEMENT), rs.getDouble(ACCOUNT_AVAILABLE_BALANCE),
						rs.getDouble(ACCOUNT_ACTUAL_BALANCE));
				i++;
			}
		} 
		catch (SQLException e) 
		{
			System.err.println(SQL_WAS + sql + ">");
			logger.severe(e.getLocalizedMessage());
			logger.exiting(this.getClass().getName(), GET_ACCOUNTS_BY_BALANCE_WITH_LIMIT_AND_OFFSET,null);
			return null;
		}
		Account[] real = new Account[i];
		for(int j=0;j<i;j++)
		{
			real[j] = temp[j];
		}
		logger.exiting(this.getClass().getName(), GET_ACCOUNTS_BY_BALANCE_WITH_LIMIT_AND_OFFSET,real);
		return real;
	}



	public int getAccountsCountOnly(Integer sortCode2) 
	{
		logger.entering(this.getClass().getName(), GET_ACCOUNTS_COUNT_ONLY2);

		openConnection();
		int accountCount = 0;
		StringBuffer myStringBuffer = new StringBuffer(sortCode2);
		for(int z = myStringBuffer.length(); z < 6;z++)
		{
			myStringBuffer = myStringBuffer.insert(0, "0");	
		}
		String sortCodeString = myStringBuffer.toString();
		String sql = "SELECT COUNT(*) as ACCOUNT_COUNT from ACCOUNT where ACCOUNT_EYECATCHER LIKE 'ACCT' AND ACCOUNT_SORTCODE like ?";
		logger.fine(PRE_SELECT_MSG + sql + ">");
		try (PreparedStatement stmt = conn.prepareStatement(sql);)
		{
			stmt.setString(1, sortCodeString);
			ResultSet rs = stmt.executeQuery();
			if(rs.next())
			{
				accountCount = rs.getInt(ACCOUNT_COUNT);
			}
			else
			{
				logger.exiting(this.getClass().getName(), GET_ACCOUNTS_COUNT_ONLY2,-1);
				return -1;
			}
		} catch (SQLException e) {
			logger.severe(e.getLocalizedMessage());
			logger.exiting(this.getClass().getName(), GET_ACCOUNTS_COUNT_ONLY2,-1);
			return -1;
		}
		logger.exiting(this.getClass().getName(), GET_ACCOUNTS_COUNT_ONLY2,accountCount);
		return accountCount;
	}



	public int getAccountsByBalanceCountOnly(Integer sortCode2, BigDecimal balance, boolean lessThan, Integer offset, Integer limit) 
	{
		logger.entering(this.getClass().getName(), GET_ACCOUNTS_BY_BALANCE_COUNT_ONLY);

		int accountCount = 0;
		openConnection();
		StringBuffer myStringBuffer = new StringBuffer(sortCode2);
		for(int z = myStringBuffer.length(); z < 6;z++)
		{
			myStringBuffer = myStringBuffer.insert(0, "0");	
		}
		String sortCodeString = myStringBuffer.toString();
		String sql = "SELECT COUNT(*) AS ACCOUNT_COUNT from ACCOUNT where ACCOUNT_EYECATCHER LIKE 'ACCT' AND ACCOUNT_SORTCODE like ?";
		if(lessThan)
		{
			sql=sql.concat(SQL_LESS_THAN);
		}
		else
		{
			sql=sql.concat(SQL_MORE_THAN);
		}


		logger.fine(PRE_SELECT_MSG + sql + ">");
		try(PreparedStatement stmt = conn.prepareStatement(sql);)
		{
			stmt.setString(1,sortCodeString);
			stmt.setBigDecimal(2, balance);
			ResultSet rs = stmt.executeQuery();
			if(rs.next())
			{
				accountCount = rs.getInt(ACCOUNT_COUNT);
			}
			else
			{
				logger.exiting(this.getClass().getName(), GET_ACCOUNTS_BY_BALANCE_COUNT_ONLY,-1);
				return -1;
			}
		} catch (SQLException e) {
			System.err.println(SQL_WAS + sql + ">");
			logger.severe(e.getLocalizedMessage());
			logger.exiting(this.getClass().getName(), GET_ACCOUNTS_BY_BALANCE_COUNT_ONLY,-1);
			return -1;
		}
		logger.exiting(this.getClass().getName(), GET_ACCOUNTS_BY_BALANCE_COUNT_ONLY,accountCount);
		return accountCount;
	}
}
