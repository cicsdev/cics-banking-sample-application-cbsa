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
	private 	Date 		lastStatement;
	private 	Date 		nextStatement;
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
		logger.log(Level.FINE, () -> "Last Statement - "+this.lastStatement.toString());
		logger.log(Level.FINE, () -> "Next Statement - "+this.nextStatement.toString());
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
		StringBuilder myStringBuilder = new StringBuilder();
		

		for (int i=this.accountNumber.length();i<8;i++)
		{
			myStringBuilder.append('0');
		}
		myStringBuilder.append(this.accountNumber);
		this.accountNumber = myStringBuilder.toString();
		return this.accountNumber;
	}

	public void setAccountNumber(String accNo) {
		StringBuilder myStringBuilder = new StringBuilder();
		

		for (int i=accNo.length();i<8;i++)
		{
			myStringBuilder.append('0');
		}
		myStringBuilder.append(accNo);
		this.accountNumber = myStringBuilder.toString();
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
		return lastStatement;
	}

	public void setLastStatement(Date lastStatement) {
		if(lastStatement == null){
			this.lastStatement = this.opened;
		}
		else{
			this.lastStatement = lastStatement;
		}
	}

	public Date getNextStatement() {
		return nextStatement;
	}

	
	public void setNextStatement(Date nextStatement) {
		if(nextStatement == null){
			Calendar cal = Calendar.getInstance();
			cal.setTime(opened);
			cal.add(Calendar.DATE, +7);
			this.nextStatement =  new Date(cal.getTime().getTime());
		}
		else{
			this.nextStatement = nextStatement;
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
			stmt.setString(4,this.lastStatement.toString());
			stmt.setString(5,this.nextStatement.toString());
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
		StringBuilder myStringBuilder = new StringBuilder();
		
		
		for (int i=Integer.valueOf(sortCode).toString().length();i<6;i++)
		{
			myStringBuilder.append('0');
		}
		myStringBuilder.append(Integer.valueOf(sortCode).toString());

		String sortCodeString = myStringBuilder.toString();
		String sql9999 = "SELECT * from ACCOUNT where ACCOUNT_EYECATCHER LIKE 'ACCT' AND ACCOUNT_SORTCODE like ? order by ACCOUNT_NUMBER DESC";
		String sql = SQL_SELECT;
		try(PreparedStatement stmt9999 = conn.prepareStatement(sql9999);PreparedStatement stmt = conn.prepareStatement(sql);)
		{
			if(accountNumber == 99999999)
			{
				
				logger.log(Level.FINE,() ->PRE_SELECT_MSG + sql9999 + ">");
				

				stmt9999.setString(1, sortCodeString);
				ResultSet rs = stmt9999.executeQuery();
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
					logger.log(Level.WARNING,() ->"No results found");
					logger.exiting(this.getClass().getName(),GET_ACCOUNT + accountNumber,null);
					return null;
				}


			}
			else
			{
				
				
					logger.log(Level.FINE,() ->PRE_SELECT_MSG + sql + ">");
					myStringBuilder  = new StringBuilder();
					for(int z = Integer.valueOf(accountNumber).toString().length(); z < 8;z++)
					{
						myStringBuilder = myStringBuilder.append("0");	
					}
					myStringBuilder.append(Integer.toString(accountNumber));
					String accountNumberString = myStringBuilder.toString();
					stmt.setString(1,accountNumberString);
					stmt.setString(2,sortCodeString);

					ResultSet rs = stmt.executeQuery();
					if(rs.isClosed())
					{
						logger.log(Level.WARNING,() ->"Result set is closed");
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

				}

		}
		catch (SQLException e) {
			logger.severe(e.getLocalizedMessage());
			logger.exiting(this.getClass().getName(),GET_ACCOUNT + accountNumber,null);
			return null;
		}
		logger.exiting(this.getClass().getName(),GET_ACCOUNT + accountNumber,temp);
		return temp;
	}

	public Account[] getAccounts(long l, int sortCode){
		logger.entering(this.getClass().getName(),GET_ACCOUNTS_CUSTNO + l);
		openConnection();
		Account[] temp = new Account[10];
		StringBuilder myStringBuilder = new StringBuilder();
		
		for (int i=Long.toString(l).length();i<10;i++)
		{
			myStringBuilder.append('0');
		}

		myStringBuilder.append(Long.toString(l));
		String customerNumberString = myStringBuilder.toString();

		myStringBuilder = new StringBuilder(); 
		for(int z = Integer.toString(sortCode).toString().length(); z < 6;z++)
		{
			myStringBuilder = myStringBuilder.append("0");	
		}
		myStringBuilder.append(Integer.toString(sortCode));
		String sortCodeString = myStringBuilder.toString();

		String sql = "SELECT * from ACCOUNT where ACCOUNT_EYECATCHER LIKE 'ACCT' AND ACCOUNT_CUSTOMER_NUMBER like ? and ACCOUNT_SORTCODE like ? ORDER BY ACCOUNT_NUMBER";
		logger.log(Level.FINE,() ->PRE_SELECT_MSG + sql + ">");
		int i = 0;
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
		StringBuilder myStringBuilder = new StringBuilder();
		
		for (i=Integer.toString(sortCode).length();i<6;i++)
		{
			myStringBuilder.append('0');
		}
		myStringBuilder.append(Integer.toString(sortCode));
		String sortCodeString = myStringBuilder.toString();
		String sql = "SELECT * from ACCOUNT where ACCOUNT_EYECATCHER LIKE 'ACCT' AND ACCOUNT_SORTCODE like ? ORDER BY ACCOUNT_NUMBER";
		logger.log(Level.FINE,() ->"About is issue query SQL <" + sql + ">");
		i = 0;
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
		StringBuilder myStringBuilder = new StringBuilder();
		int i;
		for (i=Integer.toString(sortCode).length();i<6;i++)
		{
			myStringBuilder.append('0');
		}
		myStringBuilder.append(Integer.toString(sortCode));
		String sortCodeString = myStringBuilder.toString();
		String sql = "SELECT COUNT(*) as ACCOUNT_COUNT from ACCOUNT where ACCOUNT_EYECATCHER LIKE 'ACCT' AND ACCOUNT_SORTCODE like ?";
		logger.log(Level.FINE,() ->PRE_SELECT_MSG + sql + ">");
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
		StringBuilder myStringBuilder = new StringBuilder();
		
		for (int i=Integer.toString(account).length();i<8;i++)
		{
			myStringBuilder.append('0');
		}
		myStringBuilder.append(Integer.toString(account));
		String accountNumberString = myStringBuilder.toString();

		myStringBuilder = new StringBuilder();
		for (int i=Integer.toString(sortcode).length();i<6;i++)
		{
			myStringBuilder.append('0');
		}
		myStringBuilder.append(Integer.toString(sortcode));
		String sortCodeString = myStringBuilder.toString();

		String sql1 = SQL_SELECT;
		String sql2 = "DELETE from ACCOUNT where ACCOUNT_EYECATCHER LIKE 'ACCT' AND ACCOUNT_NUMBER like ? and ACCOUNT_SORTCODE like ?";
		try (PreparedStatement stmt = conn.prepareStatement(sql1);
			PreparedStatement stmt2 = conn.prepareStatement(sql2);)
		{
			logger.log(Level.FINE,() ->PRE_SELECT_MSG + sql1 + ">");

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
				logger.log(Level.FINE,() ->"About to issue delete SQL <" + sql2 + ">");
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



	public Account createAccount(AccountJSON account, Integer sortcode, boolean useNamedCounter) 
	{
		logger.entering(this.getClass().getName(),CREATE_ACCOUNT);

		Account temp = null;
		openConnection();
		String accountNumberString = null;

		temp = new Account();

		String sortCodeString = padSortCode(sortcode);

		Integer accountNumber =0;
		String controlString = sortcode.toString() + "-" + "ACCOUNT-LAST";
		String sqlControl = "SELECT * from CONTROL where CONTROL_NAME = ?";
		controlString = sortcode.toString() + "-" + "ACCOUNT-LAST";
		sqlControl = "SELECT * from CONTROL where CONTROL_NAME LIKE ?";
		String sqlUpdate = "UPDATE CONTROL "+
				"SET"+
				" CONTROL_VALUE_NUM = ?"+
				" WHERE CONTROL_NAME = ?";
		String sqlInsert = "INSERT INTO ACCOUNT (ACCOUNT_EYECATCHER, ACCOUNT_CUSTOMER_NUMBER, ACCOUNT_SORTCODE, ACCOUNT_NUMBER, ACCOUNT_TYPE, ACCOUNT_INTEREST_RATE, ACCOUNT_OPENED, ACCOUNT_OVERDRAFT_LIMIT, ACCOUNT_LAST_STATEMENT, ACCOUNT_NEXT_STATEMENT, ACCOUNT_AVAILABLE_BALANCE, ACCOUNT_ACTUAL_BALANCE) VALUES ('ACCT',?,?,?,?,?,?,?,?,?,0.00,0.00)";

		try(PreparedStatement stmtC = conn.prepareStatement(sqlControl);
				PreparedStatement stmt = conn.prepareStatement(sqlInsert);
				PreparedStatement stmt2 = conn.prepareStatement(sqlControl);
				PreparedStatement stmt3 = conn.prepareStatement(sqlUpdate);) 
		{
			stmtC.setString(1,controlString);
			ResultSet rs = stmtC.executeQuery();
			if(rs.next())
			{
				// CONTROL_VALUE_NUM is up to 10 digits long. Account number is only ever up to 8 digits long
				Long tempLong = rs.getLong("CONTROL_VALUE_NUM");
				accountNumber = Integer.parseInt(tempLong.toString()); 
				rs.close();

				accountNumber++;

				
				accountNumberString = padAccountNumber(accountNumber);


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
				long nextMonthInMs = getNextMonth(today);
			
				long nextStatementLong = timeNow + nextMonthInMs;
				Date nextStatement =  new Date(nextStatementLong);

				temp.setNextStatement(nextStatement);

				String customerNumberString = padCustomerNumber(account.getCustomerNumber());


				logger.log(Level.FINE,() ->"About to insert record SQL <" + sqlInsert + ">");




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

				rs = stmt2.executeQuery();
				rs.next();

				logger.log(Level.FINE,() ->"About to execute update SQL <" + sqlUpdate + ">");
				stmt3.setLong(1, accountNumber);
				stmt3.setString(2, controlString);
				stmt3.execute();

				return temp;
			}
			return null;
		} 
		catch (SQLException e) 
		{
			logger.severe("Error creating account " + e.getLocalizedMessage());
			logger.exiting(this.getClass().getName(),CREATE_ACCOUNT,null);
			try {
				Task.getTask().rollback();
			} catch (InvalidRequestException e1) {
				logger.severe(e1.toString());
			}
			return null;
		}
	}




	private String padCustomerNumber(String customerNumber2) {
		StringBuilder myStringBuilder  = new StringBuilder();
		for(int z = customerNumber2.length(); z < 10;z++)
		{
			myStringBuilder.append("0");	
		}
		myStringBuilder.append(customerNumber2.toString());
		return myStringBuilder.toString();
	}



	private String padAccountNumber(Integer accountNumber2) {
		// TODO Auto-generated method stub
		StringBuilder myStringBuilder  = new StringBuilder();
		for(int z = accountNumber2.toString().length(); z < 8;z++)
		{
			myStringBuilder.append("0");	
		}
		myStringBuilder.append(accountNumber2.toString());
		return myStringBuilder.toString();
	}



	private String padSortCode(Integer sortcode2) {
		// TODO Auto-generated method stub
		StringBuilder myStringBuilder = new StringBuilder();
		
		for(int z =sortcode2.toString().length(); z<6;z++)
		{
			myStringBuilder.append("0");	
		}
		myStringBuilder.append(sortcode2.toString());
		return myStringBuilder.toString();
		
	}



	private long getNextMonth(Date today) {
		// What is next month?
		long nextMonthInMs;
		Calendar myCalendar = Calendar.getInstance();
		myCalendar.setTime(today);
		switch(myCalendar.get(Calendar.MONTH))
		{
		case 8:
		case 3:
		case 5:
		case 10:
			nextMonthInMs = 1000L * 60L * 60L * 24L * 30L;
			break;
		case 1:
			if((myCalendar.get(Calendar.YEAR)) % 4 > 0)
			{
				nextMonthInMs = 1000L * 60L * 60L * 24L * 28L;
			}
			else
			{
				if(myCalendar.get(Calendar.YEAR) % 100 > 0)
				{
					nextMonthInMs = 1000L * 60L * 60L * 24L * 29L;
				}
				else
				{
					if(myCalendar.get(Calendar.YEAR) % 400 == 0)
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
		return nextMonthInMs;

	}



	public Account updateAccount(AccountJSON account) 
	{
		logger.entering(this.getClass().getName(),UPDATE_ACCOUNT);
		Account db2Account = this.getAccount(new Integer(account.getId()).intValue(), new Integer(sortcode).intValue());
		if(db2Account == null)
		{
			logger.log(Level.WARNING,() ->"Unable to access DB2 account " + account.getId());
			logger.exiting(this.getClass().getName(),UPDATE_ACCOUNT,null);
			return null;
		}
		Account temp = null;
		openConnection();
		String accountNumberString = db2Account.getAccountNumber();
		Integer accountNumber = new Integer(db2Account.getAccountNumber());
		accountNumberString = padAccountNumber(accountNumber);

		String sortCodeString = padSortCode(Integer.valueOf(sortcode));
		String sql1 = SQL_SELECT;
		logger.log(Level.FINE,() ->"About to perform query SQL <" + sql1 + ">");
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
				logger.log(Level.FINE,() ->"About to execute update SQL <" + sqlUpdateSafe + ">");
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
			logger.log(Level.WARNING,() ->"Unable to find account " + this.getAccountNumber());
			logger.exiting(this.getClass().getName(),DEBIT_CREDIT_ACCOUNT,false);
			return false;
		}

		openConnection();
		String accountNumberString = temp.getAccountNumber();
		StringBuilder myStringBuilder = new StringBuilder();
		
		for (int i=this.getSortcode().length();i<6;i++)
		{
			myStringBuilder.append('0');
		}

		myStringBuilder.append(this.getSortcode().toString());
		
		String sortCodeString = myStringBuilder.toString();
		String sql1 = SQL_SELECT;
		logger.log(Level.FINE,() ->"About to issue QUERY <" + sql1 + ">");
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
				logger.log(Level.WARNING,() ->"Result set had no results");
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
			logger.log(Level.FINE,() ->"About to issue update SQL <" + sqlUpdate + ">");
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
		StringBuilder myStringBuilder = new StringBuilder();
		
		for (i=sortCode2.toString().length();i<6;i++)
		{
			myStringBuilder.append('0');
		}

		myStringBuilder.append(sortCode2.toString());
		
		String sortCodeString = myStringBuilder.toString();
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
		StringBuilder myStringBuilder = new StringBuilder();
		
		for (i=sortCode.toString().length();i<6;i++)
		{
			myStringBuilder.append('0');
		}

		myStringBuilder.append(sortCode.toString());
		
		String sortCodeString = myStringBuilder.toString();
		String sql = "SELECT * from ACCOUNT where ACCOUNT_EYECATCHER LIKE 'ACCT' AND ACCOUNT_SORTCODE like ? ORDER BY ACCOUNT_NUMBER";
		logger.log(Level.FINE,() ->PRE_SELECT_MSG + sql + ">");
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
		StringBuilder myStringBuilder = new StringBuilder();
		
		for (i=sortCode2.toString().length();i<6;i++)
		{
			myStringBuilder.append('0');
		}

		myStringBuilder.append(sortCode2.toString());
		
		String sortCodeString = myStringBuilder.toString();
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

		String sortCodeString = padSortCode(sortCode2);
		String sql = "SELECT COUNT(*) as ACCOUNT_COUNT from ACCOUNT where ACCOUNT_EYECATCHER LIKE 'ACCT' AND ACCOUNT_SORTCODE like ?";
		logger.log(Level.FINE,() ->PRE_SELECT_MSG + sql + ">");
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
		StringBuilder myStringBuilder = new StringBuilder();
		
		for (int i=sortCode2.toString().length();i<6;i++)
		{
			myStringBuilder.append('0');
		}

		myStringBuilder.append(sortCode2.toString());
		
		String sortCodeString = myStringBuilder.toString();
		String sql = "SELECT COUNT(*) AS ACCOUNT_COUNT from ACCOUNT where ACCOUNT_EYECATCHER LIKE 'ACCT' AND ACCOUNT_SORTCODE like ?";
		if(lessThan)
		{
			sql=sql.concat(SQL_LESS_THAN);
		}
		else
		{
			sql=sql.concat(SQL_MORE_THAN);
		}


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
			logger.severe(e.getLocalizedMessage());
			logger.exiting(this.getClass().getName(), GET_ACCOUNTS_BY_BALANCE_COUNT_ONLY,-1);
			return -1;
		}
		logger.exiting(this.getClass().getName(), GET_ACCOUNTS_BY_BALANCE_COUNT_ONLY,accountCount);
		return accountCount;
	}
}
