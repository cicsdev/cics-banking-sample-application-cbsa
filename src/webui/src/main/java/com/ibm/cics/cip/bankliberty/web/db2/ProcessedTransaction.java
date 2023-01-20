/*
 *
 *    Copyright IBM Corp. 2022
 *
 */

package com.ibm.cics.cip.bankliberty.web.db2;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.sql.Date;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Calendar;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.ibm.cics.cip.bankliberty.api.json.HBankDataAccess;
import com.ibm.cics.cip.bankliberty.datainterfaces.PROCTRAN;
import com.ibm.cics.server.Task;

public class ProcessedTransaction extends HBankDataAccess
{

	static final String COPYRIGHT = "Copyright IBM Corp. 2022";

	private static Logger logger = Logger.getLogger("com.ibm.cics.cip.bankliberty.web.db2");

	private static final String GET_PROCESSED_TRANSACTIONS = "getProcessedTransactions(int sortCode, Integer limit, Integer offset)";
	private static final String WRITE_DEBIT = "writeDebit(String accountNumber, String sortcode, BigDecimal amount2)";
	private static final String WRITE_CREDIT = "writeCredit(String accountNumber, String sortcode, BigDecimal amount2)";
	private static final String WRITE_TRANSFER_LOCAL = "writeTransferLocal(String sortCode2, String account_number2, BigDecimal amount2, String target_account_number2)";
	private static final String WRITE_DELETE_CUSTOMER = "writeDeleteCustomer(String sortCode2, String accountNumber, double amount_which_will_be_zero, Date customerDOB, String customerName, String customerNumber)";
	private static final String WRITE_CREATE_CUSTOMER = "writeCreateCustomer(String sortCode2, String accountNumber, double amount_which_will_be_zero, Date customerDOB, String customerName, String customerNumber)";
	private static final String WRITE_DELETE_ACCOUNT = "writeDeleteAccount(String sortCode2, String accountNumber, BigDecimal actualBalance, Date lastStatement, Date nextStatement, String customerNumber, String accountType)";
	private static final String WRITE_CREATE_ACCOUNT = "writeCreateAccount(String sortCode2, String accountNumber, BigDecimal actualBalance, Date lastStatement, Date nextStatement, String customerNumber, String accountType)";

	private static final String SQL_INSERT = "INSERT INTO PROCTRAN (PROCTRAN_EYECATCHER, PROCTRAN_SORTCODE, PROCTRAN_NUMBER, PROCTRAN_DATE, PROCTRAN_TIME, PROCTRAN_REF, PROCTRAN_TYPE, PROCTRAN_DESC, PROCTRAN_AMOUNT) VALUES (?,?,?,?,?,?,?,?,?)";

	private static final String ABOUT_TO_INSERT = "About to insert record SQL <";

	// String ACCOUNT_EYECATCHER CHAR(4),
	private String sortcode;
	private String customerName;

	int retrieved = 0;
	int stored = 0;
	int offset = 0;
	int limit = 0;

	int validProctranRecords = 0;

	public String getCustomerName()
	{
		return customerName;
	}

	public void setCustomerName(String customerName)
	{
		this.customerName = customerName;
	}

	private String accountNumber;
	private String accountType;
	private Date lastStatement;
	private Date nextStatement;
	private boolean moneyTransfer = false;
	private String description;
	private Date transactionDate;
	private double amount;
	private String customer = "";

	private Date dateOfBirth;

	private String targetAccountNumber;
	private String targetSortcode;
	private String type;
	private String reference;

	private String timeString;

	private String dateString;

	private String taskRef;

	public void setDateOfBirth(Date dateOfBirth)
	{
		this.dateOfBirth = dateOfBirth;
	}

	public String getAccountType()
	{
		return accountType;
	}

	public void setAccountType(String accountType)
	{
		this.accountType = accountType;
	}

	public Date getLastStatement()
	{
		return lastStatement;
	}

	public void setLastStatement(Date lastStatement)
	{
		this.lastStatement = lastStatement;
	}

	public Date getNextStatement()
	{
		return nextStatement;
	}

	public void setNextStatement(Date nextStatement)
	{
		this.nextStatement = nextStatement;
	}

	public String getTargetSortcode()
	{
		return targetSortcode;
	}

	public void setTargetSortcode(String targetSortcode)
	{
		this.targetSortcode = targetSortcode;
	}

	public ProcessedTransaction()
	{
		sortOutLogging();
		timeString = "";
		dateString = "";
		taskRef = "";
	}

	public void setCustomer(String customer)
	{
		this.customer = customer;
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

	public void setAccountNumber(String accountNumberIn)
	{
		this.accountNumber = accountNumberIn;
	}

	public String getType()
	{
		return type;
	}

	public void setType(String type)
	{
		this.type = type;
	}

	public ProcessedTransaction[] getProcessedTransactions(int sortCode, Integer limit, Integer offset)
	{
		logger.entering(this.getClass().getName(), GET_PROCESSED_TRANSACTIONS);

		ProcessedTransaction[] temp = new ProcessedTransaction[limit];

		this.offset = offset.intValue();
		this.limit = limit.intValue();

		StringBuilder myStringBuilder = new StringBuilder();

		for (int i = Integer.toString(sortCode).length(); i < 6; i++)
		{
			myStringBuilder.append('0');
		}

		myStringBuilder.append(Integer.toString(sortCode));
		String sortCodeString = myStringBuilder.toString();

		openConnection();
		String sql = "SELECT * from (SELECT p.*,row_number() over() as rn from PROCTRAN as p where PROCTRAN_SORTCODE like ? ORDER BY PROCTRAN_DATE ASC, PROCTRAN_TIME ASC) as col where rn between ? and ?";
		logger.log(Level.FINE,() ->"About to issue query SQL <" + sql + ">");

		int i = 0;
		try (PreparedStatement stmt = conn.prepareStatement(sql);)
		{

			stmt.setString(1, sortCodeString);
			stmt.setInt(2, offset + 1);
			stmt.setInt(3, (limit + offset));
			ResultSet rs = stmt.executeQuery();
			while (rs.next())
			{
				Calendar myCalendar = Calendar.getInstance();
				Date transactionDateFromRecord = rs.getDate("PROCTRAN_DATE");
				String transactionTime = rs.getString("PROCTRAN_TIME");
				long seconds = 0;
				long minutes = 0;
				long hours = 0;
				temp[i] = new ProcessedTransaction();

				temp[i].setAccountNumber(rs.getString("PROCTRAN_NUMBER"));
				temp[i].setReference(rs.getString("PROCTRAN_REF"));
				temp[i].setSortcode(rs.getString("PROCTRAN_SORTCODE"));
				this.setSortcode(temp[i].getSortcode());
				temp[i].setDescription(rs.getString("PROCTRAN_DESC"));
				temp[i].setAmount(rs.getDouble("PROCTRAN_AMOUNT"));
				temp[i].setType(rs.getString("PROCTRAN_TYPE"));

				temp[i] = processTransferRecord(temp[i]);
				temp[i] = processCreateDeleteAccountRecord(temp[i]);
				temp[i] = processCreateDeleteCustomerRecord(temp[i]);


				if (transactionTime.length() == 6)
				{
					seconds = Integer.valueOf(transactionTime.substring(4));
					minutes = Integer.valueOf(transactionTime.substring(2, 4));
					hours = Integer.valueOf(transactionTime.substring(0, 2));
				}

				myCalendar.setTime(transactionDateFromRecord);
				long timeInMillis = myCalendar.getTimeInMillis();
				timeInMillis = timeInMillis + (hours * 60 * 60 * 1000);
				timeInMillis = timeInMillis + (minutes * 60 * 1000);
				timeInMillis = timeInMillis + (seconds * 1000);
				transactionDateFromRecord = new Date(timeInMillis);
				temp[i].setTransactionDate(transactionDateFromRecord);
				i++;
			}
		}
		catch (SQLException e)
		{
			logger.severe(e.getLocalizedMessage());
			logger.exiting(this.getClass().getName(), GET_PROCESSED_TRANSACTIONS, null);
			return null;
		}
		ProcessedTransaction[] real = new ProcessedTransaction[i];
		System.arraycopy(temp, 0, real, 0, i);
		logger.exiting(this.getClass().getName(), GET_PROCESSED_TRANSACTIONS, real);
		return real;
	}

	private ProcessedTransaction processCreateDeleteCustomerRecord(ProcessedTransaction processedTransaction)
	{
		// Created/deleted customer records are identical
		if (processedTransaction.getType().compareTo("IDC") == 0 || processedTransaction.getType().compareTo("ODC") == 0
				|| processedTransaction.getType().compareTo("ICC") == 0
				|| processedTransaction.getType().compareTo("OCC") == 0)
		{
			String deletedCustomerNumber = processedTransaction.getDescription().substring(6, 16);
			String deletedCustomerName = processedTransaction.getDescription().substring(16, 30);
			processedTransaction.setCustomerName(deletedCustomerName);
			String dateOfBirthDD = processedTransaction.getDescription().substring(30, 32);
			String dateOfBirthMM = processedTransaction.getDescription().substring(33, 35);
			String dateOfBirthYYYY = processedTransaction.getDescription().substring(36, 40);
			Calendar myCalendar = Calendar.getInstance();
			myCalendar.set(Calendar.YEAR, (Integer.parseInt(dateOfBirthYYYY)));
			myCalendar.set(Calendar.MONTH, (Integer.parseInt(dateOfBirthMM) - 1));
			myCalendar.set(Calendar.DAY_OF_MONTH, Integer.parseInt(dateOfBirthDD));
			Date dateOfBirthFromRecord = new Date(myCalendar.getTimeInMillis());
			processedTransaction.setDateOfBirth(dateOfBirthFromRecord);
			processedTransaction.setCustomer(deletedCustomerNumber);
		}

		return processedTransaction;
	}

	private ProcessedTransaction processCreateDeleteAccountRecord(ProcessedTransaction processedTransaction)
	{
		// Creating/deleting account records are the same whether we create them
		// over the Internet or Over the Counter
		if (processedTransaction.getType().compareTo("IDA") == 0 || processedTransaction.getType().compareTo("ODA") == 0
				|| processedTransaction.getType().compareTo("ICA") == 0
				|| processedTransaction.getType().compareTo("OCA") == 0)
		{
			String deletedAccountCustomer = processedTransaction.getDescription().substring(0, 10);
			String deletedAccountType = processedTransaction.getDescription().substring(10, 18);
			String lastStatementDD = processedTransaction.getDescription().substring(18, 20);
			String lastStatementMM = processedTransaction.getDescription().substring(20, 22);
			String lastStatementYYYY = processedTransaction.getDescription().substring(22, 26);
			String nextStatementDD = processedTransaction.getDescription().substring(26, 28);
			String nextStatementMM = processedTransaction.getDescription().substring(28, 30);
			String nextStatementYYYY = processedTransaction.getDescription().substring(30, 34);

			processedTransaction.setAccountType(deletedAccountType);
			Calendar myCalendar = Calendar.getInstance();
			myCalendar.set(Calendar.YEAR, (Integer.parseInt(lastStatementYYYY)));
			myCalendar.set(Calendar.MONTH, (Integer.parseInt(lastStatementMM) - 1));
			myCalendar.set(Calendar.DAY_OF_MONTH, Integer.parseInt(lastStatementDD));
			Date lastStatementDate = new Date(myCalendar.getTimeInMillis());
			myCalendar.set(Calendar.YEAR, (Integer.parseInt(nextStatementYYYY)));
			myCalendar.set(Calendar.MONTH, (Integer.parseInt(nextStatementMM)- 1));
			myCalendar.set(Calendar.DAY_OF_MONTH, Integer.parseInt(nextStatementDD));
			Date nextStatementDate = new Date(myCalendar.getTimeInMillis());
			processedTransaction.setLastStatement(lastStatementDate);
			processedTransaction.setNextStatement(nextStatementDate);
			processedTransaction.setCustomer(deletedAccountCustomer);
		}
		return processedTransaction;
	}

	private ProcessedTransaction processTransferRecord(ProcessedTransaction processedTransaction)
	{
		// If we're a "Transfer between accounts" record, set flags
		// appropriately
		if (processedTransaction.getType().compareTo("TFR") == 0)
		{
			String targetSortcodeInRecord = processedTransaction.getDescription().substring(25, 31);
			String targetAccountInRecord = processedTransaction.getDescription().substring(31, 40);

			processedTransaction.setTargetAccountNumber(targetAccountInRecord);
			processedTransaction.setTargetSortcode(targetSortcodeInRecord);
			processedTransaction.setTransfer(true);
		}
		return processedTransaction;
	}

	public String getTargetAccountNumber()
	{
		return targetAccountNumber;
	}

	public void setTargetAccountNumber(String targetAccountNumberIn)
	{
		this.targetAccountNumber = targetAccountNumberIn;
	}

	public String getReference()
	{
		return reference;
	}

	public void setReference(String reference)
	{
		this.reference = reference;
	}

	public String getDescription()
	{
		return description;
	}

	public void setDescription(String description)
	{
		this.description = description;
	}

	public double getAmount()
	{
		return amount;
	}

	public void setAmount(double amount)
	{
		this.amount = amount;
	}

	public Date getTransactionDate()
	{
		return transactionDate;
	}

	public void setTransactionDate(Date transactionDate)
	{
		this.transactionDate = transactionDate;
	}

	public boolean isTransfer()
	{
		return moneyTransfer;
	}

	public void setTransfer(boolean moneyTransfer)
	{
		this.moneyTransfer = moneyTransfer;
	}

	public String getCustomer()
	{
		return customer;
	}

	public boolean writeDebit(String accountNumber, String sortcode, BigDecimal amount2)
	{
		logger.entering(this.getClass().getName(), WRITE_DEBIT);

		sortOutDateTimeTaskString();

		openConnection();

		logger.log(Level.FINE,() ->ABOUT_TO_INSERT + SQL_INSERT + ">");
		try (PreparedStatement stmt = conn.prepareStatement(SQL_INSERT);)
		{
			stmt.setString(1, PROCTRAN.PROC_TRAN_VALID);
			stmt.setString(2, sortcode);
			stmt.setString(3, String.format("%08d", Integer.parseInt(accountNumber)));
			stmt.setString(4, dateString);
			stmt.setString(5, timeString);
			stmt.setString(6, taskRef);
			stmt.setString(7, PROCTRAN.PROC_TY_DEBIT);
			stmt.setString(8, "INTERNET WTHDRW");
			stmt.setBigDecimal(9, amount2);
			stmt.executeUpdate();
		}
		catch (SQLException e)
		{
			logger.severe(e.getLocalizedMessage());
			logger.exiting(this.getClass().getName(), WRITE_DEBIT, false);
			return false;
		}
		logger.exiting(this.getClass().getName(), WRITE_DEBIT, true);
		return true;
	}

	public boolean writeCredit(String accountNumber, String sortcode, BigDecimal amount2)
	{
		logger.entering(this.getClass().getName(), WRITE_CREDIT, false);
		sortOutDateTimeTaskString();

		openConnection();

		logger.log(Level.FINE,() ->ABOUT_TO_INSERT + SQL_INSERT + ">");
		try (PreparedStatement stmt = conn.prepareStatement(SQL_INSERT);)
		{
			stmt.setString(1, PROCTRAN.PROC_TRAN_VALID);
			stmt.setString(2, sortcode);
			stmt.setString(3, String.format("%08d", Integer.parseInt(accountNumber)));
			stmt.setString(4, dateString);
			stmt.setString(5, timeString);
			stmt.setString(6, taskRef);
			stmt.setString(7, PROCTRAN.PROC_TY_CREDIT);
			stmt.setString(8, "INTERNET RECVED");
			stmt.setBigDecimal(9, amount2);
			stmt.executeUpdate();
		}
		catch (SQLException e)
		{
			logger.severe(e.getLocalizedMessage());
			logger.exiting(this.getClass().getName(), WRITE_CREDIT, false);
			return false;
		}
		logger.exiting(this.getClass().getName(), WRITE_CREDIT, true);
		return true;
	}

	public boolean writeTransferLocal(String sortCode2, String accountNumber2, BigDecimal amount2,
			String targetAccountNumber2)
	{
		logger.entering(this.getClass().getName(), WRITE_TRANSFER_LOCAL);

		sortOutDateTimeTaskString();

		String transferDescription = "";
		transferDescription = transferDescription + PROCTRAN.PROC_TRAN_DESC_XFR_FLAG;
		transferDescription = transferDescription.concat("                  ");

		transferDescription = transferDescription.concat(padSortCode(Integer.parseInt(sortCode2)));

		transferDescription = transferDescription.concat(padAccountNumber(Integer.parseInt(targetAccountNumber2)));

		openConnection();

		logger.log(Level.FINE,() ->ABOUT_TO_INSERT + SQL_INSERT + ">");

		try (PreparedStatement stmt = conn.prepareStatement(SQL_INSERT);)
		{
			stmt.setString(1, PROCTRAN.PROC_TRAN_VALID);
			stmt.setString(2, sortCode2);
			stmt.setString(3, String.format("%08d", Integer.parseInt(accountNumber2)));
			stmt.setString(4, dateString);
			stmt.setString(5, timeString);
			stmt.setString(6, taskRef);
			stmt.setString(7, PROCTRAN.PROC_TY_TRANSFER);
			stmt.setString(8, transferDescription);
			stmt.setBigDecimal(9, amount2);

			stmt.executeUpdate();
		}
		catch (SQLException e)
		{
			logger.severe(e.getLocalizedMessage());
			logger.exiting(this.getClass().getName(), WRITE_TRANSFER_LOCAL, false);
			return false;
		}
		logger.exiting(this.getClass().getName(), WRITE_TRANSFER_LOCAL, true);
		return true;
	}

	public boolean writeDeleteCustomer(String sortCode2, String accountNumber, double amountWhichWillAlwaysBeZero,
			Date customerDOB, String customerName, String customerNumber)
	{
		logger.entering(this.getClass().getName(), WRITE_DELETE_CUSTOMER);

		sortOutDateTimeTaskString();
		String customerDOBString = sortOutCustomerDOB(customerDOB);
		String deleteCustomerDescription = "";

		deleteCustomerDescription = deleteCustomerDescription.concat(padCustomerNumber(customerNumber));
		StringBuilder myStringBuilder = new StringBuilder();

		for (int z = customerName.length(); z < 14; z++)
		{
			myStringBuilder = myStringBuilder.append("0");
		}
		myStringBuilder.append(customerName);
		deleteCustomerDescription = deleteCustomerDescription.concat(myStringBuilder.substring(0, 14));

		deleteCustomerDescription = deleteCustomerDescription + customerDOBString;

		openConnection();

		logger.log(Level.FINE,() ->ABOUT_TO_INSERT + SQL_INSERT + ">");

		try (PreparedStatement stmt = conn.prepareStatement(SQL_INSERT);)
		{
			stmt.setString(1, PROCTRAN.PROC_TRAN_VALID);
			stmt.setString(2, sortCode2);
			stmt.setString(3, String.format("%08d", Integer.parseInt(accountNumber)));
			stmt.setString(4, dateString);
			stmt.setString(5, timeString);
			stmt.setString(6, taskRef);
			stmt.setString(7, PROCTRAN.PROC_TY_WEB_DELETE_CUSTOMER);
			stmt.setString(8, deleteCustomerDescription);
			stmt.setDouble(9, amountWhichWillAlwaysBeZero);
			stmt.executeUpdate();
		}
		catch (SQLException e)
		{
			logger.severe(e.getLocalizedMessage());
			logger.exiting(this.getClass().getName(), WRITE_DELETE_CUSTOMER, false);
			return false;
		}
		logger.exiting(this.getClass().getName(), WRITE_DELETE_CUSTOMER, true);
		return true;
	}

	public boolean writeCreateCustomer(String sortCode2, String accountNumber, double amountWhichWillBeZero,
			Date customerDOB, String customerName, String customerNumber)
	{
		logger.entering(this.getClass().getName(), WRITE_CREATE_CUSTOMER);
		sortOutDateTimeTaskString();
		String createCustomerDescription = "";
		createCustomerDescription = createCustomerDescription.concat(padSortCode(Integer.parseInt(sortCode2)));

		createCustomerDescription = createCustomerDescription.concat(padCustomerNumber(customerNumber));
		StringBuilder myStringBuilder = new StringBuilder();
		for (int z = customerName.length(); z < 14; z++)
		{
			myStringBuilder.append("0");
		}
		myStringBuilder.append(customerName);
		createCustomerDescription = createCustomerDescription.concat(myStringBuilder.substring(0, 14));

		String customerDOBStringForNewCustomer = sortOutCustomerDOB(customerDOB);

		createCustomerDescription = createCustomerDescription + customerDOBStringForNewCustomer;

		openConnection();

		logger.log(Level.FINE,() ->ABOUT_TO_INSERT + SQL_INSERT + ">");

		try (PreparedStatement stmt = conn.prepareStatement(SQL_INSERT);)
		{
			stmt.setString(1, PROCTRAN.PROC_TRAN_VALID);
			stmt.setString(2, sortCode2);
			stmt.setString(3, String.format("%08d", Integer.parseInt(accountNumber)));
			stmt.setString(4, dateString);
			stmt.setString(5, timeString);
			stmt.setString(6, taskRef);
			stmt.setString(7, PROCTRAN.PROC_TY_WEB_CREATE_CUSTOMER);
			stmt.setString(8, createCustomerDescription);
			stmt.setDouble(9, amountWhichWillBeZero);
			stmt.executeUpdate();
		}
		catch (SQLException e)
		{

			logger.severe(e.getLocalizedMessage());
			logger.exiting(this.getClass().getName(), WRITE_CREATE_CUSTOMER, false);
			return false;
		}
		logger.exiting(this.getClass().getName(), WRITE_CREATE_CUSTOMER, true);
		return true;
	}

	public boolean writeDeleteAccount(String sortCode2, String accountNumber, BigDecimal actualBalance,
			Date lastStatement, Date nextStatement, String customerNumber, String accountType)
	{
		logger.entering(this.getClass().getName(), WRITE_DELETE_ACCOUNT);

		PROCTRAN myPROCTRAN = new PROCTRAN();

		Calendar myCalendar = Calendar.getInstance();
		myCalendar.setTime(lastStatement);

		myPROCTRAN.setProcDescDelaccLastDd(myCalendar.get(Calendar.DATE));
		myPROCTRAN.setProcDescDelaccLastMm(myCalendar.get(Calendar.MONTH) + 1);
		myPROCTRAN.setProcDescDelaccLastYyyy(myCalendar.get(Calendar.YEAR));

		myCalendar.setTime(nextStatement);
		myPROCTRAN.setProcDescDelaccNextDd(myCalendar.get(Calendar.DATE));
		myPROCTRAN.setProcDescDelaccNextMm(myCalendar.get(Calendar.MONTH) + 1);
		myPROCTRAN.setProcDescDelaccNextYyyy(myCalendar.get(Calendar.YEAR));
		myPROCTRAN.setProcDescDelaccAcctype(accountType);
		myPROCTRAN.setProcDescDelaccCustomer(Integer.parseInt(customerNumber));
		myPROCTRAN.setProcDescDelaccFooter(PROCTRAN.PROC_DESC_DELACC_FLAG);

		String descriptionForDeletedAccount = myPROCTRAN.getProcTranDesc();

		sortOutDateTimeTaskString();

		openConnection();

		logger.log(Level.FINE,() ->ABOUT_TO_INSERT + SQL_INSERT + ">");
		try (PreparedStatement stmt = conn.prepareStatement(SQL_INSERT);)
		{
			stmt.setString(1, PROCTRAN.PROC_TRAN_VALID);
			stmt.setString(2, sortCode2);
			stmt.setString(3, String.format("%08d", Integer.parseInt(accountNumber)));
			stmt.setString(4, dateString);
			stmt.setString(5, timeString);
			stmt.setString(6, taskRef);
			stmt.setString(7, PROCTRAN.PROC_TY_WEB_DELETE_ACCOUNT);
			stmt.setString(8, descriptionForDeletedAccount);
			stmt.setBigDecimal(9, actualBalance.setScale(2, RoundingMode.HALF_UP));
			stmt.executeUpdate();
		}
		catch (SQLException e)
		{
			logger.severe(e.getLocalizedMessage());
			logger.exiting(this.getClass().getName(), WRITE_DELETE_ACCOUNT, false);
			return false;
		}
		logger.exiting(this.getClass().getName(), WRITE_DELETE_ACCOUNT, true);
		return true;
	}

	public Date getDateOfBirth()
	{
		return dateOfBirth;
	}

	public boolean writeCreateAccount(String sortCode2, String accountNumber, BigDecimal actualBalance,
			Date lastStatement, Date nextStatement, String customerNumber, String accountType)
	{
		logger.entering(this.getClass().getName(), WRITE_CREATE_ACCOUNT);

		sortOutDateTimeTaskString();

		PROCTRAN myPROCTRAN = new PROCTRAN();

		Calendar myCalendar = Calendar.getInstance();
		myCalendar.setTime(lastStatement);

		myPROCTRAN.setProcDescDelaccLastDd(myCalendar.get(Calendar.DATE));
		myPROCTRAN.setProcDescDelaccLastMm(myCalendar.get(Calendar.MONTH) + 1);
		myPROCTRAN.setProcDescDelaccLastYyyy(myCalendar.get(Calendar.YEAR));

		myCalendar.setTime(nextStatement);
		myPROCTRAN.setProcDescDelaccNextDd(myCalendar.get(Calendar.DATE));
		myPROCTRAN.setProcDescDelaccNextMm(myCalendar.get(Calendar.MONTH) + 1);
		myPROCTRAN.setProcDescDelaccNextYyyy(myCalendar.get(Calendar.YEAR));

		myPROCTRAN.setProcDescCreaccAcctype(accountType);
		myPROCTRAN.setProcDescCreaccCustomer(Integer.parseInt(customerNumber));
		myPROCTRAN.setProcDescCreaccFooter(PROCTRAN.PROC_DESC_CREACC_FLAG);

		String descriptionForCreatedAccount = myPROCTRAN.getProcTranDesc();

		openConnection();

		logger.log(Level.FINE,() ->ABOUT_TO_INSERT + SQL_INSERT + ">");
		try (PreparedStatement stmt = conn.prepareStatement(SQL_INSERT);)
		{
			stmt.setString(1, PROCTRAN.PROC_TRAN_VALID);
			stmt.setString(2, sortCode2);
			stmt.setString(3, String.format("%08d", Integer.parseInt(accountNumber)));
			stmt.setString(4, dateString);
			stmt.setString(5, timeString);
			stmt.setString(6, taskRef);
			stmt.setString(7, PROCTRAN.PROC_TY_WEB_CREATE_ACCOUNT);
			stmt.setString(8, descriptionForCreatedAccount);
			stmt.setBigDecimal(9, actualBalance.setScale(2, RoundingMode.HALF_UP));
			stmt.executeUpdate();
		}
		catch (SQLException e)
		{
			logger.severe(e.getLocalizedMessage());
			logger.exiting(this.getClass().getName(), WRITE_CREATE_ACCOUNT, false);
			return false;
		}
		logger.exiting(this.getClass().getName(), WRITE_CREATE_ACCOUNT, true);
		return true;
	}

	private void sortOutDateTimeTaskString()
	{
		Calendar now = Calendar.getInstance();
		StringBuilder myStringBuilder = new StringBuilder(Integer.toString(now.get(Calendar.HOUR_OF_DAY)));
		for (int z = myStringBuilder.length(); z < 2; z++)
		{
			myStringBuilder = myStringBuilder.insert(0, "0");
		}
		timeString = timeString.concat(myStringBuilder.toString());
		myStringBuilder = new StringBuilder(Integer.valueOf(now.get(Calendar.MINUTE)));
		for (int z = myStringBuilder.length(); z < 2; z++)
		{
			myStringBuilder = myStringBuilder.insert(0, "0");
		}
		timeString = timeString.concat(myStringBuilder.toString());
		myStringBuilder = new StringBuilder(Integer.valueOf(now.get(Calendar.SECOND)));
		for (int z = myStringBuilder.length(); z < 2; z++)
		{
			myStringBuilder = myStringBuilder.insert(0, "0");
		}
		timeString = timeString.concat(myStringBuilder.toString());

		myStringBuilder = new StringBuilder(Integer.valueOf(now.get(Calendar.DATE)));
		for (int z = myStringBuilder.length(); z < 2; z++)
		{
			myStringBuilder = myStringBuilder.insert(0, "0");
		}
		dateString = timeString.concat(myStringBuilder.toString());

		myStringBuilder = new StringBuilder(Integer.valueOf(now.get(Calendar.MONTH) + 1));
		for (int z = myStringBuilder.length(); z < 2; z++)
		{
			myStringBuilder = myStringBuilder.insert(0, "0");
		}
		dateString = timeString.concat(myStringBuilder.toString());

		myStringBuilder = new StringBuilder(Integer.valueOf(now.get(Calendar.YEAR)));
		for (int z = myStringBuilder.length(); z < 4; z++)
		{
			myStringBuilder = myStringBuilder.insert(0, "0");
		}
		dateString = timeString.concat(myStringBuilder.toString());

		myStringBuilder = new StringBuilder(Integer.toString(Task.getTask().getTaskNumber()));
		for (int z = myStringBuilder.length(); z < 12; z++)
		{
			myStringBuilder = myStringBuilder.insert(0, "0");
		}
		taskRef = myStringBuilder.toString();
	}

	String sortOutCustomerDOB(Date customerDOB)
	{
		Calendar myCalendar = Calendar.getInstance();
		myCalendar.setTime(customerDOB);

		StringBuilder myStringBuilder = new StringBuilder(Integer.valueOf(myCalendar.get(Calendar.DATE)));
		for (int z = myStringBuilder.length(); z < 2; z++)
		{
			myStringBuilder = myStringBuilder.insert(0, "0");
		}
		String customerDOBString = myStringBuilder.toString();
		customerDOBString = customerDOBString + "-";
		myStringBuilder = new StringBuilder(Integer.valueOf(myCalendar.get(Calendar.MONTH) + 1));
		for (int z = myStringBuilder.length(); z < 2; z++)
		{
			myStringBuilder = myStringBuilder.insert(0, "0");
		}
		customerDOBString = customerDOBString + myStringBuilder.toString();
		customerDOBString = customerDOBString + "-";
		myStringBuilder = new StringBuilder(Integer.valueOf(myCalendar.get(Calendar.YEAR)));
		for (int z = myStringBuilder.length(); z < 4; z++)
		{
			myStringBuilder = myStringBuilder.insert(0, "0");
		}
		customerDOBString = customerDOBString + myStringBuilder.toString();
		return customerDOBString;
	}

	private String padCustomerNumber(String customerNumber2)
	{
		StringBuilder myStringBuilder = new StringBuilder();
		for (int z = customerNumber2.length(); z < 10; z++)
		{
			myStringBuilder.append("0");
		}
		myStringBuilder.append(customerNumber2);
		return myStringBuilder.toString();
	}

	private String padAccountNumber(Integer accountNumber2)
	{
		StringBuilder myStringBuilder = new StringBuilder();
		for (int z = accountNumber2.toString().length(); z < 8; z++)
		{
			myStringBuilder.append("0");
		}
		myStringBuilder.append(accountNumber2.toString());
		return myStringBuilder.toString();
	}

	private String padSortCode(Integer sortcode2)
	{
		StringBuilder myStringBuilder = new StringBuilder();

		for (int z = sortcode2.toString().length(); z < 6; z++)
		{
			myStringBuilder.append("0");
		}
		myStringBuilder.append(sortcode2.toString());
		return myStringBuilder.toString();

	}
}