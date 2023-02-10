/*
 *
 *    Copyright IBM Corp. 2022
 *
 */
package com.ibm.cics.cip.bankliberty.web.vsam;

import java.io.IOException;
import java.io.UnsupportedEncodingException;

import java.sql.Date;
import java.util.Calendar;
import java.util.logging.Level;
import java.util.logging.LogManager;
import java.util.logging.Logger;

import com.ibm.cics.cip.bankliberty.api.json.CreditScore;
import com.ibm.cics.cip.bankliberty.api.json.CustomerJSON;
import com.ibm.cics.cip.bankliberty.datainterfaces.CUSTOMER;
import com.ibm.cics.cip.bankliberty.datainterfaces.CustomerControl;
import com.ibm.cics.server.ChangedException;
import com.ibm.cics.server.DuplicateKeyException;
import com.ibm.cics.server.DuplicateRecordException;
import com.ibm.cics.server.EndOfFileException;
import com.ibm.cics.server.FileDisabledException;
import com.ibm.cics.server.FileNotFoundException;
import com.ibm.cics.server.IOErrorException;
import com.ibm.cics.server.ISCInvalidRequestException;
import com.ibm.cics.server.InvalidRequestException;
import com.ibm.cics.server.InvalidSystemIdException;
import com.ibm.cics.server.KSDS;
import com.ibm.cics.server.KeyHolder;
import com.ibm.cics.server.KeyedFileBrowse;
import com.ibm.cics.server.LengthErrorException;
import com.ibm.cics.server.LoadingException;
import com.ibm.cics.server.LockedException;
import com.ibm.cics.server.LogicException;
import com.ibm.cics.server.NameResource;
import com.ibm.cics.server.NoSpaceException;
import com.ibm.cics.server.NotAuthorisedException;
import com.ibm.cics.server.NotOpenException;
import com.ibm.cics.server.RecordBusyException;
import com.ibm.cics.server.RecordHolder;
import com.ibm.cics.server.RecordNotFoundException;
import com.ibm.cics.server.ResourceUnavailableException;

public class Customer
{

	static final String COPYRIGHT = "Copyright IBM Corp. 2022";

	private static Logger logger = Logger
			.getLogger("com.ibm.cics.cip.bankliberty.web.vsam");

	private static final String GET_CUSTOMER = "getCustomer(long customerNumber, int sortCode";

	private static final String GET_CUSTOMERS = "getCustomers(int sortCode)";

	private static final String GET_CUSTOMERS_WITH_OFFSET_AND_LIMIT = "getCustomers(int sortCode, int limit, int offset)";

	private static final String GET_CUSTOMERS_BY_NAME_WITH_OFFSET_AND_LIMIT = "getCustomersByName(int sortCode, int limit, int offset, String name)";

	private static final String GET_CUSTOMERS_BY_NAME = "getCustomersByName(int sortCode, String name)";

	private static final String GET_CUSTOMERS_BY_NAME_COUNT_ONLY = "getCustomersByNameCountOnly(int sortCode, String name)";

	private static final String GET_CUSTOMERS_BY_TOWN = "getCustomersbyTown(String town)";

	private static final String GET_CUSTOMERS_BY_SURNAME = "getCustomersbySurname(String surname)";

	private static final String GET_CUSTOMERS_BY_AGE = "getCustomersbyAge(int age)";

	private static final String GET_CUSTOMERS_COUNT_ONLY = "getCustomersCountOnly(int sortCode))";

	private static final String UPDATE_CUSTOMER = "updateCustomer(CustomerJSON customer)";

	private static final String DELETE_CUSTOMER = "deleteCustomer(long customerNumber, int sortCode)";

	private static final String CREATE_CUSTOMER = "createCustomer(CustomerJSON customer, Integer sortCodeInteger, boolean useNamedCounter)";

	private static final String FILENAME = "CUSTOMER";

	private static final String CODEPAGE = "Cp1047";

	private static final String ERROR_START_BROWSE = "Error starting browse of file CUSTOMER ";

	private static final String ERROR_BROWSE = "Error browsing file CUSTOMER ";

	private static final String ERROR_END_BROWSE = "Error ending browse of file CUSTOMER ";

	private static final String ERROR_DELETE1 = "Error deleting customer ";

	private static final String LAST_CUSTOMER = "0000009999999999";

	private static final int VSAM_KEY_LENGTH = 16;

	private static final int CUSTOMER_NUMBER_LENGTH = 10;

	private static final int SORT_CODE_LENGTH = 6;

	private String customerNumber;

	private String sortcode;

	private String name;

	private String address;

	private Date dob;

	private String creditScore;

	private Date reviewDate;

	private KSDS customerFile;

	private CUSTOMER myCustomer;

	private boolean notFound;

	private RecordHolder holder;

	private KeyHolder keyHolder;


	public Customer(String custNo, String sc, String n, String a, Date d,
			String creditScore, Date reviewDate)
	{
		setCustomerNumber(custNo);
		setSortcode(sc);
		setName(n);
		setAddress(a);
		setDob(d);
		setCreditScore(creditScore);
		setReviewDate(reviewDate);
		sortOutLogging();
	}


	public Customer()
	{
		sortOutLogging();
		customerFile = new KSDS();

	}


	public String getCustomerNumber()
	{
		if (this.customerNumber.length() < CUSTOMER_NUMBER_LENGTH)
		{
			this.customerNumber = padCustomerNumber(this.customerNumber);
		}
		return this.customerNumber;
	}


	public void setCustomerNumber(String custNo)
	{
		this.customerNumber = padCustomerNumber(custNo);
	}


	public String getSortcode()
	{
		return sortcode;
	}


	public void setSortcode(String sortcode)
	{
		this.sortcode = sortcode;
	}


	public String getName()
	{
		return name;
	}


	public void setName(String name)
	{
		this.name = name;
	}


	public String getAddress()
	{
		return address;
	}


	public void setAddress(String address)
	{
		this.address = address;
	}


	public Date getDob()
	{
		return dob;
	}


	public void setDob(Date dob)
	{
		this.dob = dob;
	}


	public String getCreditScore()
	{
		return creditScore;
	}


	public void setCreditScore(String creditScore)
	{
		this.creditScore = creditScore;
	}


	public Date getReviewDate()
	{
		return reviewDate;
	}


	public void setReviewDate(Date reviewDate)
	{
		this.reviewDate = reviewDate;
	}


	/**
	 * printCustomerDetails Test method by Tom
	 * 
	 */
	public void printCustomerDetails()
	{
		logger.log(Level.FINE, () -> "VSAM CUSTMOMER----");
		logger.log(Level.FINE, () -> "Customer Number: " + this.customerNumber);
		logger.log(Level.FINE, () -> "Name: " + this.name);
		logger.log(Level.FINE, () -> "Address: " + this.address);
		logger.log(Level.FINE, () -> "Dob: " + this.dob.toString());
		logger.log(Level.FINE, () -> "Sortcode : " + this.sortcode);
		logger.log(Level.FINE, () -> "Credit Score: " + this.creditScore);
		logger.log(Level.FINE,
				() -> "Review Date: " + this.reviewDate.toString());
	}


	public Customer getCustomer(long customerNumber, int sortCode)
	{
		logger.entering(this.getClass().getName(), GET_CUSTOMER);
		Customer temp = null;

		customerFile.setName(FILENAME);

		myCustomer = new CUSTOMER();

		if (customerNumber == 9999999999L)
		{
			holder = getLastCustomer();
			byte[] lastCustomerBytes;
			if (holder != null)
			{
				lastCustomerBytes = holder.getValue();
			}
			else
				return null;

			if (lastCustomerBytes != null)
			{
				myCustomer = new CUSTOMER();
			}
			else
			{
				return null;
			}

		}

		if (customerNumber > 0 && customerNumber < 9999999999L)
		{
			holder = new RecordHolder();
			byte[] key = buildKey(sortCode, customerNumber);
			try
			{
				customerFile.read(key, holder);
			}
			catch (InvalidSystemIdException | LogicException
					| InvalidRequestException | IOErrorException
					| ChangedException | LockedException | LoadingException
					| RecordBusyException | FileDisabledException
					| DuplicateKeyException | FileNotFoundException
					| ISCInvalidRequestException | NotAuthorisedException
					| RecordNotFoundException | NotOpenException e)
			{
				logger.severe("Error reading customer file for "
						+ customerNumber + " " + e.getLocalizedMessage());
				logger.exiting(this.getClass().getName(), GET_CUSTOMER, null);
				return null;
			}
			myCustomer = new CUSTOMER(holder.getValue());
		}

		Calendar myCalendar = Calendar.getInstance();
		myCalendar.set(Calendar.YEAR, myCustomer.getCustomerBirthYear());
		myCalendar.set(Calendar.MONTH, myCustomer.getCustomerBirthMonth());
		myCalendar.set(Calendar.DAY_OF_MONTH, myCustomer.getCustomerBirthDay());
		Date myCustomerBirthDate = new Date(
				myCalendar.toInstant().toEpochMilli());
		myCalendar.set(Calendar.YEAR, myCustomer.getCustomerCsReviewYear());
		myCalendar.set(Calendar.MONTH, myCustomer.getCustomerCsReviewMonth());
		myCalendar.set(Calendar.DAY_OF_MONTH,
				myCustomer.getCustomerCsReviewDay());
		Date myCustomerReviewDate = new Date(
				myCalendar.toInstant().toEpochMilli());

		temp = new Customer(Long.toString(myCustomer.getCustomerNumber()),
				Integer.toString(myCustomer.getCustomerSortcode()),
				myCustomer.getCustomerName(), myCustomer.getCustomerAddress(),
				myCustomerBirthDate,
				Integer.toString(myCustomer.getCustomerCreditScore()),
				myCustomerReviewDate);
		logger.exiting(this.getClass().getName(), GET_CUSTOMER, temp);
		return temp;
	}


	private RecordHolder getLastCustomer()
	{
		// The last customer in the file is accessed by reading backwards from
		// the end
		holder = new RecordHolder();
		keyHolder = new KeyHolder();

		// We need to set the key to high values. This is awkward in Java
		byte[] key = new byte[VSAM_KEY_LENGTH];

		for (int z = 0; z < VSAM_KEY_LENGTH; z++)
		{

			key[z] = (byte) -1;
		}

		try
		{
			KeyedFileBrowse myKeyedFileBrowse = customerFile.startBrowse(key);
			myKeyedFileBrowse.previous(holder, keyHolder);
			myKeyedFileBrowse.end();
			key = keyHolder.getValue();
			customerFile.read(key, holder);
		}
		catch (InvalidSystemIdException | LengthErrorException
				| EndOfFileException | LogicException | InvalidRequestException
				| IOErrorException | ChangedException | LockedException
				| LoadingException | RecordBusyException | FileDisabledException
				| DuplicateKeyException | FileNotFoundException
				| ISCInvalidRequestException | NotAuthorisedException
				| RecordNotFoundException | NotOpenException e)
		{
			logger.severe("Error reading customer " + customerNumber + " "
					+ e.getLocalizedMessage());
			logger.exiting(this.getClass().getName(), GET_CUSTOMER, null);
			return null;
		}
		return holder;
	}


	public Customer[] getCustomers(int sortCode)
	{
		logger.entering(this.getClass().getName(), GET_CUSTOMERS, null);
		Customer[] temp = new Customer[250000];
		int i = 0;

		customerFile.setName(FILENAME);

		myCustomer = new CUSTOMER();

		holder = new RecordHolder();
		KeyHolder keyHolder = new KeyHolder();
		byte[] key = buildKey(sortCode, 0);

		// We need to convert the key to EBCDIC
		String keyString = new String(key);
		try
		{
			key = keyString.getBytes(CODEPAGE);
		}
		catch (UnsupportedEncodingException e2)
		{
			logger.severe(e2.getLocalizedMessage());
			logger.exiting(this.getClass().getName(), GET_CUSTOMERS, null);
			return null;
		}

		KeyedFileBrowse customerFileBrowse = null;
		try
		{
			customerFileBrowse = customerFile.startBrowse(key);
		}
		catch (InvalidSystemIdException | LogicException
				| InvalidRequestException | IOErrorException | LockedException
				| RecordBusyException | LoadingException | ChangedException
				| FileDisabledException | FileNotFoundException
				| ISCInvalidRequestException | NotAuthorisedException
				| RecordNotFoundException | NotOpenException e1)
		{
			logger.severe(ERROR_START_BROWSE + e1.getLocalizedMessage());
			logger.exiting(this.getClass().getName(), GET_CUSTOMERS, null);
			return null;
		}
		i = 0;
		boolean carryOn = true;
		for (int j = 0; j < 250000 && carryOn; j++)
		{
			try
			{
				customerFileBrowse.next(holder, keyHolder);
			}
			catch (DuplicateKeyException e)
			{
				// we don't care about this one
			}
			catch (EndOfFileException e)
			{
				// This one we do care about but we expect it
				carryOn = false;

			}
			catch (InvalidSystemIdException | LogicException
					| InvalidRequestException | IOErrorException
					| ChangedException | LockedException | LoadingException
					| RecordBusyException | FileDisabledException
					| FileNotFoundException | ISCInvalidRequestException
					| NotAuthorisedException | RecordNotFoundException
					| NotOpenException | LengthErrorException e)
			{
				logger.severe(ERROR_BROWSE + e.getLocalizedMessage());
				logger.exiting(this.getClass().getName(), GET_CUSTOMERS, null);
				return null;
			}

			myCustomer = new CUSTOMER(holder.getValue());

			temp[j] = new Customer();
			temp[j].setAddress(myCustomer.getCustomerAddress());
			temp[j].setCustomerNumber(
					Long.toString(myCustomer.getCustomerNumber()));
			temp[j].setName(myCustomer.getCustomerName());
			temp[j].setSortcode(
					Integer.toString(myCustomer.getCustomerSortcode()));
			Calendar myCalendar = Calendar.getInstance();
			myCalendar.set(Calendar.YEAR, myCustomer.getCustomerBirthYear());
			myCalendar.set(Calendar.MONTH, myCustomer.getCustomerBirthMonth());
			myCalendar.set(Calendar.DAY_OF_MONTH,
					myCustomer.getCustomerBirthDay());
			Date myCustomerBirthDate = new Date(
					myCalendar.toInstant().toEpochMilli());
			temp[j].setDob(myCustomerBirthDate);
			temp[j].setCreditScore(
					Integer.toString(myCustomer.getCustomerCreditScore()));
			myCalendar.set(Calendar.YEAR,
					myCustomer.getCustomerCsReviewYear() - 1900);
			myCalendar.set(Calendar.MONTH,
					myCustomer.getCustomerCsReviewMonth() - 1);
			myCalendar.set(Calendar.DAY_OF_MONTH,
					myCustomer.getCustomerCsReviewDay());
			Date myCustomerCsReviewDate = new Date(
					myCalendar.toInstant().toEpochMilli());
			temp[j].setReviewDate(myCustomerCsReviewDate);
			if (Integer.parseInt(temp[j].getSortcode()) == sortCode)
			{
				i++;
			}
		}
		try
		{
			customerFileBrowse.end();
		}
		catch (InvalidSystemIdException | LogicException
				| InvalidRequestException | FileDisabledException
				| FileNotFoundException | ISCInvalidRequestException
				| NotAuthorisedException | NotOpenException e)
		{
			logger.severe(ERROR_END_BROWSE + e.getLocalizedMessage());
			logger.exiting(this.getClass().getName(), GET_CUSTOMERS, null);
			return null;
		}

		Customer[] real = new Customer[i];
		System.arraycopy(temp, 0, real, 0, temp.length);
		logger.exiting(this.getClass().getName(), GET_CUSTOMERS, real);
		return real;
	}


	public Customer updateCustomer(CustomerJSON customer)
	{
		logger.entering(this.getClass().getName(), UPDATE_CUSTOMER, null);

		customerFile.setName(FILENAME);
		Customer temp;
		holder = new RecordHolder();

		Long customerNumberLong = Long.parseLong(customer.getId());

		customer.setId(padCustomerNumber(customer.getId()));

		byte[] key = buildKey(Integer.valueOf(customer.getSortCode()),
				Long.valueOf(customer.getId()));

		try
		{
			customerFile.readForUpdate(key, holder);
			myCustomer = new CUSTOMER(holder.getValue());
			myCustomer.setCustomerAddress(customer.getCustomerAddress());
			myCustomer.setCustomerName(customer.getCustomerName());
			customerFile.rewrite(myCustomer.getByteBuffer());
			myCustomer = new CUSTOMER(holder.getValue());
		}
		catch (InvalidSystemIdException | LogicException
				| InvalidRequestException | IOErrorException | ChangedException
				| LockedException | LoadingException | RecordBusyException
				| FileDisabledException | DuplicateKeyException
				| FileNotFoundException | ISCInvalidRequestException
				| NotAuthorisedException | NotOpenException
				| LengthErrorException | DuplicateRecordException
				| NoSpaceException e)
		{
			logger.severe("Error updating customer " + customerNumberLong + " "
					+ e.getLocalizedMessage());
			logger.exiting(this.getClass().getName(), UPDATE_CUSTOMER, null);
			return null;
		}
		catch (RecordNotFoundException e2)
		{
			Customer customer404 = new Customer();
			customer404.setNotFound(true);
			logger.exiting(this.getClass().getName(), UPDATE_CUSTOMER,
					customer404);
			return customer404;
		}

		Calendar myCalendar = Calendar.getInstance();
		myCalendar.set(Calendar.YEAR, myCustomer.getCustomerBirthYear());
		myCalendar.set(Calendar.MONTH, myCustomer.getCustomerBirthMonth());
		myCalendar.set(Calendar.DAY_OF_MONTH, myCustomer.getCustomerBirthDay());
		Date myCustomerBirthDate = new Date(
				myCalendar.toInstant().toEpochMilli());
		myCalendar.set(Calendar.YEAR, myCustomer.getCustomerCsReviewYear());
		myCalendar.set(Calendar.MONTH, myCustomer.getCustomerCsReviewMonth());
		myCalendar.set(Calendar.DAY_OF_MONTH,
				myCustomer.getCustomerCsReviewDay());
		Date myCustomerReviewDate = new Date(
				myCalendar.toInstant().toEpochMilli());
		myCustomer.getCustomerNumber();
		String myCustomerNumber = padCustomerNumber(
				Long.toString(myCustomer.getCustomerNumber()));

		temp = new Customer(myCustomerNumber,
				Integer.toString(myCustomer.getCustomerSortcode()),
				myCustomer.getCustomerName(), myCustomer.getCustomerAddress(),
				myCustomerBirthDate,
				Integer.toString(myCustomer.getCustomerCreditScore()),
				myCustomerReviewDate);
		logger.exiting(this.getClass().getName(), UPDATE_CUSTOMER, temp);
		return temp;
	}


	private void setNotFound(boolean b)
	{
		this.notFound = b;

	}


	public boolean isNotFound()
	{
		return this.notFound;

	}


	public Customer deleteCustomer(long customerNumber, int sortCode)
	{

		logger.entering(this.getClass().getName(), DELETE_CUSTOMER);

		Customer temp = null;

		customerFile.setName(FILENAME);

		myCustomer = new CUSTOMER();

		holder = null;
		byte[] key = new byte[VSAM_KEY_LENGTH];

		if (customerNumber == 9999999999L)
		{
			holder = getLastCustomer();
			if (holder == null)
				return null;

			key = buildKey(sortCode, customerNumber);

		}

		if (customerNumber > 0 && customerNumber < 9999999999L)
		{
			holder = new RecordHolder();
			key = buildKey(sortCode, customerNumber);
		}
		try
		{
			customerFile.readForUpdate(key, holder);
			customerFile.delete();
		}
		catch (InvalidSystemIdException | LogicException
				| InvalidRequestException | IOErrorException | ChangedException
				| LockedException | LoadingException | RecordBusyException
				| FileDisabledException | DuplicateKeyException
				| FileNotFoundException | ISCInvalidRequestException
				| NotAuthorisedException | NotOpenException e)
		{
			logger.severe(ERROR_DELETE1 + customerNumber + " in CUSTOMER file,"
					+ e.getLocalizedMessage());
			logger.exiting(this.getClass().getName(), DELETE_CUSTOMER, null);
			return null;
		}
		catch (RecordNotFoundException e)
		{
			Customer customer404 = new Customer();
			customer404.setNotFound(true);
			logger.exiting(this.getClass().getName(), DELETE_CUSTOMER,
					customer404);
			return customer404;
		}

		decrementNumberOfCustomers();

		Calendar myCalendar = Calendar.getInstance();
		myCalendar.set(Calendar.YEAR, myCustomer.getCustomerBirthYear());
		myCalendar.set(Calendar.MONTH, myCustomer.getCustomerBirthMonth());
		myCalendar.set(Calendar.DAY_OF_MONTH, myCustomer.getCustomerBirthDay());
		Date myCustomerBirthDate = new Date(
				myCalendar.toInstant().toEpochMilli());
		myCalendar.set(Calendar.YEAR, myCustomer.getCustomerCsReviewYear());
		myCalendar.set(Calendar.MONTH, myCustomer.getCustomerCsReviewMonth());
		myCalendar.set(Calendar.DAY_OF_MONTH,
				myCustomer.getCustomerCsReviewDay());
		Date myCustomerReviewDate = new Date(
				myCalendar.toInstant().toEpochMilli());
		temp = new Customer(Long.toString(myCustomer.getCustomerNumber()),
				Integer.toString(myCustomer.getCustomerSortcode()),
				myCustomer.getCustomerName(), myCustomer.getCustomerAddress(),
				myCustomerBirthDate,
				Integer.toString(myCustomer.getCustomerCreditScore()),
				myCustomerReviewDate);

		logger.exiting(this.getClass().getName(), DELETE_CUSTOMER, temp);
		return temp;

	}


	private boolean decrementNumberOfCustomers()
	{
		CustomerControl myCustomerControl = new CustomerControl();
		customerFile.setName(FILENAME);

		myCustomerControl.setCustomerControlSortcode(0);
		myCustomerControl.setCustomerControlNumber(9999999999L);

		byte[] key = LAST_CUSTOMER.getBytes();

		holder = new RecordHolder();

		try
		{
			customerFile.readForUpdate(key, holder);
		}
		catch (LogicException | InvalidRequestException | IOErrorException
				| InvalidSystemIdException | LockedException | ChangedException
				| LoadingException | RecordBusyException | FileDisabledException
				| DuplicateKeyException | FileNotFoundException
				| ISCInvalidRequestException | NotAuthorisedException
				| RecordNotFoundException | NotOpenException e)
		{
			logger.severe(e.getLocalizedMessage());
			logger.exiting(this.getClass().getName(), DELETE_CUSTOMER, null);
			return false;
		}
		myCustomerControl = new CustomerControl(holder.getValue());

		long numberOfCustomers = myCustomerControl.getNumberOfCustomers();
		numberOfCustomers--;

		myCustomerControl.setNumberOfCustomers(numberOfCustomers);
		try
		{
			customerFile.rewrite(myCustomerControl.getByteBuffer());
		}
		catch (LogicException | InvalidRequestException | IOErrorException
				| LengthErrorException | InvalidSystemIdException
				| ChangedException | LockedException | LoadingException
				| RecordBusyException | FileDisabledException
				| DuplicateRecordException | FileNotFoundException
				| ISCInvalidRequestException | NoSpaceException
				| NotAuthorisedException | NotOpenException e)
		{
			logger.severe(e.getLocalizedMessage());
			logger.exiting(this.getClass().getName(), DELETE_CUSTOMER, null);
			return false;
		}
		return true;

	}


	public Customer createCustomer(CustomerJSON customer,
			Integer sortCodeInteger)
	{
		logger.entering(this.getClass().getName(), CREATE_CUSTOMER);

		Customer temp = null;

		customerFile.setName(FILENAME);
		myCustomer = new CUSTOMER();

		String sortCodeString = sortCodeInteger.toString();
		long customerNumberAsPrimitive = getNextCustomerNumber(sortCodeString);

		Long customerNumberLong = Long.valueOf(customerNumberAsPrimitive);
		if (customerNumberLong == -1)
		{
			return null;
		}

		byte[] key = buildKey(sortCodeInteger, customerNumberLong);

		myCustomer = new CUSTOMER();
		myCustomer.setCustomerEyecatcher(CUSTOMER.CUSTOMER_EYECATCHER_VALUE);
		myCustomer.setCustomerAddress(customer.getCustomerAddress().trim());
		// What about title validation?
		myCustomer.setCustomerName(customer.getCustomerName().trim());

		Calendar myCalendar = Calendar.getInstance();
		myCalendar.setTime(customer.getDateOfBirth());

		myCustomer.setCustomerBirthDay(myCalendar.get(Calendar.DAY_OF_MONTH));
		myCustomer.setCustomerBirthMonth(myCalendar.get(Calendar.MONTH) + 1);
		myCustomer.setCustomerBirthYear(myCalendar.get(Calendar.YEAR));

		myCustomer.setCustomerSortcode(sortCodeInteger);
		myCustomer.setCustomerNumber(customerNumberAsPrimitive);

		customer.setId(customerNumberLong.toString());

		customer = CreditScore.populateCreditScoreAndReviewDate(customer);

		if (customer != null)
		{
			customer.setCreditScore(customer.getCreditScore());
			customer.setReviewDate(customer.getReviewDate());
			creditScore = customer.getCreditScore();
			reviewDate = customer.getReviewDate();
		}
		else
		{
			logger.severe(
					"Error! populateCreditScoreAndReviewDate returned null");
			logger.exiting(this.getClass().getName(), CREATE_CUSTOMER, null);
			return null;
		}

		myCustomer.setCustomerCreditScore(
				Integer.parseInt(customer.getCreditScore()));
		Date myCustomerCsReviewDate = customer.getReviewDate();
		myCalendar.setTime(myCustomerCsReviewDate);
		myCustomer
				.setCustomerCsReviewDay(myCalendar.get(Calendar.DAY_OF_MONTH));
		myCustomer.setCustomerCsReviewMonth(myCalendar.get(Calendar.MONTH) + 1);
		myCustomer.setCustomerCsReviewYear(myCalendar.get(Calendar.YEAR));

		try
		{
			customerFile.write(key, myCustomer.getByteBuffer());
			myCustomer = new CUSTOMER(myCustomer.getByteBuffer());
		}
		catch (InvalidSystemIdException | NoSpaceException | LogicException
				| InvalidRequestException | IOErrorException
				| LengthErrorException | ChangedException | LockedException
				| LoadingException | RecordBusyException | FileDisabledException
				| FileNotFoundException | ISCInvalidRequestException
				| NotAuthorisedException | NotOpenException e)
		{
			logger.severe("Error writing record to CUSTOMER file, "
					+ e.getLocalizedMessage());
			logger.exiting(this.getClass().getName(), CREATE_CUSTOMER, null);
			return null;
		}
		catch (DuplicateRecordException e)
		{
			logger.severe(
					"DuplicateRecordException duplicate value. Have you combined named counter and non-named counter with the same data? com.ibm.cics.cip.bankliberty.web.vsam.Customer.");
			logger.exiting(this.getClass().getName(), CREATE_CUSTOMER, null);
			resetNextCustomerNumber(sortCodeString);
			return null;
		}

		myCalendar.set(Calendar.YEAR, myCustomer.getCustomerBirthYear());
		myCalendar.set(Calendar.MONTH, myCustomer.getCustomerBirthMonth());
		myCalendar.set(Calendar.DAY_OF_MONTH, myCustomer.getCustomerBirthDay());
		Date myCustomerBirthDate = new Date(
				myCalendar.toInstant().toEpochMilli());
		myCalendar.set(Calendar.YEAR, myCustomer.getCustomerCsReviewYear());
		myCalendar.set(Calendar.MONTH, myCustomer.getCustomerCsReviewMonth());
		myCalendar.set(Calendar.DAY_OF_MONTH,
				myCustomer.getCustomerCsReviewDay());
		Date myCustomerReviewDate = new Date(
				myCalendar.toInstant().toEpochMilli());

		String myCustomerNumber = padCustomerNumber(
				Long.toString(myCustomer.getCustomerNumber()));

		temp = new Customer(myCustomerNumber,
				Integer.toString(myCustomer.getCustomerSortcode()),
				myCustomer.getCustomerName(), myCustomer.getCustomerAddress(),
				myCustomerBirthDate,
				Integer.toString(myCustomer.getCustomerCreditScore()),
				myCustomerReviewDate);
		logger.exiting(this.getClass().getName(), CREATE_CUSTOMER, temp);
		return temp;

	}


	private long getNextCustomerNumber(String sortCodeString)
	{
		// We need to get a NEW customer number
		// We need to enqueue, then get the last customer number

		NameResource enqueue = new NameResource();

		enqueue.setName("HBNKCUST" + sortCodeString + "  ");
		try
		{
			enqueue.enqueue();
		}
		catch (LengthErrorException | ResourceUnavailableException e)
		{
			logger.severe(e.getLocalizedMessage());
			logger.exiting(this.getClass().getName(), CREATE_CUSTOMER, null);
			return -1;
		}
		CustomerControl myCustomerControl = new CustomerControl();

		customerFile.setName(FILENAME);

		myCustomerControl.setCustomerControlSortcode(0);
		myCustomerControl.setCustomerControlNumber(9999999999L);

		byte[] key = LAST_CUSTOMER.getBytes();

		String keyString = new String(key);
		try
		{
			key = keyString.getBytes(CODEPAGE);
		}
		catch (UnsupportedEncodingException e2)
		{
			logger.severe(e2.getLocalizedMessage());
			logger.exiting(this.getClass().getName(), CREATE_CUSTOMER, null);
			return -1;
		}

		holder = new RecordHolder();

		try
		{
			customerFile.readForUpdate(key, holder);
			myCustomerControl = new CustomerControl(holder.getValue());
			long lastCustomerNumber = myCustomerControl.getLastCustomerNumber();
			lastCustomerNumber++;

			long numberOfCustomers = myCustomerControl.getNumberOfCustomers();
			numberOfCustomers++;

			myCustomerControl.setLastCustomerNumber(lastCustomerNumber);
			myCustomerControl.setNumberOfCustomers(numberOfCustomers);
			customerFile.rewrite(myCustomerControl.getByteBuffer());
		}
		catch (LogicException | InvalidRequestException | IOErrorException
				| InvalidSystemIdException | LockedException | ChangedException
				| LoadingException | RecordBusyException | FileDisabledException
				| DuplicateKeyException | FileNotFoundException
				| ISCInvalidRequestException | NotAuthorisedException
				| RecordNotFoundException | NotOpenException
				| LengthErrorException | DuplicateRecordException
				| NoSpaceException e)
		{
			logger.severe(e.getLocalizedMessage());
			logger.exiting(this.getClass().getName(), CREATE_CUSTOMER, null);
			return -1;
		}

		return myCustomerControl.getLastCustomerNumber();
	}


	private long resetNextCustomerNumber(String sortCodeString)
	{
		// We need to get a NEW customer number
		// We need to enqueue, then get the last customer number

		NameResource enqueue = new NameResource();

		enqueue.setName("HBNKCUST" + sortCodeString + "  ");
		try
		{
			enqueue.enqueue();
		}
		catch (LengthErrorException | ResourceUnavailableException e)
		{
			logger.severe(e.getLocalizedMessage());
			logger.exiting(this.getClass().getName(), CREATE_CUSTOMER, null);
			return -1;
		}
		CustomerControl myCustomerControl = new CustomerControl();
		customerFile.setName(FILENAME);

		myCustomerControl.setCustomerControlSortcode(0);
		myCustomerControl.setCustomerControlNumber(9999999999L);

		byte[] key = LAST_CUSTOMER.getBytes();

		String keyString = new String(key);
		try
		{
			key = keyString.getBytes(CODEPAGE);
		}
		catch (UnsupportedEncodingException e2)
		{
			logger.severe(e2.getLocalizedMessage());
			logger.exiting(this.getClass().getName(), CREATE_CUSTOMER, null);
			return -1;
		}

		holder = new RecordHolder();

		try
		{
			customerFile.readForUpdate(key, holder);
			myCustomerControl = new CustomerControl(holder.getValue());

			long lastCustomerNumber = myCustomerControl.getLastCustomerNumber();
			lastCustomerNumber--;

			long numberOfCustomers = myCustomerControl.getNumberOfCustomers();
			numberOfCustomers--;

			myCustomerControl.setLastCustomerNumber(lastCustomerNumber);
			myCustomerControl.setNumberOfCustomers(numberOfCustomers);
			customerFile.rewrite(myCustomerControl.getByteBuffer());
		}
		catch (LogicException | InvalidRequestException | IOErrorException
				| InvalidSystemIdException | LockedException | ChangedException
				| LoadingException | RecordBusyException | FileDisabledException
				| DuplicateKeyException | FileNotFoundException
				| ISCInvalidRequestException | NotAuthorisedException
				| RecordNotFoundException | NotOpenException
				| LengthErrorException | DuplicateRecordException
				| NoSpaceException e)
		{
			logger.severe(e.getLocalizedMessage());
			logger.exiting(this.getClass().getName(), CREATE_CUSTOMER, null);
			return -1;
		}

		return myCustomerControl.getLastCustomerNumber();
	}


	public Customer[] getCustomers(int sortCode, int limit, int offset)
	{
		logger.entering(this.getClass().getName(),
				GET_CUSTOMERS_WITH_OFFSET_AND_LIMIT);
		Customer[] temp = new Customer[limit];
		int stored = 0;
		int retrieved = 0;

		customerFile.setName(FILENAME);

		myCustomer = new CUSTOMER();

		holder = new RecordHolder();
		keyHolder = new KeyHolder();
		byte[] key = buildKey(sortCode, 0L);

		// We need to convert the key to EBCDIC
		String keyString = new String(key);
		try
		{
			key = keyString.getBytes(CODEPAGE);
		}
		catch (UnsupportedEncodingException e2)
		{
			logger.severe(e2.getLocalizedMessage());
			logger.exiting(this.getClass().getName(),
					GET_CUSTOMERS_WITH_OFFSET_AND_LIMIT, null);
			return null;
		}

		KeyedFileBrowse customerFileBrowse = null;
		try
		{
			customerFileBrowse = customerFile.startBrowse(key);
			boolean carryOn = true;

			for (retrieved = 0; carryOn && stored < limit; retrieved++)
			{
				try
				{
					customerFileBrowse.next(holder, keyHolder);
				}
				catch (DuplicateKeyException e)
				{
					// we don't care about this one
				}
				catch (EndOfFileException e)
				{
					// This one we do care about but we expect it
					carryOn = false;
				}
				myCustomer = new CUSTOMER(holder.getValue());
				if (retrieved >= offset
						&& (myCustomer.getCustomerSortcode() == sortCode))
				{
					temp[stored] = new Customer();
					temp[stored].setAddress(myCustomer.getCustomerAddress());
					temp[stored].setCustomerNumber(
							Long.toString(myCustomer.getCustomerNumber()));
					temp[stored].setName(myCustomer.getCustomerName());
					temp[stored].setSortcode(Integer
							.toString((myCustomer.getCustomerSortcode())));
					Calendar dobCalendar = Calendar.getInstance();
					dobCalendar.set(Calendar.YEAR,
							myCustomer.getCustomerBirthYear());
					dobCalendar.set(Calendar.MONTH,
							myCustomer.getCustomerBirthMonth());
					dobCalendar.set(Calendar.DAY_OF_MONTH,
							myCustomer.getCustomerBirthDay());
					Date dobDate = new Date(dobCalendar.getTimeInMillis());
					temp[stored].setDob(dobDate);
					temp[stored].setCreditScore(Integer
							.toString(myCustomer.getCustomerCreditScore()));
					Calendar reviewCalendar = Calendar.getInstance();
					reviewCalendar.set(Calendar.YEAR,
							myCustomer.getCustomerCsReviewYear());
					reviewCalendar.set(Calendar.MONTH,
							myCustomer.getCustomerCsReviewMonth());
					reviewCalendar.set(Calendar.DAY_OF_MONTH,
							myCustomer.getCustomerCsReviewDay());
					Date reviewDateForThisCustomer = new Date(
							reviewCalendar.getTimeInMillis());
					temp[stored].setReviewDate(reviewDateForThisCustomer);
					temp[stored].printCustomerDetails();
					stored++;
				}
			}

			customerFileBrowse.end();

			Customer[] real = new Customer[stored];
			System.arraycopy(temp, 0, real, 0, stored);
			logger.exiting(this.getClass().getName(),
					GET_CUSTOMERS_WITH_OFFSET_AND_LIMIT, real);
			return real;
		}
		catch (LengthErrorException | InvalidSystemIdException | LogicException
				| InvalidRequestException | IOErrorException | LockedException
				| RecordBusyException | LoadingException | ChangedException
				| FileDisabledException | FileNotFoundException
				| ISCInvalidRequestException | NotAuthorisedException
				| RecordNotFoundException | NotOpenException e1)
		{
			logger.severe(e1.getLocalizedMessage());
			logger.exiting(this.getClass().getName(),
					GET_CUSTOMERS_WITH_OFFSET_AND_LIMIT, null);
			return null;
		}

	}


	public Customer[] getCustomersByName(int sortCode, int limit, int offset,
			String name)
	{
		logger.entering(this.getClass().getName(),
				GET_CUSTOMERS_BY_NAME_WITH_OFFSET_AND_LIMIT);
		Customer[] temp = new Customer[1000000];

		int stored = 0;

		customerFile.setName(FILENAME);

		myCustomer = new CUSTOMER();

		holder = new RecordHolder();
		keyHolder = new KeyHolder();
		byte[] key = buildKey(sortCode, 0);

		// We need to convert the key to EBCDIC
		String keyString = new String(key);
		try
		{
			key = keyString.getBytes(CODEPAGE);
		}
		catch (UnsupportedEncodingException e2)
		{
			logger.severe(e2.getLocalizedMessage());
			logger.exiting(this.getClass().getName(),
					GET_CUSTOMERS_BY_NAME_WITH_OFFSET_AND_LIMIT, null);
			return null;
		}

		KeyedFileBrowse customerFileBrowse = null;
		try
		{

			customerFileBrowse = customerFile.startBrowse(key);
			boolean carryOn = true;
			boolean endOfFile = false;
			while (carryOn && stored < limit)
			{
				try
				{
					customerFileBrowse.next(holder, keyHolder);
				}
				catch (DuplicateKeyException e)
				{
					// we don't care about this one
				}
				catch (EndOfFileException e)
				{
					// This one we do care about but we expect it
					carryOn = false;
					endOfFile = true;
				}
				myCustomer = new CUSTOMER(holder.getValue());
				// We get here because we either successfully read a record, or
				// hit end of file. if we hit end of file then we might add the
				// last customer twice!
				if (!endOfFile && (myCustomer.getCustomerSortcode() == sortCode)
						&& myCustomer.getCustomerName().contains(name))
				{
					temp[stored] = new Customer();
					temp[stored].setAddress(myCustomer.getCustomerAddress());
					temp[stored].setCustomerNumber(
							Long.toString(myCustomer.getCustomerNumber()));
					temp[stored].setName(myCustomer.getCustomerName());
					temp[stored].setSortcode(
							Integer.toString(myCustomer.getCustomerSortcode()));
					Calendar dobCalendar = Calendar.getInstance();
					dobCalendar.set(Calendar.YEAR,
							myCustomer.getCustomerBirthYear());
					dobCalendar.set(Calendar.MONTH,
							myCustomer.getCustomerBirthMonth());
					dobCalendar.set(Calendar.DAY_OF_MONTH,
							myCustomer.getCustomerBirthDay());
					dob = new Date(dobCalendar.getTimeInMillis());
					temp[stored].setDob(dob);
					temp[stored].setCreditScore(Integer
							.toString(myCustomer.getCustomerCreditScore()));
					Calendar csReviewCalendar = Calendar.getInstance();
					csReviewCalendar.set(Calendar.YEAR,
							myCustomer.getCustomerCsReviewYear());
					csReviewCalendar.set(Calendar.MONTH,
							myCustomer.getCustomerCsReviewMonth());
					csReviewCalendar.set(Calendar.DAY_OF_MONTH,
							myCustomer.getCustomerCsReviewDay());
					Date csReviewDate = new Date(
							csReviewCalendar.getTimeInMillis());
					temp[stored].setReviewDate(csReviewDate);
					stored++;
				}
			}

			customerFileBrowse.end();

			Customer[] real = new Customer[stored];

			System.arraycopy(temp, 0, real, 0, stored);

			logger.exiting(this.getClass().getName(),
					GET_CUSTOMERS_BY_NAME_WITH_OFFSET_AND_LIMIT, real);
			return real;
		}
		catch (LengthErrorException | RecordNotFoundException | LockedException
				| ChangedException | IOErrorException | LoadingException
				| RecordBusyException | InvalidSystemIdException
				| LogicException | InvalidRequestException
				| FileDisabledException | FileNotFoundException
				| ISCInvalidRequestException | NotAuthorisedException
				| NotOpenException e)
		{
			logger.severe(e.getLocalizedMessage());
			logger.exiting(this.getClass().getName(),
					GET_CUSTOMERS_BY_NAME_WITH_OFFSET_AND_LIMIT, null);
			return null;
		}
	}


	public Customer[] getCustomersByName(int sortCode, String name)
	{
		logger.entering(this.getClass().getName(), GET_CUSTOMERS_BY_NAME);
		Customer[] temp = new Customer[250000];
		int stored = 0;
		int retrieved = 0;

		customerFile.setName(FILENAME);

		myCustomer = new CUSTOMER();

		holder = new RecordHolder();
		keyHolder = new KeyHolder();
		byte[] key = buildKey(sortCode, 0);

		// We need to convert the key to EBCDIC
		String keyString = new String(key);
		try
		{
			key = keyString.getBytes(CODEPAGE);
		}
		catch (UnsupportedEncodingException e2)
		{
			logger.severe(e2.getLocalizedMessage());
			logger.exiting(this.getClass().getName(), GET_CUSTOMERS_BY_NAME,
					null);
			return null;
		}

		KeyedFileBrowse customerFileBrowse = null;
		try
		{

			customerFileBrowse = customerFile.startBrowse(key);

			boolean carryOn = true;
			boolean endOfFile = false;
			for (retrieved = 0; retrieved < 250000 && carryOn; retrieved++)
			{
				try
				{
					customerFileBrowse.next(holder, keyHolder);
				}
				catch (DuplicateKeyException e)
				{
					// we don't care about this one
				}
				catch (EndOfFileException e)
				{
					// This one we do care about but we expect it
					carryOn = false;
					endOfFile = true;
				}
				// We get here because we either successfully read a record, or
				// hit end of file. if we hit end of file then we might add the
				// last customer twice!
				if (!endOfFile)
				{
					myCustomer = new CUSTOMER(holder.getValue());
					if ((myCustomer.getCustomerSortcode() == sortCode)
							&& myCustomer.getCustomerName().contains(name))
					{
						temp[stored] = new Customer();
						temp[stored]
								.setAddress(myCustomer.getCustomerAddress());
						temp[stored].setCustomerNumber(
								Long.toString(myCustomer.getCustomerNumber()));
						temp[stored].setName(myCustomer.getCustomerName());
						temp[stored].setSortcode(Integer
								.toString(myCustomer.getCustomerSortcode()));
						Calendar dobCalendar = Calendar.getInstance();
						dobCalendar.set(Calendar.YEAR,
								myCustomer.getCustomerBirthYear());
						dobCalendar.set(Calendar.MONTH,
								myCustomer.getCustomerBirthMonth());
						dobCalendar.set(Calendar.DAY_OF_MONTH,
								myCustomer.getCustomerBirthDay());
						dob = new Date(dobCalendar.getTimeInMillis());
						temp[stored].setDob(dob);
						temp[stored].setCreditScore(Integer
								.toString(myCustomer.getCustomerCreditScore()));
						Calendar csReviewCalendar = Calendar.getInstance();
						csReviewCalendar.set(Calendar.YEAR,
								myCustomer.getCustomerCsReviewYear());
						csReviewCalendar.set(Calendar.MONTH,
								myCustomer.getCustomerCsReviewMonth());
						csReviewCalendar.set(Calendar.DAY_OF_MONTH,
								myCustomer.getCustomerCsReviewDay());
						Date csReviewDate = new Date(
								csReviewCalendar.getTimeInMillis());
						temp[stored].setReviewDate(csReviewDate);
						stored++;
					}
				}
			}
			customerFileBrowse.end();

			Customer[] real = new Customer[stored];
			System.arraycopy(temp, 0, real, 0, stored);
			logger.exiting(this.getClass().getName(), GET_CUSTOMERS_BY_NAME,
					real);
			return real;
		}
		catch (LengthErrorException | InvalidSystemIdException | LogicException
				| InvalidRequestException | IOErrorException | LockedException
				| RecordBusyException | LoadingException | ChangedException
				| FileDisabledException | FileNotFoundException
				| ISCInvalidRequestException | NotAuthorisedException
				| RecordNotFoundException | NotOpenException e1)
		{
			logger.severe(e1.getLocalizedMessage());
			logger.exiting(this.getClass().getName(), GET_CUSTOMERS_BY_NAME,
					null);
			return null;
		}
	}


	public long getCustomersCountOnly()
	{
		logger.entering(this.getClass().getName(), GET_CUSTOMERS_COUNT_ONLY);

		customerFile.setName(FILENAME);

		holder = new RecordHolder();

		byte[] key = buildKey(0, 9999999999L);
		// We need to convert the key to EBCDIC

		try
		{
			customerFile.read(key, holder);
		}
		catch (InvalidSystemIdException | LogicException
				| InvalidRequestException | IOErrorException | LockedException
				| RecordBusyException | LoadingException | ChangedException
				| FileDisabledException | FileNotFoundException
				| ISCInvalidRequestException | NotAuthorisedException
				| RecordNotFoundException | DuplicateKeyException
				| NotOpenException e1)
		{
			logger.severe("Error reading control record for customer file "
					+ e1.getLocalizedMessage());
			logger.exiting(this.getClass().getName(), GET_CUSTOMERS_COUNT_ONLY,
					-1L);
			return -1L;
		}

		CustomerControl myCustomerControl = new CustomerControl(
				holder.getValue());
		logger.exiting(this.getClass().getName(), GET_CUSTOMERS_COUNT_ONLY,
				myCustomerControl.getNumberOfCustomers());
		return myCustomerControl.getNumberOfCustomers();

	}


	public long getCustomersByNameCountOnly(int sortCode, String name)
	{
		logger.entering(this.getClass().getName(),
				GET_CUSTOMERS_BY_NAME_COUNT_ONLY);

		long matchingCustomers = 0;

		customerFile.setName(FILENAME);

		myCustomer = new CUSTOMER();

		holder = new RecordHolder();
		keyHolder = new KeyHolder();
		byte[] key = buildKey(sortCode, 0);

		// We need to convert the key to EBCDIC
		String keyString = new String(key);
		try
		{
			key = keyString.getBytes(CODEPAGE);
		}
		catch (UnsupportedEncodingException e2)
		{
			logger.severe(e2.getLocalizedMessage());
			logger.exiting(this.getClass().getName(),
					GET_CUSTOMERS_BY_NAME_COUNT_ONLY, -1L);
			return -1L;
		}

		KeyedFileBrowse customerFileBrowse = null;
		try
		{

			customerFileBrowse = customerFile.startBrowse(key);

			boolean carryOn = true;
			boolean endOfFile = false;
			while (carryOn)
			{
				try
				{
					customerFileBrowse.next(holder, keyHolder);
				}
				catch (DuplicateKeyException e)
				{
					// we don't care about this one
				}
				catch (EndOfFileException e)
				{
					// This one we do care about but we expect it
					carryOn = false;
					endOfFile = true;
				}

				myCustomer = new CUSTOMER(holder.getValue());

				// We get here because we either successfully read a record, or
				// hit end of file. if we hit end of file then we might add the
				// last customer twice!
				if (!endOfFile && myCustomer.getCustomerName().contains(name))
				{
					matchingCustomers++;
				}
			}

			customerFileBrowse.end();

			logger.exiting(this.getClass().getName(),
					GET_CUSTOMERS_BY_NAME_COUNT_ONLY, matchingCustomers);
			return matchingCustomers;
		}
		catch (LengthErrorException | FileDisabledException | NotOpenException
				| LogicException | InvalidRequestException
				| FileNotFoundException | ISCInvalidRequestException
				| NotAuthorisedException | IOErrorException
				| InvalidSystemIdException | LockedException
				| RecordBusyException | LoadingException | ChangedException
				| RecordNotFoundException e3)
		{
			logger.log(Level.FINE, e3::getLocalizedMessage);
			logger.exiting(this.getClass().getName(),
					GET_CUSTOMERS_BY_NAME_COUNT_ONLY, -1L);
			return -1L;
		}

	}


	private void sortOutLogging()
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


	public Customer[] getCustomersByTown(String town)
	{
		logger.entering(this.getClass().getName(), GET_CUSTOMERS_BY_TOWN, null);
		Customer[] temp = new Customer[250000];
		int i = 0;

		customerFile.setName(FILENAME);

		myCustomer = new CUSTOMER();

		holder = new RecordHolder();
		keyHolder = new KeyHolder();
		byte[] key = buildKey(Integer.valueOf(this.getSortcode()), 0L);

		// We need to convert the key to EBCDIC
		String keyString = new String(key);
		try
		{
			key = keyString.getBytes(CODEPAGE);
		}
		catch (UnsupportedEncodingException e2)
		{
			logger.severe(e2.getLocalizedMessage());
			logger.exiting(this.getClass().getName(), GET_CUSTOMERS_BY_TOWN,
					null);
			return null;
		}

		KeyedFileBrowse customerFileBrowse = null;
		try
		{
			customerFileBrowse = customerFile.startBrowse(key);

			i = 0;
			boolean carryOn = true;
			for (int j = 0; j < 250000 && carryOn; j++)
			{
				try
				{
					customerFileBrowse.next(holder, keyHolder);
				}
				catch (DuplicateKeyException e)
				{
					// we don't care about this one
				}
				catch (EndOfFileException e)
				{
					// This one we do care about but we expect it
					carryOn = false;

				}
				myCustomer = new CUSTOMER(holder.getValue());

				temp[j] = new Customer();
				temp[j].setAddress(myCustomer.getCustomerAddress());
				temp[j].setCustomerNumber(
						Long.toString(myCustomer.getCustomerNumber()));
				temp[j].setName(myCustomer.getCustomerName());
				temp[j].setSortcode(
						Integer.toString(myCustomer.getCustomerSortcode()));
				Calendar dobCalendar = Calendar.getInstance();
				dobCalendar.set(Calendar.YEAR,
						myCustomer.getCustomerBirthYear());
				dobCalendar.set(Calendar.MONTH,
						myCustomer.getCustomerBirthMonth());
				dobCalendar.set(Calendar.DAY_OF_MONTH,
						myCustomer.getCustomerBirthDay());
				dob = new Date(dobCalendar.getTimeInMillis());
				temp[j].setDob(dob);
				temp[j].setCreditScore(
						Integer.toString(myCustomer.getCustomerCreditScore()));
				Calendar csReviewCalendar = Calendar.getInstance();
				csReviewCalendar.set(Calendar.YEAR,
						myCustomer.getCustomerCsReviewYear());
				csReviewCalendar.set(Calendar.MONTH,
						myCustomer.getCustomerCsReviewMonth());
				csReviewCalendar.set(Calendar.DAY_OF_MONTH,
						myCustomer.getCustomerCsReviewDay());
				Date csReviewDate = new Date(
						csReviewCalendar.getTimeInMillis());
				temp[j].setReviewDate(csReviewDate);
				if (temp[j].getAddress().contains(town))
				{
					i++;
				}
			}

			customerFileBrowse.end();

			Customer[] real = new Customer[i];
			System.arraycopy(temp, 0, real, 0, i);

			logger.exiting(this.getClass().getName(), GET_CUSTOMERS_BY_TOWN,
					real);
			return real;
		}
		catch (LengthErrorException | InvalidSystemIdException | LogicException
				| InvalidRequestException | IOErrorException | LockedException
				| RecordBusyException | LoadingException | ChangedException
				| FileDisabledException | FileNotFoundException
				| ISCInvalidRequestException | NotAuthorisedException
				| RecordNotFoundException | NotOpenException e1)
		{
			logger.severe(e1.getLocalizedMessage());
			logger.exiting(this.getClass().getName(), GET_CUSTOMERS_BY_TOWN,
					null);
			return null;
		}
	}


	public Customer[] getCustomersBySurname(String surname)
	{
		logger.entering(this.getClass().getName(), GET_CUSTOMERS_BY_SURNAME,
				null);
		Customer[] temp = new Customer[250000];
		int i = 0;

		customerFile.setName(FILENAME);

		myCustomer = new CUSTOMER();

		Integer sortCodeInteger = Integer.parseInt(this.getSortcode());

		holder = new RecordHolder();
		keyHolder = new KeyHolder();
		byte[] key = buildKey(sortCodeInteger, 0L);

		// We need to convert the key to EBCDIC
		String keyString = new String(key);
		try
		{
			key = keyString.getBytes(CODEPAGE);
		}
		catch (UnsupportedEncodingException e2)
		{
			logger.severe(e2.getLocalizedMessage());
			logger.exiting(this.getClass().getName(), GET_CUSTOMERS_BY_SURNAME,
					null);
			return null;
		}

		KeyedFileBrowse customerFileBrowse = null;
		try
		{
			customerFileBrowse = customerFile.startBrowse(key);

			i = 0;
			boolean carryOn = true;
			for (int j = 0; j < 250000 && carryOn; j++)
			{
				try
				{
					customerFileBrowse.next(holder, keyHolder);
				}
				catch (DuplicateKeyException e)
				{
					// we don't care about this one
				}
				catch (EndOfFileException e)
				{
					// This one we do care about but we expect it
					carryOn = false;

				}

				myCustomer = new CUSTOMER(holder.getValue());

				temp[j] = new Customer();
				temp[j].setAddress(myCustomer.getCustomerAddress());
				temp[j].setCustomerNumber(
						Long.toString(myCustomer.getCustomerNumber()));
				temp[j].setName(myCustomer.getCustomerName());
				temp[j].setSortcode(
						Integer.toString(myCustomer.getCustomerSortcode()));
				Calendar dobCalendar = Calendar.getInstance();
				dobCalendar.set(Calendar.YEAR,
						myCustomer.getCustomerBirthYear());
				dobCalendar.set(Calendar.MONTH,
						myCustomer.getCustomerBirthMonth());
				dobCalendar.set(Calendar.DAY_OF_MONTH,
						myCustomer.getCustomerBirthDay());
				dob = new Date(dobCalendar.getTimeInMillis());
				temp[j].setDob(dob);
				temp[j].setCreditScore(
						Integer.toString(myCustomer.getCustomerCreditScore()));
				Calendar csReviewCalendar = Calendar.getInstance();
				csReviewCalendar.set(Calendar.YEAR,
						myCustomer.getCustomerCsReviewYear());
				csReviewCalendar.set(Calendar.MONTH,
						myCustomer.getCustomerCsReviewMonth());
				csReviewCalendar.set(Calendar.DAY_OF_MONTH,
						myCustomer.getCustomerCsReviewDay());
				Date csReviewDate = new Date(
						csReviewCalendar.getTimeInMillis());
				temp[j].setReviewDate(csReviewDate);
				if (temp[j].getName().contains(surname))
				{
					i++;
				}
			}

			customerFileBrowse.end();

			Customer[] real = new Customer[i];
			System.arraycopy(temp, 0, real, 0, i);
			logger.exiting(this.getClass().getName(), GET_CUSTOMERS_BY_SURNAME,
					real);
			return real;
		}
		catch (LengthErrorException | LogicException | InvalidRequestException
				| IOErrorException | LockedException | RecordBusyException
				| LoadingException | ChangedException | FileDisabledException
				| FileNotFoundException | ISCInvalidRequestException
				| NotAuthorisedException | RecordNotFoundException
				| NotOpenException | InvalidSystemIdException e1)
		{
			logger.severe(ERROR_START_BROWSE + e1.getLocalizedMessage());
			logger.exiting(this.getClass().getName(), GET_CUSTOMERS_BY_SURNAME,
					null);
			return null;
		}
	}


	public Customer[] getCustomersByAge(int age)
	{
		logger.entering(this.getClass().getName(), GET_CUSTOMERS_BY_AGE, null);
		Customer[] temp = new Customer[250000];
		int i = 0;

		customerFile.setName(FILENAME);

		myCustomer = new CUSTOMER();

		Integer sortCodeInteger = Integer.parseInt(this.getSortcode());

		holder = new RecordHolder();
		keyHolder = new KeyHolder();
		byte[] key = buildKey(sortCodeInteger, 0L);

		// We need to convert the key to EBCDIC
		String keyString = new String(key);
		try
		{
			key = keyString.getBytes(CODEPAGE);
		}
		catch (UnsupportedEncodingException e2)
		{
			logger.severe(e2.getLocalizedMessage());
			logger.exiting(this.getClass().getName(), GET_CUSTOMERS_BY_AGE,
					null);
			return null;
		}

		KeyedFileBrowse customerFileBrowse = null;
		try
		{
			customerFileBrowse = customerFile.startBrowse(key);

			i = 0;
			boolean carryOn = true;

			for (int j = 0; j < 250000 && carryOn; j++)
			{

				customerFileBrowse = getNextRecord(customerFileBrowse);
				if (holder != null)
				{
					myCustomer = new CUSTOMER(holder.getValue());
					Calendar dobCalendar = Calendar.getInstance();
					dobCalendar.set(Calendar.YEAR,
							myCustomer.getCustomerBirthYear());
					dobCalendar.set(Calendar.MONTH,
							myCustomer.getCustomerBirthMonth());
					dobCalendar.set(Calendar.DAY_OF_MONTH,
							myCustomer.getCustomerBirthDay());
					dob = new Date(dobCalendar.getTimeInMillis());
					if (customerAgeInYears(dob) == age)
					{
						temp[i] = new Customer();
						temp[i].setAddress(myCustomer.getCustomerAddress());
						temp[i].setCustomerNumber(
								Long.toString(myCustomer.getCustomerNumber()));
						temp[i].setName(myCustomer.getCustomerName());
						temp[i].setSortcode(Integer
								.toString(myCustomer.getCustomerSortcode()));
						temp[i].setDob(dob);
						temp[i].setCreditScore(Integer
								.toString(myCustomer.getCustomerCreditScore()));
						Calendar csReviewCalendar = Calendar.getInstance();
						csReviewCalendar.set(Calendar.YEAR,
								myCustomer.getCustomerCsReviewYear());
						csReviewCalendar.set(Calendar.MONTH,
								myCustomer.getCustomerCsReviewMonth());
						csReviewCalendar.set(Calendar.DAY_OF_MONTH,
								myCustomer.getCustomerCsReviewDay());
						Date csReviewDate = new Date(
								csReviewCalendar.getTimeInMillis());
						temp[i].setReviewDate(csReviewDate);
						i++;
					}
				}
				else
				{
					carryOn = false;
				}

			}

			customerFileBrowse.end();


			Customer[] real = new Customer[i];
			System.arraycopy(temp, 0, real, 0, i);
			logger.exiting(this.getClass().getName(), GET_CUSTOMERS_BY_AGE,
					real);
			return real;
		}
		catch (InvalidSystemIdException | LogicException
				| InvalidRequestException | IOErrorException | LockedException
				| RecordBusyException | LoadingException | ChangedException
				| FileDisabledException | FileNotFoundException
				| ISCInvalidRequestException | NotAuthorisedException
				| RecordNotFoundException | NotOpenException e1)
		{
			logger.severe(ERROR_START_BROWSE + e1.getLocalizedMessage());
			logger.exiting(this.getClass().getName(), GET_CUSTOMERS_BY_AGE,
					null);
			return null;
		}
	}


	private KeyedFileBrowse getNextRecord(
			KeyedFileBrowse localCustomerFileBrowse)
	{
		try
		{
			localCustomerFileBrowse.next(holder, keyHolder);

		}
		catch (DuplicateKeyException e)
		{
			// we don't care about this one
		}
		catch (EndOfFileException e)
		{
			// This one we do care about but we expect it
			holder = null;

		}
		catch (LogicException | InvalidRequestException | IOErrorException
				| LengthErrorException | InvalidSystemIdException
				| ChangedException | LockedException | LoadingException
				| RecordBusyException | FileDisabledException
				| FileNotFoundException | ISCInvalidRequestException
				| NotAuthorisedException | RecordNotFoundException
				| NotOpenException e)
		{
			logger.log(Level.SEVERE, e.toString());
			holder = null;
		}
		return localCustomerFileBrowse;

	}


	int customerAgeInYears(Date dob)
	{
		Calendar nowCalendar = Calendar.getInstance();
		Calendar birthCalendar = Calendar.getInstance();
		birthCalendar.setTime(dob);
		int age = 0;
		int years = (nowCalendar.get(Calendar.YEAR) + 1900)
				- (birthCalendar.get(Calendar.YEAR) + 1900);
		age = years;
		if (birthCalendar.get(Calendar.MONTH) > nowCalendar.get(Calendar.MONTH))
		{
			age--;
			return age;
		}
		if (birthCalendar.get(Calendar.MONTH) == nowCalendar.get(Calendar.MONTH)
				&& birthCalendar.get(Calendar.DAY_OF_MONTH) > nowCalendar
						.get(Calendar.DAY_OF_MONTH))
		{
			age--;
			return age;
		}

		return age;

	}


	byte[] buildKey(int sortCode2, long customerNumber2)
	{
		StringBuilder myStringBuilder = new StringBuilder();

		for (int i = Integer.toString(sortCode2).length(); i < SORT_CODE_LENGTH; i++)
		{
			myStringBuilder.append('0');
		}
		myStringBuilder.append(Integer.toString(sortCode2));
		for (int z = Long.toString(customerNumber2).length(); z < CUSTOMER_NUMBER_LENGTH; z++)
		{
			myStringBuilder = myStringBuilder.append('0');
		}
		myStringBuilder.append(Long.toString(customerNumber2));

		try
		{
			return myStringBuilder.toString().getBytes(CODEPAGE);
		}
		catch (UnsupportedEncodingException e)
		{
			logger.log(Level.SEVERE, e.toString());
			return null;
		}
	}


	private String padCustomerNumber(String customerNumber2)
	{
		StringBuilder myStringBuilder = new StringBuilder();
		for (int z = customerNumber2.length(); z < CUSTOMER_NUMBER_LENGTH; z++)
		{
			myStringBuilder.append("0");
		}
		myStringBuilder.append(customerNumber2);
		return myStringBuilder.toString();
	}

}
