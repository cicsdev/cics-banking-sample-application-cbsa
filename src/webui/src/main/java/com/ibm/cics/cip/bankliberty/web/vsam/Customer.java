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

import com.ibm.cics.cip.bankliberty.api.json.CounterResource;
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
import com.ibm.json.java.JSONObject;

public class Customer {

	static final String COPYRIGHT =
			"Copyright IBM Corp. 2022";


	private static Logger logger = Logger.getLogger("com.ibm.cics.cip.bankliberty.web.vsam");

	private static final String GET_CUSTOMER = "getCustomer(long customerNumber, int sortCode";
	private static final String GET_CUSTOMERS = "getCustomers(int sortCode)";
	private static final String GET_CUSTOMERS_WITH_OFFSET_AND_LIMIT = "getCustomers(int sortCode, int limit, int offset)";
	private static final String GET_CUSTOMERS_BY_NAME_WITH_OFFSET_AND_LIMIT = "getCustomersByName(int sortCode, int limit, int offset, String name)";
	private static final String GET_CUSTOMERS_BY_NAME = "getCustomersByName(int sortCode, String name)";
	private static final String GET_CUSTOMERS_BY_NAME_COUNT_ONLY = "getCustomersByNameCountOnly(int sortCode, String name)";
	private static final String GET_CUSTOMERS_BY_TOWN = "getCustomersbyTown(String town)";
	private static final String GET_CUSTOMERS_BY_SURNAME = "getCustomersbySurname(String surname)";
	private static final String GET_CUSTOMERS_BY_AGE =	"getCustomersbyAge(int age)";
	private static final String GET_CUSTOMERS_COUNT_ONLY = "getCustomersCountOnly(int sortCode))";
	private static final String UPDATE_CUSTOMER = "updateCustomer(CustomerJSON customer)";
	private static final String DELETE_CUSTOMER = "deleteCustomer(long customerNumber, int sortCode)";
	private static final String CREATE_CUSTOMER = "createCustomer(CustomerJSON customer, Integer sortCodeInteger, boolean useNamedCounter)";

	private static final String FILENAME = "CUSTOMER";

	private static final String ABOUT_TO_GO_TO_SLEEP = "About to go to sleep for ";
	private static final String MILLISECONDS = " milliseconds";
	private static final String READ_GIVE_UP = "Cannot read CUSTOMER file after 100 attempts";
	private static final String BROWSE_GIVE_UP = "Cannot browse CUSTOMER file after 100 attempts";
	private static final String CODEPAGE = "Cp1047";

	private static final String ERROR_START_BROWSE = "Error starting browse of file CUSTOMER ";
	private static final String ERROR_BROWSE = "Error browsing file CUSTOMER ";
	private static final String ERROR_END_BROWSE = "Error ending browse of file CUSTOMER ";
	private static final String ERROR_DELETE1 = "Error deleting customer ";
	private static final String ERROR_DELETE2 = " in CUSTOMER file ";

	private static final String LAST_CUSTOMER = "0000009999999999";

	// String ACCOUNT_EYECATCHER             CHAR(4),
	private 	String 		customerNumber;
	private 	String 		sortcode;              
	private 	String 		name;
	private 	String 		address;
	private 	Date 		dob;
	private		String		creditScore;
	private		Date		reviewDate;


	private KSDS customerFile;

	private CUSTOMER myCustomer;

	private     int maximumRetries = 100, 
			totalSleep = 3000;

	private boolean notFound;

	public Customer (String custNo, String sc, String n, String a, Date d, String creditScore, Date reviewDate) {
		setCustomer_number(custNo);
		setSortcode(sc);
		setName(n);
		setAddress(a);
		setDob(d);
		setCreditScore(creditScore);
		setReviewDate(reviewDate);
		sortOutLogging();
	}

	public Customer() {
		sortOutLogging();

	}

	public String getCustomer_number() {
		if(this.customerNumber.length()<10)
		{
			for (int i=this.customerNumber.length();i<10;i++)
			{
				this.customerNumber = "0" + this.customerNumber;
			}
		}
		return this.customerNumber;
	}

	public void setCustomer_number(String custNo) {
		StringBuilder myStringBuilder = new StringBuilder();

		for (int i=custNo.length();i<10;i++)
		{
			myStringBuilder.append('0');
		}

		myStringBuilder.append(custNo);
		this.customerNumber = myStringBuilder.toString();
	}

	public String getSortcode() {
		return sortcode;
	}

	public void setSortcode(String sortcode) {
		this.sortcode = sortcode;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getAddress() {
		return address;
	}

	public void setAddress(String address) {
		this.address = address;
	}

	public Date getDob() {
		return dob;
	}

	public void setDob(Date dob) {
		this.dob = dob;
	}

	public String getCreditScore() {
		return creditScore;
	}

	public void setCreditScore(String creditScore) {
		this.creditScore = creditScore;
	}

	public Date getReviewDate() {
		return reviewDate;
	}

	public void setReviewDate(Date reviewDate) {
		this.reviewDate = reviewDate;
	}

	/** printCustomerDetails
	 * Test method by Tom
	 * 
	 */
	public void printCustomerDetails(){
		logger.log(Level.FINE, () -> "VSAM CUSTMOMER----");
		logger.log(Level.FINE, () -> "Customer Number: " + this.customerNumber);
		logger.log(Level.FINE, () -> "Name: " + this.name);
		logger.log(Level.FINE, () -> "Address: " + this.address);
		logger.log(Level.FINE, () -> "Dob: " + this.dob.toString());
		logger.log(Level.FINE, () -> "Sortcode : " + this.sortcode);
		logger.log(Level.FINE, () -> "Credit Score: " + this.creditScore);
		logger.log(Level.FINE, () -> "Review Date: " + this.reviewDate.toString());
	}



	public Customer getCustomer(long customerNumber, int sortCode) 
	{
		logger.entering(this.getClass().getName(),GET_CUSTOMER);
		Customer temp = null;

		customerFile = new KSDS();
		customerFile.setName(FILENAME);

		myCustomer = new CUSTOMER();



		if(customerNumber == 9999999999L)
		{
			RecordHolder recHolder = getLastCustomer();
			byte[] lastCustomerBytes;
			if(recHolder!=null)
			{
				lastCustomerBytes = recHolder.getValue();
			}
			else return null;

			if(lastCustomerBytes != null)
			{
				myCustomer = new CUSTOMER();
			}
			else
			{
				return null;
			}

		}

		if(customerNumber > 0 && customerNumber < 9999999999L)
		{
			RecordHolder holder = new RecordHolder();
			byte[] key = new byte[16];
			StringBuilder myStringBuilder = new StringBuilder(); 
			for(int z = Integer.toString(sortCode).length(); z < 6;z++)
			{
				myStringBuilder = myStringBuilder.append("0");	
			}
			myStringBuilder.append(Integer.toString(sortCode));


			for(int z = Long.toString(customerNumber).length(); z < 10;z++)
			{
				myStringBuilder = myStringBuilder.append("0");	
			}
			myStringBuilder.append(Long.toString(customerNumber));


			String keyString = myStringBuilder.toString();
			try {
				key = keyString.getBytes(CODEPAGE);
			} catch (UnsupportedEncodingException e2) {
				logger.severe(e2.getLocalizedMessage());
				logger.exiting(this.getClass().getName(),GET_CUSTOMER,null);
				return null;
			}
			try {
				customerFile.read(key, holder);
			} catch (InvalidSystemIdException | LogicException | InvalidRequestException | IOErrorException 
					| ChangedException | LockedException | LoadingException | RecordBusyException
					| FileDisabledException | DuplicateKeyException | FileNotFoundException | ISCInvalidRequestException
					| NotAuthorisedException | RecordNotFoundException | NotOpenException e) {
				logger.severe("Error reading customer file for " + customerNumber + " " + e.getLocalizedMessage());
				logger.exiting(this.getClass().getName(),GET_CUSTOMER,null);
				return null;
			}
			myCustomer = new CUSTOMER(holder.getValue());
		}


		Calendar myCalendar = Calendar.getInstance();
		myCalendar.set(Calendar.YEAR, myCustomer.getCustomerBirthYear());
		myCalendar.set(Calendar.MONTH, myCustomer.getCustomerBirthMonth());
		myCalendar.set(Calendar.DAY_OF_MONTH, myCustomer.getCustomerBirthDay());
		Date myCustomerBirthDate = new Date(myCalendar.toInstant().toEpochMilli());
		myCalendar.set(Calendar.YEAR, myCustomer.getCustomerCsReviewYear());
		myCalendar.set(Calendar.MONTH, myCustomer.getCustomerCsReviewMonth());
		myCalendar.set(Calendar.DAY_OF_MONTH, myCustomer.getCustomerCsReviewDay());
		Date myCustomerReviewDate = new Date(myCalendar.toInstant().toEpochMilli());

		temp = new Customer(new String(new Long(myCustomer.getCustomerNumber()).toString()),
				Integer.toString(myCustomer.getCustomerSortcode()),
				new String(myCustomer.getCustomerName()),
				new String(myCustomer.getCustomerAddress()),
				myCustomerBirthDate,
				Integer.toString(myCustomer.getCustomerCreditScore()),
				myCustomerReviewDate
				);
		logger.exiting(this.getClass().getName(),GET_CUSTOMER,temp);
		return temp;
	}

	private RecordHolder getLastCustomer() {
		// The last customer in the file is accessed by reading backwards from the end
		RecordHolder holder = new RecordHolder();
		KeyHolder keyHolder = new KeyHolder();

		// We need to set the key to high values. This is awkward in Java
		byte[] key = new byte[16];

		for(int z = 0; z < 16;z++)
		{

			key[z] = (byte) -1;	
		}

		try {
			KeyedFileBrowse myKeyedFileBrowse =	customerFile.startBrowse(key);
			myKeyedFileBrowse.previous(holder, keyHolder);
			myKeyedFileBrowse.end();
			key = keyHolder.getValue();
			customerFile.read(key, holder);
		} catch (InvalidSystemIdException | LengthErrorException | EndOfFileException | LogicException | InvalidRequestException | IOErrorException 
				| ChangedException | LockedException | LoadingException | RecordBusyException
				| FileDisabledException | DuplicateKeyException | FileNotFoundException | ISCInvalidRequestException
				| NotAuthorisedException | RecordNotFoundException | NotOpenException e) {
			logger.severe("Error reading customer " + customerNumber + " " + e.getLocalizedMessage());
			logger.exiting(this.getClass().getName(),GET_CUSTOMER,null);
			return null;
		} 
		return holder;
	}

	public Customer[] getCustomers(int sortCode) {
		logger.entering(this.getClass().getName(),GET_CUSTOMERS,null);
		Customer[] temp = new Customer[250000];
		int i = 0;


		KSDS customerFile = new KSDS();
		customerFile.setName(FILENAME);

		myCustomer = new CUSTOMER();

		Integer sortCodeInteger = new Integer(sortCode);
		Integer customerNumberInteger = new Integer(0);
		StringBuffer myStringBuffer = new StringBuffer(customerNumberInteger.toString());
		for(int z = myStringBuffer.length(); z < 10;z++)
		{
			myStringBuffer = myStringBuffer.insert(0, "0");	
		}

		RecordHolder holder = new RecordHolder();
		KeyHolder keyHolder = new KeyHolder();
		byte[] key = new byte[16];
		for(int z = 0,y=0; z < 10; z++,y++)
		{
			key[z] = (byte) myStringBuffer.toString().charAt(y);
		}
		for(int z = 10,y=0; z < 16; z++,y++)
		{
			key[z] = (byte) sortCodeInteger.toString().charAt(y);
		}

		// We need to convert the key to EBCDIC
		String keyString = new String(key);
		try {
			key = keyString.getBytes(CODEPAGE);
		} catch (UnsupportedEncodingException e2) {
			logger.severe(e2.getLocalizedMessage());
			logger.exiting(this.getClass().getName(),GET_CUSTOMERS,null);
			return null;
		}

		KeyedFileBrowse customerFileBrowse = null;
		try {
			customerFileBrowse = customerFile.startBrowse(key);
		} catch (InvalidSystemIdException | LogicException | InvalidRequestException | IOErrorException
				| LockedException | RecordBusyException | LoadingException | ChangedException | FileDisabledException
				| FileNotFoundException | ISCInvalidRequestException | NotAuthorisedException | RecordNotFoundException
				| NotOpenException e1) {
			logger.severe(ERROR_START_BROWSE + e1.getLocalizedMessage());
			logger.exiting(this.getClass().getName(),GET_CUSTOMERS,null);
			return null;
		}
		i = 0;
		boolean carryOn = true;
		for(int j=0;j<250000 && carryOn;j++)
		{
			try 
			{
				customerFileBrowse.next(holder, keyHolder);
			}
			catch(DuplicateKeyException e)
			{
				// we don't care about this one
			}
			catch(EndOfFileException e)
			{
				// This one we do care about but we expect it
				carryOn = false;

			}
			catch (InvalidSystemIdException | LogicException | InvalidRequestException
					| IOErrorException 
					| ChangedException | LockedException | LoadingException
					| RecordBusyException | FileDisabledException
					| FileNotFoundException
					| ISCInvalidRequestException | NotAuthorisedException
					| RecordNotFoundException | NotOpenException | LengthErrorException e) {
				logger.severe(ERROR_BROWSE + e.getLocalizedMessage());
				logger.exiting(this.getClass().getName(),GET_CUSTOMERS,null);
				return null;
			}

			myCustomer = new CUSTOMER(holder.getValue());

			temp[j] = new Customer();
			temp[j].setAddress(myCustomer.getCustomerAddress());
			temp[j].setCustomer_number(new Long(myCustomer.getCustomerNumber()).toString());
			temp[j].setName(myCustomer.getCustomerName());
			temp[j].setSortcode(new Integer(myCustomer.getCustomerSortcode()).toString());
			Calendar myCalendar = Calendar.getInstance();
			myCalendar.set(Calendar.YEAR, myCustomer.getCustomerBirthYear()) ;
			myCalendar.set(Calendar.MONTH, myCustomer.getCustomerBirthMonth());
			myCalendar.set(Calendar.DAY_OF_MONTH, myCustomer.getCustomerBirthDay());
			Date myCustomerBirthDate = new Date(myCalendar.toInstant().toEpochMilli());
			temp[j].setDob(myCustomerBirthDate);
			temp[j].setCreditScore(Integer.toString(myCustomer.getCustomerCreditScore()));
			myCalendar.set(Calendar.YEAR, myCustomer.getCustomerCsReviewYear() - 1900);
			myCalendar.set(Calendar.MONTH, myCustomer.getCustomerCsReviewMonth() - 1);
			myCalendar.set(Calendar.DAY_OF_MONTH, myCustomer.getCustomerCsReviewDay());
			Date myCustomerCsReviewDate = new Date(myCalendar.toInstant().toEpochMilli());
			temp[j].setReviewDate(myCustomerCsReviewDate);
			if(new Integer(temp[j].getSortcode()).intValue() == sortCode)
			{
				i++;
			}
		}
		try {
			customerFileBrowse.end();
		} catch (InvalidSystemIdException | LogicException | InvalidRequestException | FileDisabledException
				| FileNotFoundException | ISCInvalidRequestException | NotAuthorisedException
				| NotOpenException e) {
			logger.severe(ERROR_END_BROWSE  + e.getLocalizedMessage());
			logger.exiting(this.getClass().getName(),GET_CUSTOMERS,null);
			return null;
		}

		Customer[] real = new Customer[i];
		System.arraycopy(temp, 0, real, 0, temp.length);
		logger.exiting(this.getClass().getName(),GET_CUSTOMERS,real);
		return real;
	}




	public Customer updateCustomer(CustomerJSON customer) 
	{
		logger.entering(this.getClass().getName(),UPDATE_CUSTOMER,null);

		customerFile = new KSDS();
		customerFile.setName(FILENAME);
		Customer temp;
		RecordHolder holder = new RecordHolder();
		byte[] key = new byte[16];
		Long customerNumberLong = new Long(customer.getId());
		Integer sortCodeInteger = new Integer(customer.getSortCode());
		StringBuffer myStringBuffer = new StringBuffer(sortCodeInteger.toString());
		for(int z = myStringBuffer.length(); z < 6;z++)
		{
			myStringBuffer = myStringBuffer.insert(0, "0");	
		}

		for(int i = 0; i < 6; i++)
		{
			key[i] = (byte) myStringBuffer.toString().charAt(i);
		}

		myStringBuffer = new StringBuffer(customerNumberLong.toString());
		for(int z = myStringBuffer.length(); z < 10;z++)
		{
			myStringBuffer = myStringBuffer.insert(0, "0");	
		}
		customer.setId(myStringBuffer.toString());

		for(int i = 6, j = 0; i < 16; i++,j++)
		{
			key[i] = (byte) myStringBuffer.toString().charAt(j);
		}

		String keyString = new String(key);
		try 
		{
			key = keyString.getBytes(CODEPAGE);
		}
		catch (UnsupportedEncodingException e2) 
		{
			logger.severe(e2.getLocalizedMessage());
			logger.exiting(this.getClass().getName(),UPDATE_CUSTOMER,null);
			return null;
		}
		try 
		{
			customerFile.readForUpdate(key, holder);
			myCustomer = new CUSTOMER(holder.getValue());
			myCustomer.setCustomerAddress(customer.getCustomerAddress());
			myCustomer.setCustomerName(customer.getCustomerName());
			customerFile.rewrite(myCustomer.getByteBuffer());
			myCustomer = new CUSTOMER(holder.getValue());
		} 
		catch (InvalidSystemIdException | LogicException | InvalidRequestException | IOErrorException
				| ChangedException | LockedException | LoadingException | RecordBusyException
				| FileDisabledException | DuplicateKeyException | FileNotFoundException | ISCInvalidRequestException
				| NotAuthorisedException | NotOpenException | LengthErrorException | DuplicateRecordException | NoSpaceException e) 
		{
			logger.severe("Error updating customer " + customerNumberLong + " " + e.getLocalizedMessage());
			logger.exiting(this.getClass().getName(),UPDATE_CUSTOMER,null);
			return null;
		}
		catch (RecordNotFoundException e2)
		{
			Customer customer404 = new Customer();
			customer404.setNot_found(true);
			logger.exiting(this.getClass().getName(),UPDATE_CUSTOMER,customer404);
			return customer404;
		}

		Calendar myCalendar = Calendar.getInstance();
		myCalendar.set(Calendar.YEAR, myCustomer.getCustomerBirthYear());
		myCalendar.set(Calendar.MONTH, myCustomer.getCustomerBirthMonth());
		myCalendar.set(Calendar.DAY_OF_MONTH, myCustomer.getCustomerBirthDay());
		Date myCustomerBirthDate = new Date(myCalendar.toInstant().toEpochMilli());
		myCalendar.set(Calendar.YEAR, myCustomer.getCustomerCsReviewYear());
		myCalendar.set(Calendar.MONTH, myCustomer.getCustomerCsReviewMonth());
		myCalendar.set(Calendar.DAY_OF_MONTH, myCustomer.getCustomerCsReviewDay());
		Date myCustomerReviewDate = new Date(myCalendar.toInstant().toEpochMilli());
		myCustomer.getCustomerNumber();
		String myCustomerNumber =(new Long(myCustomer.getCustomerNumber())).toString();
		for(int i=10;myCustomerNumber.length()<10||i>0;i--)
		{
			myCustomerNumber = "0" + myCustomerNumber;
		}

		temp = new Customer(myCustomerNumber, 
				new String(new Integer(myCustomer.getCustomerSortcode()).toString()), 
				new String(myCustomer.getCustomerName()),
				new String(myCustomer.getCustomerAddress()), 
				myCustomerBirthDate, 
				Integer.toString(myCustomer.getCustomerCreditScore()),
				myCustomerReviewDate);
		logger.exiting(this.getClass().getName(),UPDATE_CUSTOMER,temp);
		return temp;
	}


	private void setNot_found(boolean b) {
		this.notFound = b;

	}

	public boolean isNot_found() {
		return this.notFound;

	}

	public Customer deleteCustomer(long customerNumber, int sortCode) 
	{

		logger.entering(this.getClass().getName(),DELETE_CUSTOMER);


		Customer temp = null;

		customerFile = new KSDS();
		customerFile.setName(FILENAME);

		myCustomer = new CUSTOMER();

		Integer sortCodeInteger = new Integer(sortCode);
		Long customerNumberLong = new Long(customerNumber);


		RecordHolder holder = null;
		byte[] key = new byte[16];
		String keyString = null;

		if(customerNumber == 9999999999L)
		{
			holder = getLastCustomer();
			if(holder == null) return null;
			myCustomer = new CUSTOMER(holder.getValue());
			StringBuilder myStringBuilder = new StringBuilder();

			for (int i=sortCodeInteger.toString().length();i<6;i++)
			{
				myStringBuilder.append('0');
			}

			myStringBuilder.append(sortCodeInteger);
			for(int i = 0; i < 6; i++)
			{
				key[i] = (byte) myStringBuilder.toString().charAt(i);
			}

			myStringBuilder = new StringBuilder();
			for(int z = Long.toString(myCustomer.getCustomerNumber()).length(); z < 10;z++)
			{
				myStringBuilder = myStringBuilder.insert(0, "0");	
			}
			myStringBuilder.append(customerNumberLong.toString());

			for(int i = 6, j = 0; i < 16; i++,j++)
			{
				key[i] = (byte) myStringBuilder.toString().charAt(j);
			}
		}

		if(customerNumber > 0 && customerNumber < 9999999999L)
		{
			holder = new RecordHolder();
			StringBuilder myStringBuilder = new StringBuilder();

			for (int i=sortCodeInteger.toString().length();i<6;i++)
			{
				myStringBuilder.append('0');
			}

			myStringBuilder.append(sortCodeInteger);
			for(int i = 0; i < 6; i++)
			{
				key[i] = (byte) myStringBuilder.toString().charAt(i);
			}

			myStringBuilder = new StringBuilder();
			for(int z = customerNumberLong.toString().length(); z < 10;z++)
			{
				myStringBuilder = myStringBuilder.insert(0, "0");	
			}
			myStringBuilder.append(customerNumberLong.toString());

			for(int i = 6, j = 0; i < 16; i++,j++)
			{
				key[i] = (byte) myStringBuilder.toString().charAt(j);
			}

			keyString = new String(key);
			try {
				key = keyString.getBytes(CODEPAGE);
			} catch (UnsupportedEncodingException e2) {
				logger.severe(e2.getLocalizedMessage());
				logger.exiting(this.getClass().getName(),DELETE_CUSTOMER,null);
				return null;
			}
		}
		try 
		{
			customerFile.readForUpdate(key, holder);
			customerFile.delete();
		}
		catch (InvalidSystemIdException | LogicException | InvalidRequestException | IOErrorException 
				| ChangedException | LockedException | LoadingException | RecordBusyException
				| FileDisabledException | DuplicateKeyException | FileNotFoundException | ISCInvalidRequestException
				| NotAuthorisedException | NotOpenException e) {
			logger.severe(ERROR_DELETE1 + customerNumber + " in CUSTOMER file," + e.getLocalizedMessage());
			logger.exiting(this.getClass().getName(),DELETE_CUSTOMER,null);
			return null;
		}
		catch(RecordNotFoundException e)
		{
			Customer customer404 = new Customer();
			customer404.setNot_found(true);
			logger.exiting(this.getClass().getName(),DELETE_CUSTOMER,customer404);
			return customer404;
		}

		decrementNumberOfCustomers();

		Calendar myCalendar = Calendar.getInstance();
		myCalendar.set(Calendar.YEAR, myCustomer.getCustomerBirthYear());
		myCalendar.set(Calendar.MONTH, myCustomer.getCustomerBirthMonth());
		myCalendar.set(Calendar.DAY_OF_MONTH, myCustomer.getCustomerBirthDay());
		Date myCustomerBirthDate = new Date(myCalendar.toInstant().toEpochMilli());
		myCalendar.set(Calendar.YEAR, myCustomer.getCustomerCsReviewYear());
		myCalendar.set(Calendar.MONTH, myCustomer.getCustomerCsReviewMonth());
		myCalendar.set(Calendar.DAY_OF_MONTH, myCustomer.getCustomerCsReviewDay());
		Date myCustomerReviewDate = new Date(myCalendar.toInstant().toEpochMilli());
		temp = new Customer(Long.toString(myCustomer.getCustomerNumber()), 
				Integer.toString(myCustomer.getCustomerSortcode()), 
				myCustomer.getCustomerName(),
				myCustomer.getCustomerAddress(), 
				myCustomerBirthDate, 
				Integer.toString(myCustomer.getCustomerCreditScore()),
				myCustomerReviewDate);

		logger.exiting(this.getClass().getName(),DELETE_CUSTOMER,temp);
		return temp;

	}

	private boolean decrementNumberOfCustomers() {
		CustomerControl myCustomerControl = new CustomerControl();
		KSDS customerKSDS = new KSDS();
		customerKSDS.setName(FILENAME);

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
			logger.exiting(this.getClass().getName(),DELETE_CUSTOMER,null);
			return false;
		}

		RecordHolder holder = new RecordHolder();

		try {
			customerKSDS.readForUpdate(key, holder);
		} catch (LogicException | InvalidRequestException | IOErrorException | InvalidSystemIdException
				| LockedException | ChangedException | LoadingException | RecordBusyException
				| FileDisabledException | DuplicateKeyException | FileNotFoundException | ISCInvalidRequestException
				| NotAuthorisedException | RecordNotFoundException | NotOpenException e) {
			logger.severe(e.getLocalizedMessage());
			logger.exiting(this.getClass().getName(),DELETE_CUSTOMER,null);
			return false;
		}
		myCustomerControl = new CustomerControl(holder.getValue());

		long numberOfCustomers = myCustomerControl.getNumberOfCustomers();
		numberOfCustomers--;

		myCustomerControl.setNumberOfCustomers(numberOfCustomers);
		try {
			customerKSDS.rewrite(myCustomerControl.getByteBuffer());
		} catch (LogicException | InvalidRequestException | IOErrorException | LengthErrorException
				| InvalidSystemIdException | ChangedException | LockedException | LoadingException
				| RecordBusyException | FileDisabledException | DuplicateRecordException | FileNotFoundException
				| ISCInvalidRequestException | NoSpaceException | NotAuthorisedException | NotOpenException e) {
			logger.severe(e.getLocalizedMessage());
			logger.exiting(this.getClass().getName(),DELETE_CUSTOMER,null);
			return false;
		}
		return true;

	}

	public Customer createCustomer(CustomerJSON customer, Integer sortCodeInteger) 
	{
		logger.entering(this.getClass().getName(),CREATE_CUSTOMER);

		Customer temp = null;

		customerFile = new KSDS();
		customerFile.setName(FILENAME);
		CounterResource myCounterResource = null;
		myCustomer = new CUSTOMER();
		long customerNumber = 1234567890L;
		String sortCodeString = sortCodeInteger.toString();



		customerNumber = getNextCustomerNumber(sortCodeString);

		Long customerNumberLong = new Long(customerNumber);
		if(customerNumberLong == -1)
		{
			return null;
		}

		byte[] key = new byte[16];
		StringBuffer myStringBuffer = new StringBuffer(sortCodeInteger.toString());
		for(int z = myStringBuffer.length(); z < 6;z++)
		{
			myStringBuffer = myStringBuffer.insert(0, "0");	
		}

		for(int i = 0; i < 6; i++)
		{
			key[i] = (byte) myStringBuffer.toString().charAt(i);
		}

		myStringBuffer = new StringBuffer(customerNumberLong.toString());
		for(int z = myStringBuffer.length(); z < 10;z++)
		{
			myStringBuffer = myStringBuffer.insert(0, "0");	
		}

		for(int i = 6, j = 0; i < 16; i++,j++)
		{
			key[i] = (byte) myStringBuffer.toString().charAt(j);
		}

		String keyString = new String(key);
		try 
		{
			key = keyString.getBytes(CODEPAGE);
		}
		catch (UnsupportedEncodingException e2) 
		{
			logger.severe(e2.getLocalizedMessage());
			logger.exiting(this.getClass().getName(),CREATE_CUSTOMER,null);
			return null;
		}
		myCustomer = new CUSTOMER();
		myCustomer.setCustomerEyecatcher(CUSTOMER.CUSTOMER_EYECATCHER_VALUE);
		myCustomer.setCustomerAddress(customer.getCustomerAddress().trim());
		// What about title validation?
		myCustomer.setCustomerName(customer.getCustomerName().trim());

		Calendar myCalendar = Calendar.getInstance();
		myCalendar.setTime(customer.getDateOfBirth());

		myCustomer.setCustomerBirthDay(myCalendar.get(Calendar.DAY_OF_MONTH));
		myCustomer.setCustomerBirthMonth(myCalendar.get(Calendar.MONTH)+1);
		myCustomer.setCustomerBirthYear(myCalendar.get(Calendar.YEAR));

		myCustomer.setCustomerSortcode(sortCodeInteger);
		myCustomer.setCustomerNumber(customerNumber);

		customer.setId(customerNumberLong.toString());


		customer = CreditScore.populateCreditScoreAndReviewDate(customer);


		if(customer != null)
		{
			customer.setCreditScore(customer.getCreditScore());
			customer.setReviewDate(customer.getReviewDate());
			creditScore = customer.getCreditScore();
			reviewDate = customer.getReviewDate();
		}
		else
		{
			logger.severe("Error! populateCreditScoreAndReviewDate returned null");
			logger.exiting(this.getClass().getName(),CREATE_CUSTOMER,null);
			return null;
		}

		myCustomer.setCustomerCreditScore(new Integer(customer.getCreditScore()).intValue());
		Date myCustomerCsReviewDate = customer.getReviewDate();
		myCalendar.setTime(myCustomerCsReviewDate);
		myCustomer.setCustomerCsReviewDay(myCalendar.get(Calendar.DAY_OF_MONTH));
		myCustomer.setCustomerCsReviewMonth(myCalendar.get(Calendar.MONTH) + 1); 
		myCustomer.setCustomerCsReviewYear(myCalendar.get(Calendar.YEAR));

		try 
		{
			customerFile.write(key, myCustomer.getByteBuffer());
			myCustomer = new CUSTOMER(myCustomer.getByteBuffer());
		}
		catch (InvalidSystemIdException | NoSpaceException | LogicException | InvalidRequestException | IOErrorException | LengthErrorException
				| ChangedException | LockedException | LoadingException | RecordBusyException
				| FileDisabledException | FileNotFoundException | ISCInvalidRequestException
				| NotAuthorisedException | NotOpenException e) 
		{
			logger.severe("Error writing record to CUSTOMER file, " + e.getLocalizedMessage());
			logger.exiting(this.getClass().getName(),CREATE_CUSTOMER,null);
			return null;
		} catch (DuplicateRecordException e) {
			logger.severe("DuplicateRecordException duplicate value. Have you combined named counter and non-named counter with the same data? com.ibm.cics.cip.bankliberty.web.vsam.Customer.");
			logger.exiting(this.getClass().getName(),CREATE_CUSTOMER,null);
			resetNextCustomerNumber(sortCodeString);
			return null;
		}


		myCalendar.set(Calendar.YEAR, myCustomer.getCustomerBirthYear());
		myCalendar.set(Calendar.MONTH, myCustomer.getCustomerBirthMonth());
		myCalendar.set(Calendar.DAY_OF_MONTH, myCustomer.getCustomerBirthDay());
		Date myCustomerBirthDate = new Date(myCalendar.toInstant().toEpochMilli());
		myCalendar.set(Calendar.YEAR, myCustomer.getCustomerCsReviewYear());
		myCalendar.set(Calendar.MONTH, myCustomer.getCustomerCsReviewMonth());
		myCalendar.set(Calendar.DAY_OF_MONTH, myCustomer.getCustomerCsReviewDay());
		Date myCustomerReviewDate = new Date(myCalendar.toInstant().toEpochMilli());


		String myCustomerNumber = new Long(myCustomer.getCustomerNumber()).toString();
		// I think this was it
		for(int i=myCustomerNumber.length();i<10;i++)
		{
			myCustomerNumber = "0" + myCustomerNumber;
		}



		temp = new Customer(myCustomerNumber, 
				new String(new Integer(myCustomer.getCustomerSortcode()).toString()), 
				new String(myCustomer.getCustomerName()),
				new String(myCustomer.getCustomerAddress()), 
				myCustomerBirthDate, 
				Integer.toString(myCustomer.getCustomerCreditScore()),
				myCustomerReviewDate);
		logger.exiting(this.getClass().getName(),CREATE_CUSTOMER,temp);
		return temp;

	}

	private long getNextCustomerNumber(String sortCodeString) {
		// We need to get a NEW customer number
		// We need to enqueue, then get the last customer number

		NameResource enqueue = new NameResource();

		enqueue.setName("HBNKCUST" + sortCodeString + "  ");
		try {
			enqueue.enqueue();
		} catch (LengthErrorException | ResourceUnavailableException e) {
			logger.severe(e.getLocalizedMessage());
			logger.exiting(this.getClass().getName(),CREATE_CUSTOMER,null);
			return -1;
		} 
		CustomerControl myCustomerControl = new CustomerControl();
		KSDS customerKSDS = new KSDS();
		customerKSDS.setName(FILENAME);

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
			logger.exiting(this.getClass().getName(),CREATE_CUSTOMER,null);
			return -1;
		}

		RecordHolder holder = new RecordHolder();

		try {
			customerKSDS.readForUpdate(key, holder);
		} catch (LogicException | InvalidRequestException | IOErrorException | InvalidSystemIdException
				| LockedException | ChangedException | LoadingException | RecordBusyException
				| FileDisabledException | DuplicateKeyException | FileNotFoundException | ISCInvalidRequestException
				| NotAuthorisedException | RecordNotFoundException | NotOpenException e) {
			logger.severe(e.getLocalizedMessage());
			logger.exiting(this.getClass().getName(),CREATE_CUSTOMER,null);
			return -1;
		}

		myCustomerControl = new CustomerControl(holder.getValue());


		long lastCustomerNumber = myCustomerControl.getLastCustomerNumber();
		lastCustomerNumber++;

		long numberOfCustomers = myCustomerControl.getNumberOfCustomers();
		numberOfCustomers++;

		myCustomerControl.setLastCustomerNumber(lastCustomerNumber);
		myCustomerControl.setNumberOfCustomers(numberOfCustomers);
		return lastCustomerNumber;
	}

	private long resetNextCustomerNumber(String sortCodeString) {
		// We need to get a NEW customer number
		// We need to enqueue, then get the last customer number

		NameResource enqueue = new NameResource();

		enqueue.setName("HBNKCUST" + sortCodeString + "  ");
		try {
			enqueue.enqueue();
		} catch (LengthErrorException | ResourceUnavailableException e) {
			logger.severe(e.getLocalizedMessage());
			logger.exiting(this.getClass().getName(),CREATE_CUSTOMER,null);
			return -1;
		} 
		CustomerControl myCustomerControl = new CustomerControl();
		KSDS customerKSDS = new KSDS();
		customerKSDS.setName(FILENAME);

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
			logger.exiting(this.getClass().getName(),CREATE_CUSTOMER,null);
			return -1;
		}

		RecordHolder holder = new RecordHolder();

		try {
			customerKSDS.readForUpdate(key, holder);
		} catch (LogicException | InvalidRequestException | IOErrorException | InvalidSystemIdException
				| LockedException | ChangedException | LoadingException | RecordBusyException
				| FileDisabledException | DuplicateKeyException | FileNotFoundException | ISCInvalidRequestException
				| NotAuthorisedException | RecordNotFoundException | NotOpenException e) {
			logger.severe(e.getLocalizedMessage());
			logger.exiting(this.getClass().getName(),CREATE_CUSTOMER,null);
			return -1;
		}

		myCustomerControl = new CustomerControl(holder.getValue());


		long lastCustomerNumber = myCustomerControl.getLastCustomerNumber();
		lastCustomerNumber--;

		long numberOfCustomers = myCustomerControl.getNumberOfCustomers();
		numberOfCustomers--;

		myCustomerControl.setLastCustomerNumber(lastCustomerNumber);
		myCustomerControl.setNumberOfCustomers(numberOfCustomers);
		return lastCustomerNumber;
	}

	public Customer[] getCustomers(int sortCode, int limit, int offset) 
	{
		logger.entering(this.getClass().getName(),GET_CUSTOMERS_WITH_OFFSET_AND_LIMIT);
		Customer[] temp = new Customer[limit];
		int stored = 0, retrieved = 0;


		KSDS customerFile = new KSDS();
		customerFile.setName(FILENAME);

		myCustomer = new CUSTOMER();

		Integer sortCodeInteger = new Integer(sortCode);
		Integer customerNumberInteger = new Integer(0);
		StringBuffer myStringBuffer = new StringBuffer(customerNumberInteger.toString());
		for(int z = myStringBuffer.length(); z < 10;z++)
		{
			myStringBuffer = myStringBuffer.insert(0, "0");	
		}

		StringBuffer mySortCodeBuffer = new StringBuffer(sortCodeInteger.toString());
		for(int z = mySortCodeBuffer.length(); z < 6;z++)
		{
			mySortCodeBuffer = mySortCodeBuffer.insert(0, "0");	
		}

		RecordHolder holder = new RecordHolder();
		KeyHolder keyHolder = new KeyHolder();
		byte[] key = new byte[16];
		for(int z = 0,y=0; z < 6; z++,y++)
		{
			key[z] = (byte) mySortCodeBuffer.charAt(y);
		}
		for(int z = 6,y=0; z < 16; z++,y++)
		{
			key[z] = (byte) myStringBuffer.toString().charAt(y);
		}


		// We need to convert the key to EBCDIC
		String keyString = new String(key);
		try {
			key = keyString.getBytes(CODEPAGE);
		} catch (UnsupportedEncodingException e2) {
			logger.severe(e2.getLocalizedMessage());
			logger.exiting(this.getClass().getName(),GET_CUSTOMERS_WITH_OFFSET_AND_LIMIT,null);
			return null;
		}

		KeyedFileBrowse customerFileBrowse = null;
		try {
			customerFileBrowse = customerFile.startBrowse(key);
			boolean carryOn = true;


			for(retrieved = 0;carryOn && stored < limit;retrieved++)
			{
				try 
				{
					customerFileBrowse.next(holder, keyHolder);
				}
				catch(DuplicateKeyException e)
				{
					// we don't care about this one
				}
				catch(EndOfFileException e)
				{
					// This one we do care about but we expect it
					carryOn = false;
				}
				myCustomer = new CUSTOMER(holder.getValue());
				if(retrieved >= offset && (myCustomer.getCustomerSortcode() == sortCode))
				{	
					temp[stored] = new Customer();
					temp[stored].setAddress(myCustomer.getCustomerAddress());
					temp[stored].setCustomer_number(new Long(myCustomer.getCustomerNumber()).toString());
					temp[stored].setName(myCustomer.getCustomerName());
					temp[stored].setSortcode(new Integer(myCustomer.getCustomerSortcode()).toString());
					Calendar dobCalendar = Calendar.getInstance();
					dobCalendar.set(Calendar.YEAR,myCustomer.getCustomerBirthYear());
					dobCalendar.set(Calendar.MONTH,myCustomer.getCustomerBirthMonth());
					dobCalendar.set(Calendar.DAY_OF_MONTH,myCustomer.getCustomerBirthDay());
					Date dobDate = new Date(dobCalendar.getTimeInMillis());
					temp[stored].setDob(dobDate);
					temp[stored].setCreditScore(new Integer(myCustomer.getCustomerCreditScore()).toString());
					Calendar reviewCalendar = Calendar.getInstance();
					reviewCalendar.set(Calendar.YEAR,myCustomer.getCustomerCsReviewYear());
					reviewCalendar.set(Calendar.MONTH,myCustomer.getCustomerCsReviewMonth());
					reviewCalendar.set(Calendar.DAY_OF_MONTH,myCustomer.getCustomerCsReviewDay());
					Date reviewDate = new Date(reviewCalendar.getTimeInMillis());
					temp[stored].setReviewDate(reviewDate);
					temp[stored].printCustomerDetails();
					stored++;
				}
			}

			customerFileBrowse.end();


			Customer[] real = new Customer[stored];
			System.arraycopy(temp,  0, real,0,stored);
			logger.exiting(this.getClass().getName(),GET_CUSTOMERS_WITH_OFFSET_AND_LIMIT,real);
			return real;
		}
		catch (LengthErrorException | InvalidSystemIdException | LogicException | InvalidRequestException | IOErrorException
				| LockedException | RecordBusyException | LoadingException | ChangedException | FileDisabledException
				| FileNotFoundException | ISCInvalidRequestException | NotAuthorisedException | RecordNotFoundException
				| NotOpenException e1) {
			logger.severe( e1.getLocalizedMessage());
			logger.exiting(this.getClass().getName(),GET_CUSTOMERS_WITH_OFFSET_AND_LIMIT,null);
			return null;
		}


	}

	public Customer[] getCustomersByName(int sortCode, int limit, int offset, String name) {
		logger.entering(this.getClass().getName(),GET_CUSTOMERS_BY_NAME_WITH_OFFSET_AND_LIMIT);
		Customer[] temp = new Customer[1000000];


		int stored = 0;

		KSDS customerFile = new KSDS();
		customerFile.setName(FILENAME);

		myCustomer = new CUSTOMER();

		Integer sortCodeInteger = new Integer(sortCode);
		Integer customerNumberInteger = new Integer(0);
		StringBuffer myStringBuffer = new StringBuffer(customerNumberInteger.toString());
		for(int z = myStringBuffer.length(); z < 10;z++)
		{
			myStringBuffer = myStringBuffer.insert(0, "0");	
		}

		StringBuffer mySortCodeBuffer = new StringBuffer(sortCodeInteger.toString());
		for(int z = mySortCodeBuffer.length(); z < 6;z++)
		{
			mySortCodeBuffer = mySortCodeBuffer.insert(0, "0");	
		}

		RecordHolder holder = new RecordHolder();
		KeyHolder keyHolder = new KeyHolder();
		byte[] key = new byte[16];
		for(int z = 0,y=0; z < 6; z++,y++)
		{
			key[z] = (byte) mySortCodeBuffer.charAt(y);
		}
		for(int z = 6,y=0; z < 16; z++,y++)
		{
			key[z] = (byte) myStringBuffer.toString().charAt(y);
		}


		// We need to convert the key to EBCDIC
		String keyString = new String(key);
		try {
			key = keyString.getBytes(CODEPAGE);
		} catch (UnsupportedEncodingException e2) {
			logger.severe(e2.getLocalizedMessage());
			logger.exiting(this.getClass().getName(),GET_CUSTOMERS_BY_NAME_WITH_OFFSET_AND_LIMIT,null);
			return null;
		}

		KeyedFileBrowse customerFileBrowse = null;
		try {

			customerFileBrowse = customerFile.startBrowse(key);
			boolean carryOn = true, endOfFile = false;
			while(carryOn)
			{
				try 
				{
					customerFileBrowse.next(holder, keyHolder);
				}
				catch(DuplicateKeyException e)
				{
					// we don't care about this one
				}
				catch(EndOfFileException e)
				{
					// This one we do care about but we expect it
					carryOn = false;
					endOfFile = true;
				}
				myCustomer = new CUSTOMER(holder.getValue());
				// We get here because we either successfully read a record, or hit end of file. if we hit end of file then we might add the last customer twice!
				if(!endOfFile)
				{
					//				if(retrieved >= offset && (myCustomer.getCustomerSortcode() == sortCode) && myCustomer.getCustomerName().contains(name))
					if((myCustomer.getCustomerSortcode() == sortCode) && myCustomer.getCustomerName().contains(name))					
					{
						temp[stored] = new Customer();
						temp[stored].setAddress(myCustomer.getCustomerAddress());
						temp[stored].setCustomer_number(new Long(myCustomer.getCustomerNumber()).toString());
						temp[stored].setName(myCustomer.getCustomerName());
						temp[stored].setSortcode(new Integer(myCustomer.getCustomerSortcode()).toString());
						Calendar dobCalendar = Calendar.getInstance();
						dobCalendar.set(Calendar.YEAR, myCustomer.getCustomerBirthYear());
						dobCalendar.set(Calendar.MONTH, myCustomer.getCustomerBirthMonth());
						dobCalendar.set(Calendar.DAY_OF_MONTH, myCustomer.getCustomerBirthDay());
						Date dob = new Date(dobCalendar.getTimeInMillis());
						temp[stored].setDob(dob);
						temp[stored].setCreditScore(Integer.toString(myCustomer.getCustomerCreditScore()));
						Calendar csReviewCalendar = Calendar.getInstance();
						csReviewCalendar.set(Calendar.YEAR, myCustomer.getCustomerCsReviewYear());
						csReviewCalendar.set(Calendar.MONTH, myCustomer.getCustomerCsReviewMonth());
						csReviewCalendar.set(Calendar.DAY_OF_MONTH, myCustomer.getCustomerCsReviewDay());
						Date csReviewDate = new Date(csReviewCalendar.getTimeInMillis());
						temp[stored].setReviewDate(csReviewDate);
						stored++;
					}
				}
			}
			customerFileBrowse.end();

			Customer[] real = new Customer[limit];

			System.arraycopy(temp, 0, real, 0,limit);

			logger.exiting(this.getClass().getName(),GET_CUSTOMERS_BY_NAME_WITH_OFFSET_AND_LIMIT,real);
			return real;
		}
		catch (LengthErrorException | RecordNotFoundException | LockedException | ChangedException | IOErrorException | LoadingException | RecordBusyException | InvalidSystemIdException | LogicException | InvalidRequestException | FileDisabledException
				| FileNotFoundException | ISCInvalidRequestException | NotAuthorisedException
				| NotOpenException e) 
		{
			logger.severe(e.getLocalizedMessage());
			logger.exiting(this.getClass().getName(),GET_CUSTOMERS_BY_NAME_WITH_OFFSET_AND_LIMIT,null);
			return null;
		}
	}

	public Customer[] getCustomersByName(int sortCode, String name) 
	{
		logger.entering(this.getClass().getName(),GET_CUSTOMERS_BY_NAME);
		Customer[] temp = new Customer[250000];
		int stored = 0, retrieved = 0;

		KSDS customerFile = new KSDS();
		customerFile.setName(FILENAME);

		myCustomer = new CUSTOMER();

		Integer sortCodeInteger = new Integer(sortCode);
		Integer customerNumberInteger = new Integer(0);
		StringBuffer myStringBuffer = new StringBuffer(customerNumberInteger.toString());
		for(int z = myStringBuffer.length(); z < 10;z++)
		{
			myStringBuffer = myStringBuffer.insert(0, "0");	
		}

		StringBuffer mySortCodeBuffer = new StringBuffer(sortCodeInteger.toString());
		for(int z = mySortCodeBuffer.length(); z < 6;z++)
		{
			mySortCodeBuffer = mySortCodeBuffer.insert(0, "0");	
		}

		RecordHolder holder = new RecordHolder();
		KeyHolder keyHolder = new KeyHolder();
		byte[] key = new byte[16];
		for(int z = 0,y=0; z < 6; z++,y++)
		{
			key[z] = (byte) mySortCodeBuffer.charAt(y);
		}
		for(int z = 6,y=0; z < 16; z++,y++)
		{
			key[z] = (byte) myStringBuffer.toString().charAt(y);
		}



		// We need to convert the key to EBCDIC
		String keyString = new String(key);
		try {
			key = keyString.getBytes(CODEPAGE);
		} catch (UnsupportedEncodingException e2) {
			logger.severe(e2.getLocalizedMessage());
			logger.exiting(this.getClass().getName(),GET_CUSTOMERS_BY_NAME,null);
			return null;
		}

		KeyedFileBrowse customerFileBrowse = null;
		try {

			customerFileBrowse = customerFile.startBrowse(key);

			boolean carryOn = true, endOfFile = false;
			for(retrieved = 0;retrieved<250000 && carryOn;retrieved++)
			{
				try 
				{
					customerFileBrowse.next(holder, keyHolder);
				}
				catch(DuplicateKeyException e)
				{
					// we don't care about this one
				}
				catch(EndOfFileException e)
				{
					// This one we do care about but we expect it
					carryOn = false;
					endOfFile = true;
				} 
				// We get here because we either successfully read a record, or hit end of file. if we hit end of file then we might add the last customer twice!
				if(!endOfFile)
				{
					myCustomer = new CUSTOMER(holder.getValue());
					if((myCustomer.getCustomerSortcode() == sortCode) && myCustomer.getCustomerName().contains(name))
					{
						temp[stored] = new Customer();
						temp[stored].setAddress(myCustomer.getCustomerAddress());
						temp[stored].setCustomer_number(new Long(myCustomer.getCustomerNumber()).toString());
						temp[stored].setName(myCustomer.getCustomerName());
						temp[stored].setSortcode(new Integer(myCustomer.getCustomerSortcode()).toString());
						Calendar dobCalendar = Calendar.getInstance();
						dobCalendar.set(Calendar.YEAR, myCustomer.getCustomerBirthYear());
						dobCalendar.set(Calendar.MONTH, myCustomer.getCustomerBirthMonth());
						dobCalendar.set(Calendar.DAY_OF_MONTH, myCustomer.getCustomerBirthDay());
						Date dob = new Date(dobCalendar.getTimeInMillis());
						temp[stored].setDob(dob);
						temp[stored].setCreditScore(Integer.toString(myCustomer.getCustomerCreditScore()));
						Calendar csReviewCalendar = Calendar.getInstance();
						csReviewCalendar.set(Calendar.YEAR, myCustomer.getCustomerCsReviewYear());
						csReviewCalendar.set(Calendar.MONTH, myCustomer.getCustomerCsReviewMonth());
						csReviewCalendar.set(Calendar.DAY_OF_MONTH, myCustomer.getCustomerCsReviewDay());
						Date csReviewDate = new Date(csReviewCalendar.getTimeInMillis());
						temp[stored].setReviewDate(csReviewDate);
						stored++;
					}
				}
			}
			customerFileBrowse.end();


			Customer[] real = new Customer[stored];
			System.arraycopy(temp, 0, real, 0, stored);
			logger.exiting(this.getClass().getName(),GET_CUSTOMERS_BY_NAME,real);
			return real;
		}
		catch (LengthErrorException | InvalidSystemIdException | LogicException | InvalidRequestException | IOErrorException
				| LockedException | RecordBusyException | LoadingException | ChangedException | FileDisabledException
				| FileNotFoundException | ISCInvalidRequestException | NotAuthorisedException | RecordNotFoundException
				| NotOpenException e1) {
			logger.severe(e1.getLocalizedMessage());
			logger.exiting(this.getClass().getName(),GET_CUSTOMERS_BY_NAME,null);
			return null;
		}
	}

	public long getCustomersCountOnly(int sortCode)
	{
		logger.entering(this.getClass().getName(),GET_CUSTOMERS_COUNT_ONLY);

		KSDS customerFile = new KSDS();
		customerFile.setName(FILENAME);

		RecordHolder holder = new RecordHolder();

		byte[] key = new byte[16];
		// We need to convert the key to EBCDIC
		String keyString = new String(LAST_CUSTOMER);
		try {
			key = keyString.getBytes(CODEPAGE);
		} catch (UnsupportedEncodingException e2) {
			logger.severe(e2.getLocalizedMessage());
			logger.exiting(this.getClass().getName(),GET_CUSTOMERS_COUNT_ONLY,-1L);
			return -1L;
		}

		try {
			customerFile.read(key, holder);
		} catch (LogicException | InvalidRequestException | IOErrorException
				| LockedException | RecordBusyException | LoadingException | ChangedException | FileDisabledException
				| FileNotFoundException | ISCInvalidRequestException | NotAuthorisedException | RecordNotFoundException
				| DuplicateKeyException | NotOpenException e1) {
			logger.severe("Error reading control record for customer file " + e1.getLocalizedMessage());
			logger.exiting(this.getClass().getName(),GET_CUSTOMERS_COUNT_ONLY,-1L);
			return -1L;
		}
		catch (InvalidSystemIdException  e1) 
		{
			if(keepTryingUntilWeSucceedOrGiveUp(key,holder) == -1)
				return -1;
		}

		CustomerControl myCustomerControl = new CustomerControl(holder.getValue());
		logger.exiting(this.getClass().getName(),GET_CUSTOMERS_COUNT_ONLY,myCustomerControl.getNumberOfCustomers());
		return myCustomerControl.getNumberOfCustomers();

	}

	private long keepTryingUntilWeSucceedOrGiveUp(byte[] key, RecordHolder holder) {
		int numberOfRetries = 0;
		boolean success= false;
		for(; numberOfRetries < maximumRetries && success == false;numberOfRetries++)
		{
			try {
				logger.log(Level.FINE, () -> ABOUT_TO_GO_TO_SLEEP + totalSleep + MILLISECONDS);
				Thread.sleep(3000);
			}
			catch (InterruptedException e) 
			{
				logger.warning(e.toString());
				Thread.currentThread().interrupt();
			}
			try {
				customerFile.read(key, holder);
				success = true;

			} catch (FileDisabledException | DuplicateKeyException | NotOpenException | LogicException | InvalidRequestException | IOErrorException  | ChangedException | LockedException | LoadingException | RecordBusyException | FileNotFoundException | ISCInvalidRequestException | NotAuthorisedException | RecordNotFoundException
					e3) {
				logger.log(Level.SEVERE,() -> "Error reading control record for customer file file, " + e3.getLocalizedMessage());
				logger.exiting(this.getClass().getName(),GET_CUSTOMERS_COUNT_ONLY,-1L);
				return -1L;
			}
			catch (InvalidSystemIdException e4)
			{
				// This is expected. Log as a warning.
				logger.log(Level.WARNING,() -> "Error reading control record for customer file file, " + e4.getLocalizedMessage());
			} 

		}
		if(numberOfRetries == maximumRetries && success == false)
		{
			logger.severe("Cannot read control record for CUSTOMER file after 100 attempts");
			logger.exiting(this.getClass().getName(),GET_CUSTOMERS_COUNT_ONLY,-1L);
			return -1L;
		}
		return 0;
	}

	public long getCustomersByNameCountOnly(int sortCode, String name) {
		logger.entering(this.getClass().getName(),GET_CUSTOMERS_BY_NAME_COUNT_ONLY);

		long matchingCustomers = 0;



		KSDS customerFile = new KSDS();
		customerFile.setName(FILENAME);

		myCustomer = new CUSTOMER();

		Integer sortCodeInteger = new Integer(sortCode);
		Integer customerNumberInteger = new Integer(0);
		StringBuffer myStringBuffer = new StringBuffer(customerNumberInteger.toString());
		for(int z = myStringBuffer.length(); z < 10;z++)
		{
			myStringBuffer = myStringBuffer.insert(0, "0");	
		}
		StringBuffer mySortCodeBuffer = new StringBuffer(sortCodeInteger.toString());
		for(int z = mySortCodeBuffer.length(); z < 6;z++)
		{
			mySortCodeBuffer = mySortCodeBuffer.insert(0, "0");	
		}

		RecordHolder holder = new RecordHolder();
		KeyHolder keyHolder = new KeyHolder();
		byte[] key = new byte[16];
		for(int z = 0,y=0; z < 6; z++,y++)
		{
			key[z] = (byte) mySortCodeBuffer.charAt(y);
		}
		for(int z = 6,y=0; z < 16; z++,y++)
		{
			key[z] = (byte) myStringBuffer.toString().charAt(y);
		}



		// We need to convert the key to EBCDIC
		String keyString = new String(key);
		try {
			key = keyString.getBytes(CODEPAGE);
		} catch (UnsupportedEncodingException e2) {
			logger.severe(e2.getLocalizedMessage());
			logger.exiting(this.getClass().getName(),GET_CUSTOMERS_BY_NAME_COUNT_ONLY,-1L);
			return -1L;
		}

		KeyedFileBrowse customerFileBrowse = null;
		try {

			customerFileBrowse = customerFile.startBrowse(key);

			boolean carryOn = true, endOfFile = false;
			for(carryOn = true, endOfFile = false;carryOn && !endOfFile;)
			{
				try 
				{
					customerFileBrowse.next(holder, keyHolder);
				}
				catch(DuplicateKeyException e)
				{
					// we don't care about this one
				}
				catch(EndOfFileException e)
				{
					// This one we do care about but we expect it
					carryOn = false;
					endOfFile = true;
				}

				myCustomer = new CUSTOMER(holder.getValue());

				// We get here because we either successfully read a record, or hit end of file. if we hit end of file then we might add the last customer twice!
				if(!endOfFile)
				{
					if(myCustomer.getCustomerName().contains(name))
					{
						matchingCustomers++;
					}
				}
			}
			customerFileBrowse.end();

			logger.exiting(this.getClass().getName(),GET_CUSTOMERS_BY_NAME_COUNT_ONLY,matchingCustomers);
			return matchingCustomers;
		}
		catch (LengthErrorException | FileDisabledException | NotOpenException | LogicException | InvalidRequestException | FileNotFoundException | ISCInvalidRequestException | NotAuthorisedException | IOErrorException | InvalidSystemIdException | LockedException | RecordBusyException | LoadingException | ChangedException | RecordNotFoundException
				e3) {
			logger.log(Level.FINE, () -> e3.getLocalizedMessage());
			logger.exiting(this.getClass().getName(),GET_CUSTOMERS_BY_NAME_COUNT_ONLY,-1L);
			return -1L;
		}

	}

	private void sortOutLogging()
	{
		try {
			LogManager.getLogManager().readConfiguration();
		} catch (SecurityException | IOException e) 
		{
			logger.severe(e.toString());
		}
	}
	public Customer[] getCustomersByTown(String town) {
		logger.entering(this.getClass().getName(),GET_CUSTOMERS_BY_TOWN,null);
		Customer[] temp = new Customer[250000];
		int i = 0;


		KSDS customerFile = new KSDS();
		customerFile.setName(FILENAME);

		myCustomer = new CUSTOMER();

		Integer sortCodeInteger = new Integer(this.getSortcode());
		Integer customerNumberInteger = new Integer(0);
		StringBuffer myStringBuffer = new StringBuffer(customerNumberInteger.toString());
		for(int z = myStringBuffer.length(); z < 10;z++)
		{
			myStringBuffer = myStringBuffer.insert(0, "0");	
		}

		RecordHolder holder = new RecordHolder();
		KeyHolder keyHolder = new KeyHolder();
		byte[] key = new byte[16];
		for(int z = 0,y=0; z < 10; z++,y++)
		{
			key[z] = (byte) myStringBuffer.toString().charAt(y);
		}
		for(int z = 10,y=0; z < 16; z++,y++)
		{
			key[z] = (byte) sortCodeInteger.toString().charAt(y);
		}

		// We need to convert the key to EBCDIC
		String keyString = new String(key);
		try {
			key = keyString.getBytes(CODEPAGE);
		} catch (UnsupportedEncodingException e2) {
			logger.severe(e2.getLocalizedMessage());
			logger.exiting(this.getClass().getName(),GET_CUSTOMERS_BY_TOWN,null);
			return null;
		}

		KeyedFileBrowse customerFileBrowse = null;
		try {
			customerFileBrowse = customerFile.startBrowse(key);

			i = 0;
			boolean carryOn = true;
			for(int j=0;j<250000 && carryOn;j++)
			{
				try 
				{
					customerFileBrowse.next(holder, keyHolder);
				}
				catch(DuplicateKeyException e)
				{
					// we don't care about this one
				}
				catch(EndOfFileException e)
				{
					// This one we do care about but we expect it
					carryOn = false;

				} 
				myCustomer = new CUSTOMER(holder.getValue());

				temp[j] = new Customer();
				temp[j].setAddress(myCustomer.getCustomerAddress());
				temp[j].setCustomer_number(new Long(myCustomer.getCustomerNumber()).toString());
				temp[j].setName(myCustomer.getCustomerName());
				temp[j].setSortcode(new Integer(myCustomer.getCustomerSortcode()).toString());
				Calendar dobCalendar = Calendar.getInstance();
				dobCalendar.set(Calendar.YEAR, myCustomer.getCustomerBirthYear());
				dobCalendar.set(Calendar.MONTH, myCustomer.getCustomerBirthMonth());
				dobCalendar.set(Calendar.DAY_OF_MONTH, myCustomer.getCustomerBirthDay());
				Date dob = new Date(dobCalendar.getTimeInMillis());
				temp[j].setDob(dob);
				temp[j].setCreditScore(Integer.toString(myCustomer.getCustomerCreditScore()));
				Calendar csReviewCalendar = Calendar.getInstance();
				csReviewCalendar.set(Calendar.YEAR, myCustomer.getCustomerCsReviewYear());
				csReviewCalendar.set(Calendar.MONTH, myCustomer.getCustomerCsReviewMonth());
				csReviewCalendar.set(Calendar.DAY_OF_MONTH, myCustomer.getCustomerCsReviewDay());
				Date csReviewDate = new Date(csReviewCalendar.getTimeInMillis());
				temp[j].setReviewDate(csReviewDate);
				if(temp[j].getAddress().contains(town))
				{
					i++;
				}
			}

			customerFileBrowse.end();

			Customer[] real = new Customer[i];
			System.arraycopy(temp, 0, real, 0, i);

			logger.exiting(this.getClass().getName(),GET_CUSTOMERS_BY_TOWN,real);
			return real;
		} catch (LengthErrorException | InvalidSystemIdException | LogicException | InvalidRequestException | IOErrorException
				| LockedException | RecordBusyException | LoadingException | ChangedException | FileDisabledException
				| FileNotFoundException | ISCInvalidRequestException | NotAuthorisedException | RecordNotFoundException
				| NotOpenException e1) {
			logger.severe(e1.getLocalizedMessage());
			logger.exiting(this.getClass().getName(),GET_CUSTOMERS_BY_TOWN,null);
			return null;
		}
	}

	public Customer[] getCustomersBySurname(String surname) {
		logger.entering(this.getClass().getName(),GET_CUSTOMERS_BY_SURNAME,null);
		Customer[] temp = new Customer[250000];
		int i = 0;


		KSDS customerFile = new KSDS();
		customerFile.setName(FILENAME);

		myCustomer = new CUSTOMER();

		Integer sortCodeInteger = new Integer(this.getSortcode());
		Integer customerNumberInteger = new Integer(0);
		StringBuffer myStringBuffer = new StringBuffer(customerNumberInteger.toString());
		for(int z = myStringBuffer.length(); z < 10;z++)
		{
			myStringBuffer = myStringBuffer.insert(0, "0");	
		}

		RecordHolder holder = new RecordHolder();
		KeyHolder keyHolder = new KeyHolder();
		byte[] key = new byte[16];
		for(int z = 0,y=0; z < 10; z++,y++)
		{
			key[z] = (byte) myStringBuffer.toString().charAt(y);
		}
		for(int z = 10,y=0; z < 16; z++,y++)
		{
			key[z] = (byte) sortCodeInteger.toString().charAt(y);
		}

		// We need to convert the key to EBCDIC
		String keyString = new String(key);
		try {
			key = keyString.getBytes(CODEPAGE);
		} catch (UnsupportedEncodingException e2) {
			logger.severe(e2.getLocalizedMessage());
			logger.exiting(this.getClass().getName(),GET_CUSTOMERS_BY_SURNAME,null);
			return null;
		}

		KeyedFileBrowse customerFileBrowse = null;
		try {
			customerFileBrowse = customerFile.startBrowse(key);

			i = 0;
			boolean carryOn = true;
			for(int j=0;j<250000 && carryOn;j++)
			{
				try 
				{
					customerFileBrowse.next(holder, keyHolder);
				}
				catch(DuplicateKeyException e)
				{
					// we don't care about this one
				}
				catch(EndOfFileException e)
				{
					// This one we do care about but we expect it
					carryOn = false;

				}


				myCustomer = new CUSTOMER(holder.getValue());

				temp[j] = new Customer();
				temp[j].setAddress(myCustomer.getCustomerAddress());
				temp[j].setCustomer_number(new Long(myCustomer.getCustomerNumber()).toString());
				temp[j].setName(myCustomer.getCustomerName());
				temp[j].setSortcode(new Integer(myCustomer.getCustomerSortcode()).toString());
				Calendar dobCalendar = Calendar.getInstance();
				dobCalendar.set(Calendar.YEAR, myCustomer.getCustomerBirthYear());
				dobCalendar.set(Calendar.MONTH, myCustomer.getCustomerBirthMonth());
				dobCalendar.set(Calendar.DAY_OF_MONTH, myCustomer.getCustomerBirthDay());
				Date dob = new Date(dobCalendar.getTimeInMillis());
				temp[j].setDob(dob);
				temp[j].setCreditScore(Integer.toString(myCustomer.getCustomerCreditScore()));
				Calendar csReviewCalendar = Calendar.getInstance();
				csReviewCalendar.set(Calendar.YEAR, myCustomer.getCustomerCsReviewYear());
				csReviewCalendar.set(Calendar.MONTH, myCustomer.getCustomerCsReviewMonth());
				csReviewCalendar.set(Calendar.DAY_OF_MONTH, myCustomer.getCustomerCsReviewDay());
				Date csReviewDate = new Date(csReviewCalendar.getTimeInMillis());
				temp[j].setReviewDate(csReviewDate);
				if(temp[j].getName().contains(surname))
				{
					i++;
				}
			}

			customerFileBrowse.end();

			Customer[] real = new Customer[i];
			System.arraycopy(temp, 0,real,0,i);
			logger.exiting(this.getClass().getName(),GET_CUSTOMERS_BY_SURNAME,real);
			return real;
		} catch (LengthErrorException | LogicException | InvalidRequestException | IOErrorException
				| LockedException | RecordBusyException | LoadingException | ChangedException | FileDisabledException
				| FileNotFoundException | ISCInvalidRequestException | NotAuthorisedException | RecordNotFoundException
				| NotOpenException | InvalidSystemIdException e1) {
			logger.severe(ERROR_START_BROWSE + e1.getLocalizedMessage());
			logger.exiting(this.getClass().getName(),GET_CUSTOMERS_BY_SURNAME,null);
			return null;
		}
	}

	public Customer[] getCustomersByAge(int age) {
		logger.entering(this.getClass().getName(),GET_CUSTOMERS_BY_AGE,null);
		Customer[] temp = new Customer[250000];
		int i = 0;


		KSDS customerFile = new KSDS();
		customerFile.setName(FILENAME);

		myCustomer = new CUSTOMER();

		Integer sortCodeInteger = new Integer(this.getSortcode());
		Integer customerNumberInteger = new Integer(0);
		StringBuffer myStringBuffer = new StringBuffer(customerNumberInteger.toString());
		for(int z = myStringBuffer.length(); z < 10;z++)
		{
			myStringBuffer = myStringBuffer.insert(0, "0");	
		}

		RecordHolder holder = new RecordHolder();
		KeyHolder keyHolder = new KeyHolder();
		byte[] key = new byte[16];
		for(int z = 0,y=0; z < 10; z++,y++)
		{
			key[z] = (byte) myStringBuffer.toString().charAt(y);
		}
		for(int z = 10,y=0; z < 16; z++,y++)
		{
			key[z] = (byte) sortCodeInteger.toString().charAt(y);
		}

		// We need to convert the key to EBCDIC
		String keyString = new String(key);
		try {
			key = keyString.getBytes(CODEPAGE);
		} catch (UnsupportedEncodingException e2) {
			logger.severe(e2.getLocalizedMessage());
			logger.exiting(this.getClass().getName(),GET_CUSTOMERS_BY_AGE,null);
			return null;
		}

		KeyedFileBrowse customerFileBrowse = null;
		try {
			customerFileBrowse = customerFile.startBrowse(key);

			i = 0;
			boolean carryOn = true;
			Date today = new Date(Calendar.getInstance().getTimeInMillis());

			for(int j=0;j<250000 && carryOn;j++)
			{
				try 
				{
					customerFileBrowse.next(holder, keyHolder);
				}
				catch(DuplicateKeyException e)
				{
					// we don't care about this one
				}
				catch(EndOfFileException e)
				{
					// This one we do care about but we expect it
					carryOn = false;

				}

				myCustomer = new CUSTOMER(holder.getValue());

				temp[j] = new Customer();
				temp[j].setAddress(myCustomer.getCustomerAddress());
				temp[j].setCustomer_number(new Long(myCustomer.getCustomerNumber()).toString());
				temp[j].setName(myCustomer.getCustomerName());
				temp[j].setSortcode(new Integer(myCustomer.getCustomerSortcode()).toString());
				Calendar dobCalendar = Calendar.getInstance();
				dobCalendar.set(Calendar.YEAR, myCustomer.getCustomerBirthYear());
				dobCalendar.set(Calendar.MONTH, myCustomer.getCustomerBirthMonth());
				dobCalendar.set(Calendar.DAY_OF_MONTH, myCustomer.getCustomerBirthDay());
				Date dob = new Date(dobCalendar.getTimeInMillis());
				temp[j].setDob(dob);
				temp[j].setCreditScore(Integer.toString(myCustomer.getCustomerCreditScore()));
				Calendar csReviewCalendar = Calendar.getInstance();
				csReviewCalendar.set(Calendar.YEAR, myCustomer.getCustomerCsReviewYear());
				csReviewCalendar.set(Calendar.MONTH, myCustomer.getCustomerCsReviewMonth());
				csReviewCalendar.set(Calendar.DAY_OF_MONTH, myCustomer.getCustomerCsReviewDay());
				Date csReviewDate = new Date(csReviewCalendar.getTimeInMillis());
				temp[j].setReviewDate(csReviewDate);
				if(customerAgeInYears(today,dob) == age)
				{
					i++;
				}
			}

			customerFileBrowse.end();

			Customer[] real = new Customer[i];
			for(int j=0;j<i;j++)
			{
				real[j] = temp[j];	
			}
			logger.exiting(this.getClass().getName(),GET_CUSTOMERS_BY_AGE,real);
			return real;
		} 
		catch (LengthErrorException | InvalidSystemIdException | LogicException | InvalidRequestException | IOErrorException
				| LockedException | RecordBusyException | LoadingException | ChangedException | FileDisabledException
				| FileNotFoundException | ISCInvalidRequestException | NotAuthorisedException | RecordNotFoundException
				| NotOpenException e1) {
			logger.severe(ERROR_START_BROWSE + e1.getLocalizedMessage());
			logger.exiting(this.getClass().getName(),GET_CUSTOMERS_BY_AGE,null);
			return null;
		}
	}

	int customerAgeInYears(Date now, Date dob)
	{
		Calendar nowCalendar = Calendar.getInstance();
		Calendar birthCalendar = Calendar.getInstance();
		birthCalendar.setTime(dob);
		int age = 0;
		int years = (nowCalendar.get(Calendar.YEAR) + 1900) - (birthCalendar.get(Calendar.YEAR) + 1900);
		age = years;
		if (birthCalendar.get(Calendar.MONTH) > nowCalendar.get(Calendar.MONTH))
		{
			age--;
			return age;
		}
		if(birthCalendar.get(Calendar.MONTH) == nowCalendar.get(Calendar.MONTH))
		{
			if (birthCalendar.get(Calendar.DAY_OF_MONTH) > nowCalendar.get(Calendar.DAY_OF_MONTH))
			{
				age--;
				return age;
			}

		}

		return age;

	}
}
