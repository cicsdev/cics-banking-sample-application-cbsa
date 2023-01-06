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
import java.util.logging.LogManager;
import java.util.logging.Logger;

import com.ibm.cics.cip.bankliberty.api.json.CounterResource;
import com.ibm.cics.cip.bankliberty.api.json.CreditScore;
import com.ibm.cics.cip.bankliberty.api.json.CustomerJSON;
import com.ibm.cics.cip.bankliberty.dataInterfaces.CUSTOMER;
import com.ibm.cics.cip.bankliberty.dataInterfaces.CustomerControl;

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


	// String ACCOUNT_EYECATCHER             CHAR(4),
	private 	String 		customer_number;
	private 	String 		sortcode;              
	private 	String 		name;
	private 	String 		address;
	private 	Date 		dob;
	private		String		creditScore;
	private		Date		reviewDate;


	private KSDS customerFile;

	private CUSTOMER myCustomer;

	private     int maximum_retries = 100, totalSleep = 3000;

	private boolean not_found;

	public Customer (String c_n, String sc, String n, String a, Date d, String c_s, Date r_d) {
		setCustomer_number(c_n);
		setSortcode(sc);
		setName(n);
		setAddress(a);
		setDob(d);
		setCreditScore(c_s);
		setReviewDate(r_d);
		sortOutLogging();
	}

	public Customer() {
		sortOutLogging();

	}

	public String getCustomer_number() {
		if(this.customer_number.length()<10)
		{
			for (int i=this.customer_number.length();i<10;i++)
			{
				this.customer_number = "0" + this.customer_number;
			}
		}
		return this.customer_number;
	}

	public void setCustomer_number(String customer_number) {
		if(customer_number.length()<10)
		{
			for (int i=customer_number.length();i<10;i++)
			{
				customer_number = "0" + customer_number;
			}
		}
		this.customer_number = customer_number;
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
		logger.fine("VSAM CUSTMOMER----");
		logger.fine("Customer Numeber: " + this.customer_number);
		logger.fine("Name: " + this.name);
		logger.fine("Address: " + this.address);
		logger.fine("Dob: " + this.dob.toString());
		logger.fine("Sortcode : " + this.sortcode);
		logger.fine("Credit Score: " + this.creditScore);
		logger.fine("Review Date: " + this.reviewDate.toString());
	}



	public Customer getCustomer(long customerNumber, int sortCode) 
	{
		logger.entering(this.getClass().getName(),"getCustomer(long customerNumber, int sortCode");
		Customer temp = null;

		customerFile = new KSDS();
		customerFile.setName("CUSTOMER");

		myCustomer = new CUSTOMER();

		Integer sortCodeInteger = new Integer(sortCode);
		Long customerNumberLong = new Long(customerNumber);

		if(customerNumber == 9999999999L)
		{
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
			} catch (LogicException | InvalidRequestException | IOErrorException 
					| ChangedException | LockedException | LoadingException | RecordBusyException
					| FileDisabledException | DuplicateKeyException | FileNotFoundException | ISCInvalidRequestException
					| NotAuthorisedException | RecordNotFoundException | NotOpenException e) {
				logger.severe("Error reading customer " + customerNumber + " " + e.getLocalizedMessage());
				logger.exiting(this.getClass().getName(),"getCustomer(long customerNumber, int sortCode",null);
				return null;
			} catch (LengthErrorException e) {
				logger.severe(e.getLocalizedMessage());
				logger.exiting(this.getClass().getName(),"getCustomer(long customerNumber, int sortCode",null);
				return null;
			} catch (EndOfFileException e) {
				logger.severe(e.getLocalizedMessage());
				logger.exiting(this.getClass().getName(),"getCustomer(long customerNumber, int sortCode",null);
				return null;
			}
			catch (InvalidSystemIdException  e1) {
				int number_of_retries = 0;
				boolean success;
				for(number_of_retries = 0,success = false; number_of_retries < maximum_retries && success == false;number_of_retries++)
				{
					try {
						logger.fine("About to go to sleep for " + totalSleep + " milliseconds");
						Thread.sleep(3000);
					} catch (InterruptedException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					} 					try {
						KeyedFileBrowse myKeyedFileBrowse =	customerFile.startBrowse(key);
						myKeyedFileBrowse.previous(holder, keyHolder);
						myKeyedFileBrowse.end();
						key = keyHolder.getValue();
						customerFile.read(key, holder);
						success = true;

					} catch (EndOfFileException | FileDisabledException | DuplicateKeyException | NotOpenException | LogicException | InvalidRequestException | IOErrorException  | LengthErrorException 
							| ChangedException | LockedException | LoadingException | RecordBusyException | FileNotFoundException | ISCInvalidRequestException | NotAuthorisedException | RecordNotFoundException
							e3) {
						logger.severe(e3.getLocalizedMessage());
						logger.exiting(this.getClass().getName(),"getCustomer(long customerNumber, int sortCode",null);
						return null;
					}
					catch (InvalidSystemIdException e4)
					{
					} 

				}
				if(number_of_retries == maximum_retries && success == false)
				{
					logger.severe("Cannot read CUSTOMER file after 100 attempts");
					logger.exiting(this.getClass().getName(),"getCustomer(long customerNumber, int sortCode",null);
					return null;
				}
			}

			myCustomer = new CUSTOMER(holder.getValue());		
		}

		if(customerNumber > 0 && customerNumber < 9999999999L)
		{
			RecordHolder holder = new RecordHolder();
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
			try {
				key = keyString.getBytes("Cp1047");
			} catch (UnsupportedEncodingException e2) {
				logger.severe(e2.getLocalizedMessage());
				logger.exiting(this.getClass().getName(),"getCustomer(long customerNumber, int sortCode",null);
				return null;
			}
			try {
				customerFile.read(key, holder);
			} catch (LogicException | InvalidRequestException | IOErrorException 
					| ChangedException | LockedException | LoadingException | RecordBusyException
					| FileDisabledException | DuplicateKeyException | FileNotFoundException | ISCInvalidRequestException
					| NotAuthorisedException | RecordNotFoundException | NotOpenException e) {
				logger.severe("Error reading customer file for " + customerNumber + " " + e.getLocalizedMessage());
				logger.exiting(this.getClass().getName(),"getCustomer(long customerNumber, int sortCode",null);
				return null;
			}
			catch (InvalidSystemIdException  e1) {
				int number_of_retries = 0;
				boolean success;
				for(number_of_retries = 0,success = false; number_of_retries < maximum_retries && success == false;number_of_retries++)
				{
					try {
						logger.fine("About to go to sleep for " + totalSleep + " milliseconds");
						Thread.sleep(3000);
					} catch (InterruptedException e) {
					} 
					try {
						customerFile.read(key, holder);
						success = true;

					} catch (FileDisabledException | DuplicateKeyException | NotOpenException | LogicException | InvalidRequestException | IOErrorException  | ChangedException | LockedException | LoadingException | RecordBusyException | FileNotFoundException | ISCInvalidRequestException | NotAuthorisedException | RecordNotFoundException
							e3) {
						logger.severe("Error reading customer file for " + customerNumber + "," + e3.getLocalizedMessage());
						logger.exiting(this.getClass().getName(),"getCustomer(long customerNumber, int sortCode",null);
						return null;
					}
					catch (InvalidSystemIdException e4)
					{
					} 

				}
				if(number_of_retries == maximum_retries && success == false)
				{
					logger.severe("Cannot read CUSTOMER file after 100 attempts");
					logger.exiting(this.getClass().getName(),"getCustomer(long customerNumber, int sortCode",null);
					return null;
				}
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
		logger.exiting(this.getClass().getName(),"getCustomer(long customerNumber, int sortCode",temp);
		return temp;
	}

	public Customer[] getCustomers(int sortCode) {
		logger.entering(this.getClass().getName(),"getCustomers(int sortCode)",null);
		Customer[] temp = new Customer[250000];
		int i = 0;


		KSDS customerFile = new KSDS();
		customerFile.setName("CUSTOMER");

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
			key = keyString.getBytes("Cp1047");
		} catch (UnsupportedEncodingException e2) {
			logger.severe(e2.getLocalizedMessage());
			logger.exiting(this.getClass().getName(),"getCustomers(int sortCode)",null);
			return null;
		}

		KeyedFileBrowse customerFileBrowse = null;
		try {
			customerFileBrowse = customerFile.startBrowse(key);
		} catch (LogicException | InvalidRequestException | IOErrorException
				| LockedException | RecordBusyException | LoadingException | ChangedException | FileDisabledException
				| FileNotFoundException | ISCInvalidRequestException | NotAuthorisedException | RecordNotFoundException
				| NotOpenException e1) {
			logger.severe("Error starting browse of customer " + e1.getLocalizedMessage());
			logger.exiting(this.getClass().getName(),"getCustomers(int sortCode)",null);
			return null;
		}
		catch (InvalidSystemIdException  e1) {
			int number_of_retries = 0;
			boolean success;
			for(number_of_retries = 0,success = false; number_of_retries < maximum_retries && success == false;number_of_retries++)
			{
				try {
					logger.fine("About to sleep for " + totalSleep + " milliseconds");
					Thread.sleep(3000);
				}  catch (InterruptedException e) {
				} 				try {
					customerFileBrowse = customerFile.startBrowse(key);
					success = true;

				} catch (FileDisabledException | NotOpenException | LogicException | InvalidRequestException | IOErrorException  | ChangedException | LockedException | LoadingException | RecordBusyException | FileNotFoundException | ISCInvalidRequestException | NotAuthorisedException | RecordNotFoundException
						e3) {
					logger.severe("Error starting browse of CUSTOMER file, " + e3.getLocalizedMessage());
					logger.exiting(this.getClass().getName(),"getCustomers(int sortCode)",null);
					return null;
				}
				catch (InvalidSystemIdException e4)
				{
				} 

			}
			if(number_of_retries == maximum_retries && success == false)
			{
				logger.severe("Cannot read CUSTOMER file after 100 attempts");
				logger.exiting(this.getClass().getName(),"getCustomers(int sortCode)",null);
				return null;
			}
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
			catch (LogicException | InvalidRequestException
					| IOErrorException 
					| ChangedException | LockedException | LoadingException
					| RecordBusyException | FileDisabledException
					| FileNotFoundException
					| ISCInvalidRequestException | NotAuthorisedException
					| RecordNotFoundException | NotOpenException | LengthErrorException e) {
				logger.severe("Error browsing CUSTOMER file, " + e.getLocalizedMessage());
				logger.exiting(this.getClass().getName(),"getCustomers(int sortCode)",null);
				return null;
			}
			catch (InvalidSystemIdException  e1) {
				int number_of_retries = 0;
				boolean success;
				for(number_of_retries = 0,success = false; number_of_retries < maximum_retries && success == false;number_of_retries++)
				{
					try {
						logger.fine("About to go to sleep for " + totalSleep + " milliseconds");
						Thread.sleep(3000);
					} catch (InterruptedException e) {
					}					try {
						customerFileBrowse.next(holder, keyHolder);
						success = true;

					} catch (DuplicateKeyException | LengthErrorException | FileDisabledException | NotOpenException | LogicException | InvalidRequestException | IOErrorException  | ChangedException | LockedException | LoadingException | RecordBusyException | FileNotFoundException | ISCInvalidRequestException | NotAuthorisedException | RecordNotFoundException
							e3) {
						logger.severe(e3.getLocalizedMessage());
						logger.exiting(this.getClass().getName(),"getCustomers(int sortCode)",null);
						return null;
					}
					catch (InvalidSystemIdException e4)
					{
					}
					catch (EndOfFileException e4)
					{
						success = true;
						carryOn = false;
					}

				}
				if(number_of_retries == maximum_retries && success == false)
				{
					logger.severe("Cannot browse CUSTOMER file after 100 attempts");
					logger.exiting(this.getClass().getName(),"getCustomers(int sortCode)",null);
					return null;
				}

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
		} catch (LogicException | InvalidRequestException | FileDisabledException
				| FileNotFoundException | ISCInvalidRequestException | NotAuthorisedException
				| NotOpenException e) {
			logger.severe("Error ending browse of CUSTOMER file "  + e.getLocalizedMessage());
			logger.exiting(this.getClass().getName(),"getCustomers(int sortCode)",null);
			return null;
		}
		catch (InvalidSystemIdException  e2) {
			int number_of_retries = 0;
			boolean success;
			for(number_of_retries = 0,success = false; number_of_retries < maximum_retries && success == false;number_of_retries++)
			{
				try {
					logger.fine("About to go to sleep for " + totalSleep + " milliseconds");
					Thread.sleep(3000);
				} catch (InterruptedException e) {
					// 
				} 
				try {
					customerFileBrowse.end();
					success = true;
				} catch (FileDisabledException | NotOpenException | LogicException | InvalidRequestException | FileNotFoundException | ISCInvalidRequestException | NotAuthorisedException
						e3) {
					logger.severe("Error ending browse of CUSTOMER file, " + e3.getLocalizedMessage());
					logger.exiting(this.getClass().getName(),"getCustomers(int sortCode)",null);
					return null;
				}
				catch (InvalidSystemIdException e4)
				{
				} 

			}
			if(number_of_retries == maximum_retries && success == false)
			{
				logger.severe("Cannot browse CUSTOMER file after 100 attempts");
				logger.exiting(this.getClass().getName(),"getCustomers(int sortCode)",null);
				return null;
			}
		}

		Customer[] real = new Customer[i];
		for(int j=0;j<i;j++)
		{
			real[j] = temp[j];	
		}
		logger.exiting(this.getClass().getName(),"getCustomers(int sortCode)",real);
		return real;
	}




	public Customer updateCustomer(CustomerJSON customer) 
	{
		logger.entering(this.getClass().getName(),"updateCustomer(CustomerJSON customer)",null);

		customerFile = new KSDS();
		customerFile.setName("CUSTOMER");
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
			key = keyString.getBytes("Cp1047");
		}
		catch (UnsupportedEncodingException e2) 
		{
			logger.severe(e2.getLocalizedMessage());
			logger.exiting(this.getClass().getName(),"updateCustomer(CustomerJSON customer)",null);
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
		catch (LogicException | InvalidRequestException | IOErrorException
				| ChangedException | LockedException | LoadingException | RecordBusyException
				| FileDisabledException | DuplicateKeyException | FileNotFoundException | ISCInvalidRequestException
				| NotAuthorisedException | NotOpenException | LengthErrorException | DuplicateRecordException | NoSpaceException e) 
		{
			logger.severe("Error updating customer " + customerNumberLong + " " + e.getLocalizedMessage());
			logger.exiting(this.getClass().getName(),"updateCustomer(CustomerJSON customer)",null);
			return null;
		}
		catch (RecordNotFoundException e2)
		{
			Customer customer404 = new Customer();
			customer404.setNot_found(true);
			logger.exiting(this.getClass().getName(),"updateCustomer(CustomerJSON customer)",customer404);
			return customer404;
		}
		catch (InvalidSystemIdException  e2) {
			int number_of_retries = 0;
			boolean success;
			for(number_of_retries = 0,success = false; number_of_retries < maximum_retries && success == false;number_of_retries++)
			{
				try {
					logger.fine("About to go to sleep for " + totalSleep + " milliseconds");
					Thread.sleep(3000);
				} catch (InterruptedException e) {
				}
				try {
					customerFile.readForUpdate(key, holder);
					myCustomer = new CUSTOMER(holder.getValue());
					myCustomer.setCustomerAddress(customer.getCustomerAddress());
					myCustomer.setCustomerName(customer.getCustomerName());
					customerFile.rewrite(myCustomer.getByteBuffer());
					myCustomer = new CUSTOMER(holder.getValue());
					success = true;

				} catch (NoSpaceException | DuplicateRecordException | DuplicateKeyException | ChangedException | LoadingException | RecordBusyException | LockedException | IOErrorException | LengthErrorException | FileDisabledException | NotOpenException | LogicException | InvalidRequestException | FileNotFoundException | ISCInvalidRequestException | NotAuthorisedException
						e3) {
					logger.severe("Error updating CUSTOMER file, " + e3.getLocalizedMessage());
					logger.exiting(this.getClass().getName(),"updateCustomer(CustomerJSON customer)",null);
					return null;
				}
				catch (InvalidSystemIdException e4)
				{
				} 
				catch (RecordNotFoundException e5)
				{
					Customer customer404 = new Customer();
					customer404.setNot_found(true);
					logger.exiting(this.getClass().getName(),"updateCustomer(CustomerJSON customer)",customer404);
					return customer404;
				}
			}
			if(number_of_retries == maximum_retries && success == false)
			{
				logger.severe("Cannot update CUSTOMER file after 100 attempts");
				logger.exiting(this.getClass().getName(),"updateCustomer(CustomerJSON customer)",null);
				return null;
			}
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
		logger.exiting(this.getClass().getName(),"updateCustomer(CustomerJSON customer)",temp);
		return temp;
	}


	private void setNot_found(boolean b) {
		this.not_found = b;

	}

	public boolean isNot_found() {
		return this.not_found;

	}

	public Customer deleteCustomer(long customerNumber, int sortCode) {

		logger.entering(this.getClass().getName(),"deleteCustomer(long customerNumber, int sortCode)");


		Customer temp = null;

		customerFile = new KSDS();
		customerFile.setName("CUSTOMER");

		myCustomer = new CUSTOMER();

		Integer sortCodeInteger = new Integer(sortCode);
		Long customerNumberLong = new Long(customerNumber);



		if(customerNumber == 9999999999L)
		{

			RecordHolder holder = new RecordHolder();
			KeyHolder keyHolder = new KeyHolder();

			// We need to set the key to high values. This is awkward in Java
			byte[] key = new byte[16];

			for(int z = 0; z < 16;z++)
			{

				key[z] = (byte) -1;	
			}

			KeyedFileBrowse myKeyedFileBrowse =	null;
			try {
				myKeyedFileBrowse =	customerFile.startBrowse(key);
				myKeyedFileBrowse.previous(holder, keyHolder);
				myKeyedFileBrowse.end();
				key = keyHolder.getValue();

				customerFile.readForUpdate(key, holder);
				customerFile.delete();
			} catch (LengthErrorException | EndOfFileException | LogicException | InvalidRequestException | IOErrorException
					| ChangedException | LockedException | LoadingException | RecordBusyException
					| FileDisabledException | DuplicateKeyException | FileNotFoundException | ISCInvalidRequestException
					| NotAuthorisedException | RecordNotFoundException | NotOpenException e) {
				logger.severe("Error deleting customer " + customerNumber + " in CUSTOMER file " + e.getLocalizedMessage());
				logger.exiting(this.getClass().getName(),"deleteCustomer(long customerNumber, int sortCode)",null);
				return null;
			}
			catch (InvalidSystemIdException  e2) {
				int number_of_retries = 0;
				boolean success;
				for(number_of_retries = 0,success = false; number_of_retries < maximum_retries && success == false;number_of_retries++)
				{
					try {
						logger.fine("About to go to sleep for " + totalSleep + " milliseconds");
						Thread.sleep(3000);
					}
					catch (InterruptedException e) 
					{
					}
					try {
						myKeyedFileBrowse =	customerFile.startBrowse(key);
						myKeyedFileBrowse.previous(holder, keyHolder);
						myKeyedFileBrowse.end();
						key = keyHolder.getValue();

						customerFile.readForUpdate(key, holder);
						customerFile.delete();
						success = true;

					} catch (EndOfFileException | DuplicateKeyException | ChangedException | LoadingException | RecordBusyException | LockedException | RecordNotFoundException | IOErrorException | LengthErrorException | FileDisabledException | NotOpenException | LogicException | InvalidRequestException | FileNotFoundException | ISCInvalidRequestException | NotAuthorisedException
							e3) {
						logger.severe("Error deleting customer " + customerNumber + " in CUSTOMER file " + e3.getLocalizedMessage());
						logger.exiting(this.getClass().getName(),"deleteCustomer(long customerNumber, int sortCode)",null);
						return null;
					}
					catch (InvalidSystemIdException e4)
					{
					} 
				}
				if(number_of_retries == maximum_retries && success == false)
				{
					logger.severe("Cannot delete customer " + customerNumber + " in CUSTOMER file after 100 attempts");
					logger.exiting(this.getClass().getName(),"deleteCustomer(long customerNumber, int sortCode)",null);
					return null;
				}
			}

			myCustomer = new CUSTOMER(holder.getValue());		
		}

		if(customerNumber > 0 && customerNumber < 9999999999L)
		{
			RecordHolder holder = new RecordHolder();
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
			try {
				key = keyString.getBytes("Cp1047");
			} catch (UnsupportedEncodingException e2) {
				logger.severe(e2.getLocalizedMessage());
				logger.exiting(this.getClass().getName(),"deleteCustomer(long customerNumber, int sortCode)",null);
				return null;
			}
			try {
				customerFile.readForUpdate(key, holder);
				customerFile.delete();
			} catch (LogicException | InvalidRequestException | IOErrorException 
					| ChangedException | LockedException | LoadingException | RecordBusyException
					| FileDisabledException | DuplicateKeyException | FileNotFoundException | ISCInvalidRequestException
					| NotAuthorisedException | NotOpenException e) {
				logger.severe("Error deleting customer " + customerNumber + " in CUSTOMER file," + e.getLocalizedMessage());
				logger.exiting(this.getClass().getName(),"deleteCustomer(long customerNumber, int sortCode)",null);
				return null;
			}
			catch (InvalidSystemIdException  e2) {
				int number_of_retries = 0;
				boolean success;
				for(number_of_retries = 0,success = false; number_of_retries < maximum_retries && success == false;number_of_retries++)
				{
					try {
						logger.fine("About to go to sleep for " + totalSleep + " milliseconds");
						Thread.sleep(3000);
					}
					catch (InterruptedException e) 
					{
					}
					try {
						customerFile.readForUpdate(key, holder);
						customerFile.delete();
						success = true;

					} catch (DuplicateKeyException | ChangedException | LoadingException | RecordBusyException | LockedException |IOErrorException | FileDisabledException | NotOpenException | LogicException | InvalidRequestException | FileNotFoundException | ISCInvalidRequestException | NotAuthorisedException
							e3) {
						logger.severe("Error deleting customer " + customerNumber + " in CUSTOMER file " + e3.getLocalizedMessage());
						logger.exiting(this.getClass().getName(),"deleteCustomer(long customerNumber, int sortCode)",null);
						return null;
					}
					catch (InvalidSystemIdException e4)
					{
					}
					catch(RecordNotFoundException e)
					{
						Customer customer404 = new Customer();
						customer404.setNot_found(true);
						return customer404;
					}
				}
				if(number_of_retries == maximum_retries && success == false)
				{
					logger.severe("Cannot delete customer " + customerNumber + " in CUSTOMER file after 100 attempts");
					logger.exiting(this.getClass().getName(),"deleteCustomer(long customerNumber, int sortCode)",null);
					return null;
				}
			}
			catch(RecordNotFoundException e)
			{
				Customer customer404 = new Customer();
				customer404.setNot_found(true);
				logger.exiting(this.getClass().getName(),"deleteCustomer(long customerNumber, int sortCode)",customer404);
				return customer404;
			}

			myCustomer = new CUSTOMER(holder.getValue());
			CustomerControl myCustomerControl = new CustomerControl();
			KSDS customerKSDS = new KSDS();
			customerKSDS.setName("CUSTOMER");

			myCustomerControl.setCustomerControlSortcode(0);
			myCustomerControl.setCustomerControlNumber(9999999999L);

			key = "0000009999999999".getBytes();

			keyString = new String(key);
			try 
			{
				key = keyString.getBytes("Cp1047");
			}
			catch (UnsupportedEncodingException e2) 
			{
				logger.severe(e2.getLocalizedMessage());
				logger.exiting(this.getClass().getName(),"deleteCustomer(long customerNumber, int sortCode)",null);
				return null;
			}

			holder = new RecordHolder();

			try {
				customerKSDS.readForUpdate(key, holder);
			} catch (LogicException | InvalidRequestException | IOErrorException | InvalidSystemIdException
					| LockedException | ChangedException | LoadingException | RecordBusyException
					| FileDisabledException | DuplicateKeyException | FileNotFoundException | ISCInvalidRequestException
					| NotAuthorisedException | RecordNotFoundException | NotOpenException e) {
				logger.severe(e.getLocalizedMessage());
				logger.exiting(this.getClass().getName(),"deleteCustomer(long customerNumber, int sortCode)",null);
				return null;
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
				logger.exiting(this.getClass().getName(),"deleteCustomer(long customerNumber, int sortCode)",null);
				return null;
			}
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
				new String(new Integer(myCustomer.getCustomerSortcode()).toString()), 
				new String(myCustomer.getCustomerName()),
				new String(myCustomer.getCustomerAddress()), 
				myCustomerBirthDate, 
				Integer.toString(myCustomer.getCustomerCreditScore()),
				myCustomerReviewDate);

		logger.exiting(this.getClass().getName(),"deleteCustomer(long customerNumber, int sortCode)",temp);
		return temp;

	}

	public Customer createCustomer(CustomerJSON customer, Integer sortCodeInteger, boolean useNamedCounter) 
	{
		logger.entering(this.getClass().getName(),"createCustomer(CustomerJSON customer, Integer sortCodeInteger, boolean useNamedCounter)");
		
		Customer temp = null;

		customerFile = new KSDS();
		customerFile.setName("CUSTOMER");
		CounterResource myCounterResource = null;
		myCustomer = new CUSTOMER();
		long customerNumber = 1234567890L;
		String sortCodeString = sortCodeInteger.toString();


		// We need to get a NEW customer number
		if(useNamedCounter)
		{
			myCounterResource = new CounterResource();
			try {
				JSONObject myCounterJSON = JSONObject.parse(myCounterResource.incrementCustomerCounter().getEntity().toString());
				customerNumber = (Long) myCounterJSON.get("customerNumber");
			} catch (IOException e) {
				logger.severe(e.getLocalizedMessage());
				logger.exiting(this.getClass().getName(),"createCustomer(CustomerJSON customer, Integer sortCodeInteger, boolean useNamedCounter)",null);
				return null;

			}
		}
		else
		{
			// We need to enqueue, then get the last customer number
			NameResource enqueue = new NameResource();

			enqueue.setName("HBNKCUST" + sortCodeString + "  ");
			try {
				enqueue.enqueue();
			} catch (ResourceUnavailableException e) {
				logger.severe(e.getLocalizedMessage());
				logger.exiting(this.getClass().getName(),"createCustomer(CustomerJSON customer, Integer sortCodeInteger, boolean useNamedCounter)",null);
				return null;
			} catch (LengthErrorException e) {
				logger.severe(e.getLocalizedMessage());
				logger.exiting(this.getClass().getName(),"createCustomer(CustomerJSON customer, Integer sortCodeInteger, boolean useNamedCounter)",null);
				return null;
			}
			CustomerControl myCustomerControl = new CustomerControl();
			KSDS customerKSDS = new KSDS();
			customerKSDS.setName("CUSTOMER");

			myCustomerControl.setCustomerControlSortcode(0);
			myCustomerControl.setCustomerControlNumber(9999999999L);

			byte[] key = "0000009999999999".getBytes();

			String keyString = new String(key);
			try 
			{
				key = keyString.getBytes("Cp1047");
			}
			catch (UnsupportedEncodingException e2) 
			{
				logger.severe(e2.getLocalizedMessage());
				logger.exiting(this.getClass().getName(),"createCustomer(CustomerJSON customer, Integer sortCodeInteger, boolean useNamedCounter)",null);
				return null;
			}

			RecordHolder holder = new RecordHolder();

			try {
				customerKSDS.readForUpdate(key, holder);
			} catch (LogicException | InvalidRequestException | IOErrorException | InvalidSystemIdException
					| LockedException | ChangedException | LoadingException | RecordBusyException
					| FileDisabledException | DuplicateKeyException | FileNotFoundException | ISCInvalidRequestException
					| NotAuthorisedException | RecordNotFoundException | NotOpenException e) {
				logger.severe(e.getLocalizedMessage());
				logger.exiting(this.getClass().getName(),"createCustomer(CustomerJSON customer, Integer sortCodeInteger, boolean useNamedCounter)",null);
				return null;
			}

			myCustomerControl = new CustomerControl(holder.getValue());


			long lastCustomerNumber = myCustomerControl.getLastCustomerNumber();
			lastCustomerNumber++;

			long numberOfCustomers = myCustomerControl.getNumberOfCustomers();
			numberOfCustomers++;

			myCustomerControl.setLastCustomerNumber(lastCustomerNumber);
			myCustomerControl.setNumberOfCustomers(numberOfCustomers);
			customerNumber = lastCustomerNumber;
			try {
				customerKSDS.rewrite(myCustomerControl.getByteBuffer());
			} catch (LogicException | InvalidRequestException | IOErrorException | LengthErrorException
					| InvalidSystemIdException | ChangedException | LockedException | LoadingException
					| RecordBusyException | FileDisabledException | DuplicateRecordException | FileNotFoundException
					| ISCInvalidRequestException | NoSpaceException | NotAuthorisedException | NotOpenException e) {
				logger.severe(e.getLocalizedMessage());
				logger.exiting(this.getClass().getName(),"createCustomer(CustomerJSON customer, Integer sortCodeInteger, boolean useNamedCounter)",null);
				return null;
			}
		}

		Long customerNumberLong = new Long(customerNumber);

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
			key = keyString.getBytes("Cp1047");
		}
		catch (UnsupportedEncodingException e2) 
		{
			logger.severe(e2.getLocalizedMessage());
			logger.exiting(this.getClass().getName(),"createCustomer(CustomerJSON customer, Integer sortCodeInteger, boolean useNamedCounter)",null);
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
			logger.exiting(this.getClass().getName(),"createCustomer(CustomerJSON customer, Integer sortCodeInteger, boolean useNamedCounter)",null);
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
		catch (NoSpaceException | LogicException | InvalidRequestException | IOErrorException | LengthErrorException
				| ChangedException | LockedException | LoadingException | RecordBusyException
				| FileDisabledException | FileNotFoundException | ISCInvalidRequestException
				| NotAuthorisedException | NotOpenException e) 
		{
			if(useNamedCounter)
			{
				myCounterResource.decrementCustomerCounter();
			}
			logger.severe("Error writing record to CUSTOMER file, " + e.getLocalizedMessage());
			logger.exiting(this.getClass().getName(),"createCustomer(CustomerJSON customer, Integer sortCodeInteger, boolean useNamedCounter)",null);
			return null;
		} catch (DuplicateRecordException e) {
			logger.severe("DuplicateRecordException duplicate value. Have you combined named counter and non-named counter with the same data? com.ibm.cics.cip.bankliberty.web.vsam.Customer.");
			logger.exiting(this.getClass().getName(),"createCustomer(CustomerJSON customer, Integer sortCodeInteger, boolean useNamedCounter)",null);
			if(useNamedCounter)
			{
				myCounterResource.decrementCustomerCounter();
			}
			return null;
		}
		catch (InvalidSystemIdException  e2) {
			int number_of_retries = 0;
			boolean success;
			for(number_of_retries = 0,success = false; number_of_retries < maximum_retries && success == false;number_of_retries++)
			{
				try {
					logger.fine("About to go to sleep for " + totalSleep + " milliseconds");
					Thread.sleep(3000);
				}
				catch (InterruptedException e) {
				}
				try {
					customerFile.write(key, myCustomer.getByteBuffer());
					myCustomer = new CUSTOMER(myCustomer.getByteBuffer());
					success = true;

				} catch (NoSpaceException | LengthErrorException | DuplicateRecordException | ChangedException | LoadingException | RecordBusyException | LockedException | IOErrorException | FileDisabledException | NotOpenException | LogicException | InvalidRequestException | FileNotFoundException | ISCInvalidRequestException | NotAuthorisedException
						e3) {
					logger.severe("Error inserting customer " + customerNumber + " into CUSTOMER file " + e3.getLocalizedMessage());
					logger.exiting(this.getClass().getName(),"createCustomer(CustomerJSON customer, Integer sortCodeInteger, boolean useNamedCounter)",null);
					return null;
				}
				catch (InvalidSystemIdException e4)
				{
				} 
			}
			if(number_of_retries == maximum_retries && success == false)
			{
				logger.severe("Cannot insert customer " + customerNumber + " into CUSTOMER file after 100 attempts");
				logger.exiting(this.getClass().getName(),"createCustomer(CustomerJSON customer, Integer sortCodeInteger, boolean useNamedCounter)",null);
				return null;
			}
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
		logger.exiting(this.getClass().getName(),"createCustomer(CustomerJSON customer, Integer sortCodeInteger, boolean useNamedCounter)",temp);
		return temp;

	}

	public Customer[] getCustomers(int sortCode, int limit, int offset) 
	{
		logger.entering(this.getClass().getName(),"getCustomers(int sortCode, int limit, int offset)");
		Customer[] temp = new Customer[limit];
		int stored = 0, retrieved = 0;


		KSDS customerFile = new KSDS();
		customerFile.setName("CUSTOMER");

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
			key = keyString.getBytes("Cp1047");
		} catch (UnsupportedEncodingException e2) {
			logger.severe(e2.getLocalizedMessage());
			logger.exiting(this.getClass().getName(),"getCustomers(int sortCode, int limit, int offset)",null);
			return null;
		}

		KeyedFileBrowse customerFileBrowse = null;
		try {
			customerFileBrowse = customerFile.startBrowse(key);
		} catch (LogicException | InvalidRequestException | IOErrorException
				| LockedException | RecordBusyException | LoadingException | ChangedException | FileDisabledException
				| FileNotFoundException | ISCInvalidRequestException | NotAuthorisedException | RecordNotFoundException
				| NotOpenException e1) {
			logger.severe("Error starting browse of customer " + e1.getLocalizedMessage());
			logger.exiting(this.getClass().getName(),"getCustomers(int sortCode, int limit, int offset)",null);
			return null;
		}
		catch (InvalidSystemIdException  e1) {
			int number_of_retries = 0;
			boolean success;
			for(number_of_retries = 0,success = false; number_of_retries < maximum_retries && success == false;number_of_retries++)
			{
				try {
					logger.fine("About to go to sleep for " + totalSleep + " milliseconds");
					Thread.sleep(3000);
				}
				 catch (InterruptedException e) {
					}
				try {
					customerFileBrowse = customerFile.startBrowse(key);
					success = true;

				} catch (FileDisabledException | NotOpenException | LogicException | InvalidRequestException | IOErrorException  | ChangedException | LockedException | LoadingException | RecordBusyException | FileNotFoundException | ISCInvalidRequestException | NotAuthorisedException | RecordNotFoundException
						e3) {
					logger.severe(e3.getLocalizedMessage());
					logger.exiting(this.getClass().getName(),"getCustomers(int sortCode, int limit, int offset)",null);
					return null;
				}
				catch (InvalidSystemIdException e4)
				{
				} 

			}
			if(number_of_retries == maximum_retries && success == false)
			{
				logger.severe("Cannot read CUSTOMER file after 100 attempts");
				return null;
			}
		}
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
			catch (LogicException | InvalidRequestException
					| IOErrorException 
					| ChangedException | LockedException | LoadingException
					| RecordBusyException | FileDisabledException
					| FileNotFoundException
					| ISCInvalidRequestException | NotAuthorisedException
					| RecordNotFoundException | NotOpenException | LengthErrorException e) {
				logger.severe("Error browsing CUSTOMER file, " + e.getLocalizedMessage());
				logger.exiting(this.getClass().getName(),"getCustomers(int sortCode, int limit, int offset)",null);
				return null;
			}
			catch (InvalidSystemIdException  e1) {
				int number_of_retries = 0;
				boolean success;
				for(number_of_retries = 0,success = false; number_of_retries < maximum_retries && success == false;number_of_retries++)
				{
					try {
						logger.fine("About to go to sleep for " + totalSleep + " milliseconds");
						Thread.sleep(3000);
					}
						 catch (InterruptedException e) {
							}
					try {
						customerFileBrowse.next(holder, keyHolder);
						success = true;

					} catch (DuplicateKeyException | LengthErrorException | FileDisabledException | NotOpenException | LogicException | InvalidRequestException | IOErrorException  | ChangedException | LockedException | LoadingException | RecordBusyException | FileNotFoundException | ISCInvalidRequestException | NotAuthorisedException | RecordNotFoundException
							e3) {
						logger.severe(e3.getLocalizedMessage());
						logger.exiting(this.getClass().getName(),"getCustomers(int sortCode, int limit, int offset)",null);
						return null;
					}
					catch (InvalidSystemIdException e4)
					{
					}
					catch (EndOfFileException e4)
					{
						success = true;
						carryOn = false;
					}

				}
				if(number_of_retries == maximum_retries && success == false)
				{
					logger.severe("Cannot browse CUSTOMER file after 100 attempts");
					logger.exiting(this.getClass().getName(),"getCustomers(int sortCode, int limit, int offset)",null);
					return null;
				}

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
		try {
			customerFileBrowse.end();
		} catch (LogicException | InvalidRequestException | FileDisabledException
				| FileNotFoundException | ISCInvalidRequestException | NotAuthorisedException
				| NotOpenException e) {
			logger.severe("Error ending browse of CUSTOMER file "  + e.getLocalizedMessage());
			logger.exiting(this.getClass().getName(),"getCustomers(int sortCode, int limit, int offset)",null);
			return null;
		}
		catch (InvalidSystemIdException  e2) {
			int number_of_retries = 0;
			boolean success;
			for(number_of_retries = 0,success = false; number_of_retries < maximum_retries && success == false;number_of_retries++)
			{
				try {
					logger.fine("About to go to sleep for " + totalSleep + " milliseconds");
					Thread.sleep(3000);
				}
					 catch (InterruptedException e) {
						}
				try {
					customerFileBrowse.end();
					success = true;

				} catch (FileDisabledException | NotOpenException | LogicException | InvalidRequestException | FileNotFoundException | ISCInvalidRequestException | NotAuthorisedException
						e3) {
					logger.severe("Error ending browse of CUSTOMER file, " + e3.getLocalizedMessage());
					logger.exiting(this.getClass().getName(),"getCustomers(int sortCode, int limit, int offset)",null);
					return null;
				}
				catch (InvalidSystemIdException e4)
				{
				} 

			}
			if(number_of_retries == maximum_retries && success == false)
			{
				logger.severe("Cannot browse CUSTOMER file after 100 attempts");
				logger.exiting(this.getClass().getName(),"getCustomers(int sortCode, int limit, int offset)",null);
				return null;
			}
		}


		Customer[] real = new Customer[stored];
		for(int j=0; j<stored; j++)
		{
			real[j] = temp[j];	
		}
		logger.exiting(this.getClass().getName(),"getCustomers(int sortCode, int limit, int offset)",real);
		return real;

	}

	public Customer[] getCustomersByName(int sortCode, int limit, int offset, String name) {
		logger.entering(this.getClass().getName(),"getCustomersByName(int sortCode, int limit, int offset, String name)");
		Customer[] temp = new Customer[1000000];

	
		int stored = 0;


		
		KSDS customerFile = new KSDS();
		customerFile.setName("CUSTOMER");

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
			key = keyString.getBytes("Cp1047");
		} catch (UnsupportedEncodingException e2) {
			logger.severe(e2.getLocalizedMessage());
			logger.exiting(this.getClass().getName(),"getCustomersByName(int sortCode, int limit, int offset, String name)",null);
			return null;
		}

		KeyedFileBrowse customerFileBrowse = null;
		try {

			customerFileBrowse = customerFile.startBrowse(key);
		} catch (LogicException | InvalidRequestException | IOErrorException
				| LockedException | RecordBusyException | LoadingException | ChangedException | FileDisabledException
				| FileNotFoundException | ISCInvalidRequestException | NotAuthorisedException | RecordNotFoundException
				| NotOpenException e1) {
			logger.severe("Error starting browse of customer " + e1.getLocalizedMessage());
			logger.exiting(this.getClass().getName(),"getCustomersByName(int sortCode, int limit, int offset, String name)",null);
			return null;
		}
		catch (InvalidSystemIdException  e1) {
			int number_of_retries = 0;
			boolean success;
			for(number_of_retries = 0,success = false; number_of_retries < maximum_retries && success == false;number_of_retries++)
			{
				try {
					logger.fine("About to go to sleep for " + totalSleep + " milliseconds");
					Thread.sleep(3000);
				}
					 catch (InterruptedException e) {
						}
				try {
					customerFileBrowse = customerFile.startBrowse(key);
					success = true;

				} catch (FileDisabledException | NotOpenException | LogicException | InvalidRequestException | IOErrorException  | ChangedException | LockedException | LoadingException | RecordBusyException | FileNotFoundException | ISCInvalidRequestException | NotAuthorisedException | RecordNotFoundException
						e3) {
					logger.severe("Error starting browse of CUSTOMER file, " + e3.getLocalizedMessage());
					logger.exiting(this.getClass().getName(),"getCustomersByName(int sortCode, int limit, int offset, String name)",null);
					return null;
				}
				catch (InvalidSystemIdException e4)
				{
				} 

			}
			if(number_of_retries == maximum_retries && success == false)
			{
				logger.severe("Cannot read CUSTOMER file after 100 attempts");
				logger.exiting(this.getClass().getName(),"getCustomersByName(int sortCode, int limit, int offset, String name)",null);
				return null;
			}
		}
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
			catch (LogicException | InvalidRequestException
					| IOErrorException 
					| ChangedException | LockedException | LoadingException
					| RecordBusyException | FileDisabledException
					| FileNotFoundException
					| ISCInvalidRequestException | NotAuthorisedException
					| RecordNotFoundException | NotOpenException | LengthErrorException e) {
				logger.severe("Error browsing CUSTOMER file, " + e.getLocalizedMessage());
				logger.exiting(this.getClass().getName(),"getCustomersByName(int sortCode, int limit, int offset, String name)",null);
				return null;
			}
			catch (InvalidSystemIdException  e1) {
				int number_of_retries = 0;
				boolean success;
				for(number_of_retries = 0,success = false; number_of_retries < maximum_retries && success == false;number_of_retries++)
				{
					try {
						logger.fine("About to go to sleep for " + totalSleep + " milliseconds");
						Thread.sleep(3000);
					}
					 catch (InterruptedException e) {
						}
					try {
						customerFileBrowse.next(holder, keyHolder);
						success = true;

					} catch (DuplicateKeyException | LengthErrorException | FileDisabledException | NotOpenException | LogicException | InvalidRequestException | IOErrorException  | ChangedException | LockedException | LoadingException | RecordBusyException | FileNotFoundException | ISCInvalidRequestException | NotAuthorisedException | RecordNotFoundException
							e3) {
						logger.severe("Error browsing CUSTOMER file, " + e3.getLocalizedMessage());
						logger.exiting(this.getClass().getName(),"getCustomersByName(int sortCode, int limit, int offset, String name)",null);
						return null;
					}
					catch (InvalidSystemIdException e4)
					{
					}
					catch (EndOfFileException e4)
					{
						success = true;
						carryOn = false;
						endOfFile = true;
					}

				}
				if(number_of_retries == maximum_retries && success == false)
				{
					logger.severe("Cannot browse CUSTOMER file after 100 attempts");
					logger.exiting(this.getClass().getName(),"getCustomersByName(int sortCode, int limit, int offset, String name)",null);
					return null;
				}

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
		try {
			customerFileBrowse.end();
		} catch (LogicException | InvalidRequestException | FileDisabledException
				| FileNotFoundException | ISCInvalidRequestException | NotAuthorisedException
				| NotOpenException e) {
			logger.severe("Error ending browse of CUSTOMER file "  + e.getLocalizedMessage());
			logger.exiting(this.getClass().getName(),"getCustomersByName(int sortCode, int limit, int offset, String name)",null);
			return null;
		}
		catch (InvalidSystemIdException  e2) {
			int number_of_retries = 0;
			boolean success;
			for(number_of_retries = 0,success = false; number_of_retries < maximum_retries && success == false;number_of_retries++)
			{
				try {
					logger.fine("About to go to sleep for " + totalSleep + " milliseconds");
					Thread.sleep(3000);
				}

				 catch (InterruptedException e) {
					}
				try {
					customerFileBrowse.end();
					success = true;

				} catch (FileDisabledException | NotOpenException | LogicException | InvalidRequestException | FileNotFoundException | ISCInvalidRequestException | NotAuthorisedException
						e3) {
					logger.severe("Error ending browse of CUSTOMER file, " + e3.getLocalizedMessage());
					logger.exiting(this.getClass().getName(),"getCustomersByName(int sortCode, int limit, int offset, String name)",null);
					return null;
				}
				catch (InvalidSystemIdException e4)
				{
				} 

			}
			if(number_of_retries == maximum_retries && success == false)
			{
				logger.severe("Cannot browse CUSTOMER file after 100 attempts");
				logger.exiting(this.getClass().getName(),"getCustomersByName(int sortCode, int limit, int offset, String name)",null);
				return null;
			}
		}

		Customer[] real = new Customer[limit];

		for(int j=offset,i=0; i< limit; j++,i++)
		{
			real[i] = temp[j];	
		}
		int j=0;
		

		for(;j<real.length;j++)
		{
			if(real[j] ==null)
			{
				break;
			}
		}

		if(j==0)
		{
			Customer[] returnCust = new Customer[0];
			return returnCust;
		}
		Customer[] returnCust = new Customer[j];
		for(int i=0;i<returnCust.length;i++)
		{
			returnCust[i]=real[i];
		}

		
		logger.exiting(this.getClass().getName(),"getCustomersByName(int sortCode, int limit, int offset, String name)",returnCust);
		return returnCust;
	}

	public Customer[] getCustomers(int sortCode, String name) 
	{
		logger.entering(this.getClass().getName(),"getCustomers(int sortCode, String name)");
		Customer[] temp = new Customer[250000];
		int stored = 0, retrieved = 0;

		KSDS customerFile = new KSDS();
		customerFile.setName("CUSTOMER");

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
			key = keyString.getBytes("Cp1047");
		} catch (UnsupportedEncodingException e2) {
			logger.severe(e2.getLocalizedMessage());
			logger.exiting(this.getClass().getName(),"getCustomers(int sortCode, String name)",null);
			return null;
		}

		KeyedFileBrowse customerFileBrowse = null;
		try {

			customerFileBrowse = customerFile.startBrowse(key);
		} catch (LogicException | InvalidRequestException | IOErrorException
				| LockedException | RecordBusyException | LoadingException | ChangedException | FileDisabledException
				| FileNotFoundException | ISCInvalidRequestException | NotAuthorisedException | RecordNotFoundException
				| NotOpenException e1) {
			logger.severe("Error starting browse of customer " + e1.getLocalizedMessage());
			logger.exiting(this.getClass().getName(),"getCustomers(int sortCode, String name)",null);
			return null;
		}
		catch (InvalidSystemIdException  e1) {
			int number_of_retries = 0;
			boolean success;
			for(number_of_retries = 0,success = false; number_of_retries < maximum_retries && success == false;number_of_retries++)
			{
				try {
					logger.fine("About to go to sleep for " + totalSleep + " milliseconds");
					Thread.sleep(3000);
				}
				 catch (InterruptedException e) {
					}
				try {

					customerFileBrowse = customerFile.startBrowse(key);
					success = true;

				} catch (FileDisabledException | NotOpenException | LogicException | InvalidRequestException | IOErrorException  | ChangedException | LockedException | LoadingException | RecordBusyException | FileNotFoundException | ISCInvalidRequestException | NotAuthorisedException | RecordNotFoundException
						e3) {
					logger.severe("Error starting browse of CUSTOMER file, " + e3.getLocalizedMessage());
					logger.exiting(this.getClass().getName(),"getCustomers(int sortCode, String name)",null);
					return null;
				}
				catch (InvalidSystemIdException e4)
				{
				} 

			}
			if(number_of_retries == maximum_retries && success == false)
			{
				logger.severe("Cannot read CUSTOMER file after 100 attempts");
				logger.exiting(this.getClass().getName(),"getCustomers(int sortCode, String name)",null);
				return null;
			}
		}
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
			catch (LogicException | InvalidRequestException
					| IOErrorException 
					| ChangedException | LockedException | LoadingException
					| RecordBusyException | FileDisabledException
					| FileNotFoundException
					| ISCInvalidRequestException | NotAuthorisedException
					| RecordNotFoundException | NotOpenException | LengthErrorException e) {
				logger.severe("Error browsing CUSTOMER file, " + e.getLocalizedMessage());
				logger.exiting(this.getClass().getName(),"getCustomers(int sortCode, String name)",null);
				return null;
			}
			catch (InvalidSystemIdException  e1) {
				int number_of_retries = 0;
				boolean success;
				for(number_of_retries = 0,success = false; number_of_retries < maximum_retries && success == false;number_of_retries++)
				{
					try {
						logger.fine("About to go to sleep for " + totalSleep + " milliseconds");
						Thread.sleep(3000);
					}
					 catch (InterruptedException e) {
						}
					try {
						customerFileBrowse.next(holder, keyHolder);
						success = true;

					} catch (DuplicateKeyException | LengthErrorException | FileDisabledException | NotOpenException | LogicException | InvalidRequestException | IOErrorException  | ChangedException | LockedException | LoadingException | RecordBusyException | FileNotFoundException | ISCInvalidRequestException | NotAuthorisedException | RecordNotFoundException
							e3) {
						logger.severe("Error browsing CUSTOMER file, " + e3.getLocalizedMessage());
						logger.exiting(this.getClass().getName(),"getCustomers(int sortCode, String name)",null);
						return null;
					}
					catch (InvalidSystemIdException e4)
					{
					}
					catch (EndOfFileException e4)
					{
						success = true;
						carryOn = false;
						endOfFile = true;
					}

				}
				if(number_of_retries == maximum_retries && success == false)
				{
					logger.severe("Cannot browse CUSTOMER file after 100 attempts");
					logger.exiting(this.getClass().getName(),"getCustomers(int sortCode, String name)",null);
					return null;
				}

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
		try {
			customerFileBrowse.end();
		} catch (LogicException | InvalidRequestException | FileDisabledException
				| FileNotFoundException | ISCInvalidRequestException | NotAuthorisedException
				| NotOpenException e) {
			logger.severe("Error ending browse of CUSTOMER file "  + e.getLocalizedMessage());
			logger.exiting(this.getClass().getName(),"getCustomers(int sortCode, String name)",null);
			return null;
		}
		catch (InvalidSystemIdException  e2) {
			int number_of_retries = 0;
			boolean success;
			for(number_of_retries = 0,success = false; number_of_retries < maximum_retries && success == false;number_of_retries++)
			{
				try {
					logger.fine("About to go to sleep for " + totalSleep + " milliseconds");
					Thread.sleep(3000);
				}
				 catch (InterruptedException e) {
					}
				try {
					customerFileBrowse.end();
					success = true;

				} catch (FileDisabledException | NotOpenException | LogicException | InvalidRequestException | FileNotFoundException | ISCInvalidRequestException | NotAuthorisedException
						e3) {
					logger.severe("Error ending browse of CUSTOMER file, " + e3.getLocalizedMessage());
					logger.exiting(this.getClass().getName(),"getCustomers(int sortCode, String name)",null);
					return null;
				}
				catch (InvalidSystemIdException e4)
				{
				} 

			}
			if(number_of_retries == maximum_retries && success == false)
			{
				logger.severe("Cannot browse CUSTOMER file after 100 attempts");
				logger.exiting(this.getClass().getName(),"getCustomers(int sortCode, String name)",null);
				return null;
			}
		}


		Customer[] real = new Customer[stored];
		for(int j=0; j<stored; j++)
		{
			real[j] = temp[j];	
		}
		logger.exiting(this.getClass().getName(),"getCustomers(int sortCode, String name)",real);
		return real;
	}

	public long getCustomersCountOnly(int sortCode)
	{
		logger.entering(this.getClass().getName(),"getCustomersCountOnly(int sortCode))");

		KSDS customerFile = new KSDS();
		customerFile.setName("CUSTOMER");




		RecordHolder holder = new RecordHolder();

		byte[] key = new byte[16];
		// We need to convert the key to EBCDIC
		String keyString = new String("0000009999999999");
		try {
			key = keyString.getBytes("Cp1047");
		} catch (UnsupportedEncodingException e2) {
			logger.severe(e2.getLocalizedMessage());
			logger.exiting(this.getClass().getName(),"getCustomersCountOnly(int sortCode))",-1L);
			return -1L;
		}

		try {
			customerFile.read(key, holder);
		} catch (LogicException | InvalidRequestException | IOErrorException
				| LockedException | RecordBusyException | LoadingException | ChangedException | FileDisabledException
				| FileNotFoundException | ISCInvalidRequestException | NotAuthorisedException | RecordNotFoundException
				| DuplicateKeyException | NotOpenException e1) {
			logger.severe("Error reading control record for customer file " + e1.getLocalizedMessage());
			logger.exiting(this.getClass().getName(),"getCustomersCountOnly(int sortCode))",-1L);
			return -1L;
		}
		catch (InvalidSystemIdException  e1) {
			int number_of_retries = 0;
			boolean success;
			for(number_of_retries = 0,success = false; number_of_retries < maximum_retries && success == false;number_of_retries++)
			{
				try {
					logger.fine("About to go to sleep for " + totalSleep + " milliseconds");
					Thread.sleep(3000);
				}
				 catch (InterruptedException e) {
					}
				try {
					customerFile.read(key, holder);
					success = true;

				} catch (FileDisabledException | DuplicateKeyException | NotOpenException | LogicException | InvalidRequestException | IOErrorException  | ChangedException | LockedException | LoadingException | RecordBusyException | FileNotFoundException | ISCInvalidRequestException | NotAuthorisedException | RecordNotFoundException
						e3) {
					logger.severe("Error reading control record for customer file file, " + e3.getLocalizedMessage());
					logger.exiting(this.getClass().getName(),"getCustomersCountOnly(int sortCode))",-1L);
					return -1L;
				}
				catch (InvalidSystemIdException e4)
				{
				} 

			}
			if(number_of_retries == maximum_retries && success == false)
			{
				logger.severe("Cannot read control record for CUSTOMER file after 100 attempts");
				logger.exiting(this.getClass().getName(),"getCustomersCountOnly(int sortCode))",-1L);
				return -1L;
			}
		}

		CustomerControl myCustomerControl = new CustomerControl(holder.getValue());
		logger.exiting(this.getClass().getName(),"getCustomersCountOnly(int sortCode))",myCustomerControl.getNumberOfCustomers());
		return myCustomerControl.getNumberOfCustomers();

	}

	public long getCustomersByNameCountOnly(int sortCode, String name) {
		logger.entering(this.getClass().getName(),"getCustomersByNameCountOnly(int sortCode, String name)");

		long matchingCustomers = 0;



		KSDS customerFile = new KSDS();
		customerFile.setName("CUSTOMER");

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
			key = keyString.getBytes("Cp1047");
		} catch (UnsupportedEncodingException e2) {
			logger.severe(e2.getLocalizedMessage());
			logger.exiting(this.getClass().getName(),"getCustomersByNameCountOnly(int sortCode, String name)",-1L);
			return -1L;
		}

		KeyedFileBrowse customerFileBrowse = null;
		try {

			customerFileBrowse = customerFile.startBrowse(key);
		} catch (LogicException | InvalidRequestException | IOErrorException
				| LockedException | RecordBusyException | LoadingException | ChangedException | FileDisabledException
				| FileNotFoundException | ISCInvalidRequestException | NotAuthorisedException | RecordNotFoundException
				| NotOpenException e1) {
			logger.severe("Error starting browse of customer " + e1.getLocalizedMessage());
			logger.exiting(this.getClass().getName(),"getCustomersByNameCountOnly(int sortCode, String name)",-1L);
			return -1L;
		}
		catch (InvalidSystemIdException  e1) {
			int number_of_retries = 0;
			boolean success;
			for(number_of_retries = 0,success = false; number_of_retries < maximum_retries && success == false;number_of_retries++)
			{
				try {
					logger.fine("About to go to sleep for " + totalSleep + " milliseconds");
					Thread.sleep(3000);
				}



				 catch (InterruptedException e) {
					}
				try {
					customerFileBrowse = customerFile.startBrowse(key);
					success = true;

				} catch (FileDisabledException | NotOpenException | LogicException | InvalidRequestException | IOErrorException  | ChangedException | LockedException | LoadingException | RecordBusyException | FileNotFoundException | ISCInvalidRequestException | NotAuthorisedException | RecordNotFoundException
						e3) {
					logger.severe("Error starting browse of CUSTOMER file, " + e3.getLocalizedMessage());
					logger.exiting(this.getClass().getName(),"getCustomersByNameCountOnly(int sortCode, String name)",-1L);
					return -1L;
				}
				catch (InvalidSystemIdException e4)
				{
				} 

			}
			if(number_of_retries == maximum_retries && success == false)
			{
				logger.severe("Cannot read CUSTOMER file after 100 attempts");
				logger.exiting(this.getClass().getName(),"getCustomersByNameCountOnly(int sortCode, String name)",-1L);
				return -1L;
			}
		}
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
			catch (LogicException | InvalidRequestException
					| IOErrorException 
					| ChangedException | LockedException | LoadingException
					| RecordBusyException | FileDisabledException
					| FileNotFoundException
					| ISCInvalidRequestException | NotAuthorisedException
					| RecordNotFoundException | NotOpenException | LengthErrorException e) {
				logger.severe("Error browsing CUSTOMER file, " + e.getLocalizedMessage());
				logger.exiting(this.getClass().getName(),"getCustomersByNameCountOnly(int sortCode, String name)",-1L);
				return -1L;
			}
			catch (InvalidSystemIdException  e1) {
				int number_of_retries = 0;
				boolean success;
				for(number_of_retries = 0,success = false; number_of_retries < maximum_retries && success == false;number_of_retries++)
				{
					try {
						logger.fine("About to go to sleep for " + totalSleep + " milliseconds");
						Thread.sleep(3000);
					}
					 catch (InterruptedException e) {
						}
					try {
						customerFileBrowse.next(holder, keyHolder);
						success = true;

					} catch (DuplicateKeyException | LengthErrorException | FileDisabledException | NotOpenException | LogicException | InvalidRequestException | IOErrorException  | ChangedException | LockedException | LoadingException | RecordBusyException | FileNotFoundException | ISCInvalidRequestException | NotAuthorisedException | RecordNotFoundException
							e3) {
						logger.severe("Error browsing CUSTOMER file, " + e3.getLocalizedMessage());
						logger.exiting(this.getClass().getName(),"getCustomersByNameCountOnly(int sortCode, String name)",-1L);
						return -1L;
					}
					catch (InvalidSystemIdException e4)
					{
					}
					catch (EndOfFileException e4)
					{
						success = true;
						carryOn = false;
						endOfFile = true;
					}

				}
				if(number_of_retries == maximum_retries && success == false)
				{
					logger.severe("Cannot browse CUSTOMER file after 100 attempts");
					logger.exiting(this.getClass().getName(),"getCustomersByNameCountOnly(int sortCode, String name)",-1L);
					return -1L;
				}

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
		try {
			customerFileBrowse.end();
		} catch (LogicException | InvalidRequestException | FileDisabledException
				| FileNotFoundException | ISCInvalidRequestException | NotAuthorisedException
				| NotOpenException e) {
			logger.severe("Error ending browse of CUSTOMER file "  + e.getLocalizedMessage());
			logger.exiting(this.getClass().getName(),"getCustomersByNameCountOnly(int sortCode, String name)",-1L);
			return -1L;
		}
		catch (InvalidSystemIdException  e2) {
			int number_of_retries = 0;
			boolean success;
			for(number_of_retries = 0,success = false; number_of_retries < maximum_retries && success == false;number_of_retries++)
			{
				try {
					logger.fine("About to go to sleep for " + totalSleep + " milliseconds");
					Thread.sleep(3000);
				} catch (InterruptedException e) {
				} 
				try {
					customerFileBrowse.end();
					success = true;

				} catch (FileDisabledException | NotOpenException | LogicException | InvalidRequestException | FileNotFoundException | ISCInvalidRequestException | NotAuthorisedException
						e3) {
					logger.fine("Error ending browse of CUSTOMER file, " + e3.getLocalizedMessage());
					logger.exiting(this.getClass().getName(),"getCustomersByNameCountOnly(int sortCode, String name)",-1L);
					return -1L;
				}
				catch (InvalidSystemIdException e4)
				{
				} 

			}
			if(number_of_retries == maximum_retries && success == false)
			{
				logger.severe("Cannot browse CUSTOMER file after 100 attempts");
				logger.exiting(this.getClass().getName(),"getCustomersByNameCountOnly(int sortCode, String name)",-1L);
				return -1L;
			}
		}

		logger.exiting(this.getClass().getName(),"getCustomersByNameCountOnly(int sortCode, String name)",matchingCustomers);
		return matchingCustomers;

	}

	private void sortOutLogging()
	{
		try {
			LogManager.getLogManager().readConfiguration();
		} catch (SecurityException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	public Customer[] getCustomersByTown(String town) {
		logger.entering(this.getClass().getName(),"getCustomersbyTown(String town)",null);
		Customer[] temp = new Customer[250000];
		int i = 0;


		KSDS customerFile = new KSDS();
		customerFile.setName("CUSTOMER");

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
			key = keyString.getBytes("Cp1047");
		} catch (UnsupportedEncodingException e2) {
			logger.severe(e2.getLocalizedMessage());
			logger.exiting(this.getClass().getName(),"getCustomers(int sortCode)",null);
			return null;
		}

		KeyedFileBrowse customerFileBrowse = null;
		try {
			customerFileBrowse = customerFile.startBrowse(key);
		} catch (LogicException | InvalidRequestException | IOErrorException
				| LockedException | RecordBusyException | LoadingException | ChangedException | FileDisabledException
				| FileNotFoundException | ISCInvalidRequestException | NotAuthorisedException | RecordNotFoundException
				| NotOpenException e1) {
			logger.severe("Error starting browse of customer " + e1.getLocalizedMessage());
			logger.exiting(this.getClass().getName(),"getCustomers(int sortCode)",null);
			return null;
		}
		catch (InvalidSystemIdException  e1) {
			int number_of_retries = 0;
			boolean success;
			for(number_of_retries = 0,success = false; number_of_retries < maximum_retries && success == false;number_of_retries++)
			{
				try {
					logger.fine("About to sleep for " + totalSleep + " milliseconds");
					Thread.sleep(3000);
				}  catch (InterruptedException e) {
				} 				try {
					customerFileBrowse = customerFile.startBrowse(key);
					success = true;

				} catch (FileDisabledException | NotOpenException | LogicException | InvalidRequestException | IOErrorException  | ChangedException | LockedException | LoadingException | RecordBusyException | FileNotFoundException | ISCInvalidRequestException | NotAuthorisedException | RecordNotFoundException
						e3) {
					logger.severe("Error starting browse of CUSTOMER file, " + e3.getLocalizedMessage());
					logger.exiting(this.getClass().getName(),"getCustomers(int sortCode)",null);
					return null;
				}
				catch (InvalidSystemIdException e4)
				{
				} 

			}
			if(number_of_retries == maximum_retries && success == false)
			{
				logger.severe("Cannot read CUSTOMER file after 100 attempts");
				logger.exiting(this.getClass().getName(),"getCustomers(int sortCode)",null);
				return null;
			}
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
			catch (LogicException | InvalidRequestException
					| IOErrorException 
					| ChangedException | LockedException | LoadingException
					| RecordBusyException | FileDisabledException
					| FileNotFoundException
					| ISCInvalidRequestException | NotAuthorisedException
					| RecordNotFoundException | NotOpenException | LengthErrorException e) {
				logger.severe("Error browsing CUSTOMER file, " + e.getLocalizedMessage());
				logger.exiting(this.getClass().getName(),"getCustomers(int sortCode)",null);
				return null;
			}
			catch (InvalidSystemIdException  e1) {
				int number_of_retries = 0;
				boolean success;
				for(number_of_retries = 0,success = false; number_of_retries < maximum_retries && success == false;number_of_retries++)
				{
					try {
						logger.fine("About to go to sleep for " + totalSleep + " milliseconds");
						Thread.sleep(3000);
					} catch (InterruptedException e) {
					}					try {
						customerFileBrowse.next(holder, keyHolder);
						success = true;

					} catch (DuplicateKeyException | LengthErrorException | FileDisabledException | NotOpenException | LogicException | InvalidRequestException | IOErrorException  | ChangedException | LockedException | LoadingException | RecordBusyException | FileNotFoundException | ISCInvalidRequestException | NotAuthorisedException | RecordNotFoundException
							e3) {
						logger.severe(e3.getLocalizedMessage());
						logger.exiting(this.getClass().getName(),"getCustomers(int sortCode)",null);
						return null;
					}
					catch (InvalidSystemIdException e4)
					{
					}
					catch (EndOfFileException e4)
					{
						success = true;
						carryOn = false;
					}

				}
				if(number_of_retries == maximum_retries && success == false)
				{
					logger.severe("Cannot browse CUSTOMER file after 100 attempts");
					logger.exiting(this.getClass().getName(),"getCustomers(int sortCode)",null);
					return null;
				}

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
		try {
			customerFileBrowse.end();
		} catch (LogicException | InvalidRequestException | FileDisabledException
				| FileNotFoundException | ISCInvalidRequestException | NotAuthorisedException
				| NotOpenException e) {
			logger.severe("Error ending browse of CUSTOMER file "  + e.getLocalizedMessage());
			logger.exiting(this.getClass().getName(),"getCustomers(int sortCode)",null);
			return null;
		}
		catch (InvalidSystemIdException  e2) {
			int number_of_retries = 0;
			boolean success;
			for(number_of_retries = 0,success = false; number_of_retries < maximum_retries && success == false;number_of_retries++)
			{
				try {
					logger.fine("About to go to sleep for " + totalSleep + " milliseconds");
					Thread.sleep(3000);
				} catch (InterruptedException e) {
					// TODO Auto-generated catch block
				} 
				try {
					customerFileBrowse.end();
					success = true;
				} catch (FileDisabledException | NotOpenException | LogicException | InvalidRequestException | FileNotFoundException | ISCInvalidRequestException | NotAuthorisedException
						e3) {
					logger.severe("Error ending browse of CUSTOMER file, " + e3.getLocalizedMessage());
					logger.exiting(this.getClass().getName(),"getCustomers(int sortCode)",null);
					return null;
				}
				catch (InvalidSystemIdException e4)
				{
				} 

			}
			if(number_of_retries == maximum_retries && success == false)
			{
				logger.severe("Cannot browse CUSTOMER file after 100 attempts");
				logger.exiting(this.getClass().getName(),"getCustomers(int sortCode)",null);
				return null;
			}
		}

		Customer[] real = new Customer[i];
		for(int j=0;j<i;j++)
		{
			real[j] = temp[j];	
		}
		logger.exiting(this.getClass().getName(),"getCustomers(int sortCode)",real);
		return real;
	}

	public Customer[] getCustomersBySurname(String surname) {
		logger.entering(this.getClass().getName(),"getCustomersbySurname(String surname)",null);
		Customer[] temp = new Customer[250000];
		int i = 0;


		KSDS customerFile = new KSDS();
		customerFile.setName("CUSTOMER");

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
			key = keyString.getBytes("Cp1047");
		} catch (UnsupportedEncodingException e2) {
			logger.severe(e2.getLocalizedMessage());
			logger.exiting(this.getClass().getName(),"getCustomers(int sortCode)",null);
			return null;
		}

		KeyedFileBrowse customerFileBrowse = null;
		try {
			customerFileBrowse = customerFile.startBrowse(key);
		} catch (LogicException | InvalidRequestException | IOErrorException
				| LockedException | RecordBusyException | LoadingException | ChangedException | FileDisabledException
				| FileNotFoundException | ISCInvalidRequestException | NotAuthorisedException | RecordNotFoundException
				| NotOpenException e1) {
			logger.severe("Error starting browse of customer " + e1.getLocalizedMessage());
			logger.exiting(this.getClass().getName(),"getCustomers(int sortCode)",null);
			return null;
		}
		catch (InvalidSystemIdException  e1) {
			int number_of_retries = 0;
			boolean success;
			for(number_of_retries = 0,success = false; number_of_retries < maximum_retries && success == false;number_of_retries++)
			{
				try {
					logger.fine("About to sleep for " + totalSleep + " milliseconds");
					Thread.sleep(3000);
				}  catch (InterruptedException e) {
				} 				try {
					customerFileBrowse = customerFile.startBrowse(key);
					success = true;

				} catch (FileDisabledException | NotOpenException | LogicException | InvalidRequestException | IOErrorException  | ChangedException | LockedException | LoadingException | RecordBusyException | FileNotFoundException | ISCInvalidRequestException | NotAuthorisedException | RecordNotFoundException
						e3) {
					logger.severe("Error starting browse of CUSTOMER file, " + e3.getLocalizedMessage());
					logger.exiting(this.getClass().getName(),"getCustomers(int sortCode)",null);
					return null;
				}
				catch (InvalidSystemIdException e4)
				{
				} 

			}
			if(number_of_retries == maximum_retries && success == false)
			{
				logger.severe("Cannot read CUSTOMER file after 100 attempts");
				logger.exiting(this.getClass().getName(),"getCustomers(int sortCode)",null);
				return null;
			}
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
			catch (LogicException | InvalidRequestException
					| IOErrorException 
					| ChangedException | LockedException | LoadingException
					| RecordBusyException | FileDisabledException
					| FileNotFoundException
					| ISCInvalidRequestException | NotAuthorisedException
					| RecordNotFoundException | NotOpenException | LengthErrorException e) {
				logger.severe("Error browsing CUSTOMER file, " + e.getLocalizedMessage());
				logger.exiting(this.getClass().getName(),"getCustomers(int sortCode)",null);
				return null;
			}
			catch (InvalidSystemIdException  e1) {
				int number_of_retries = 0;
				boolean success;
				for(number_of_retries = 0,success = false; number_of_retries < maximum_retries && success == false;number_of_retries++)
				{
					try {
						logger.fine("About to go to sleep for " + totalSleep + " milliseconds");
						Thread.sleep(3000);
					} catch (InterruptedException e) {
					}					try {
						customerFileBrowse.next(holder, keyHolder);
						success = true;

					} catch (DuplicateKeyException | LengthErrorException | FileDisabledException | NotOpenException | LogicException | InvalidRequestException | IOErrorException  | ChangedException | LockedException | LoadingException | RecordBusyException | FileNotFoundException | ISCInvalidRequestException | NotAuthorisedException | RecordNotFoundException
							e3) {
						logger.severe(e3.getLocalizedMessage());
						logger.exiting(this.getClass().getName(),"getCustomers(int sortCode)",null);
						return null;
					}
					catch (InvalidSystemIdException e4)
					{
					}
					catch (EndOfFileException e4)
					{
						success = true;
						carryOn = false;
					}

				}
				if(number_of_retries == maximum_retries && success == false)
				{
					logger.severe("Cannot browse CUSTOMER file after 100 attempts");
					logger.exiting(this.getClass().getName(),"getCustomers(int sortCode)",null);
					return null;
				}

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
		try {
			customerFileBrowse.end();
		} catch (LogicException | InvalidRequestException | FileDisabledException
				| FileNotFoundException | ISCInvalidRequestException | NotAuthorisedException
				| NotOpenException e) {
			logger.severe("Error ending browse of CUSTOMER file "  + e.getLocalizedMessage());
			logger.exiting(this.getClass().getName(),"getCustomers(int sortCode)",null);
			return null;
		}
		catch (InvalidSystemIdException  e2) {
			int number_of_retries = 0;
			boolean success;
			for(number_of_retries = 0,success = false; number_of_retries < maximum_retries && success == false;number_of_retries++)
			{
				try {
					logger.fine("About to go to sleep for " + totalSleep + " milliseconds");
					Thread.sleep(3000);
				} catch (InterruptedException e) {
					// TODO Auto-generated catch block
				} 
				try {
					customerFileBrowse.end();
					success = true;
				} catch (FileDisabledException | NotOpenException | LogicException | InvalidRequestException | FileNotFoundException | ISCInvalidRequestException | NotAuthorisedException
						e3) {
					logger.severe("Error ending browse of CUSTOMER file, " + e3.getLocalizedMessage());
					logger.exiting(this.getClass().getName(),"getCustomers(int sortCode)",null);
					return null;
				}
				catch (InvalidSystemIdException e4)
				{
				} 

			}
			if(number_of_retries == maximum_retries && success == false)
			{
				logger.severe("Cannot browse CUSTOMER file after 100 attempts");
				logger.exiting(this.getClass().getName(),"getCustomers(int sortCode)",null);
				return null;
			}
		}

		Customer[] real = new Customer[i];
		for(int j=0;j<i;j++)
		{
			real[j] = temp[j];	
		}
		logger.exiting(this.getClass().getName(),"getCustomers(int sortCode)",real);
		return real;
	}

	public Customer[] getCustomersByAge(int age) {
		logger.entering(this.getClass().getName(),"getCustomersbyAge(int age)",null);
		Customer[] temp = new Customer[250000];
		int i = 0;


		KSDS customerFile = new KSDS();
		customerFile.setName("CUSTOMER");

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
			key = keyString.getBytes("Cp1047");
		} catch (UnsupportedEncodingException e2) {
			logger.severe(e2.getLocalizedMessage());
			logger.exiting(this.getClass().getName(),"getCustomers(int sortCode)",null);
			return null;
		}

		KeyedFileBrowse customerFileBrowse = null;
		try {
			customerFileBrowse = customerFile.startBrowse(key);
		} catch (LogicException | InvalidRequestException | IOErrorException
				| LockedException | RecordBusyException | LoadingException | ChangedException | FileDisabledException
				| FileNotFoundException | ISCInvalidRequestException | NotAuthorisedException | RecordNotFoundException
				| NotOpenException e1) {
			logger.severe("Error starting browse of customer " + e1.getLocalizedMessage());
			logger.exiting(this.getClass().getName(),"getCustomers(int sortCode)",null);
			return null;
		}
		catch (InvalidSystemIdException  e1) {
			int number_of_retries = 0;
			boolean success;
			for(number_of_retries = 0,success = false; number_of_retries < maximum_retries && success == false;number_of_retries++)
			{
				try {
					logger.fine("About to sleep for " + totalSleep + " milliseconds");
					Thread.sleep(3000);
				}  catch (InterruptedException e) {
				} 				try {
					customerFileBrowse = customerFile.startBrowse(key);
					success = true;

				} catch (FileDisabledException | NotOpenException | LogicException | InvalidRequestException | IOErrorException  | ChangedException | LockedException | LoadingException | RecordBusyException | FileNotFoundException | ISCInvalidRequestException | NotAuthorisedException | RecordNotFoundException
						e3) {
					logger.severe("Error starting browse of CUSTOMER file, " + e3.getLocalizedMessage());
					logger.exiting(this.getClass().getName(),"getCustomers(int sortCode)",null);
					return null;
				}
				catch (InvalidSystemIdException e4)
				{
				} 

			}
			if(number_of_retries == maximum_retries && success == false)
			{
				logger.severe("Cannot read CUSTOMER file after 100 attempts");
				logger.exiting(this.getClass().getName(),"getCustomers(int sortCode)",null);
				return null;
			}
		}
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
			catch (LogicException | InvalidRequestException
					| IOErrorException 
					| ChangedException | LockedException | LoadingException
					| RecordBusyException | FileDisabledException
					| FileNotFoundException
					| ISCInvalidRequestException | NotAuthorisedException
					| RecordNotFoundException | NotOpenException | LengthErrorException e) {
				logger.severe("Error browsing CUSTOMER file, " + e.getLocalizedMessage());
				logger.exiting(this.getClass().getName(),"getCustomers(int sortCode)",null);
				return null;
			}
			catch (InvalidSystemIdException  e1) {
				int number_of_retries = 0;
				boolean success;
				for(number_of_retries = 0,success = false; number_of_retries < maximum_retries && success == false;number_of_retries++)
				{
					try {
						logger.fine("About to go to sleep for " + totalSleep + " milliseconds");
						Thread.sleep(3000);
					} catch (InterruptedException e) {
					}					try {
						customerFileBrowse.next(holder, keyHolder);
						success = true;

					} catch (DuplicateKeyException | LengthErrorException | FileDisabledException | NotOpenException | LogicException | InvalidRequestException | IOErrorException  | ChangedException | LockedException | LoadingException | RecordBusyException | FileNotFoundException | ISCInvalidRequestException | NotAuthorisedException | RecordNotFoundException
							e3) {
						logger.severe(e3.getLocalizedMessage());
						logger.exiting(this.getClass().getName(),"getCustomers(int sortCode)",null);
						return null;
					}
					catch (InvalidSystemIdException e4)
					{
					}
					catch (EndOfFileException e4)
					{
						success = true;
						carryOn = false;
					}

				}
				if(number_of_retries == maximum_retries && success == false)
				{
					logger.severe("Cannot browse CUSTOMER file after 100 attempts");
					logger.exiting(this.getClass().getName(),"getCustomers(int sortCode)",null);
					return null;
				}

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
		try {
			customerFileBrowse.end();
		} catch (LogicException | InvalidRequestException | FileDisabledException
				| FileNotFoundException | ISCInvalidRequestException | NotAuthorisedException
				| NotOpenException e) {
			logger.severe("Error ending browse of CUSTOMER file "  + e.getLocalizedMessage());
			logger.exiting(this.getClass().getName(),"getCustomers(int sortCode)",null);
			return null;
		}
		catch (InvalidSystemIdException  e2) {
			int number_of_retries = 0;
			boolean success;
			for(number_of_retries = 0,success = false; number_of_retries < maximum_retries && success == false;number_of_retries++)
			{
				try {
					logger.fine("About to go to sleep for " + totalSleep + " milliseconds");
					Thread.sleep(3000);
				} catch (InterruptedException e) {
					// TODO Auto-generated catch block
				} 
				try {
					customerFileBrowse.end();
					success = true;
				} catch (FileDisabledException | NotOpenException | LogicException | InvalidRequestException | FileNotFoundException | ISCInvalidRequestException | NotAuthorisedException
						e3) {
					logger.severe("Error ending browse of CUSTOMER file, " + e3.getLocalizedMessage());
					logger.exiting(this.getClass().getName(),"getCustomers(int sortCode)",null);
					return null;
				}
				catch (InvalidSystemIdException e4)
				{
				} 

			}
			if(number_of_retries == maximum_retries && success == false)
			{
				logger.severe("Cannot browse CUSTOMER file after 100 attempts");
				logger.exiting(this.getClass().getName(),"getCustomers(int sortCode)",null);
				return null;
			}
		}

		Customer[] real = new Customer[i];
		for(int j=0;j<i;j++)
		{
			real[j] = temp[j];	
		}
		logger.exiting(this.getClass().getName(),"getCustomers(int sortCode)",real);
		return real;
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
			age=age--;
			return age;
		}
		if(birthCalendar.get(Calendar.MONTH) == nowCalendar.get(Calendar.MONTH))
		{
			if (birthCalendar.get(Calendar.DAY_OF_MONTH) > nowCalendar.get(Calendar.DAY_OF_MONTH))
			{
				age=age--;
				return age;
			}
			
		}
		
		return age;
		
	}
}
