/*
 *
 *    Copyright IBM Corp. 2022
 *
 */
package com.ibm.cics.cip.bankliberty.webui.data_access;



import java.io.IOException;

import java.sql.Date;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.LogManager;
import java.util.logging.Logger;

import javax.servlet.ServletException;

import javax.ws.rs.core.Response;

import com.ibm.cics.cip.bankliberty.api.json.CustomerResource;
import com.ibm.cics.cip.bankliberty.api.json.SortCodeResource;
import com.ibm.cics.cip.bankliberty.webui.data_access.Customer;
import com.ibm.json.java.JSONArray;
import com.ibm.json.java.JSONObject;



public class CustomerList {

    static final String COPYRIGHT =
      "Copyright IBM Corp. 2022";
    
    private static Logger logger = Logger.getLogger("com.ibm.cics.cip.bankliberty.webui.dataAccess");



	private List<Customer> customerList = new ArrayList<Customer>();

	private static String sortcode = null;
	private int count;
	
	private static final String JSON_SORT_CODE = "sortCode";
	private static final String JSON_ID = "id";
	private static final String JSON_CUSTOMER_NAME = "customerName";
	private static final String JSON_CUSTOMER_ADDRESS = "customerAddress";
	private static final String JSON_CUSTOMER_CREDIT_SCORE = "customerCreditScore";
	private static final String JSON_CUSTOMER_REVIEW_DATE = "customerCreditScoreReviewDate";
	private static final String JSON_DATE_OF_BIRTH = "dateOfBirth";
	private static final String JSON_CUSTOMERS = "customers";
	private static final String JSON_NUMBER_OF_CUSTOMERS = "numberOfCustomers";


	public int getCount(String filter){
		if(this.customerList.isEmpty())
		{
			howMany(filter);
		}
		return this.count;
	}

	private void howMany(String filter){
		//TEST.out.println("TEST - Getting count of Customers");
		CustomerResource myCustomerResource = new CustomerResource();
		Response myCustomerResponse = null;

		//                0123456789012345678901234
		if(filter.startsWith(" AND CUSTOMER_NAME like '"))
		{
			//01234567890123456789012345678901234567890
			// AND ACCOUNT_AVAILABLE_BALANCE <= 33558.0

			String customerNameFilter = filter.substring(25);
			customerNameFilter = customerNameFilter.substring(0,customerNameFilter.length() -1);

			myCustomerResponse = myCustomerResource.getCustomersByNameExternal(customerNameFilter,0,0,true);
			String myCustomersString = myCustomerResponse.getEntity().toString();
			JSONObject myCustomersJSON;
			try 
			{
				myCustomersJSON = JSONObject.parse(myCustomersString);
				long customerCount = (Long) myCustomersJSON.get(JSON_NUMBER_OF_CUSTOMERS);
				this.count = (int)customerCount;
			} 
			catch (IOException e) 
			{
				logger.severe(e.toString());
			}
		}

		//                    01234567890123456789012
		if(filter.startsWith(" AND CUSTOMER_NUMBER = "))
		{
			String customerNumberFilter = filter.substring(23);
			Long customerNumber = new Long(customerNumberFilter);

			myCustomerResponse = myCustomerResource.getCustomerExternal(customerNumber);
			String myCustomersString = myCustomerResponse.getEntity().toString();
			JSONObject myCustomerJSON;
			if(myCustomerResponse.getStatus() == 200)
			{
				try 
				{
					myCustomerJSON = JSONObject.parse(myCustomersString);
					Date myDate = sortOutDate((String)myCustomerJSON.get(JSON_DATE_OF_BIRTH));
					String id = (String)myCustomerJSON.get(JSON_ID);
					if(id != null)
					{
						this.count = 1;
					}
				} 
				catch (IOException e) 
				{
					logger.severe(e.toString());
				}
			}
			else this.count= 0;
		}

		if(filter.length() ==  0)
		{
			//("No filter so get the lot please!");
			myCustomerResponse = myCustomerResource.getCustomersExternal(new Integer(250000), new Integer(0),true);
			String myCustomersString = myCustomerResponse.getEntity().toString();
			if(myCustomerResponse.getStatus() == 200)
			{
				JSONObject myCustomersJSON;
				try 
				{
					myCustomersJSON = JSONObject.parse(myCustomersString);
					long customerCount = (Long) myCustomersJSON.get(JSON_NUMBER_OF_CUSTOMERS);
					this.count = (int)customerCount;
				} 
				catch (IOException e) 
				{
					logger.severe(e.toString());
				}
			}
			else
			{
				System.err.println(myCustomerResponse.getStatus() + " " + myCustomersString);
			}
		}
	}


	public void doGet(int limit, int offset, String filter) throws ServletException, IOException {
		
		
		CustomerResource myCustomerResource = new CustomerResource();

		Response myCustomerResponse = null;

		String myCustomerString = null;
 

		if(filter.length() == 0)
		{
			try {
				myCustomerResponse = myCustomerResource.getCustomersExternal(limit,offset,false);

				if(myCustomerResponse.getStatus() == 200)
				{
					myCustomerString = myCustomerResponse.getEntity().toString();
					this.customerList.clear();
								
					JSONObject myCustomersJSON = JSONObject.parse(myCustomerString);
					JSONArray myCustomersArrayJSON = (JSONArray) myCustomersJSON.get(JSON_CUSTOMERS);
					long customerCount = myCustomersArrayJSON.size();
					for(int i = 0;i < customerCount;i++)
					{
						JSONObject myCustomer = (JSONObject) myCustomersArrayJSON.get(i);
						Date dateOfBirth = sortOutDate((String)myCustomer.get(JSON_DATE_OF_BIRTH));
						Date creditScoreReviewDate = sortOutDate((String)myCustomer.get(JSON_CUSTOMER_REVIEW_DATE));

						String id = (String)myCustomer.get(JSON_ID);
						
						Customer myListCustomer = new Customer(id, 
								(String)myCustomer.get(JSON_SORT_CODE),
								(String)myCustomer.get(JSON_CUSTOMER_NAME),
								(String)myCustomer.get(JSON_CUSTOMER_ADDRESS), 
								dateOfBirth, (String)myCustomer.get(JSON_CUSTOMER_CREDIT_SCORE), 
								creditScoreReviewDate);

						
					
						this.customerList.add(myListCustomer);

					}
				}
				else
				{
					logger.severe(myCustomerResponse.getStatus() + " " + myCustomerResponse.getEntity().toString());
				}
			} 
			catch (IOException e1) 
			{
				logger.severe(e1.toString());
			}
		}
		if(filter.startsWith(" AND CUSTOMER_NUMBER = "))
		{

			this.customerList.clear();
			String customerNumberFilter = filter.substring(23);
			Long customerNumber = new Long(customerNumberFilter);

			myCustomerResponse = myCustomerResource.getCustomerExternal(customerNumber);
			String myCustomersString = myCustomerResponse.getEntity().toString();
			JSONObject myCustomerJSON;
			if(myCustomerResponse.getStatus() == 200)
			{
				try {
					myCustomerJSON = JSONObject.parse(myCustomersString);
										
					Date dateOfBirth = sortOutDate((String)myCustomerJSON.get(JSON_DATE_OF_BIRTH));
					String id = (String)myCustomerJSON.get(JSON_ID);
									
					Date creditScoreReviewDate = sortOutDate((String)myCustomerJSON.get(JSON_CUSTOMER_REVIEW_DATE));
					
					Customer myListCustomer = new Customer(id, (String)myCustomerJSON.get(JSON_SORT_CODE),
							(String)myCustomerJSON.get(JSON_CUSTOMER_NAME),(String)myCustomerJSON.get(JSON_CUSTOMER_ADDRESS), dateOfBirth, (String)myCustomerJSON.get(JSON_CUSTOMER_CREDIT_SCORE), creditScoreReviewDate);

					this.customerList.add(myListCustomer);
				} 
				catch (IOException e) 
				{
					logger.severe(e.toString());
				}
			}
			else this.count = 0;
		}

		
		if(filter.startsWith(" AND CUSTOMER_NAME like '"))
		{
			String customerNameFilter = filter.substring(25);
			customerNameFilter = customerNameFilter.substring(0,customerNameFilter.length() -1);

			myCustomerResponse = myCustomerResource.getCustomersByNameExternal(customerNameFilter,limit,offset,false);
			myCustomerString = myCustomerResponse.getEntity().toString();
			if(myCustomerResponse.getStatus() == 200)
			{
				try {
					this.customerList.clear();
					JSONObject myCustomersJSON = JSONObject.parse(myCustomerString);
					JSONArray myCustomersArrayJSON = (JSONArray) myCustomersJSON.get(JSON_CUSTOMERS);
					long customerCount = myCustomersArrayJSON.size();
					for(int i = 0;i < customerCount;i++)
					{
						JSONObject myCustomer = (JSONObject) myCustomersArrayJSON.get(i);
						Date myDate = sortOutDate((String)myCustomer.get(JSON_DATE_OF_BIRTH));
						String id = (String)myCustomer.get(JSON_ID);
						Date creditScoreReviewDate = sortOutDate((String)myCustomer.get(JSON_CUSTOMER_REVIEW_DATE));

						Customer myListCustomer = new Customer(id, (String)myCustomer.get(JSON_SORT_CODE),
								(String)myCustomer.get(JSON_CUSTOMER_NAME),(String)myCustomer.get(JSON_CUSTOMER_ADDRESS), myDate, (String)myCustomer.get(JSON_CUSTOMER_CREDIT_SCORE), creditScoreReviewDate);
						
						this.customerList.add(myListCustomer);
					}
				} 
				catch (IOException e1) 
				{
					logger.severe(e1.toString());
				}
				
			}
			else
			{
				logger.severe(myCustomerResponse.getStatus() + " " + myCustomerString);
			}
		}



		if(offset == 0)
		{
			howMany(filter);
		}

	}

	private Date sortOutDate(String dateString) 
	{
		String[] dateArray = dateString.split("-");

		Integer year = new Integer(dateArray[0]);
		Integer month = new Integer(dateArray[1]);
		Integer day = new Integer(dateArray[2]);

		Date sortedOutDate = new Date(year.intValue() - 1900,month.intValue() - 1, day.intValue());
		return sortedOutDate;
	}



	public Customer getCustomer(int i){
		return this.customerList.get(i);
	}
	public int size(){
		return this.customerList.size();
	}
	public CustomerList()
	{
		sortOutLogging();
		CustomerList.setSortcode();
	}


	private static void setSortcode(){
		if(sortcode == null)
		{
			SortCodeResource mySortCodeResource = new SortCodeResource();
			Response mySortCodeJSON = mySortCodeResource.getSortCode();
			sortcode = ((String) mySortCodeJSON.getEntity()).substring(13, 19);
		}

	}

	public List<Customer> getList(){
		return customerList;
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
