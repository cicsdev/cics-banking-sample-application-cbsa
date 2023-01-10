/*
 *
 *    Copyright IBM Corp. 2022
 *
 */
package com.ibm.cics.cip.bankliberty.webui.dataAccess;



import java.io.IOException;

import java.sql.Date;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.LogManager;
import java.util.logging.Logger;

import javax.servlet.ServletException;

import javax.ws.rs.core.Response;

import com.ibm.cics.cip.bankliberty.webui.dataAccess.Customer;

import com.ibm.cics.cip.bankliberty.api.json.CustomerResource;
import com.ibm.cics.cip.bankliberty.api.json.SortCodeResource;

import com.ibm.json.java.JSONArray;
import com.ibm.json.java.JSONObject;



public class CustomerList {

    static final String COPYRIGHT =
      "Copyright IBM Corp. 2022";
    
    private static Logger logger = Logger.getLogger("com.ibm.cics.cip.bankliberty.webui.dataAccess");



	private List<Customer> customerList = new ArrayList<Customer>();

	private static String sortcode = null;
	private int count;


	public int getCount(String filter){
		if(this.customerList.isEmpty())
		{
			howMany(filter);
		}
		return this.count;
	}

	@SuppressWarnings("unused")
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
				long customerCount = (Long) myCustomersJSON.get("numberOfCustomers");
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
					Date myDate = sortOutDate((String)myCustomerJSON.get("dateOfBirth"));
					String id = (String)myCustomerJSON.get("id");
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
					long customerCount = (Long) myCustomersJSON.get("numberOfCustomers");
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
		
		//TEST.out.println("TEST - Doing get with filter offset and limit");
		
		CustomerResource myCustomerResource = new CustomerResource();

		Response myCustomerResponse = null;

		String myCustomerString = null;
 
		//TEST.out.println("TEST - is the length of the filter 0? " + (filter.length()==0) + " what actually is it? " + filter.length() );

		if(filter.length() == 0)
		{
			try {
				//TEST.out.println("1");
				myCustomerResponse = myCustomerResource.getCustomersExternal(limit,offset,false);
				//TEST.out.println("2");

				if(myCustomerResponse.getStatus() == 200)
				{
					myCustomerString = myCustomerResponse.getEntity().toString();
					this.customerList.clear();
								
					JSONObject myCustomersJSON = JSONObject.parse(myCustomerString);
					JSONArray myCustomersArrayJSON = (JSONArray) myCustomersJSON.get("customers");
					long customerCount = myCustomersArrayJSON.size();
					for(int i = 0;i < customerCount;i++)
					{
						JSONObject myCustomer = (JSONObject) myCustomersArrayJSON.get(i);
						Date dateOfBirth = sortOutDate((String)myCustomer.get("dateOfBirth"));
						Date creditScoreReviewDate = sortOutDate((String)myCustomer.get("customerCreditScoreReviewDate"));

						String id = (String)myCustomer.get("id");
						
						Customer myListCustomer = new Customer(id, 
								(String)myCustomer.get("sortCode"),
								(String)myCustomer.get("customerName"),
								(String)myCustomer.get("customerAddress"), 
								dateOfBirth, (String)myCustomer.get("customerCreditScore"), 
								creditScoreReviewDate);

						
						//TEST REMOVE
						////TEST.out.println("Full List from Customer Resource: CUSTOMER NUMBER" + myListCustomer.getCustomer_number());
						
						this.customerList.add(myListCustomer);

					}
				}
				else
				{
					System.err.println(myCustomerResponse.getStatus() + " " + myCustomerResponse.getEntity().toString());
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
										
					Date dateOfBirth = sortOutDate((String)myCustomerJSON.get("dateOfBirth"));
					String id = (String)myCustomerJSON.get("id");
									
					Date creditScoreReviewDate = sortOutDate((String)myCustomerJSON.get("customerCreditScoreReviewDate"));
					
					Customer myListCustomer = new Customer(id, (String)myCustomerJSON.get("sortCode"),
							(String)myCustomerJSON.get("customerName"),(String)myCustomerJSON.get("customerAddress"), dateOfBirth, (String)myCustomerJSON.get("customerCreditScore"), creditScoreReviewDate);

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

			//01234567890123456789012345678901234567890
			// AND ACCOUNT_AVAILABLE_BALANCE <= 33558.0

			String customerNameFilter = filter.substring(25);
			customerNameFilter = customerNameFilter.substring(0,customerNameFilter.length() -1);

			myCustomerResponse = myCustomerResource.getCustomersByNameExternal(customerNameFilter,limit,offset,false);
			myCustomerString = myCustomerResponse.getEntity().toString();
			if(myCustomerResponse.getStatus() == 200)
			{
				try {
					this.customerList.clear();
					JSONObject myCustomersJSON = JSONObject.parse(myCustomerString);
					JSONArray myCustomersArrayJSON = (JSONArray) myCustomersJSON.get("customers");
					long customerCount = myCustomersArrayJSON.size();
					for(int i = 0;i < customerCount;i++)
					{
						JSONObject myCustomer = (JSONObject) myCustomersArrayJSON.get(i);
						Date myDate = sortOutDate((String)myCustomer.get("dateOfBirth"));
						String id = (String)myCustomer.get("id");
						Date creditScoreReviewDate = sortOutDate((String)myCustomer.get("customerCreditScoreReviewDate"));

						Customer myListCustomer = new Customer(id, (String)myCustomer.get("sortCode"),
								(String)myCustomer.get("customerName"),(String)myCustomer.get("customerAddress"), myDate, (String)myCustomer.get("customerCreditScore"), creditScoreReviewDate);
						
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
				System.err.println(myCustomerResponse.getStatus() + " " + myCustomerString);
			}
		}



		if(offset == 0){
			howMany(filter);
		}

	}
	@SuppressWarnings("deprecation")
	private Date sortOutDate(String dateString) {
		String[] dateArray = dateString.split("-");

		Integer year = new Integer(dateArray[0]);
		Integer month = new Integer(dateArray[1]);
		Integer day = new Integer(dateArray[2]);

		Date sortedOutDate = new Date(year.intValue() - 1900,month.intValue() - 1, day.intValue());
		return sortedOutDate;
	}

	@SuppressWarnings("unused")
	private String getSortCode() {
		// TODO Auto-generated method stub
		return sortcode;
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
		setSortcode();
	}


	private void setSortcode(){
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
