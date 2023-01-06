/*
 *
 *    Copyright IBM Corp. 2022
 *
 */
package com.ibm.cics.cip.bankliberty.webui.dataAccess;

import java.io.IOException;
import java.sql.Date;

import javax.ws.rs.core.Response;

import com.ibm.cics.cip.bankliberty.api.json.CustomerJSON;
import com.ibm.cics.cip.bankliberty.api.json.CustomerResource;
import com.ibm.json.java.JSONObject;

public class Customer {

    static final String COPYRIGHT =
      "Copyright IBM Corp. 2022";







	// String ACCOUNT_EYECATCHER             CHAR(4),
	private 	String 		customer_number;
	private 	String 		sortcode;              
	private 	String 		name;
	private 	String 		address;
	private 	Date 		dob;
	private 	String		creditScore;
	private		Date		creditScoreReviewDate;
	private		Boolean		editingCustomer;
		
	
	
	//NEW CUSTOMER
	public Customer (String customer_number, String sortCode, String name, String address, Date dob) {
		editingCustomer = false;
		setCustomer_number(customer_number);
		setSortcode(sortCode);
		setName(name);
		setAddress(address);
		setDob(dob);
	}
	
	//EDITING CUSTOMER
	public Customer (String customer_number, String sortCode, String name, String address, Date dob, String creditScore, Date creditScoreReviewDate) {
		editingCustomer = true;
		setCustomer_number(customer_number);
		setSortcode(sortCode);
		setName(name);
		setAddress(address);
		setDob(dob);
		setCreditScore(creditScore);
		setCreditScoreReviewDate(creditScoreReviewDate);
	}

	public String getCustomer_number() {
		return customer_number;
	}

	public void setCustomer_number(String customer_number) {
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

	public String getCreditScore(){
		return creditScore;
	}
	
	public void setCreditScore(String creditScore){
		this.creditScore = creditScore;
	}
	
	public Date getCreditScoreReviewDate(){
		return creditScoreReviewDate;
	}
	
	public void setCreditScoreReviewDate(Date creditScoreReviewDate){
		this.creditScoreReviewDate = creditScoreReviewDate;
	}
	
	
	public boolean updateThis(){
		CustomerResource myCustomerResource = new CustomerResource();

		CustomerJSON myCustomerJSON = new CustomerJSON();

		myCustomerJSON.setCustomerAddress(this.getAddress());
		myCustomerJSON.setCustomerName(this.getName());
		myCustomerJSON.setSortCode(this.getSortcode());
		myCustomerJSON.setSortCode(this.getSortcode());
		
		Response myCustomerResponse = myCustomerResource.updateCustomerExternal(new Long(this.getCustomer_number()), myCustomerJSON);

		String myCustomerString = null;
		JSONObject myCustomer = null;

		if(myCustomerResponse.getStatus() == 200)
		{
			myCustomerString = myCustomerResponse.getEntity().toString();
			try {
				myCustomer = JSONObject.parse(myCustomerString);
			} catch (IOException e) {
				myCustomerResponse.close();
 				return false;
			}

			this.setDob(sortOutDate((String) myCustomer.get("dateOfBirth")));
			this.setAddress((String) myCustomer.get("customerAddress"));
			this.setName((String) myCustomer.get("customerName"));
			this.setSortcode((String) myCustomer.get("sortCode"));
			String customerNoString = (String) myCustomer.get("id");
			this.setCustomer_number(customerNoString);

		}
		else
		{
			myCustomerResponse.close();
			return false;
		}
		myCustomerResponse.close();
		return true;
	}

	public boolean deleteFromDB(){
		CustomerResource myCustomerResource = new CustomerResource();
		
		Response myCustomerResponse = myCustomerResource.deleteCustomerExternal(new Long(this.getCustomer_number()));

		String myCustomerString = null;
		JSONObject myCustomer = null;

		if(myCustomerResponse.getStatus() == 200)
		{
			myCustomerString = myCustomerResponse.getEntity().toString();
			try {
				myCustomer = JSONObject.parse(myCustomerString);
			} catch (IOException e) {
				e.printStackTrace();
				myCustomerResponse.close();
				return false;
			}

			this.setDob(sortOutDate((String) myCustomer.get("dateOfBirth")));
			this.setAddress((String) myCustomer.get("customerAddress"));
			this.setName((String) myCustomer.get("customerName"));
			this.setSortcode((String) myCustomer.get("sortCode"));
			this.setCreditScore((String) myCustomer.get("customerCreditScore"));
			this.setCreditScoreReviewDate(sortOutDate((String) myCustomer.get("customerCreditScoreReviewDate")));
			String customerNoString = (String) myCustomer.get("id");

			this.setCustomer_number(customerNoString);
		}
		else
		{
			myCustomerResponse.close();
			return false;
		}
		myCustomerResponse.close();
		return true;	
	}

	
	public String addToDB(){
		CustomerResource myCustomerResource = new CustomerResource();

		CustomerJSON myCustomerJSON = new CustomerJSON();

		myCustomerJSON.setCustomerAddress(this.getAddress());
		myCustomerJSON.setCustomerName(this.getName());
		myCustomerJSON.setDateOfBirth(this.getDob());
		myCustomerJSON.setSortCode(this.getSortcode());
		Response myCustomerResponse = myCustomerResource.createCustomerExternal(myCustomerJSON);

		String myCustomerString = null;
		JSONObject myCustomer = null;

		if(myCustomerResponse.getStatus() == 201)
		{
			myCustomerString = myCustomerResponse.getEntity().toString();
			try {
				myCustomer = JSONObject.parse(myCustomerString);
			} catch (IOException e) {
				e.printStackTrace();
				myCustomerResponse.close();
				return "-1";
			}

			this.setDob(sortOutDate((String) myCustomer.get("dateOfBirth")));
			this.setAddress((String) myCustomer.get("customerAddress"));
			this.setName((String) myCustomer.get("customerName"));
			this.setSortcode((String) myCustomer.get("sortCode"));
		
			String customerNoString = (String) myCustomer.get("id");
			this.setCustomer_number(customerNoString);
			myCustomerResponse.close();
			return  customerNoString;
		}
		else
		{
			System.err.println(myCustomerResponse.getStatus() + " " + myCustomerResponse.getEntity().toString());
			myCustomerResponse.close();
			return "-1";
		}
	}

	/** inDB
	 * 
	 * Checks if the customer is in the database
	 * 
	 * @return
	 */
	public boolean inDB(){
		CustomerResource myCustomerResource = new CustomerResource();

		Response myCustomerResponse = null;
		String myCustomerString = null;
		JSONObject myCustomer = null;

		myCustomerResponse = myCustomerResource.getCustomerExternal(new Long(this.customer_number));
		if(myCustomerResponse.getStatus() == 200)
		{
			myCustomerString = myCustomerResponse.getEntity().toString();
			try {
				myCustomer = JSONObject.parse(myCustomerString);
			} catch (IOException e) {
				e.printStackTrace();
				myCustomerResponse.close();
				return false;
			}

			this.setDob(sortOutDate((String) myCustomer.get("dateOfBirth")));

			String customerNoString = (String) myCustomer.get("id");
			this.setCustomer_number(customerNoString);
			this.setAddress((String) myCustomer.get("customerAddress"));
			this.setName((String) myCustomer.get("customerName"));
			this.setSortcode((String) myCustomer.get("sortCode")); 	
			myCustomerResponse.close();
			return true;
		}
		myCustomerResponse.close();
		return false;
	}

	/** sortOutDate
	 * 
	 * Returns a correctly formatted java date from the one input
	 * 
	 * @param dateString
	 * @return
	 */
	@SuppressWarnings("deprecation")
	private Date sortOutDate(String dateString) {
		String[] dateArray = dateString.split("-");

		Integer year = new Integer(dateArray[0]);
		Integer month = new Integer(dateArray[1]);
		Integer day = new Integer(dateArray[2]);

		Date sortedOutDate = new Date(year.intValue() - 1900,month.intValue() - 1, day.intValue());
		return sortedOutDate;
	}

	
	/** showInfo
	 * 
	 * Displays all the info stored about the customer
	 * Will show a credit score if the customer is being edited
	 * 
	 */
	public void showInfo() {
		if(editingCustomer == false){
			System.out.println("------------"+ this.customer_number+":"+this.sortcode+"------------");
			System.out.println("Sortcode - "+ this.sortcode);
			System.out.println("Customer name - "+this.getName());
			System.out.println("Customer address - " +this.getAddress());
			System.out.println("Customer Date of Birth - "+ this.getDob().toString());
			System.out.println("Customer is new");
		} else {
			System.out.println("------------"+ this.customer_number+":"+this.sortcode+"------------");
			System.out.println("Sortcode - "+ this.sortcode);
			System.out.println("Customer name - "+this.getName());
			System.out.println("Customer address - " +this.getAddress());
			System.out.println("Customer Date of Birth - "+ this.getDob().toString());
			System.out.println("Customer credit score - " + this.getCreditScore());
			System.out.println("Customer cs review date - "+ this.getCreditScoreReviewDate().toString());
			System.out.println("Customer is being edited");
		}
	}


}
