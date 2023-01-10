/*
 *
 *    Copyright IBM Corp. 2022
 *
 */


package com.ibm.cics.cip.bankliberty.webui.dataAccess;

import java.io.IOException;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.sql.Date;



import java.util.Calendar;
import java.util.logging.LogManager;
import java.util.logging.Logger;

import javax.ws.rs.core.Response;

import com.ibm.cics.cip.bankliberty.api.json.AccountJSON;
import com.ibm.cics.cip.bankliberty.api.json.AccountsResource;
import com.ibm.json.java.JSONObject;



public class Account {

    static final String COPYRIGHT =
      "Copyright IBM Corp. 2022";

    private static Logger logger = Logger.getLogger("com.ibm.cics.cip.bankliberty.webui.dataAccess");
	


	private 	String 		customer_number;
	private 	String 		sortcode;              
	private 	String 		account_number;
	private 	String 		type;
	private 	BigDecimal	interest_rate;
	private 	Date 		opened;
	private 	int 		overdraft_limit;
	private 	Date 		last_statement;
	private 	Date 		next_statement;
	private	 	BigDecimal	available_balance;    
	private 	BigDecimal	actual_balance;
	private		int			db2type					=		4;

	public Account()
	{
		sortOutLogging();
	}

	public Account (String c_n, String sc, String a_n, String type, BigDecimal i_r, Date opened, int o_l, Date l_s, Date n_s, BigDecimal av_b, BigDecimal ac_b) {
		setCustomer_number(c_n);
		setSortcode(sc);
		setAccount_number(a_n);
		setType(type);
		setInterest_rate(i_r);
		setOpened(opened);
		setOverdraft_limit(o_l);
		setLast_statement(l_s);
		setNext_statement(n_s);
		setAvailable_balance(av_b);
		setActual_balance(ac_b);
	}

	public Account(String c_n, String sc, String a_n, String type, BigDecimal i_r, java.util.Date opened,
			int o_l, Object l_s, Object n_s, BigDecimal av_b, BigDecimal ac_b) {
		setCustomer_number(c_n);
		setSortcode(sc);
		setAccount_number(a_n);
		setType(type);
		setInterest_rate(i_r.setScale(2,RoundingMode.HALF_UP));
		setOpened(new Date(opened.getTime()));
		setOverdraft_limit(o_l);
		setLast_statement(null);
		setNext_statement(null);
		setAvailable_balance(av_b.setScale(2,RoundingMode.HALF_UP));
		setActual_balance(ac_b.setScale(2,RoundingMode.HALF_UP));
	}

	public void showInfo(){
		System.out.println("------------"+this.account_number+":"+this.sortcode+"------------");
		System.out.println("Customer number - "+this.customer_number);
		System.out.println("Type - "+this.type);
		System.out.println("Interest rate - "+this.interest_rate);
		System.out.println("Opened - "+this.opened.toString());
		System.out.println("Overdraft Limit - "+this.overdraft_limit);
		System.out.println("Last Statement - "+this.last_statement.toString());
		System.out.println("Next Statement - "+this.next_statement.toString());
		System.out.println("Available Balance - "+this.available_balance);
		System.out.println("Actual Balance - "+this.actual_balance);
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

	public String getAccount_number() {
		return account_number;
	}

	public void setAccount_number(String account_number) {
		this.account_number = account_number;
	}

	public String getType() {
		return type;
	}

	public void setType(String type) {
		this.type = type;
	}

	public BigDecimal getInterest_rate() {
		return interest_rate;
	}

	public void setInterest_rate(BigDecimal interest_rate) {
		this.interest_rate = interest_rate;
	}

	public Date getOpened() {
		return opened;
	}

	public void setOpened(Date opened) {
		this.opened = opened;
	}

	public int getOverdraft_limit() {
		return overdraft_limit;
	}

	public void setOverdraft_limit(int overdraft_limit) {
		this.overdraft_limit = overdraft_limit;
	}

	public Date getLast_statement() {
		return last_statement;
	}

	public void setLast_statement(Date last_statement) {
		if(last_statement == null){
			this.last_statement = this.opened;
		}
		else{
			this.last_statement = last_statement;
		}
	}

	public Date getNext_statement() {
		return next_statement;
	}

	public void setNext_statement(Date next_statement) {
		if(next_statement == null){
			Calendar cal = Calendar.getInstance();
			cal.setTime(opened);
			cal.add(Calendar.DATE, +7);
			this.next_statement =  new Date(cal.getTime().getTime());
		}
		else{
			this.next_statement = next_statement;
		}
	}



	public BigDecimal getAvailable_balance() {
		return available_balance;
	}

	public void setAvailable_balance(BigDecimal available_balance) {
		this.available_balance = available_balance;
	}

	public BigDecimal getActual_balance() {
		return actual_balance;
	}

	public void setActual_balance(BigDecimal actual_balance) {
		this.actual_balance = actual_balance;
	}

	public boolean updateThis(){
		AccountsResource myAccountsResource = new AccountsResource();

		AccountJSON myAccountJSON = new AccountJSON();

		myAccountJSON.setAccountType(this.getType());
		myAccountJSON.setInterestRate(this.getInterest_rate());
		myAccountJSON.setOverdraft(new Integer(this.getOverdraft_limit()));
		myAccountJSON.setSortCode(this.getSortcode());

		Response myAccountsResponse = myAccountsResource.updateAccountInternal(new Long(this.getAccount_number()), myAccountJSON);

		String myAccountsString = null;
		JSONObject myAccountsJSON = null;

		if(myAccountsResponse.getStatus() == 200)
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

			JSONObject myAccount = (JSONObject) myAccountsJSON;

			this.last_statement = sortOutDate((String) myAccount.get("lastStatementDate"));
			this.next_statement = sortOutDate((String) myAccount.get("nextStatementDate"));
			this.opened = sortOutDate((String) myAccount.get("dateOpened"));


			String accountNoString = (String) myAccount.get("id");

			String customerNoString = (String) myAccount.get("customerNumber");

			this.account_number = accountNoString;
			this.customer_number = customerNoString;

			this.actual_balance = BigDecimal.valueOf((Double) myAccount.get("actualBalance")).setScale(2,RoundingMode.HALF_UP);
			this.available_balance = BigDecimal.valueOf((Double) myAccount.get("availableBalance")).setScale(2,RoundingMode.HALF_UP);
			this.interest_rate = BigDecimal.valueOf((Double) myAccount.get("interestRate")).setScale(2,RoundingMode.HALF_UP);
			this.overdraft_limit = Integer.valueOf((String)myAccount.get("overdraft"));
			this.sortcode = (String) myAccount.get("sortCode");
			this.type = (String) myAccount.get("accountType");
		}
		else
		{
			return false;
		}
		return true;
	}

	public boolean deleteFromDB(){
		AccountsResource myAccountsResource = new AccountsResource();

		Response myAccountsResponse = null;
		String myAccountsString = null;
		JSONObject myAccountsJSON = null;

		myAccountsResponse = myAccountsResource.deleteAccountInternal(new Long(this.account_number));
		if(myAccountsResponse.getStatus() == 200)
		{
			myAccountsString = myAccountsResponse.getEntity().toString();
			try 
			{
				myAccountsJSON = JSONObject.parse(myAccountsString);
			} 
			catch (IOException e) 
			{

				logger.severe(e.toString());
				myAccountsResponse.close();
				return false;
			}

			JSONObject myAccount = (JSONObject) myAccountsJSON;

			this.last_statement = sortOutDate((String) myAccount.get("lastStatementDate"));
			this.next_statement = sortOutDate((String) myAccount.get("nextStatementDate"));
			this.opened = sortOutDate((String) myAccount.get("dateOpened"));


			String accountNoString = (String) myAccount.get("id");

			String customerNoString = (String) myAccount.get("customerNumber");

			this.account_number = accountNoString;
			this.customer_number = customerNoString;

			this.actual_balance = BigDecimal.valueOf((Double) myAccount.get("actualBalance")).setScale(2,RoundingMode.HALF_UP);
			this.available_balance = BigDecimal.valueOf((Double) myAccount.get("availableBalance")).setScale(2,RoundingMode.HALF_UP);
			this.interest_rate = BigDecimal.valueOf((Double) myAccount.get("interestRate")).setScale(2,RoundingMode.HALF_UP);
			this.overdraft_limit = Integer.valueOf((String)myAccount.get("overdraft"));
			this.sortcode = (String) myAccount.get("sortCode");
			this.type = (String) myAccount.get("accountType");
			return true;
		}
		myAccountsResponse.close();
		return false;
	}

	public int addToDB(){
		AccountsResource myAccountsResource = new AccountsResource();

		Response myAccountsResponse = null;
		String myAccountsString = null;
		JSONObject myAccountsJSON = null;
		
		AccountJSON myAccountJSON = new AccountJSON();
		
		myAccountJSON.setAccountType(this.getType());
		myAccountJSON.setInterestRate(this.getInterest_rate().setScale(2,RoundingMode.HALF_UP));
		myAccountJSON.setOverdraft(new Integer(this.getOverdraft_limit()));
		myAccountJSON.setCustomerNumber(this.getCustomer_number());
		myAccountJSON.setSortCode(this.getSortcode());

		myAccountsResponse = myAccountsResource.createAccountInternal(myAccountJSON);
		if(myAccountsResponse.getStatus() == 201)
		{
			myAccountsString = myAccountsResponse.getEntity().toString();
			try 
			{
				myAccountsJSON = JSONObject.parse(myAccountsString);
			}
			catch (IOException e) 
			{
				logger.severe(e.toString());
				myAccountsResponse.close();
				return -1;
			}

			JSONObject myAccount = (JSONObject) myAccountsJSON;

			this.last_statement = sortOutDate((String) myAccount.get("lastStatementDate"));
			this.next_statement = sortOutDate((String) myAccount.get("nextStatementDate"));
			this.opened = sortOutDate((String) myAccount.get("dateOpened"));


			String accountNoString = (String) myAccount.get("id");

			String customerNoString = (String) myAccount.get("customerNumber");

			this.account_number = accountNoString;
			this.customer_number = customerNoString;

			this.actual_balance = BigDecimal.valueOf((Double) myAccount.get("actualBalance")).setScale(2,RoundingMode.HALF_UP);
			this.available_balance = BigDecimal.valueOf((Double) myAccount.get("availableBalance")).setScale(2,RoundingMode.HALF_UP);
			this.interest_rate = BigDecimal.valueOf((Double) myAccount.get("interestRate")).setScale(2,RoundingMode.HALF_UP);
			this.overdraft_limit = Integer.valueOf((String)myAccount.get("overdraft"));
			this.sortcode = (String) myAccount.get("sortCode");
			this.type = (String) myAccount.get("accountType");
			myAccountsResponse.close();
			return new Integer(this.account_number).intValue();
		}
		myAccountsResponse.close();
		return -1;
	}
	public boolean inDB(){
		AccountsResource myAccountsResource = new AccountsResource();

		Response myAccountsResponse = null;
		String myAccountsString = null;
		JSONObject myAccountsJSON = null;

		myAccountsResponse = myAccountsResource.getAccountInternal(new Long(this.account_number));
		if(myAccountsResponse.getStatus() == 200)
		{
			myAccountsString = myAccountsResponse.getEntity().toString();
			try {
				myAccountsJSON = JSONObject.parse(myAccountsString);
			} catch (IOException e) {
				logger.severe(e.toString());
				myAccountsResponse.close();
				return false;
			}

			JSONObject myAccount = (JSONObject) myAccountsJSON;

			this.last_statement = sortOutDate((String) myAccount.get("lastStatementDate"));
			this.next_statement = sortOutDate((String) myAccount.get("nextStatementDate"));
			this.opened = sortOutDate((String) myAccount.get("dateOpened"));


			String accountNoString = (String) myAccount.get("id");

			String customerNoString = (String) myAccount.get("customerNumber");

			this.account_number = accountNoString;
			this.customer_number = customerNoString;

			this.actual_balance = BigDecimal.valueOf((Double) myAccount.get("actualBalance")).setScale(2,RoundingMode.HALF_UP);
			this.available_balance = BigDecimal.valueOf((Double) myAccount.get("availableBalance")).setScale(2,RoundingMode.HALF_UP);
			this.interest_rate = BigDecimal.valueOf((Double) myAccount.get("interestRate")).setScale(2,RoundingMode.HALF_UP);
			this.overdraft_limit = Integer.valueOf((String)myAccount.get("overdraft"));
			this.sortcode = (String) myAccount.get("sortCode");
			this.type = (String) myAccount.get("accountType");
			myAccountsResponse.close();
			return true;
		}
		myAccountsResponse.close();
		return false;


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
