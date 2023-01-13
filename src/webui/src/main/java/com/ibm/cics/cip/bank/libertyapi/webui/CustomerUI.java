/*
 *
 *    Copyright IBM Corp. 2022
 *
 *
 */

package com.ibm.cics.cip.bank.libertyapi.webui;

import java.sql.Date;
import java.util.Calendar;

import javax.ws.rs.core.Response;

import com.ibm.cics.cip.bankliberty.api.json.SortCodeResource;
import com.ibm.cics.cip.bankliberty.webui.data_access.Customer;
import com.vaadin.ui.Alignment;
import com.vaadin.ui.Button;

import com.vaadin.ui.HorizontalLayout;
import com.vaadin.ui.Label;
import com.vaadin.ui.TextField;
import com.vaadin.ui.TextArea;
import com.vaadin.ui.UI;
import com.vaadin.ui.VerticalLayout;

import com.vaadin.ui.Button.ClickEvent;
import com.vaadin.ui.DateField;





public class CustomerUI extends VerticalLayout{

    static final String COPYRIGHT =
      "Copyright IBM Corp. 2022";


	private static final long serialVersionUID = 1L;
	private transient Customer c;
	private UI ui;
	private Boolean edit = false;
	private TextField cusNumT; 
	private TextField cusNameT;
	private TextArea  cusAddressT;
	private TextField sCodeT;
	private DateField cusDoBT;
	private TextField cusCreditScoreT;
	private DateField cusCreditScoreReviewDateT;
	private static String sortcode;
	private static String editing = "Editing customer ";


	public CustomerUI(UI ui, Welcome back){
		//Constructor for creating a customer
		createCustUI(ui, back);
		edit = false;
		setSortcode();
	}

	public CustomerUI(UI ui, Welcome back, Customer cust){
		//Constructor for editing a customer
		edit = true;
		this.c = cust;
		editCustUI(ui, back, cust);
		setFields(cust);
		setSortcode();
	}
	
	/** 
	The two functions editCustUI and createCustUI are nearly identical, so only editCustUI is commented fully, 
	createCustUI is commented where it differs from editCustUI
	**/

	//Build the edit customer UI
	private void editCustUI(UI ui, Welcome back, Customer cust) {
		//ui is passed to the template - it's used to add the components to the UI using .addComponent()
		//cust is passed to the template - it's used to populate the relevant fields, such as customer number etc.
		this.ui = ui;
		HbHeader header = new HbHeader(ui, back);
		this.addComponent(header);
		this.setExpandRatio(header, 0.1f);

		Label title = new Label("Customer Update");
		this.addComponent(title);

		//This creates a container that fields can be added to by using cusNumL.addComponent, the container can then be added to the UI
		HorizontalLayout cusNumL = new HorizontalLayout();

		//The following set of components do not get their values from user input, rather fetch the appropriate information from the Customer object 'cust'
		
		//Textbox to enter customer number
		cusNumT = new TextField("Customer Number");
		cusNumT.setValue(cust.getCustomerNumber());
		cusNumT.setEnabled(false);

		//Textbox to enter customer name
		cusNameT = new TextField("Customer Name");
		cusNameT.setValue(cust.getName());
		cusNameT.setEnabled(true);	

		//Textbox to enter customer address
		cusAddressT = new TextArea("Customer Address");
		cusAddressT.setValue(cust.getAddress());
		cusAddressT.setEnabled(true);		

		//Datefield to enter customer's DOB
		cusDoBT = new DateField("Date of Birth dd-M-yyyy");
		cusDoBT.setValue(cust.getDob());
		cusDoBT.setEnabled(false);
		cusDoBT.setDateFormat("dd-M-yyyy");		

		//Textfield to enter customer's credit score
		cusCreditScoreT = new TextField("Credit Score");
		cusCreditScoreT.setValue(cust.getCreditScore());
		cusCreditScoreT.setEnabled(false);

		//Datefield to enter customer's credit score review date
		cusCreditScoreReviewDateT = new DateField("Review Date");
		cusCreditScoreReviewDateT.setValue(cust.getCreditScoreReviewDate());
		cusCreditScoreReviewDateT.setEnabled(false);

		//Textfield to display customer's sortcode
		sCodeT = new TextField("Sortcode");
		sCodeT.setValue(getSortcode());
		sCodeT.setEnabled(false);

		//Set the width of the components to be added to the UI
		cusNumL.setWidth("95%");
		cusNumT.setWidth("115");
		cusNameT.setWidth("95%");
		cusAddressT.setWidth("95%");
		cusDoBT.setWidth("90%");
		sCodeT.setWidth("70%");
		cusCreditScoreT.setWidth("80%");
		cusCreditScoreReviewDateT.setWidth("80%");

		//Add the components to the container
		cusNumL.addComponent(cusNumT);
		cusNumL.addComponent(cusNameT);
		cusNumL.addComponent(cusAddressT);
		cusNumL.addComponent(cusDoBT);
		cusNumL.addComponent(sCodeT);
		cusNumL.addComponent(cusCreditScoreT);
		cusNumL.addComponent(cusCreditScoreReviewDateT);
		cusNumL.setSpacing(true);

		Button submit;
		//Change button text depending on context (editing vs creating)
		if (Boolean.TRUE.equals(this.edit))
		{
			submit = new Button("Edit customer");
		}
		else
		{
			submit = new Button("Create customer");
		}
		//Create new container and scale, add button component to the container and set the width
		HorizontalLayout buttonL = new HorizontalLayout();
		buttonL.setWidth("60%");
		buttonL.addComponent(submit);
		submit.setWidth("100%");

		submit.addClickListener(new Button.ClickListener() {
			private static final long serialVersionUID = 3760485465663201508L;

			public void buttonClick(ClickEvent event) {
				//Determine what to set the button label to
				if(Boolean.FALSE.equals(edit))
				{
					//if creating a customer
					String temp = createNewCustomer();
					if(temp.startsWith("-1"))
					{
						event.getButton().setCaption("Create new customer failed");
					}
					else
					{
						event.getButton().setCaption("Create new customer successful for customer " + temp);
					}
				}
				else{
					//if editing a customer
					if(!editCustomer())
					{
						event.getButton().setCaption(editing + c.getCustomerNumber() + " failed");
					}
					else
					{
						event.getButton().setCaption(editing + c.getCustomerNumber() + " successful");
					}
				}
			}
		});

		//Add the containers to ui and align
		this.addComponent(cusNumL);
		this.addComponent(buttonL);

		this.setSpacing(true);
		this.setComponentAlignment(buttonL, Alignment.MIDDLE_CENTER);
		this.setComponentAlignment(cusNumL, Alignment.MIDDLE_CENTER);

	}

	private void setFields(Customer cust){
		//Set each component's value to the relevant value held in the 'cust' object 
		cusNumT.setValue(cust.getCustomerNumber());
		sCodeT.setValue(cust.getSortcode());
		cusAddressT.setValue(cust.getAddress().trim());
		cusNameT.setValue(cust.getName().trim());
		cusDoBT.setValue(cust.getDob());
		cusCreditScoreT.setValue(cust.getCreditScore());
		cusCreditScoreReviewDateT.setValue(cust.getCreditScoreReviewDate());

	}

	@SuppressWarnings("serial")
	//Build the customer creation UI
	private void createCustUI(UI ui, Welcome back){

		this.ui = ui;
		HbHeader header = new HbHeader(ui, back);
		this.addComponent(header);
		this.setExpandRatio(header, 0.1f);

		Label title = new Label("Customer Creation");
		this.addComponent(title);

		HorizontalLayout cusNumL = new HorizontalLayout();
		
		//The following containers are not populated by default

		cusNumT = new TextField("Customer Number");
		cusNumT.setEnabled(false);

		cusNameT = new TextField("Customer Name");
		cusNameT.setEnabled(true);

		cusAddressT = new TextArea("Customer Address");
		cusAddressT.setEnabled(true);

		cusDoBT = new DateField("Date of Birth dd-M-yyyy");
		cusDoBT.setDateFormat("dd-M-yyyy");
		// The customer will not have been born today
		Calendar now = Calendar.getInstance();
		long nowMs = now.getTimeInMillis();
		// Subtract 18 years from now
		long eighteenYears = 365L * 18L * 24L * 60L * 60L * 1000L;
		long happyEighteenthBirthday = nowMs - eighteenYears;
		cusDoBT.setValue(new Date(happyEighteenthBirthday));
		cusDoBT.setEnabled(true);

		sCodeT = new TextField("Sortcode");
		sCodeT.setValue(getSortcode());
		sCodeT.setEnabled(false);

		cusNumL.setWidth("95%");
		cusNumT.setWidth("115");
		cusNameT.setWidth("95%");
		cusAddressT.setWidth("95%");
		cusDoBT.setWidth("90%");
		sCodeT.setWidth("70%");
		cusNumL.setSpacing(true);


		cusNumL.addComponent(cusNumT);
		cusNumL.addComponent(cusNameT);
		cusNumL.addComponent(cusAddressT);
		cusNumL.addComponent(cusDoBT);
		cusNumL.addComponent(sCodeT);

		Button submit = new Button("Create customer");
		HorizontalLayout buttonL = new HorizontalLayout();
		buttonL.setWidth("60%");
		buttonL.addComponent(submit);
		submit.setWidth("100%");

		submit.addClickListener(new Button.ClickListener() {
			public void buttonClick(ClickEvent event) {
				if(Boolean.FALSE.equals(edit))
				{
					String temp = createNewCustomer();
					if(temp.startsWith("-1"))
					{
						event.getButton().setCaption("Create new customer failed");
					}
					else
					{
						event.getButton().setCaption("Create new customer successful for customer " + temp);
					}
				}
				else{
					if(!editCustomer())
					{
						event.getButton().setCaption(editing + c.getCustomerNumber() + " failed");
					}
					else
					{
						event.getButton().setCaption(editing + c.getCustomerNumber() + " successful");
					}
				}
			}

		});


		this.addComponent(cusNumL);
		this.addComponent(buttonL);

		this.setComponentAlignment(buttonL, Alignment.MIDDLE_CENTER);
		this.setComponentAlignment(cusNumL, Alignment.MIDDLE_CENTER);

	}

	//Create a new customer object, then add the customer to the database. If customer already in database then return "-1"
	private String createNewCustomer(){
		if(validateSimple()){
			String temp = "";
			Customer newCust;
			//create a new customer
			newCust = new Customer("0",sCodeT.getValue(),
					cusNameT.getValue().replace("\n", ""),
					cusAddressT.getValue().replace("\n", ""),
					new java.sql.Date(cusDoBT.getValue().getTime()));

			
			//add customer to the database
			cusNumT.setValue(newCust.addToDB());
			
			//Check if the customer now exists in the database
			if(newCust.inDB()){
				return cusNumT.getValue();
			}else{
				return "-1";
			}
		}
		return "-1";
	}

	private boolean editCustomer(){
		c.setName(cusNameT.getValue().replace("\n", ""));
		c.setAddress(cusAddressT.getValue().replace("\n", ""));


		return c.updateThis();
	}

	private boolean validateSimple(){
		//Returns false if sortcode, customer name, customer address or DOB fields are not populated
		if(sCodeT.getValue().isEmpty())
		{
			return false;
		}
		if(cusNameT.getValue().isEmpty())
		{
			return false;
		}
		if(cusAddressT.getValue().isEmpty())
		{
			return false;
		}
		return !cusDoBT.isEmpty();
	}
	
	///get sortcode
	private static String getSortcode(){
		if(sortcode == null)
		{
			setSortcode();
		}
		return sortcode;
	}

	//set sortcode
	private static void setSortcode(){
		if(sortcode == null)
		{
			SortCodeResource mySortCodeResource = new SortCodeResource();
			Response mySortCodeJSON = mySortCodeResource.getSortCode();
			sortcode = ((String) mySortCodeJSON.getEntity()).substring(13, 19);
		}
	}

}
