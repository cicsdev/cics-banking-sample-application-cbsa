/*
 *
 *    Copyright contributors to the CICS Banking Sample Application (CBSA) project
 *
 */

package com.ibm.cics.cip.bank.libertyapi.webui;

import java.sql.Date;
import java.util.Calendar;

import javax.ws.rs.core.Response;

import com.ibm.cics.cip.bankliberty.webui.dataAccess.Customer;
import com.ibm.cics.cip.bankliberty.api.json.SortCodeResource;



import com.vaadin.ui.Alignment;
import com.vaadin.ui.Button;

import com.vaadin.ui.HorizontalLayout;
import com.vaadin.ui.Label;
import com.vaadin.ui.TextField;
import com.vaadin.ui.TextArea;
import com.vaadin.ui.UI;
import com.vaadin.ui.ComboBox;
import com.vaadin.ui.VerticalLayout;

import com.vaadin.ui.Button.ClickEvent;
import com.vaadin.ui.DateField;





public class CustomerUI extends VerticalLayout{

    static final String COPYRIGHT =
      "Copyright contributors to the CICS Banking Sample Application (CBSA) project.";

	private static final long serialVersionUID = 1L;
	private Customer c;
	private UI ui;
	private VerticalLayout vl = new VerticalLayout();
	private Boolean edit = false;
	private TextField cusNumT; 
	private TextField cusNameT;
	private TextArea  cusAddressT;
	private TextField sCodeT;
	private DateField cusDoBT;
	private TextField cusCreditScoreT;
	private DateField cusCreditScoreReviewDateT;
	private static String sortcode;


	public CustomerUI(UI ui, String user, Welcome back){
		//Constructor for creating a customer
		createCustUI(ui, back, user);
		edit = false;
		setSortcode();
	}

	public CustomerUI(UI ui, String user, Welcome back, Customer cust){
		//Constructor for editing a customer
		edit = true;
		this.c = cust;
		editCustUI(ui, back, user, cust);
		setFields(cust);
		setSortcode();
	}
	
	/** 
	The two functions editCustUI and createCustUI are nearly identical, so only editCustUI is commented fully, 
	createCustUI is commented where it differs from editCustUI
	**/

	//Build the edit customer UI
	private void editCustUI(UI ui, Welcome back, String user, Customer cust) {
		//ui is passed to the template - it's used to add the components to the UI using .addComponent()
		//cust is passed to the template - it's used to populate the relevant fields, such as customer number etc.
		this.ui = ui;
		HB_Header header = new HB_Header(ui, "Customer Update", back);
		this.addComponent(header);
		this.setExpandRatio(header, 0.1f);

		Label title = new Label("Customer Update");
		this.addComponent(title);

		//This creates a container that fields can be added to by using cusNumL.addComponent, the container can then be added to the UI
		HorizontalLayout cusNumL = new HorizontalLayout();

		//The following set of components do not get their values from user input, rather fetch the appropriate information from the Customer object 'cust'
		
		//Textbox to enter customer number
		cusNumT = new TextField("Customer Number");
		cusNumT.setValue(cust.getCustomer_number());
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
		if (this.edit)
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
				if(edit == false){
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
						event.getButton().setCaption("Editing customer " + c.getCustomer_number() + " failed");
					}
					else
					{
						event.getButton().setCaption("Editing customer " + c.getCustomer_number() + " successful");
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
		cust.showInfo();
		cusNumT.setValue(cust.getCustomer_number());
		sCodeT.setValue(cust.getSortcode());
		cusAddressT.setValue(cust.getAddress().trim());
		cusNameT.setValue(cust.getName().trim());
		cusDoBT.setValue(cust.getDob());
		cusCreditScoreT.setValue(cust.getCreditScore());
		cusCreditScoreReviewDateT.setValue(cust.getCreditScoreReviewDate());

	}

	@SuppressWarnings("serial")
	//Build the customer creation UI
	private void createCustUI(UI ui, Welcome back, String user){

		this.ui = ui;
		HB_Header header = new HB_Header(ui, "Customer Creation", back);
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
				if(edit == false){
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
						event.getButton().setCaption("Editing customer " + c.getCustomer_number() + " failed");
					}
					else
					{
						event.getButton().setCaption("Editing customer " + c.getCustomer_number() + " successful");
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
					cusNameT.getValue().replaceAll("\n", ""),
					cusAddressT.getValue().replaceAll("\n", ""),
					new java.sql.Date(cusDoBT.getValue().getTime()));

			newCust.showInfo();
			
			//add customer to the database
			temp = newCust.addToDB();
			cusNumT.setValue(new Long(temp).toString());
			
			//Check if the customer now exists in the database
			if(newCust.inDB()){
				return temp;
			}else{
				return "-1";
			}
		}
		return "-1";
	}

	private boolean editCustomer(){
		c.setName(cusNameT.getValue().replaceAll("\n", ""));
		c.setAddress(cusAddressT.getValue().replaceAll("\n", ""));


		if(c.updateThis())
		{
			return true;
		}
		else
		{
			return false;
		}
	}

	private boolean validateSimple(){
		//Returns false if sortcode, customer name, customer address or DOB fields are not populated
		if(sCodeT.getValue().isEmpty())
		{
			return false;
		}
		if(cusNameT.getValue().toString().isEmpty())
		{
			return false;
		}
		if(cusAddressT.getValue().toString().isEmpty())
		{
			return false;
		}
		if(cusDoBT.isEmpty())
		{
			return false;
		}

		return true;
	}
	
	///get sortcode
	private String getSortcode(){
		if(sortcode == null)
		{
			setSortcode();
		}
		return sortcode;
	}

	//set sortcode
	private void setSortcode(){
		if(sortcode == null)
		{
			SortCodeResource mySortCodeResource = new SortCodeResource();
			Response mySortCodeJSON = mySortCodeResource.getSortCode();
			sortcode = ((String) mySortCodeJSON.getEntity()).substring(13, 19);
		}
	}

}