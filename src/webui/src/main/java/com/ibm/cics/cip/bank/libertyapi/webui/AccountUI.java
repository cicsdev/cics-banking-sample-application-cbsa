/*
 *
 *    Copyright IBM Corp. 2022
 *
 *
 */

package com.ibm.cics.cip.bank.libertyapi.webui;

/**
 * This class is part of the "Vaadin" user interface.
 */



import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.Date;

import javax.ws.rs.core.Response;

import com.ibm.cics.cip.bankliberty.api.json.SortCodeResource;
import com.ibm.cics.cip.bankliberty.webui.data_access.Account;
import com.vaadin.ui.Alignment;
import com.vaadin.ui.Button;

import com.vaadin.ui.HorizontalLayout;
import com.vaadin.ui.Label;
import com.vaadin.ui.TextField;
import com.vaadin.ui.UI;
import com.vaadin.ui.VerticalLayout;

import com.vaadin.ui.Button.ClickEvent;
import com.vaadin.ui.ComboBox;


public class AccountUI extends VerticalLayout{

    static final String COPYRIGHT =
      "Copyright IBM Corp. 2022";


	private static final long serialVersionUID = 1L;
	private transient Account a;
	private UI ui;
	private Boolean edit = false;
	 
	private TextField cusNumT; 
	private TextField sCodeT;
	private ComboBox typeT; 
	private TextField interestT;
	private TextField overdraftT; 
	private TextField balanceT;
	private static String sortcode;
	private static final String TYPE_CURRENT = "CURRENT";
	private static final String TYPE_ISA = "ISA";
	private static final String TYPE_LOAN = "LOAN";
	private static final String TYPE_MORTGAGE = "MORTGAGE";
	private static final String TYPE_SAVING = "SAVING";
	private static final String EDITING_STRING = "Editing account " ;
	

	public AccountUI(UI ui, Welcome back){
		//Create a new account		
		createAccUI(ui, back, new Account());
		edit = false;
		setSortcode();

	}

	public AccountUI(UI ui, Welcome back, Account acc){
		//Edit an existing account
		edit = true;
		this.a = acc;
		createAccUI(ui, back, acc);
		setFields(acc);
		setSortcode();

	}

	/** 
	The two functions, createAccUI and editAccUI are nearly identical, so only createAccUI is commented fully, 
	editAccUI is commented where it differs from createCustUI
	 **/

	//Function creates the UI for account creation
	private void createAccUI(UI ui, Welcome back, Account acc) {
		//ui is passed in so that containers can be added to it later using this.addComponent()
		//acc is passed in so that it can be used to populate the components
		this.ui = ui;
		HbHeader header = new HbHeader(ui, back);
		this.addComponent(header);
		this.setExpandRatio(header, 0.1f);

		Label title = new Label("Account Creation / Update");
		this.addComponent(title);

		TextField accNumT = new TextField("Account Number");

		accNumT.setEnabled(false);

		//Create a new container and scale - components can be added to it using cusNumL.addComponent()
		HorizontalLayout cusNumL = new HorizontalLayout();
		cusNumL.setWidth("60%");
		
		//The following components get their respective data from 'acc'

		//Textfield showing account number
		accNumT = new TextField("Account Number");
		accNumT.setEnabled(false);

		//Textfield showing customer number
		cusNumT = new TextField("Customer Number");

		cusNumT.setEnabled(false);

		//Textfield showing sortcode
		sCodeT = new TextField("Sortcode");
		sCodeT.setValue(getSortcode());
		sCodeT.setEnabled(false);
		
		if(acc.getAccountNumber() == null)
		{
			accNumT.setValue(acc.getAccountNumber());
			cusNumT.setValue(acc.getCustomerNumber());
		}
		else
		{
			accNumT.setValue("00000000");
			cusNumT.setValue("");
		}

		//Add the components to the container and align
		cusNumL.addComponent(accNumT);
		cusNumL.addComponent(cusNumT);
		cusNumT.setEnabled(Boolean.FALSE.equals(edit));
		cusNumL.addComponent(sCodeT);
		cusNumL.setComponentAlignment(sCodeT, Alignment.MIDDLE_RIGHT);

		//Create a new container and scale
		HorizontalLayout typeL = new HorizontalLayout();
		typeL.setWidth("60%");

		//Set the account type
		typeT = new ComboBox("Type");
		typeT.addItem(TYPE_CURRENT);
		typeT.addItem(TYPE_ISA);
		typeT.addItem(TYPE_LOAN);
		typeT.addItem(TYPE_MORTGAGE);
		typeT.addItem(TYPE_SAVING);
		typeT.setValue(TYPE_CURRENT);
		typeT.setNullSelectionAllowed(false);
		
		//Set interest rate
		interestT = new TextField("Interest");
		interestT.setValue(acc.getInterestRate().setScale(2,RoundingMode.HALF_UP).toString());
		
		//Add to container and align
		typeL.addComponent(typeT);
		typeL.addComponent(interestT);
		typeL.setComponentAlignment(interestT, Alignment.MIDDLE_RIGHT);

		//Create a new container and scale
		HorizontalLayout overdraftL = new HorizontalLayout();
		overdraftL.setWidth("60%");
		
		//Textfield for overdraft limit
		overdraftT = new TextField("Overdraft Limit");
		
		//Textfield for balance
		balanceT = new TextField("Balance");
		balanceT.setEnabled(false);
		
		//Add to container and align
		overdraftL.addComponent(overdraftT);
		overdraftL.addComponent(balanceT);
		overdraftL.setComponentAlignment(balanceT, Alignment.MIDDLE_RIGHT);

		Button submit;
		//Change button text depending on context
		if (Boolean.TRUE.equals(this.edit))
		{
			submit = new Button("Edit account");
		}
		else
		{
			submit = new Button("Create account");
		}
		
		//Create new container for the button, add the button and set the width
		HorizontalLayout buttonL = new HorizontalLayout();
		buttonL.setWidth("60%");
		buttonL.addComponent(submit);
		submit.setWidth("100%");

		submit.addClickListener(new Button.ClickListener() {
			/**
			 * 
			 */
			private static final long serialVersionUID = 3760485465663201508L;

			public void buttonClick(ClickEvent event) {
				if(Boolean.FALSE.equals(edit))
				{
					//Creating an account
					String temp = createNewAccount();
					if(temp.startsWith("-1"))
					{
						event.getButton().setCaption("Create new account failed");
					}
					else
					{
						event.getButton().setCaption("Create new account successful for account " + temp);
					}
				}
				else{
					//Editing an account	
					if(Boolean.FALSE.equals(editAccount()))
					{
						event.getButton().setCaption(EDITING_STRING + a.getAccountNumber() + " failed");
					}
					else
					{
						event.getButton().setCaption(EDITING_STRING + a.getAccountNumber() + " successful");
					}
				}
			}
		});

		// Add containers to ui and align
		this.addComponent(cusNumL);
		this.addComponent(typeL);
		this.addComponent(overdraftL);
		this.addComponent(buttonL);

		this.setComponentAlignment(buttonL, Alignment.MIDDLE_CENTER);
		this.setComponentAlignment(cusNumL, Alignment.MIDDLE_CENTER);
		this.setComponentAlignment(typeL, Alignment.MIDDLE_CENTER);
		this.setComponentAlignment(overdraftL, Alignment.MIDDLE_CENTER);



	}

	private void setFields(Account acc){
		//Set values for the fields by using acc
		cusNumT.setValue(acc.getCustomerNumber());
		sCodeT.setValue(acc.getSortcode());
		typeT.setValue(acc.getType().trim());
		interestT.setValue(String.valueOf(acc.getInterestRate().setScale(2,RoundingMode.HALF_UP)));
		overdraftT.setValue(String.valueOf(acc.getOverdraftLimit()));
		balanceT.setValue(String.valueOf(acc.getActualBalance().setScale(2,RoundingMode.HALF_UP)));
	}

	@SuppressWarnings("serial")
	private void createAccUI(UI ui, Welcome back, String user){

		this.ui = ui;
		HbHeader header = new HbHeader(ui, back);
		this.addComponent(header);
		this.setExpandRatio(header, 0.1f);

		Label title = new Label("Account Creation / Update");
		this.addComponent(title);

		HorizontalLayout cusNumL = new HorizontalLayout();
		cusNumL.setWidth("60%");
		cusNumT = new TextField("Customer Number");
		sCodeT = new TextField("Sortcode");
		sCodeT.setValue(getSortcode());
		sCodeT.setEnabled(false);
		cusNumL.addComponent(cusNumT);
		cusNumT.setEnabled(!edit);
		cusNumL.addComponent(sCodeT);
		cusNumL.setComponentAlignment(sCodeT, Alignment.MIDDLE_RIGHT);

		HorizontalLayout typeL = new HorizontalLayout();
		typeL.setWidth("60%");

		///Create combobox with type options and set the type to 'CURRENT'		
		typeT = new ComboBox("Type");
		typeT.addItem(TYPE_CURRENT);
		typeT.addItem(TYPE_ISA);
		typeT.addItem(TYPE_LOAN);
		typeT.addItem(TYPE_MORTGAGE);
		typeT.addItem(TYPE_SAVING);
		typeT.setValue(TYPE_CURRENT);
		typeT.setNullSelectionAllowed(false);
		
		//Create interest textfield and set to '0.00'	
		interestT = new TextField("Interest");
		interestT.setValue("0.00");
		typeL.addComponent(typeT);
		typeL.addComponent(interestT);
		typeL.setComponentAlignment(interestT, Alignment.MIDDLE_RIGHT);

		//Create overdraft textfield and set to '0'
		HorizontalLayout overdraftL = new HorizontalLayout();
		overdraftL.setWidth("60%");
		overdraftT = new TextField("Overdraft Limit");
		overdraftT.setValue("0");

		//Create balance textfield and set to '0'
		balanceT = new TextField("Balance");
		balanceT.setEnabled(false);
		balanceT.setValue("0.00");
		overdraftL.addComponent(overdraftT);
		overdraftL.addComponent(balanceT);
		overdraftL.setComponentAlignment(balanceT, Alignment.MIDDLE_RIGHT);

		Button submit = new Button("Create account");
		HorizontalLayout buttonL = new HorizontalLayout();
		buttonL.setWidth("60%");
		buttonL.addComponent(submit);
		submit.setWidth("100%");

		submit.addClickListener(new Button.ClickListener() {
			public void buttonClick(ClickEvent event) {
				if(!edit){
					String temp = createNewAccount();
					if(temp.startsWith("-1"))
					{
						event.getButton().setCaption("Create new account failed");
					}
					else
					{
						event.getButton().setCaption("Create new account successful for account " + temp);
					}
				}
				else{

					if(!editAccount())
					{
						event.getButton().setCaption(EDITING_STRING + a.getAccountNumber() + " failed");
					}
					else
					{
						event.getButton().setCaption(EDITING_STRING + a.getAccountNumber() + " successful");
					}

				}
			}
		});


		this.addComponent(cusNumL);
		this.addComponent(typeL);
		this.addComponent(overdraftL);
		this.addComponent(buttonL);

		this.setComponentAlignment(buttonL, Alignment.MIDDLE_CENTER);
		this.setComponentAlignment(cusNumL, Alignment.MIDDLE_CENTER);
		this.setComponentAlignment(typeL, Alignment.MIDDLE_CENTER);
		this.setComponentAlignment(overdraftL, Alignment.MIDDLE_CENTER);


	}

	//Create a new account object and add to the database
	private String createNewAccount(){
		//if required fields are all populated
		if(validateSimple()){
			int temp = 0;
			BigDecimal tempBD = BigDecimal.valueOf(Double.parseDouble(interestT.getValue()));
			//Create a new account object
			Account newAcc = new Account(cusNumT.getValue(), sCodeT.getValue(), String.format("%08d",temp), 
					typeT.getValue().toString(), tempBD, new Date(), 
					Integer.valueOf(overdraftT.getValue()),  
					BigDecimal.valueOf(Double.parseDouble(balanceT.getValue())), BigDecimal.valueOf(Double.parseDouble(balanceT.getValue())));
			
			//Add the new account to the database
			newAcc.addToDB();
			//Check that the account is now in the database
			if(newAcc.inDB()){
				return newAcc.getAccountNumber();
			}else{
				return "-1";
			}
		}
		return "-1";
	}

	//edit account "a"
	private boolean editAccount(){
		a.setType(typeT.getValue().toString());
		a.setInterestRate(BigDecimal.valueOf(Double.parseDouble(interestT.getValue())));
		BigDecimal temp = a.getInterestRate().setScale(2,RoundingMode.HALF_UP);
		a.setInterestRate(temp);
		a.setOverdraftLimit(Integer.valueOf(overdraftT.getValue()));
		a.setActualBalance(BigDecimal.valueOf(Double.valueOf(balanceT.getValue())));
		a.setSortcode(sCodeT.getValue());
		return a.updateThis();
	}

	private boolean validateSimple(){
		//Checks customer number, sort code, type, interest, overdraft and balance fields are populated
		if(cusNumT.getValue().isEmpty())
		{
			return false;
		}
		try
		{
			Long.parseLong(cusNumT.getValue());
		}
		catch(NumberFormatException e)
		{
			return false;
		}

		if(sCodeT.getValue().isEmpty())
		{
			return false;
		}
		if(typeT.getValue().toString().isEmpty())
		{
			return false;
		}

		if(interestT.getValue().isEmpty())
		{
			return false;
		}
		try
		{
			BigDecimal tempBD = BigDecimal.valueOf(Double.parseDouble(interestT.getValue()));
			if(tempBD.doubleValue() < 0)
			{
				return false;
			}
		}
		catch(NumberFormatException e)
		{
			return false;
		}

		if(overdraftT.getValue().isEmpty())
		{
			return false;
		}
		try
		{
			if(Integer.parseInt(overdraftT.getValue()) < 0)
			{
				return false;
			}
		}
		catch(NumberFormatException e)
		{
			return false;
		}

		return !(balanceT.getValue().isEmpty());
	}
	
	//Get sortcode
	private static String getSortcode(){
		if(sortcode == null)
		{
			SortCodeResource mySortCodeResource = new SortCodeResource();
			Response mySortCodeJSON = mySortCodeResource.getSortCode();
			sortcode = ((String) mySortCodeJSON.getEntity()).substring(13, 19);
		}
		return sortcode;
	}
	//Set sortcode
	private static void setSortcode(){
		if(sortcode == null)
		{
			SortCodeResource mySortCodeResource = new SortCodeResource();
			Response mySortCodeJSON = mySortCodeResource.getSortCode();
			sortcode = ((String) mySortCodeJSON.getEntity()).substring(13, 19);
		}

	}

}
