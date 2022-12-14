/*
 *
 *    Copyright IBM Corp. 2022
 *
 *
 */
package com.ibm.cics.cip.bank.libertyapi.webui;

import java.io.IOException;
import java.util.ArrayList;
import java.util.logging.LogManager;
import java.util.logging.Logger;

import javax.servlet.ServletException;

import com.ibm.cics.cip.bankliberty.webui.dataAccess.AccountList;
import com.ibm.cics.cip.bankliberty.webui.dataAccess.Customer;
import com.vaadin.data.Property.ValueChangeEvent;
import com.vaadin.ui.Button;
import com.vaadin.ui.HorizontalLayout;
import com.vaadin.ui.Label;
import com.vaadin.ui.Slider;
import com.vaadin.ui.TextField;
import com.vaadin.ui.UI;
import com.vaadin.ui.VerticalLayout;
import com.vaadin.ui.Button.ClickEvent;
import com.vaadin.ui.CheckBox;

/**
 * @author georgerushton
 * This class is part of the "Vaadin" user interface. It is used to list accounts.
 */



public class Acc_list extends VerticalLayout{

    static final String COPYRIGHT =
      "Copyright IBM Corp. 2022";


	private static final long serialVersionUID = 1L;
	private static Logger logger = Logger.getLogger("com.example.com_ibm_cics_cip_bank_libertyapi_webui.Acc_list");
	private AccountList aList = new AccountList();
	private UI ui;
	private int limit = 50;
	private int offset = 0;
	Label page = new Label();
	private VerticalLayout vl = new VerticalLayout();
	TextField cusNumT;
	TextField accNumT;
	CheckBox lt;
	CheckBox mt;
	Slider balance;
	String filter = "";
	int cur = 1;
	int next;


	public Acc_list(UI ui,  Welcome back){
		sortOutLogging();
		logger.entering(this.getClass().getName(),"Acc_list(UI ui,  welcome back)");
		this.ui = ui;
		HB_Header header = new HB_Header(ui, "Welcome", back);
		this.addComponent(header);
		this.setExpandRatio(header, 0.1f);
		createSearch();
		
		//Create new container and scale -  a container stores components of a UI, such as buttons, labels etc.
		HorizontalLayout head = new HorizontalLayout();
		head.setWidth("100%");
		
		//Create labels
		Label accLbl = new Label("Account No.");
		Label cusLbl = new Label("Customer No.");
		Label avbLbl = new Label("Available Balance");
		Label acbLbl = new Label("Actual Balance");
		
		//Create a new container
		HorizontalLayout sA = new HorizontalLayout();
		
		Button bP = new Button("<-");
		bP.setId("bbutton");
		bP.setStyleName("link"); 
		
		bP.addClickListener(new Button.ClickListener() {
			/**
			 * 
			 */
			private static final long serialVersionUID = -2915663639842484327L;

			public void buttonClick(ClickEvent event) {
				if(offset != 0){
					offset -= limit;
					if(offset<0) offset=0;
					//("Go back a page: offset from UI should now be " + offset);
					createAccList(filter);
				}
			}
		});
		
		Button bPM = new Button("<<<-");
		bPM.setId("bmbutton");
		bPM.setStyleName("link"); 
		
		bPM.addClickListener(new Button.ClickListener() {
			/**
			 * 
			 */
			private static final long serialVersionUID = 7702513785219853137L;

			public void buttonClick(ClickEvent event) {
				if(offset != 0){
					offset -= limit*10;
					if(offset < 0){
						offset = 0;
					}
					//("Go back 10 pages: offset from UI should now be " + offset);
					createAccList(filter);
				}
			}
		});
		
		Button fP = new Button("->");
		fP.setId("fbutton");
		fP.setStyleName("link"); 
		
		fP.addClickListener(new Button.ClickListener() {
			/**
			 * 
			 */
			private static final long serialVersionUID = -8009915481013975422L;

			public void buttonClick(ClickEvent event) {
				if(cur < next){
					offset += limit;
					//("Go forward a page: offset from UI should now be " + offset);
					createAccList(filter);
				}
			}
		});
		
		Button fPM = new Button("->>>");
		fPM.setId("fmbutton");
		fPM.setStyleName("link"); 
		
		fPM.addClickListener(new Button.ClickListener() {
			/**
			 * 
			 */
			private static final long serialVersionUID = -922311815047591792L;

			public void buttonClick(ClickEvent event) {
				if((cur+10) < next){
					offset += limit*10;
					//("Go forward 10 pages: offset from UI should now be " + offset);
					createAccList(filter);
				}
				else
				{
					int curNext = cur;
					for(curNext = cur; curNext < next; curNext++)
					{
						offset += limit;
					}
					createAccList(filter);
				}
			}
		});
		
		//Add components to the sA container
		sA.addComponent(bPM);
		sA.addComponent(bP);
		sA.addComponent(page);
		sA.addComponent(fP);
		sA.addComponent(fPM);
		
		//Add components to the head container
		head.addComponent(accLbl);
		head.addComponent(cusLbl);
		head.addComponent(avbLbl);
		head.addComponent(acbLbl);
		head.addComponent(sA);
		
		//Add containers to the ui
		this.addComponent(head);
		this.addComponent(vl);
		createAccList(filter);
		logger.exiting(this.getClass().getName(),"Acc_list(UI ui,  welcome back)");

	}
	
	public Acc_list(UI ui, String string, Welcome back, Customer customer) {
		sortOutLogging();
		logger.entering(this.getClass().getName(),"Acc_list(UI ui, String string, welcome back, Customer customer) for customer " + customer.getName());
		this.ui = ui;
		
		//Create a header, add to the UI and scale
		HB_Header header = new HB_Header(ui, "welcome", back);
		this.addComponent(header);
		this.setExpandRatio(header, 0.1f);
		
		createSearch(customer.getCustomer_number());
		
		//Create new container
		HorizontalLayout head = new HorizontalLayout();
		head.setWidth("100%");
		
		//Create labels
		Label accLbl = new Label("Account No.");
		Label cusLbl = new Label("Customer No.");
		Label avbLbl = new Label("Available Balance");
		Label acbLbl = new Label("Actual Balance");
		HorizontalLayout sA = new HorizontalLayout();
		
		Button bP = new Button("<-");
		bP.setId("bbutton");
		bP.setStyleName("link"); 
		
		bP.addClickListener(new Button.ClickListener() {
			/**
			 * 
			 */
			private static final long serialVersionUID = -2915663639842484327L;

			public void buttonClick(ClickEvent event) {
				if(offset != 0){
					offset -= limit;
					if(offset<0)
					{
						offset=0;
					}
					//("Go back a page: offset from UI should now be " + offset);
					createAccList(filter);
				}
			}
		});
		
		Button bPM = new Button("<<<-");
		bPM.setId("bmbutton");
		bPM.setStyleName("link"); 
		
		bPM.addClickListener(new Button.ClickListener() {
			/**
			 * 
			 */
			private static final long serialVersionUID = 7702513785219853137L;

			public void buttonClick(ClickEvent event) {
				if(offset != 0){
					offset -= limit*10;
					if(offset < 0){
						offset = 0;
					}
					//("Go back 10 pages: offset from UI should now be " + offset);
					createAccList(filter);
				}
			}
		});
		
		Button fP = new Button("->");
		fP.setId("fbutton");
		fP.setStyleName("link"); 
		
		fP.addClickListener(new Button.ClickListener() {
			/**
			 * 
			 */
			private static final long serialVersionUID = -8009915481013975422L;

			public void buttonClick(ClickEvent event) {
				if(cur < next){
					offset += limit;
					//("Go forward a page: offset from UI should now be " + offset);
					createAccList(filter);
				}
			}
		});
		
		Button fPM = new Button("->>>");
		fPM.setId("fmbutton");
		fPM.setStyleName("link"); 
		
		fPM.addClickListener(new Button.ClickListener() {
			/**
			 * 
			 */
			private static final long serialVersionUID = -922311815047591792L;

			public void buttonClick(ClickEvent event) {
				if((cur+10) < next){
					offset += limit*10;
					//("Go forward 10 pages: offset from UI should now be " + offset);
					createAccList(filter);
				}
			}
		});
		
		//Add components to sA container
		sA.addComponent(bPM);
		sA.addComponent(bP);
		sA.addComponent(page);
		sA.addComponent(fP);
		sA.addComponent(fPM);
		
		//Add components to head container
		head.addComponent(accLbl);
		head.addComponent(cusLbl);
		head.addComponent(avbLbl);
		head.addComponent(acbLbl);
		
		//Add the sA container to the head container
		head.addComponent(sA);
		
		//Add head and vl containers to the UI
		this.addComponent(head);
		this.addComponent(vl);
		String filter = " AND ACCOUNT_CUSTOMER_NUMBER = " + String.format("%010d",Integer.valueOf(customer.getCustomer_number())) + "";
		createAccList(filter);
		logger.exiting(this.getClass().getName(),"Acc_list(UI ui, String string, welcome back, Customer customer) for customer " + customer.getName());


	}

	private void createSearch(String customer_number) {
		//Search for a customer using customer number		
		sortOutLogging();
		logger.entering(this.getClass().getName(),"createSearch(String customer_number) for customer number " + customer_number);
		HorizontalLayout searchL = new HorizontalLayout();
		VerticalLayout cvl = new VerticalLayout();
		searchL.setWidth("100%");
		
		//Enter the customer number
		cusNumT = new TextField("Customer Number");
		cusNumT.setValue(customer_number);
		
		//Enter the customer account number
		accNumT = new TextField("Account Number");
		
		//Enter the balance
		balance = new Slider(1, 999999);
		mt = new CheckBox(">");
		lt = new CheckBox("<");
		
		balance.setCaption("Balance");
		
		//Add account number, customer number and balance components to "searchL" container
		searchL.addComponent(accNumT);
		searchL.addComponent(cusNumT);
		searchL.addComponent(balance);
		balance.setWidth("100%");
		
		//Add checkbox components to "cvl" container
		cvl.addComponent(lt);
		cvl.addComponent(mt);
		
		//Add "cvl" container into "searchL" container
		searchL.addComponent(cvl);
		
		//Set scaling for "searchL"
		searchL.setExpandRatio(cusNumT, 0.2f);
		searchL.setExpandRatio(accNumT, 0.2f);
		searchL.setExpandRatio(balance, 0.2f);
		searchL.setExpandRatio(cvl, 0.05f);
		
		
		Button b = new Button("Search");
		
		lt.addValueChangeListener(new CheckBox.ValueChangeListener(){
			/**
			 * 
			 */
			private static final long serialVersionUID = -5604047422477446846L;

			public void valueChange(ValueChangeEvent event) {
				//If lt (<) selected, set mt (>) to false
				if(lt.getValue()){
					mt.setValue(false);
				}
			}
		});
		mt.addValueChangeListener(new CheckBox.ValueChangeListener(){
			/**
			 * 
			 */
			private static final long serialVersionUID = -1045075489262886096L;

			public void valueChange(ValueChangeEvent event) {
				//If mt (>) selected, set lt (<) to false
				if(mt.getValue()){
					lt.setValue(false);
				}
			}
		});

				
		b.addClickListener(new Button.ClickListener() {
			/**
			 * 
			 */
			private static final long serialVersionUID = 7623423428229097831L;

			public void buttonClick(ClickEvent event) {
				filter = "";
				//Set the filter value
				if(!cusNumT.getValue().isEmpty() && !accNumT.getValue().isEmpty()){
					//use account number
					filter = " AND ACCOUNT_NUMBER = "+String.format("%08d",Integer.valueOf(accNumT.getValue()));
				}
				if(!cusNumT.getValue().isEmpty()){
					//use customer number
					filter = " AND ACCOUNT_CUSTOMER_NUMBER = "+String.format("%010d",Long.valueOf(cusNumT.getValue()));
				}
				if(!accNumT.getValue().isEmpty()){
					//use account number
					filter = " AND ACCOUNT_NUMBER = "+String.format("%08d",Integer.valueOf(accNumT.getValue()));
				}
				if(lt.getValue()){
					//use balance < than selected
					filter = " AND ACCOUNT_AVAILABLE_BALANCE <= "+balance.getValue();
				}
				if(mt.getValue()){
					//use balance > than selected
					filter = " AND ACCOUNT_AVAILABLE_BALANCE >= "+balance.getValue();
				}
				limit = 50;
				offset = 0;
				createAccList(filter);
			}
		});
		
		//Add search button "b" to container
		searchL.addComponent(b);
		searchL.setExpandRatio(b, 0.3f);
		
		//Add "searchL" container to the UI
		this.addComponent(searchL);
		limit = 50;
		offset = 0;
		logger.exiting(this.getClass().getName(),"createSearch(String customer_number) for customer number " + customer_number);

		
	}

	private void createSearch(){
		sortOutLogging();
		logger.entering(this.getClass().getName(),"createSearch()");
		
		//Create new containers
		HorizontalLayout searchL = new HorizontalLayout();
		VerticalLayout cvl = new VerticalLayout();
		searchL.setWidth("100%");
		
		//Create new textfields for customer number and account number entry
		cusNumT = new TextField("Customer Number");
		accNumT = new TextField("Account Number");
		
		//Create new slider for balance entry
		balance = new Slider(1, 999999);
		
		//Create new checkboxes
		mt = new CheckBox(">");
		lt = new CheckBox("<");
		
		balance.setCaption("Balance");
		
		//Add textfields and slider created above to "searchL" container
		searchL.addComponent(accNumT);
		searchL.addComponent(cusNumT);
		searchL.addComponent(balance);
		
		balance.setWidth("100%");
		
		//Add checkboxes created above to "cvl" container
		cvl.addComponent(lt);
		cvl.addComponent(mt);

		//Add the "cvl" container to the "searchL" container
		searchL.addComponent(cvl);
		
		searchL.setExpandRatio(cusNumT, 0.2f);
		searchL.setExpandRatio(accNumT, 0.2f);
		searchL.setExpandRatio(balance, 0.2f);
		searchL.setExpandRatio(cvl, 0.05f);
		
		//Create new button "b", labelled 'Search'
		Button b = new Button("Search");
		
		lt.addValueChangeListener(new CheckBox.ValueChangeListener(){
			private static final long serialVersionUID = -5604047422477446846L;
			//If lt (<) selected then mt (>) set to false
			public void valueChange(ValueChangeEvent event) {
				if(lt.getValue()){
					mt.setValue(false);
				}
			}
		});
		mt.addValueChangeListener(new CheckBox.ValueChangeListener(){
			private static final long serialVersionUID = -1045075489262886096L;
			//If mt (>) selected then lt (<) set to false
			public void valueChange(ValueChangeEvent event) {
				if(mt.getValue()){
					lt.setValue(false);
				}
			}
		});

				
		b.addClickListener(new Button.ClickListener() {
			//Set the filter
			private static final long serialVersionUID = 7623423428229097831L;

			public void buttonClick(ClickEvent event) {
				filter = "";
				if(!cusNumT.getValue().isEmpty() && !accNumT.getValue().isEmpty()){
					//Use account number
					filter = " AND ACCOUNT_NUMBER = "+String.format("%08d",Integer.valueOf(accNumT.getValue()));
				}
				else if(!cusNumT.getValue().isEmpty()){
					//Use customer number
					filter = " AND ACCOUNT_CUSTOMER_NUMBER = "+String.format("%010d",Integer.valueOf(cusNumT.getValue()));
				}
				else if(!accNumT.getValue().isEmpty()){
					//Use account number
					filter = " AND ACCOUNT_NUMBER = "+String.format("%08d",Integer.valueOf(accNumT.getValue()));
				}
				if(lt.getValue()){
					//where account balance < input balance
					filter = " AND ACCOUNT_AVAILABLE_BALANCE <= "+balance.getValue();
				}
				if(mt.getValue()){
					//where account balance > input balance
					filter = " AND ACCOUNT_AVAILABLE_BALANCE >= "+balance.getValue();
				}
				limit = 50;
				offset = 0;
				createAccList(filter);
				
			}
		});
		
		//Add button "b" to "searchL" container
		searchL.addComponent(b);
		searchL.setExpandRatio(b, 0.3f);
		
		//Add "searchL" container to the UI
		this.addComponent(searchL);
		logger.exiting(this.getClass().getName(),"createSearch()");
	}
	
	@SuppressWarnings("unused")
	private ArrayList<String> setupSearch(ArrayList<String> arr){
		ArrayList<String> outcome = arr;
		
		return outcome;
	}
	
	
	
	private void createAccList(final String filter){
		sortOutLogging();
		logger.entering(this.getClass().getName(),"createAccList(final String filter) for filter " + filter);
		
		vl.removeAllComponents();
		try {
			
			//Get all accounts within the limit and offset that match the filter			
			aList.doGet(limit, offset, filter);
			
			int total = aList.howMany(filter);
			

			page.setValue(((offset/limit)+1)+"/"+(((int)Math.ceil((total/limit))+1))); 
			if((((int)Math.ceil((aList.getCount(filter)/limit)))) == 0)
			{
				page.setValue("0/0");
				if(aList.getCount(filter)>0)
				{
					page.setValue("1/1");
				}
			}

			cur = ((offset/limit)+1);

			next = cur + limit;
		} catch (ServletException e1) {
			e1.printStackTrace();
		} catch (IOException e1) {
			e1.printStackTrace();
		}
		for( int i = 0; i<this.aList.size(); i++){
			HorizontalLayout hl = new HorizontalLayout();
			hl.setWidth("100%");
			
			final Label accNumb = new Label(this.aList.getAccount(i).getAccount_number());
			Label cusNumb = new Label(this.aList.getAccount(i).getCustomer_number());
			Label avb = new Label(String.valueOf(this.aList.getAccount(i).getAvailable_balance()));
			Label acb = new Label(String.valueOf(this.aList.getAccount(i).getActual_balance()));
			hl.addComponent(accNumb);
			hl.addComponent(cusNumb);
			hl.addComponent(avb);
			hl.addComponent(acb);
			
			
			HorizontalLayout hl_buttons = new HorizontalLayout();
			hl_buttons.setWidth("100%");
			
//			Button transfer = new Button("Transfer");
			
			//Create new buttons with labels 'edit' and 'delete'
			Button edit = new Button("Edit");
			Button delete = new Button("Delete");
			
//			hl_buttons.addComponent(transfer);
			
			//Add buttons to container "hl_buttons"
			hl_buttons.addComponent(edit);
			hl_buttons.addComponent(delete);
			
//			hl_buttons.setExpandRatio(transfer, 0.4f);
			hl_buttons.setExpandRatio(edit, 0.2f);
			hl_buttons.setExpandRatio(delete, 0.4f);
			
			//Add "hl_buttons" container to "hl" container
			hl.addComponent(hl_buttons);
			
			//Add "hl" container to "vl" container
			vl.addComponent(hl);
			

			final AccountList alist = this.aList;
			final int temp = i;
			edit.addClickListener(new Button.ClickListener() {
				/**
				 * 
				 */
				private static final long serialVersionUID = 8881026371623070226L;

				public void buttonClick(ClickEvent event) {
					ui.setContent(new AccountUI(ui, "test", new Welcome(ui, "Welcome"),aList.getAccount(temp)));
				}
			});
			delete.addClickListener(new Button.ClickListener() {
				private static final long serialVersionUID = -7523168693466039285L;
				//Set UI to account UI
				public void buttonClick(ClickEvent event) {
					//("About to delete from DB");
					alist.getAccount(temp).deleteFromDB();
					//("The delete didItWork says " + didItWork);
					//("About to create acclist with filter " + filter);
					offset = 0;
					limit = 50;
					createAccList(filter);
					//("Back from create acclist with filter " + filter);
				}
			});
			
//			transfer.addClickListener(new Button.ClickListener() {
//				/**
//				 * 
//				 */
//				private static final long serialVersionUID = 2178931372114325791L;
//
//				/**
//				 * 
//				 */
//
//				public void buttonClick(ClickEvent event) {
//					ui.setContent(new TransferUI(ui, "test", new welcome(ui, "welcome"),aList.getAccount(temp)));
//				}
//			});

			
		}
		logger.exiting(this.getClass().getName(),"createAccList(final String filter) for filter " + filter);
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
}