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
import java.text.SimpleDateFormat;

import javax.servlet.ServletException;


import com.ibm.cics.cip.bankliberty.webui.dataAccess.CustomerList;
import com.vaadin.ui.Button;
import com.vaadin.ui.CheckBox;
import com.vaadin.ui.HorizontalLayout;
import com.vaadin.ui.Label;
import com.vaadin.ui.Slider;
import com.vaadin.ui.TextField;
import com.vaadin.ui.UI;
import com.vaadin.ui.VerticalLayout;
import com.vaadin.ui.Button.ClickEvent;



/**
 * @author georgerushton
 *
 */

/**
 * This class is part of the "Vaadin" user interface. 
 */




public class CustList extends VerticalLayout{

    static final String COPYRIGHT =
      "Copyright IBM Corp. 2022";



	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private static Logger logger = Logger.getLogger("com.example.com_ibm_cics_cip_bank_libertyapi_webui.Cust_list");
	private CustomerList cList = new CustomerList();
	private UI ui;
	private int limit = 50;
	private int offset = 0;
	Label page = new Label();
	private VerticalLayout vl = new VerticalLayout();
	TextField cusNumT;
	TextField cusNameT;
	CheckBox lt;
	CheckBox mt;
	Slider balance;
	String filter = "";
	int cur = 1;
	int next;

	public CustList(UI ui, String user, Welcome back)
	{
		sortOutLogging();
		this.ui = ui;
		HB_Header header = new HB_Header(ui, "Welcome", back);
		this.addComponent(header);
		this.setExpandRatio(header, 0.1f);
		createSearch();
		
		//Create new container "head" and scale
		HorizontalLayout head = new HorizontalLayout();
		head.setWidth("100%");
		
		//Create labels
		Label cusLbl = new Label("Customer Number");
		Label nameLbl = new Label("Name");
		Label addressLbl = new Label("Address");
		Label dobLbl = new Label("Date of Birth dd-M-yyyy");
		
		//Create new container "sA" and scale
		HorizontalLayout sA = new HorizontalLayout();

		Button bP_cust = new Button("<-");
		bP_cust.setId("bbutton_cust");
		bP_cust.setStyleName("link"); 

		bP_cust.addClickListener(new Button.ClickListener() {
			/**
			 * 
			 */
			private static final long serialVersionUID = -8897971980390983605L;

			//Go back a page: offset from UI should now be  + offset
			public void buttonClick(ClickEvent event) {
				if(offset != 0){
					offset -= limit;
					if(offset<0) offset = 0;
					createCusList(filter);
				}
			}
		});

		Button bPM_cust = new Button("<<<-");
		bPM_cust.setId("bmbutton_cust");
		bPM_cust.setStyleName("link"); 

		bPM_cust.addClickListener(new Button.ClickListener() {
			/**
			 * 
			 */
			private static final long serialVersionUID = -140037750120660537L;
			//Go back 10 pages
			public void buttonClick(ClickEvent event) {
				if(offset != 0){
					offset -= limit*10;
					if(offset < 0){
						offset = 0;
					}
					createCusList(filter);
				}
			}
		});

		Button fP_cust = new Button("->");
		fP_cust.setId("fbutton_cust");
		fP_cust.setStyleName("link"); 

		fP_cust.addClickListener(new Button.ClickListener() {
			/**
			 * 
			 */
			private static final long serialVersionUID = 2306302397999195316L;
			//Go forward a page
			public void buttonClick(ClickEvent event) {
				if(cur < next){
					offset += limit;
					createCusList(filter);
				}
			}
		});

		Button fPM_cust = new Button("->>>");
		fPM_cust.setId("fmbutton_cust");
		fPM_cust.setStyleName("link"); 

		fPM_cust.addClickListener(new Button.ClickListener() {
			/**
			 * 
			 */
			private static final long serialVersionUID = -1097487417126789780L;
			//Go forward 10 pages
			public void buttonClick(ClickEvent event) {
				if((cur+10) < next){
					offset += limit*10;
					createCusList(filter);
				}
				else
				{
					int curNext = cur;
					for(curNext = cur; curNext < next; curNext++)
					{
						offset += limit;
					}
					createCusList(filter);
				}
			}
		});

		//Add components to sA container
		sA.addComponent(bPM_cust);
		sA.addComponent(bP_cust);
		sA.addComponent(page);
		sA.addComponent(fP_cust);
		sA.addComponent(fPM_cust);
		
		//Add components to head container
		head.addComponent(cusLbl);
		head.addComponent(nameLbl);
		head.addComponent(addressLbl);
		head.addComponent(dobLbl);
		
		//Add sA container to head container
		head.addComponent(sA);
		
		//Add containers to UI		
		this.addComponent(head);
		this.addComponent(vl);
		createCusList(filter);

	}

	private void createSearch(){
		//Create a new container "searchL"
		HorizontalLayout searchL = new HorizontalLayout();

		//Create a new container "cvl"
		VerticalLayout cvl = new VerticalLayout();
		searchL.setWidth("100%");

		//Create new textfields for customer number and name entry
		cusNumT = new TextField("Customer Number");
		cusNameT = new TextField("Customer Name");

		//Add components to searchL container
		searchL.addComponent(cusNumT);
		searchL.addComponent(cusNameT);

		searchL.setExpandRatio(cusNumT, 0.2f);
		searchL.setExpandRatio(cusNameT, 0.2f);

		Button b = new Button("Search");

		b.addClickListener(new Button.ClickListener() {

			private static final long serialVersionUID = -2311472772099591790L;

			public void buttonClick(ClickEvent event) {
				//Set filter contents
				filter = "";
				if(cusNumT.getValue().length() > 0){
					filter = " AND CUSTOMER_NUMBER = "+String.format("%010d",Long.valueOf(cusNumT.getValue()));
				}
				else 
				{
					if(cusNameT.getValue().length() > 0)
					{
						filter = " AND CUSTOMER_NAME like '"+ cusNameT.getValue() + "'";
					}
				}
				limit = 50;
				offset = 0;
				createCusList(filter);
			}
		});

		//Add to searchL container
		searchL.addComponent(b);
		searchL.setExpandRatio(b, 0.3f);

		//Add searchL container to the UI
		this.addComponent(searchL);
	}

	private ArrayList<String> setupSearch(ArrayList<String> arr){
		ArrayList<String> outcome = arr;

		return outcome;
	}



	private void createCusList(String filter){
		vl.removeAllComponents();
		try {

			//TEST.out.println("TEST - cList.doGet - limit: " + limit + ", offset: " + offset + ", filter: " + filter);

			//get list of accounts within limit and offset, using the filter
			cList.doGet(limit, offset, filter);
			int total = cList.getCount(filter);
			if(offset>total)
			{
				offset = total;
			}

			page.setValue(((offset/limit)+1)+"/"+(((int)Math.ceil((total/limit))+1))); 
			if((((int)Math.ceil((cList.getCount(filter)/limit)))) == 0)
			{
				page.setValue("0/0");
				if(cList.getCount(filter)>0)
				{
					page.setValue("1/1");
				}
			}
			//TEST.out.println("TEST - GOT COUNT OF CUSTOMERS");
			cur = ((offset/limit)+1);
			next = cur + limit;
			if (next > total)
			{
				next = cur;
			}
//			next = (int)Math.ceil((cList.getCount(filter)/limit))+1; 

		} 
		catch (ServletException | IOException e1) 
		{
			logger.severe(e1.toString());
		}
		for(int i = 0; i<this.cList.size(); i++){
			HorizontalLayout hl = new HorizontalLayout();
			hl.setWidth("100%");

			//	//TEST.out.println("CUST_LIST: INDEX -  " + (i+1) + "  CUSTOMER NUMBER -  "+ this.cList.getCustomer(i).getCustomer_number());
			//Create new labels and populate to show customer number, name, address, DOB			
			Label cusNumb = new Label(this.cList.getCustomer(i).getCustomer_number());
			Label name = new Label(this.cList.getCustomer(i).getName());
			Label address = new Label(this.cList.getCustomer(i).getAddress());
			SimpleDateFormat ddMyyyy = new SimpleDateFormat("dd-M-yyyy");
			Label dob = new Label(ddMyyyy.format(this.cList.getCustomer(i).getDob()));
			
			//Add labels to hl container
			hl.addComponent(cusNumb);
			hl.addComponent(name);
			hl.addComponent(address);
			hl.addComponent(dob);

			//Create new container and scale
			HorizontalLayout hl_buttons = new HorizontalLayout();
			hl_buttons.setWidth("100%");

			//Create new buttons for "Accounts", "edit" and "delete"
			Button showAcc = new Button("Accounts");
			Button edit = new Button("Edit");
			Button delete = new Button("Delete");

			//Add components to hl_buttons container
			hl_buttons.addComponent(showAcc);
			hl_buttons.addComponent(edit);
			hl_buttons.addComponent(delete);

			hl_buttons.setExpandRatio(showAcc, 0.3f);
			hl_buttons.setExpandRatio(edit, 0.3f);
			hl_buttons.setExpandRatio(delete, 0.3f);

			//Add "hl_buttons" container to "hl" container
			hl.addComponent(hl_buttons);

			//Add "hl" container to "vl" container
			vl.addComponent(hl);

			final int temp = i;
			showAcc.addClickListener(new Button.ClickListener() {
				/**
				 * Set UI to list of accounts
				 */
				private static final long serialVersionUID = 3139185739387258242L;

				public void buttonClick(ClickEvent event) {
					ui.setContent(new AccList(ui, "test", new Welcome(ui, "Welcome"),cList.getCustomer(temp)));
				}
			});
			edit.addClickListener(new Button.ClickListener() {
				/**
				 * set UI to customer UI
				 */
				private static final long serialVersionUID = -4092770723316728200L;

				public void buttonClick(ClickEvent event) {
					ui.setContent(new CustomerUI(ui, "test", new Welcome(ui, "Welcome"), cList.getCustomer(temp)));
				}
			});
			delete.addClickListener(new Button.ClickListener() {
				/**
				 * delete customer from database
				 */
				private static final long serialVersionUID = -6352320168324201352L;

				public void buttonClick(ClickEvent event) {
					String filter = "";
					cList.getCustomer(temp).deleteFromDB();
					createCusList(filter);
				}
			});

		}
	}
	private void sortOutLogging()
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