/*
 *
 *    Copyright IBM Corp. 2022
 *
 *
 */

package com.ibm.cics.cip.bank.libertyapi.webui;

import java.io.IOException;

import javax.ws.rs.core.Response;

import com.ibm.cics.cip.bankliberty.api.json.CompanyNameResource;
import com.ibm.json.java.JSONObject;
import com.vaadin.ui.Alignment;
import com.vaadin.ui.Button;
import com.vaadin.ui.HorizontalLayout;
import com.vaadin.ui.Label;
import com.vaadin.ui.UI;
import com.vaadin.ui.Button.ClickEvent;

/**
 * This class is part of the "Vaadin" user interface. 
 */




public class HB_Header extends HorizontalLayout{

    static final String COPYRIGHT =
      "Copyright IBM Corp. 2022";


	private static final long serialVersionUID = -5542630839520498092L;
	Button back;
	Label label;

	public HB_Header(UI ui, String name){
		setup();
		this.setComponentAlignment(label, Alignment.MIDDLE_CENTER);
	}

	public HB_Header(final UI ui, String name, final Welcome wlc){
		setupBack();
		setup();

		back.addClickListener(new Button.ClickListener() {
			//Set screen to welcome screen
			private static final long serialVersionUID = 3078798822184263174L;

			public void buttonClick(ClickEvent event) {
				ui.setContent(wlc);
			}
		});
	}

	public HB_Header(final UI ui, String name, final Com_ibm_cics_cip_bank_libertyapi_webuiUI exit){
		setupBack();
		setup();

		//Set screen to exit
		back.addClickListener(new Button.ClickListener() {

			private static final long serialVersionUID = 5051598750473268539L;

			public void buttonClick(ClickEvent event) {
				ui.setContent(exit);
			}
		});
	}



	public HB_Header(final UI ui, String name, final Acc_list acc_list){
		setupBack();
		setup();

		//Set screen to account list
		back.addClickListener(new Button.ClickListener() {
			private static final long serialVersionUID = -4825345661184875623L;

			public void buttonClick(ClickEvent event) {
				ui.setContent(acc_list);
			}
		});
	}

	private void setup(){
		this.setWidth("100%");
		this.setHeight("100px");
		//this.addStyleName("header");
		CompanyNameResource myCompanyNameResource = new CompanyNameResource();
		Response myCompanyNameResponse = null;

		myCompanyNameResponse = myCompanyNameResource.getCompanyName();

		if(myCompanyNameResponse.getStatus() == 200)
		{
			//If we get a valid status from myCompanyNameResponse, set myCompanyNameString to the name held in myCompanyNameResponse
			String myCompanyNameString = myCompanyNameResponse.getEntity().toString();

			JSONObject myCompanyNameJSON;
			try {
				//Set myCompanyNameJSON to myCompanyNameString, then set companyName to the string held in myCompanyNameJSON
				myCompanyNameJSON = JSONObject.parse(myCompanyNameString);

				String companyName = (String) myCompanyNameJSON.get("companyName");
				//Set the label to the company name
				label = new Label(companyName);
			}
			catch (IOException e) {
				//Else set the label to the default "CICS Bank Sample Application"
				e.printStackTrace();
				label = new Label("CICS Bank Sample Application");
			}

		}
		else
		{
			//Default label
			label = new Label("CICS Bank Sample Application");
		}


		//Set scale and positioning for label and add to UI
		label.setWidth(null);
		this.addComponent(label);
		this.setComponentAlignment(label, Alignment.MIDDLE_LEFT);
	}
	
	//Setup the back button
	private void setupBack(){
		back = new Button("<---");
		this.addComponent(back);
		this.setComponentAlignment(back, Alignment.MIDDLE_LEFT);
	}

}
