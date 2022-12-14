/*
 *
 *    Copyright IBM Corp. 2022
 *
 *
 */
package com.ibm.cics.cip.bank.libertyapi.webui;


import javax.servlet.annotation.WebServlet;

import com.vaadin.annotations.Theme;
import com.vaadin.annotations.VaadinServletConfiguration;

import com.vaadin.server.VaadinRequest;
import com.vaadin.ui.UI;

/**
 * This class is part of the "Vaadin" user interface. 
 */






@SuppressWarnings("serial")
@Theme("com_ibm_cics_cip_bank_libertyapi_webui")

public class Com_ibm_cics_cip_bank_libertyapi_webuiUI extends UI {

    static final String COPYRIGHT =
      "Copyright IBM Corp. 2022";



	@WebServlet(value = "/*", asyncSupported = true)
	@VaadinServletConfiguration(productionMode = false, ui = Com_ibm_cics_cip_bank_libertyapi_webuiUI.class)
	public static class Servlet extends JMeterServlet{

    static final String COPYRIGHT =
      "Copyright IBM Corp. 2022";


	}

	@Override
	protected void init(VaadinRequest request) {
		UI ui = this;
//		Authenticator.setDefault(new Authenticator() {
//
//		    @Override
//		    protected PasswordAuthentication getPasswordAuthentication() {          
//		        return new PasswordAuthentication("USERID", ("PASSWORD").toCharArray());
//		    }
//		});
		//Set the theme of the program
		setTheme("com_ibm_cics_cip_bank_libertyapi_webui");
		//Set the content to be shown on launch
		setContent(new Welcome(ui, ""));					

		
	}

}