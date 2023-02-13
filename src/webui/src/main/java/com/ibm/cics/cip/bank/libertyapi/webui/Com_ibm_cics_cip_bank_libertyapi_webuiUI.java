/*
 *
 *    Copyright IBM Corp. 2023
 *
 *
 */
package com.ibm.cics.cip.bank.libertyapi.webui;

import javax.servlet.annotation.WebServlet;

import com.vaadin.annotations.Theme;
import com.vaadin.annotations.VaadinServletConfiguration;
import com.vaadin.server.VaadinRequest;
import com.vaadin.server.VaadinServlet;
import com.vaadin.ui.UI;

/**
 * This class is part of the "Vaadin" user interface.
 */

@SuppressWarnings("serial")
@Theme("webui")

public class Com_ibm_cics_cip_bank_libertyapi_webuiUI extends UI
{

	
	@WebServlet(value = "/*", asyncSupported = true)
	@VaadinServletConfiguration(productionMode = false, ui = Com_ibm_cics_cip_bank_libertyapi_webuiUI.class)
	public static class Servlet extends VaadinServlet
	{
		
	}


	@Override
	protected void init(VaadinRequest request)
	{
		UI ui = this;

		// Set the theme of the program
		setTheme("webui");
		// Set the content to be shown on launch
		setContent(new Welcome(ui, ""));

	}

}