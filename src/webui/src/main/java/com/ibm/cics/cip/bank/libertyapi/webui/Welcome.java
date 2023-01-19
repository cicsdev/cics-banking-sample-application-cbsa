/*
 *
 *    Copyright IBM Corp. 2022
 *
 *
 */

package com.ibm.cics.cip.bank.libertyapi.webui;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.LogManager;
import java.util.logging.Logger;

import javax.ws.rs.core.Response;

import com.ibm.cics.cip.bankliberty.api.json.CompanyNameResource;
import com.ibm.json.java.JSONObject;

import com.vaadin.ui.Alignment;
import com.vaadin.ui.Button;
import com.vaadin.ui.HorizontalLayout;
import com.vaadin.ui.Label;
import com.vaadin.ui.UI;
import com.vaadin.ui.VerticalLayout;

import com.vaadin.ui.Button.ClickEvent;

/**
 * @author georgerushton
 *
 */

/**
 * This class is part of the "Vaadin" user interface. It is the first page you
 * see!
 */

public class Welcome extends VerticalLayout
{

	static final String COPYRIGHT = "Copyright IBM Corp. 2022";

	/**
	 * This screen's UI
	 */
	private static final long serialVersionUID = 1L;
	private static Logger logger = Logger.getLogger("com.example.com_ibm_cics_cip_bank_libertyapi_webui.Welcome");
	// overall ui, needed to change content view
	private UI ui;

	public Welcome(UI ui, String user)
	{
		sortOutLogging();
		this.ui = ui;
		// create an add header
		HbHeader header = new HbHeader();
		this.addComponent(header);
		this.setExpandRatio(header, 0.1f);
		// create and add labels
		addLabels(user);
		// create and add buttons
		addButtons();

	}

	private void addLabels(String user)
	{

		// creation
		Label welcomeText;
		CompanyNameResource myCompanyNameResource = new CompanyNameResource();
		Response myCompanyNameResponse = null;

		myCompanyNameResponse = myCompanyNameResource.getCompanyName();

		if (myCompanyNameResponse.getStatus() == 200)
		{
			String myCompanyNameString = myCompanyNameResponse.getEntity().toString();

			JSONObject myCompanyNameJSON;
			try
			{
				// Set welcome message to use company name and user name
				myCompanyNameJSON = JSONObject.parse(myCompanyNameString);

				String companyName = (String) myCompanyNameJSON.get("companyName");
				welcomeText = new Label("Welcome to " + companyName + " " + user);
			}
			catch (IOException e)
			{
				// Set welcome message to use just user name
				logger.severe(e.toString());
				welcomeText = new Label("Welcome to CICS Bank Sample Application " + user + "!");
			}

		}
		else
		{
			// Set welcome message to use just user name
			welcomeText = new Label("Welcome to CICS Bank Sample Application " + user + "!");
		}

		Label selectText = new Label("Please select an option");
		welcomeText.setWidth(null);
		selectText.setWidth(null);

		// property edits + add to there own section (layout)

		VerticalLayout labels = new VerticalLayout();
		labels.setWidth("100%");
		labels.setHeight("100%");

		labels.addComponent(welcomeText);
		labels.setComponentAlignment(welcomeText, Alignment.MIDDLE_CENTER);
		labels.addComponent(selectText);
		labels.setComponentAlignment(selectText, Alignment.MIDDLE_CENTER);

		// add section to class layout + class properties

		this.addComponent(labels);
		this.setExpandRatio(labels, 0.2f);
		this.setHeight("100%");
	}

	private void addButtons()
	{
		// This bit adds the buttons that we see on the screen
		final Welcome cur = this;
		// create button list (new buttons only need to be added here)
		List<Button> buttons = new ArrayList<>();
		buttons.add(new Button("List / Search Accounts"));
		buttons.add(new Button("Add Account"));
		buttons.add(new Button("List / Search Customers"));
		buttons.add(new Button("Add Customer"));

		// create horizontal layout list
		List<HorizontalLayout> hls = new ArrayList<>();

		// buttons vertical layout (all horizontal layouts added to this)
		VerticalLayout optionsVl = new VerticalLayout();
		optionsVl.setWidth("100%");
		optionsVl.setHeight("100%");

		HorizontalLayout hl = new HorizontalLayout();

		// create all buttons and stick to style

		for (int i = 0; i < buttons.size(); i++)
		{
			final int i_ = i;
			buttons.get(i).setWidth("75%");
			buttons.get(i).setId("button" + i);
			// every 2 buttons create new horizontal layout, starting from 0

			if (i % 2 == 0)
			{
				hl = new HorizontalLayout();
				hl.setWidth("80%");
			}
			else
			{
				hl.setComponentAlignment(buttons.get(i), Alignment.MIDDLE_RIGHT);
			}
			hl.addComponent(buttons.get(i));

			hls.add(hl);
			optionsVl.addComponent(hl);
			optionsVl.setComponentAlignment(hl, Alignment.MIDDLE_CENTER);

			buttons.get(i).addClickListener(new Button.ClickListener()
			{
				/**
				 * 
				 */
				private static final long serialVersionUID = 2410530021932378287L;

				@SuppressWarnings("unused")
				public void buttonClick(ClickEvent event)
				{
					int temp = 0;
					switch (i_)
					{
					case 0: // Account List
						try
						{
							ui.setContent(new AccList(ui, cur));
						}
						catch (NumberFormatException nfe)
						{
							event.getButton().setCaption("Could not get load account list: GETSORTCODE link failed");
						}
						break;
					case 1: // Create Account
						ui.setContent(new AccountUI(ui, cur));
						break;
					case 2: // Customer List
						try
						{
							ui.setContent(new CustList(ui, cur));
						}
						catch (NumberFormatException nfe)
						{
							event.getButton().setCaption("Could not get load customer list: GETSORTCODE link failed");
						}
						break;
					case 3: // Create Customer
						ui.setContent(new CustomerUI(ui, cur));
						break;
					default:
					}
				}
			});
		}
		// Add options container to the ui
		this.addComponent(optionsVl);
		this.setExpandRatio(optionsVl, 0.7f);
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
	public boolean equals(Object obj)
	{
		return super.equals(obj);
	}

}
