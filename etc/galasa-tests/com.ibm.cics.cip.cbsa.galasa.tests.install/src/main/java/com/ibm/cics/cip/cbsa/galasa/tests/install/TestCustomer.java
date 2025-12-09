package com.ibm.cics.cip.cbsa.galasa.tests.install;

import static org.assertj.core.api.Assertions.assertThat;

import com.ibm.cics.cip.cbsa.galasa.tests.manager.*;

import dev.galasa.Test;
import dev.galasa.zos3270.FieldNotFoundException;
import dev.galasa.zos3270.KeyboardLockedException;
import dev.galasa.zos3270.TerminalInterruptedException;
import dev.galasa.zos3270.TextNotFoundException;
import dev.galasa.zos3270.TimeoutException;
import dev.galasa.zos3270.spi.NetworkException;


@Test
public class TestCustomer {

	@CbsaCustomer(existing=false)
	public ICbsaCustomer customer;

	@CbsaTerminal
	public ICbsaTerminal terminal;

	/*
	 * This test checks that entering 0 returns a random customer.
	 */
	@Test
	public void randomCustomer() throws Exception {
		try {
			terminal.getRandomCustomer();
			assertThat(terminal.retrieveScreen()).contains("Customer lookup successful");
		} catch (Exception e) {
			throw e;
		}
	}

	/*
	 * This test checks that manually entering a customer number returns information
	 */
	@Test
	public void knownCustomer() throws Exception {
		try {
			terminal.getCustomerByNumber(Long.parseLong(customer.getCustomerNumber()));
			assertThat(terminal.retrieveScreen()).contains("Customer lookup successful");
		} catch (Exception e) {
			throw e;
		}
	}

	/*
	 * This test checks that entering 9999999999 successfully returns the last available customer
	 * */
	@Test
	public void lastCustomer() throws Exception {
		try {
			terminal.getLastCustomer();
			assertThat(terminal.retrieveScreen()).contains("Customer lookup successful");

			// Ensures it was definitely the last customer
			String customerNumber = terminal.retrieveFieldTextAfterFieldWithString("Customer Number").trim();
			terminal.getCustomerByNumber(Long.parseLong(customerNumber)+1);
			assertThat(terminal.retrieveScreen()).contains("Sorry, but that customer number was not found.");
		} catch (Exception e) {
			throw e;
		}
	}

	/*
	 * This test checks that a customers details cannot be updated with invalid data
	 */
	@Test
	public void updateCustomerWithInvalid() throws Exception {
		String[] customerData = ICbsaCustomer.generateRandomCustomerData();
		String name = customerData[1];

		try {
			terminal.goToScreen(CbsaMenuItem.DISPLAY_CUSTOMER);
			terminal.waitForKeyboard().type(customer.getCustomerNumber()).enter().waitForKeyboard().pf10().waitForKeyboard().eraseInput().type("Super " + name + " K Walker").enter().waitForKeyboard();
			assertThat(terminal.retrieveScreen()).contains("Valid titles are:");
		} catch (Exception e) {
			throw e;
		}
	}

	/*
	 * This test checks that a customers data can be updated with valid data
	 */
	@Test
	public void updateCustomerWithValid() throws Exception {
		String[] customerData = ICbsaCustomer.generateRandomCustomerData();
		String title = customerData[0];

		try {
			terminal.goToScreen(CbsaMenuItem.DISPLAY_CUSTOMER);
			terminal.waitForKeyboard().type(customer.getCustomerNumber()).enter().waitForKeyboard().pf10().waitForKeyboard().eraseInput().type(title + " Belinda K Walker").enter().waitForKeyboard();
			assertThat(terminal.retrieveScreen()).contains("updated successfully");
		} catch (Exception e) {
			throw e;
		}
	}

	/*
	 * This test checks that supplying an invalid customer number which does not exist, provides a warning message
	 */
	@Test
	public void lookupNonExistantAccountsCustomer() throws Exception {
		try {
			terminal.getLastCustomer();
			assertThat(terminal.retrieveScreen()).contains("Customer lookup successful");

			String customerNumber = terminal.retrieveFieldTextAfterFieldWithString("Customer Number").trim();
			String invalidCustomerNumber = Long.toString(Long.parseLong(customerNumber)+500);

			terminal.goToScreen(CbsaMenuItem.LOOKUP);
			terminal.waitForKeyboard().type(invalidCustomerNumber).enter().waitForKeyboard();
			assertThat(terminal.retrieveScreen()).contains("Unable to find customer");
		} catch (Exception e) {
			throw new CbsaException("Failed to test looking up a non-existant account", e);
		}
	}

	/*
	 * This test checks attmepting to create a customer with an invalid date of birth returns the correct warning
	 */
	@Test
	public void createInvalidCustomerDOB() throws FieldNotFoundException, KeyboardLockedException, TextNotFoundException, NetworkException, TerminalInterruptedException, CbsaException, TimeoutException {
		String[] customerData = ICbsaCustomer.generateRandomCustomerData();
		customerData[7] = "25/12/1800";

		terminal.inputDataToCustomerCreationScreen(customerData);
		assertThat(terminal.retrieveScreen()).contains("Sorry, customer is too old. Please check D.O.B");
	}

	/*
	 * This test checks attmepting to create customer with an invalid title returns the correct warning
	 */
	@Test
	public void createInvalidCustomerTitles() throws FieldNotFoundException, KeyboardLockedException, TextNotFoundException, NetworkException, TerminalInterruptedException, CbsaException, TimeoutException {
		String[] customerData = ICbsaCustomer.generateRandomCustomerData();
		customerData[0] = "King";

		terminal.inputDataToCustomerCreationScreen(customerData);
		assertThat(terminal.retrieveScreen()).contains("Valid titles are:");
	}

	/*
	 * This test checks attmepting to create an invalid customer with Non Numeric Day returns the correct warning
	 */
	@Test
	public void createInvalidCustomerNonNumericDobDD() throws FieldNotFoundException, KeyboardLockedException, TextNotFoundException, NetworkException, TerminalInterruptedException, CbsaException, TimeoutException {
		String[] customerData = ICbsaCustomer.generateRandomCustomerData();
		customerData[7] = "2e/12/2003";

		terminal.inputDataToCustomerCreationScreen(customerData);
		assertThat(terminal.retrieveScreen()).contains("Non numeric Date of Birth DD entered");
	}

	/*
	 * This test checks attmepting to create an invalid customer with Non Numeric Month returns the correct warning
	 */
	@Test
	public void createInvalidCustomerNonNumericDobMM() throws FieldNotFoundException, KeyboardLockedException, TextNotFoundException, NetworkException, TerminalInterruptedException, CbsaException, TimeoutException {
		String[] customerData = ICbsaCustomer.generateRandomCustomerData();
		customerData[7] = "25/1e/2003";

		terminal.inputDataToCustomerCreationScreen(customerData);
		assertThat(terminal.retrieveScreen()).contains("Non numeric Date of Birth MM entered");
	}

	/*
	 * This test checks attmepting to create an invalid customer with Non Numeric Year returns the correct warning
	 */
	@Test
	public void createInvalidCustomerNonNumericYYYY() throws FieldNotFoundException, KeyboardLockedException, TextNotFoundException, NetworkException, TerminalInterruptedException, CbsaException, TimeoutException {
		String[] customerData = ICbsaCustomer.generateRandomCustomerData();
		customerData[7] = "25/12/200e";

		terminal.inputDataToCustomerCreationScreen(customerData);
		assertThat(terminal.retrieveScreen()).contains("Non numeric Date of Birth YYYY entered");
	}
}
