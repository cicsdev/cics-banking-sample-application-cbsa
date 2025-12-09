package com.ibm.cics.cip.cbsa.galasa.tests.install;

import static org.assertj.core.api.Assertions.assertThat;

import com.ibm.cics.cip.cbsa.galasa.tests.manager.*;

import dev.galasa.Test;


@Test
public class TestCustomerCreateDelete {

	@CbsaTerminal
	public ICbsaTerminal terminal;

	public String delTestNumber;

	/*
	 * This test checks a customer can be created when given valid information
	 */
	@Test
	public void createValidCustomer() throws Exception {
		try {
			delTestNumber = terminal.createCustomerAndReturnCustomerNumber();
			terminal.getCustomerByNumber(Long.valueOf(delTestNumber));
			assertThat(terminal.retrieveScreen()).contains("Customer lookup successful");
		} catch (Exception e) {
			throw e;
		}
	}

	/*
	 * This test checks that a customer can be deleted
	 */
	@Test
	public void deleteCustomer() throws Exception {
		try {
			terminal.goToScreen(CbsaMenuItem.DISPLAY_CUSTOMER);
			terminal.waitForKeyboard().type(this.delTestNumber).waitForKeyboard().enter().waitForKeyboard().pf5().waitForKeyboard();
			assertThat(terminal.retrieveScreen()).contains("successfully deleted");
		} catch (Exception e) {
			throw e;
		}
	}
}
