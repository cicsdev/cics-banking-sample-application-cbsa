package com.ibm.cics.cip.cbsa.galasa.tests.install;

import static org.assertj.core.api.Assertions.assertThat;

import com.ibm.cics.cip.cbsa.galasa.tests.manager.*;

import dev.galasa.Test;

@Test
public class TestAccount {

	@CbsaTerminal
	public ICbsaTerminal terminal;

	@CbsaCustomer(existing = false,tag="cTag")
	public ICbsaCustomer customer;

	@CbsaAccount(accountType="MORTGAGE",tag="cTag")
	public ICbsaAccount mortgageAccount;

	@CbsaAccount(accountType="LOAN",tag="cTag")
	public ICbsaAccount loanAccount;

	@CbsaAccount(accountType="SAVING",tag="cTag")
	public ICbsaAccount deleteMeAccount;

	@CbsaAccount(accountType="CURRENT",tag="cTag")
	public ICbsaAccount currentAccount;


	/*
	 * This test checks that supplying a customer number provides the associated accounts
	 */
	@Test
	public void lookupAccount() throws Exception {
		try {
			terminal.goToScreen(CbsaMenuItem.LOOKUP);
			terminal.waitForKeyboard().type(customer.getCustomerNumber()).enter().waitForKeyboard();
			assertThat(terminal.retrieveScreen()).contains("accounts found");
		} catch (Exception e) {
			throw new CbsaException("Failed to test looking up an account", e);
		}
	}


	/*
	 * This test checks that the correct account is retrieved when given a valid account number
	 */
	@Test
	public void retrieveAccount() throws Exception {
		try {
 			terminal.goToScreen(CbsaMenuItem.DISPLAY_ACCOUNT);
			terminal.waitForKeyboard().type(customer.getCustomerNumber()).enter().waitForKeyboard();
			assertThat(terminal.retrieveScreen()).contains("If you wish to delete the Account press");
		} catch (Exception e) {
			throw new CbsaException("Failed to display an account", e);
		}
	}


	/*
	 * This test checks that the last account can be retrieved by entering an account number of '99999999'
	 */
	@Test
	public void displayLastAccount() throws Exception {
		try {
			terminal.getLastAccount();
			assertThat(terminal.retrieveScreen()).contains("If you wish to delete the Account press");

			// Ensures it was definitely the account
			String accountNumber = terminal.retrieveFieldTextAfterFieldWithString("Account Number").trim();
			terminal.getAccountByNumber(Long.parseLong(accountNumber)+500);
			assertThat(terminal.retrieveScreen()).contains("Sorry, but that account number was not found.");
		} catch (Exception e) {
			throw new CbsaException("Failed to display the last account", e);
		}
	}


	/*
	 * This test checks that an account can be deleted
	 */
	 @Test
	 public void deleteAccount() throws Exception {
		try {
			terminal.goToScreen(CbsaMenuItem.DISPLAY_ACCOUNT);
			terminal.waitForKeyboard().type(deleteMeAccount.getAccountNumber()).enter().waitForKeyboard().pf5().waitForKeyboard();
			assertThat(terminal.retrieveScreen()).contains("successfully deleted");
		} catch (Exception e) {
			throw new CbsaException("Failed to delete account", e);
		}
	}


	/*
	 * This test checks that an account can be updated with valid information
	 */
	@Test
	public void updateAccount() throws Exception {
		try {
			terminal.goToScreen(CbsaMenuItem.UPDATE_ACCOUNT);
			terminal.waitForKeyboard().positionCursorToFieldContaining("ACCOUNT NUMBER").tab().waitForKeyboard().type(currentAccount.getAccountNumber()).enter().waitForKeyboard().positionCursorToFieldContaining("Interest Rate").tab().waitForKeyboard().type("0111.11").waitForKeyboard().pf5().waitForKeyboard();
			assertThat(terminal.retrieveScreen()).contains("Account update successfully applied");
		} catch (Exception e) {
			throw new CbsaException("Failed to update the last account", e);
		}
	}


	/*
	 * This test checks that the correct warning is given when trying to provide a zero interest loan
	 */
	@Test
	public void updateAccountBadLoan() throws Exception {
		try {
			String interestValue = "0000.00";

			terminal.goToScreen(CbsaMenuItem.UPDATE_ACCOUNT);
			terminal.waitForKeyboard().positionCursorToFieldContaining("ACCOUNT NUMBER").tab().type(loanAccount.getAccountNumber()).enter().waitForKeyboard();
			terminal.waitForKeyboard().positionCursorToFieldContaining("Interest Rate").tab().waitForKeyboard().type(interestValue).waitForKeyboard().pf5().waitForKeyboard();
			assertThat(terminal.retrieveScreen()).contains("Interest rate cannot be 0 with this account type.");
		} catch (Exception e) {
			throw new CbsaException("Failed to update an account with a bad loan", e);
		}
	}


	/*
	 * This test checks that the correct warning is given when trying to provide a zero interest mortgage
	 */
	@Test
	public void updateAccountInvalidMortgageInput() throws Exception {
		try {
			String interestValue = "0000.00";

			terminal.goToScreen(CbsaMenuItem.UPDATE_ACCOUNT);
			terminal.waitForKeyboard().positionCursorToFieldContaining("ACCOUNT NUMBER").tab().type(mortgageAccount.getAccountNumber()).enter().waitForKeyboard();
			terminal.waitForKeyboard().positionCursorToFieldContaining("Interest Rate").tab().waitForKeyboard().type(interestValue).waitForKeyboard().pf5().waitForKeyboard();
			assertThat(terminal.retrieveScreen()).contains("Interest rate cannot be 0 with this account type.");
		} catch (Exception e) {
			throw new CbsaException("Failed to update an account with a bad mortgage", e);
		}
	}


	/*
	 * This test checks that an invalid overdraft input is flagged
	 */
	@Test
	public void updateAccountInvalidOverdraftInput() throws Exception {
		try {
			String overdraftValue = "buzzkill";

			terminal.goToScreen(CbsaMenuItem.UPDATE_ACCOUNT);
			terminal.waitForKeyboard().positionCursorToFieldContaining("ACCOUNT NUMBER").tab().type(currentAccount.getAccountNumber()).enter().waitForKeyboard();
			terminal.waitForKeyboard().positionCursorToFieldContaining("Overdraft limit").tab().waitForKeyboard().type(overdraftValue).waitForKeyboard().pf5().waitForKeyboard();
			assertThat(terminal.retrieveScreen()).contains("Overdraft must be numeric");
		} catch (Exception e) {
			throw new CbsaException("Failed to update account with a bad overdraft input", e);
		}
	}


	/*
	 * This test checks that funds can be credited to an account successfully
	 */
	@Test
	public void creditFunds() throws Exception {
		try {
			String initialValue = loanAccount.getAccountValue();

			terminal.goToScreen(CbsaMenuItem.CREDIT_FUNDS);
			terminal.waitForKeyboard().positionCursorToFieldContaining("ACCOUNT NUMBER").tab().type(loanAccount.getAccountNumber()).waitForKeyboard().tab().type("+").waitForKeyboard().type("0000005000.00").enter().waitForKeyboard();
			assertThat(terminal.retrieveScreen()).contains("Amount successfully applied to the account");

			String postCreditValue = loanAccount.getAccountValue();
			assertThat(!(initialValue.equals(postCreditValue)));
		} catch (Exception e) {
			throw new CbsaException("Failed to credit funds to an account", e);
		}
	}


	/*
	 * This test checks that funds can be debited to an account successfully
	 */
	@Test
	public void debitFunds() throws Exception {
		try {
			String initialValue = loanAccount.getAccountValue();

			terminal.goToScreen(CbsaMenuItem.CREDIT_FUNDS);
			terminal.waitForKeyboard().positionCursorToFieldContaining("ACCOUNT NUMBER").tab().type(loanAccount.getAccountNumber()).waitForKeyboard().tab().type("-").waitForKeyboard().type("0000005000.00").enter().waitForKeyboard();
			assertThat(terminal.retrieveScreen()).contains("Amount successfully applied to the account");

			String postDebitValue = loanAccount.getAccountValue();
			assertThat(!(initialValue.equals(postDebitValue)));
		} catch (Exception e) {
			throw new CbsaException("Failed to debit funds to an account", e);
		}
	}


	/*
	 * This test checks that funds cannot be credited to a non-existent account
	 */
	@Test
	public void creditNonExistentAccount() throws Exception {
		try {
			String initialValue = loanAccount.getAccountValue();

			// Takes string, turns into long, adds 500, turns back to string
			String invalidAccountNumber = Long.toString(Long.parseLong(loanAccount.getAccountNumber())+500);

			terminal.goToScreen(CbsaMenuItem.CREDIT_FUNDS);
			terminal.waitForKeyboard().positionCursorToFieldContaining("ACCOUNT NUMBER").tab().type(invalidAccountNumber).waitForKeyboard().tab().type("+").waitForKeyboard().type("0000005000.00").enter().waitForKeyboard();
			assertThat(terminal.retrieveScreen()).contains("Amount not applied");

			String postCreditValue = loanAccount.getAccountValue();
			assertThat(initialValue.equals(postCreditValue));
		} catch (Exception e) {
			throw new CbsaException("Failed to credit funds to a non-existent account", e);
		}
	}


	/*
	 * This test checks that funds cannot be debited to a non existent account
	 */
	@Test
	public void debitNonExistentAccount() throws Exception {
		try {
			String initialValue = loanAccount.getAccountValue();

			// Takes string, turns into long, adds 500, turns back to string
			String invalidAccountNumber = Long.toString(Long.parseLong(loanAccount.getAccountNumber())+500);

			terminal.goToScreen(CbsaMenuItem.CREDIT_FUNDS);
			terminal.waitForKeyboard().positionCursorToFieldContaining("ACCOUNT NUMBER").tab().type(invalidAccountNumber).waitForKeyboard().tab().type("-").waitForKeyboard().type("0000005000.00").enter().waitForKeyboard();
			assertThat(terminal.retrieveScreen()).contains("Amount not applied");

			String postCreditValue = loanAccount.getAccountValue();
			assertThat(initialValue.equals(postCreditValue));
		} catch (Exception e) {
			throw new CbsaException("Failed to debit funds to a non-existent account", e);
		}
	}


	/*
	 * This test checks that an invalid fund amount with more than two decimal places can not be credited to an account
	 */
	@Test
	public void creditInvalidFundsMoreThanTwoDecimalPlaces() throws Exception {
		try {
			String initialValue = currentAccount.getAccountValue();

			terminal.goToScreen(CbsaMenuItem.CREDIT_FUNDS);
			terminal.waitForKeyboard().positionCursorToFieldContaining("ACCOUNT NUMBER").tab().type(currentAccount.getAccountNumber()).waitForKeyboard().tab().type("+").waitForKeyboard().type("000000500.000").enter().waitForKeyboard();
			assertThat(terminal.retrieveScreen()).contains("Only up to two decimal places are supported");

			String postCreditValue = currentAccount.getAccountValue();
			assertThat((initialValue.equals(postCreditValue)));
		} catch (Exception e) {
			throw new CbsaException("Failed to credit funds to an account", e);
		}
	}


	/*
	 * This test checks that an invalid fund amount with more than two decimal places can not be debited to an account
	 */
	@Test
	public void debitInvalidFundsMoreThanTwoDecimalPlaces() throws Exception {
		try {
			String initialValue = currentAccount.getAccountValue();

			terminal.goToScreen(CbsaMenuItem.CREDIT_FUNDS);
			terminal.waitForKeyboard().positionCursorToFieldContaining("ACCOUNT NUMBER").tab().type(currentAccount.getAccountNumber()).waitForKeyboard().tab().type("-").waitForKeyboard().type("000000500.000").enter().waitForKeyboard();
			assertThat(terminal.retrieveScreen()).contains("Only up to two decimal places are supported");

			String postCreditValue = currentAccount.getAccountValue();
			assertThat((initialValue.equals(postCreditValue)));
		} catch (Exception e) {
			throw new CbsaException("Failed to debit to an account", e);
		}
	}

	// TO THINK ABOUT FIXING IN CBSA FOR CONSISTENCY??
	// /*
	//  * This test checks that an invalid fund amount with less than two decimal places can not be credited to an account
	//  */
	// @Test
	// public void creditInvalidFundsLessThanTwoDecimalPlaces() throws Exception {
	// 	try {
	// 		String initialValue = currentAccount.getAccountValue();

	// 		terminal.goToScreen(CbsaMenuItem.CREDIT_FUNDS);
	// 		terminal.waitForKeyboard().positionCursorToFieldContaining("ACCOUNT NUMBER").tab().type(currentAccount.getAccountNumber()).waitForKeyboard().tab().type("+").waitForKeyboard().type("0000005000.5").enter().waitForKeyboard();
	// 		assertThat(terminal.retrieveScreen()).contains("Only up to two decimal places are supported");

	// 		String postCreditValue = currentAccount.getAccountValue();
	// 		assertThat((initialValue.equals(postCreditValue)));
	// 	} catch (Exception e) {
	// 		throw new CbsaException("Failed to credit funds to an account", e);
	// 	}
	// }

	// TO THINK ABOUT FIXING IN CBSA FOR CONSISTENCY??
	// /*
	//  * This test checks that an invalid fund amount with less than two decimal places can not be debited to an account
	//  */
	// @Test
	// public void debitInvalidFundsLessThanTwoDecimalPlaces() throws Exception {
	// 	try {
	// 		String initialValue = currentAccount.getAccountValue();

	// 		terminal.goToScreen(CbsaMenuItem.CREDIT_FUNDS);
	// 		terminal.waitForKeyboard().positionCursorToFieldContaining("ACCOUNT NUMBER").tab().type(currentAccount.getAccountNumber()).waitForKeyboard().tab().type("-").waitForKeyboard().type("0000005000.5").enter().waitForKeyboard();
	// 		assertThat(terminal.retrieveScreen()).contains("Only up to two decimal places are supported");

	// 		String postCreditValue = currentAccount.getAccountValue();
	// 		assertThat((initialValue.equals(postCreditValue)));
	// 	} catch (Exception e) {
	// 		throw new CbsaException("Failed to debit to an account", e);
	// 	}
	// }

	/*
	 * This test checks that an invalid fund amount with more than two decimal points can not be credited to an account
	 */
	@Test
	public void creditInvalidFundsDecimalPoints() throws Exception {
		try {
			String initialValue = currentAccount.getAccountValue();

			terminal.goToScreen(CbsaMenuItem.CREDIT_FUNDS);
			terminal.waitForKeyboard().positionCursorToFieldContaining("ACCOUNT NUMBER").tab().type(currentAccount.getAccountNumber()).waitForKeyboard().tab().type("+").waitForKeyboard().type("000000500..00").enter().waitForKeyboard();
			assertThat(terminal.retrieveScreen()).contains("Use one decimal point for amount only");

			String postCreditValue = currentAccount.getAccountValue();
			assertThat((initialValue.equals(postCreditValue)));
		} catch (Exception e) {
			throw new CbsaException("Failed to credit funds to an account", e);
		}
	}


	/*
	 * This test checks that an invalid fund amount with more than two decimal points can not be debited to an account
	 */
	@Test
	public void debitInvalidFundsDecimalPoints() throws Exception {
		try {
			String initialValue = currentAccount.getAccountValue();

			terminal.goToScreen(CbsaMenuItem.CREDIT_FUNDS);
			terminal.waitForKeyboard().positionCursorToFieldContaining("ACCOUNT NUMBER").tab().type(currentAccount.getAccountNumber()).waitForKeyboard().tab().type("-").waitForKeyboard().type("000000500..00").enter().waitForKeyboard();
			assertThat(terminal.retrieveScreen()).contains("Use one decimal point for amount only");

			String postCreditValue = currentAccount.getAccountValue();
			assertThat((initialValue.equals(postCreditValue)));
		} catch (Exception e) {
			throw new CbsaException("Failed to debit funds to an account", e);
		}
	}


	/*
	 * This test checks that a credit value of zero is rejected
	 */
	@Test
	public void creditZero() throws Exception {
		try {
			String initialValue = currentAccount.getAccountValue();

			terminal.goToScreen(CbsaMenuItem.CREDIT_FUNDS);
			terminal.waitForKeyboard().positionCursorToFieldContaining("ACCOUNT NUMBER").tab().type(currentAccount.getAccountNumber()).waitForKeyboard().tab().type("+").waitForKeyboard().type("0000000000.00").enter().waitForKeyboard();
			assertThat(terminal.retrieveScreen()).contains("Please supply a non-zero amount");

			String postCreditValue = currentAccount.getAccountValue();
			assertThat((initialValue.equals(postCreditValue)));
		} catch (Exception e) {
			throw new CbsaException("Failed to credit funds to an account", e);
		}
	}


	/*
	 * This test checks that a debit value of zero is rejected
	 */
	@Test
	public void debitZero() throws Exception {
		try {
			String initialValue = currentAccount.getAccountValue();

			terminal.goToScreen(CbsaMenuItem.CREDIT_FUNDS);
			terminal.waitForKeyboard().positionCursorToFieldContaining("ACCOUNT NUMBER").tab().type(currentAccount.getAccountNumber()).waitForKeyboard().tab().type("-").waitForKeyboard().type("0000000000.00").enter().waitForKeyboard();
			assertThat(terminal.retrieveScreen()).contains("Please supply a non-zero amount");

			String postCreditValue = currentAccount.getAccountValue();
			assertThat((initialValue.equals(postCreditValue)));
		} catch (Exception e) {
			throw new CbsaException("Failed to debit funds to an account", e);
		}
	}


	// BUG NOT FIXED
	// /*
	//  * This test checks that a credit value of zero is rejected, when no decimal places are used
	//  */
	// @Test
	// public void creditZeroNoDecimals() throws Exception {
	// 	try {
	// 		String initialValue = currentAccount.getAccountValue();

	// 		terminal.goToScreen(CbsaMenuItem.CREDIT_FUNDS);
	// 		terminal.waitForKeyboard().positionCursorToFieldContaining("ACCOUNT NUMBER").tab().type(currentAccount.getAccountNumber()).waitForKeyboard().tab().type("+").waitForKeyboard().type("0000000000000").enter().waitForKeyboard();
	// 		assertThat(terminal.retrieveScreen()).contains("Please supply a non-zero amount");

	// 		String postCreditValue = currentAccount.getAccountValue();
	// 		assertThat((initialValue.equals(postCreditValue)));
	// 	} catch (Exception e) {
	// 		throw new CbsaException("Failed to credit funds to an account", e);
	// 	}
	// }


	// BUG NOT FIXED
	// /*
	//  * This test checks that a debit value of zero is rejected, when no decimal places are used
	//  */
	// @Test
	// public void debitZeroNoDecimals() throws Exception {
	// 	try {
	// 		String initialValue = currentAccount.getAccountValue();

	// 		terminal.goToScreen(CbsaMenuItem.CREDIT_FUNDS);
	// 		terminal.waitForKeyboard().positionCursorToFieldContaining("ACCOUNT NUMBER").tab().type(currentAccount.getAccountNumber()).waitForKeyboard().tab().type("-").waitForKeyboard().type("0000000000000").enter().waitForKeyboard();
	// 		assertThat(terminal.retrieveScreen()).contains("Please supply a non-zero amount");

	// 		String postCreditValue = currentAccount.getAccountValue();
	// 		assertThat((initialValue.equals(postCreditValue)));
	// 	} catch (Exception e) {
	// 		throw new CbsaException("Failed to debit funds to an account", e);
	// 	}
	// }


	/*
	 * This test checks a transfer of funds can occur between two valid accounts
	 */
	@Test
	public void transferFunds() throws Exception {
		try {
			terminal.goToScreen(CbsaMenuItem.TRANSFER_FUNDS);
			terminal.waitForKeyboard().positionCursorToFieldContaining("FROM Account Number").tab().type(mortgageAccount.getAccountNumber()).waitForKeyboard().tab().type(loanAccount.getAccountNumber()).waitForKeyboard();
			terminal.waitForKeyboard().positionCursorToFieldContaining("AMOUNT").tab().type("0000005000.00").enter().waitForKeyboard();
			assertThat(terminal.retrieveScreen()).contains("Transfer successfully applied");
		} catch (Exception e) {
			throw new CbsaException("Failed to test transfering funds between accounts", e);
		}
	}


	/*
	 * This test checks that a valid account, can not transfer funds to an invalid account
	 */
	@Test
	public void transferFundsToInvalidAccount() throws Exception {
		try {
			String invalidAccountNumber = Long.toString(Long.parseLong(loanAccount.getAccountNumber())+500);

			terminal.goToScreen(CbsaMenuItem.TRANSFER_FUNDS);
			terminal.waitForKeyboard().positionCursorToFieldContaining("FROM Account Number").tab().type(mortgageAccount.getAccountNumber()).waitForKeyboard().tab().type(invalidAccountNumber).waitForKeyboard();
			terminal.waitForKeyboard().positionCursorToFieldContaining("AMOUNT").tab().type("0000005000.00").enter().waitForKeyboard();
			assertThat(terminal.retrieveScreen()).contains("TO ACCOUNT no was not found");
		} catch (Exception e) {
			throw new CbsaException("Failed to test transferring funds between accounts", e);
		}
	}


	/*
	 * This test checks that an invalid account, can not transfer funds to a valid account
	 */
	@Test
	public void transferFundsFromInvalidAccount() throws Exception {
		try {
			String invalidAccountNumber = Long.toString(Long.parseLong(loanAccount.getAccountNumber())+500);

			terminal.goToScreen(CbsaMenuItem.TRANSFER_FUNDS);
			terminal.waitForKeyboard().positionCursorToFieldContaining("FROM Account Number").tab().type(invalidAccountNumber).waitForKeyboard().tab().type(mortgageAccount.getAccountNumber()).waitForKeyboard();
			terminal.waitForKeyboard().positionCursorToFieldContaining("AMOUNT").tab().type("0000005000.00").enter().waitForKeyboard();
			assertThat(terminal.retrieveScreen()).contains("FROM ACCOUNT no was not found");
		} catch (Exception e) {
			throw new CbsaException("Failed to test transferring funds between accounts", e);
		}
	}


	/*
	 * This test checks you cannot transfer a negative amount
	 */
	@Test
	public void transferNegativeFunds() throws Exception {
		try {
			terminal.goToScreen(CbsaMenuItem.TRANSFER_FUNDS);
			terminal.waitForKeyboard().positionCursorToFieldContaining("FROM Account Number").tab().type(mortgageAccount.getAccountNumber()).waitForKeyboard().tab().type(loanAccount.getAccountNumber()).waitForKeyboard();
			terminal.waitForKeyboard().positionCursorToFieldContaining("AMOUNT").tab().type("-0000005000.00").enter().waitForKeyboard();
			assertThat(terminal.retrieveScreen()).contains("Please supply a positive amount.");
		} catch (Exception e) {
			throw new CbsaException("Failed to test transferring negative funds", e);
		}
	}


	/*
	 * This test checks an amount of zero cannot be transferred between accounts
	 */
	@Test
	public void transferZeroFunds() throws Exception {
		try {
			terminal.goToScreen(CbsaMenuItem.TRANSFER_FUNDS);
			terminal.waitForKeyboard().positionCursorToFieldContaining("FROM Account Number").tab().type(mortgageAccount.getAccountNumber()).waitForKeyboard().tab().type(loanAccount.getAccountNumber()).waitForKeyboard();
			terminal.waitForKeyboard().positionCursorToFieldContaining("AMOUNT").tab().type("0000000000.00").enter().waitForKeyboard();
			assertThat(terminal.retrieveScreen()).contains("Please supply a non-zero amount.");
		} catch (Exception e) {
			throw new CbsaException("Failed to test transferring no funds", e);
		}
	}


	/*
	 * This test checks an amount of zero cannot be transferred between accounts
	 */
	@Test
	public void transferZeroFundsNoDecimal() throws Exception {
		try {
			terminal.goToScreen(CbsaMenuItem.TRANSFER_FUNDS);
			terminal.waitForKeyboard().positionCursorToFieldContaining("FROM Account Number").tab().type(mortgageAccount.getAccountNumber()).waitForKeyboard().tab().type(loanAccount.getAccountNumber()).waitForKeyboard();
			terminal.waitForKeyboard().positionCursorToFieldContaining("AMOUNT").tab().type("0000000000000").enter().waitForKeyboard();
			assertThat(terminal.retrieveScreen()).contains("Please supply a positive amount");
		} catch (Exception e) {
			throw new CbsaException("Failed to test transferring invalid funds", e);
		}
	}


	/*
	 * This test checks an amount that has more than two decimal places cannot be transferred between accounts
	 */
	@Test
	public void transferZeroFundsMoreThanTwoDecimalPlaces() throws Exception {
		try {
			terminal.goToScreen(CbsaMenuItem.TRANSFER_FUNDS);
			terminal.waitForKeyboard().positionCursorToFieldContaining("FROM Account Number").tab().type(mortgageAccount.getAccountNumber()).waitForKeyboard().tab().type(loanAccount.getAccountNumber()).waitForKeyboard();
			terminal.waitForKeyboard().positionCursorToFieldContaining("AMOUNT").tab().type("000000500.000").enter().waitForKeyboard();
			assertThat(terminal.retrieveScreen()).contains("Only up to two decimal places are supported.");
		} catch (Exception e) {
			throw new CbsaException("Failed to test transferring invalid funds", e);
		}
	}


	/*
	 * This test checks an amount that has more than one decimal point cannot be transferred between accounts
	 */
	@Test
	public void transferZeroFundsDecimalPoints() throws Exception {
		try {
			terminal.goToScreen(CbsaMenuItem.TRANSFER_FUNDS);
			terminal.waitForKeyboard().positionCursorToFieldContaining("FROM Account Number").tab().type(mortgageAccount.getAccountNumber()).waitForKeyboard().tab().type(loanAccount.getAccountNumber()).waitForKeyboard();
			terminal.waitForKeyboard().positionCursorToFieldContaining("AMOUNT").tab().type("000000500..00").enter().waitForKeyboard();
			assertThat(terminal.retrieveScreen()).contains("Use one decimal point for amount only");
		} catch (Exception e) {
			throw new CbsaException("Failed to test transferring invalid funds", e);
		}
	}
}
