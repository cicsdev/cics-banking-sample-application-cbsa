package com.ibm.cics.cip.cbsa.galasa.tests.manager.internal;

import java.util.ArrayList;

import com.ibm.cics.cip.cbsa.galasa.tests.manager.CbsaException;
import com.ibm.cics.cip.cbsa.galasa.tests.manager.CbsaMenuItem;
import com.ibm.cics.cip.cbsa.galasa.tests.manager.ICbsaAccount;
import com.ibm.cics.cip.cbsa.galasa.tests.manager.ICbsaCustomer;
import com.ibm.cics.cip.cbsa.galasa.tests.manager.ICbsaTerminal;

import dev.galasa.framework.spi.IFramework;
import dev.galasa.framework.spi.creds.CredentialsException;

import dev.galasa.zos3270.FieldNotFoundException;
import dev.galasa.zos3270.KeyboardLockedException;
import dev.galasa.zos3270.TerminalInterruptedException;
import dev.galasa.zos3270.TextNotFoundException;
import dev.galasa.zos3270.TimeoutException;
import dev.galasa.zos3270.Zos3270Exception;

import dev.galasa.zos3270.spi.NetworkException;

public class CbsaCustomerImpl implements ICbsaCustomer {

    private final CbsaManagerImpl               manager;
    private final IFramework                    framework;
    private final ICbsaTerminal                 cbsaTerminal;

    private String                              customerNumber;
    private ArrayList<ICbsaAccount>             associatedAccounts = new ArrayList<ICbsaAccount>();

	public CbsaCustomerImpl(CbsaManagerImpl manager, IFramework framework, boolean existing, ICbsaTerminal cbsaTerminal) throws CbsaException, CredentialsException, Zos3270Exception {
        this.manager = manager;
        this.framework = framework;
        this.cbsaTerminal = cbsaTerminal;

        if (existing) {
            generateExistingCustomer();
        } else {
            provisionCustomer();
        }
	}

    private String provisionCustomer() throws CbsaException, Zos3270Exception {
        this.customerNumber = cbsaTerminal.createCustomerAndReturnCustomerNumber();
        return this.customerNumber;
    }

    private void generateExistingCustomer() throws CbsaException {
        try {
            CbsaTerminalImpl terminal = manager.getCBSATerminal();

            terminal.waitForKeyboard().type("1").enter().waitForKeyboard().type("0").enter().waitForKeyboard();

            customerNumber = terminal.retrieveFieldTextAfterFieldWithString("Customer Number");
            terminal.returnToMenu();
        } catch (TimeoutException | KeyboardLockedException | TerminalInterruptedException | NetworkException | FieldNotFoundException | TextNotFoundException e) {
                throw new CbsaException("Unable to provision an existing customer. Terminal is most likely null: ", e);
        }
    }

    @Override
    public ICbsaAccount generateAccount(String accountType )throws CbsaException{
        CbsaAccountImpl account = new CbsaAccountImpl(manager, framework, ICbsaTerminal.removeZero(customerNumber), manager.getCBSATerminal(), accountType);
        associatedAccounts.add(account);
        return (ICbsaAccount) account;
    }

    @Override
    public void discardAccounts() throws CbsaException {
        for (ICbsaAccount icbsaAccount : associatedAccounts) {
            icbsaAccount.discardAccount(icbsaAccount.getAccountNumber());
        }
        associatedAccounts.clear();
    }

    @Override
    public String getCustomerNumber() {
       return ICbsaTerminal.removeZero(customerNumber.trim());
    }

    @Override
    public void addToAccountslist(ICbsaAccount account){
        associatedAccounts.add(account);
    }

    @Override
    public void discardSelf(CbsaTerminalImpl terminal) throws CbsaException{
        try {
            // to fix
            terminal.pf3().waitForKeyboard().type("1").enter().waitForKeyboard().type(getCustomerNumber()).enter().waitForKeyboard().pf5().waitForKeyboard();
        } catch (TimeoutException | KeyboardLockedException | TerminalInterruptedException | NetworkException | FieldNotFoundException e) {
                    throw new CbsaException("Unable to discard customer: " + customerNumber, e);
        }
    }
}
