package com.ibm.cics.cip.cbsa.galasa.tests.manager.internal;

import com.ibm.cics.cip.cbsa.galasa.tests.manager.CbsaException;
import com.ibm.cics.cip.cbsa.galasa.tests.manager.ICbsaAccount;
import com.ibm.cics.cip.cbsa.galasa.tests.manager.ICbsaTerminal;

import dev.galasa.framework.spi.IFramework;
import dev.galasa.zos3270.FieldNotFoundException;
import dev.galasa.zos3270.KeyboardLockedException;
import dev.galasa.zos3270.TerminalInterruptedException;
import dev.galasa.zos3270.TextNotFoundException;
import dev.galasa.zos3270.TimeoutException;
import dev.galasa.zos3270.spi.NetworkException;



public class CbsaAccountImpl implements ICbsaAccount {

    private CbsaTerminalImpl                    terminal;
    private CbsaManagerImpl                     manager;

    private String                              accountNumber;
    private String                              customerNumber;
    private String                              accountType;


	public CbsaAccountImpl(CbsaManagerImpl manager, IFramework framework, String customerNumber, CbsaTerminalImpl terminal, String accountType)throws CbsaException{
        this.customerNumber = customerNumber;
        this.manager = manager;
        this.accountType = accountType;
        this.terminal = terminal;
        this.accountNumber = generateAccount(accountType);

	}

    private String generateAccount(String accountType)throws CbsaException {
        try {
            String overdraftLimit = "8000";
            terminal.inputDataToAccountCreationScreen(customerNumber, accountType, overdraftLimit);

            String accountNum = terminal.waitForKeyboard().retrieveFieldTextAfterFieldWithString("Account number");
            return accountNum;
        } catch (TimeoutException | KeyboardLockedException | TerminalInterruptedException | FieldNotFoundException | NetworkException | TextNotFoundException e) {
            throw new CbsaException("Unable to generate account for customer : " + customerNumber + "\n" + e);
        }
    }

    @Override
    public void discardAccount(String accountNumber) throws CbsaException {
        manager.getCBSATerminal().discardAccount(accountNumber);
        manager.getCBSATerminal().returnToMenu();
    }

    @Override
    public String getAccountValue() throws CbsaException {
        return manager.getCBSATerminal().getAccountValue(accountNumber.trim());
    }

    @Override
    public String getAccountNumber() {
        return ICbsaTerminal.removeZero(accountNumber.trim());
    }

    @Override
    public String getCustomerNumber() {
        return customerNumber;
    }

    @Override
    public String getAccountType() {
        return accountType;
    }

}
