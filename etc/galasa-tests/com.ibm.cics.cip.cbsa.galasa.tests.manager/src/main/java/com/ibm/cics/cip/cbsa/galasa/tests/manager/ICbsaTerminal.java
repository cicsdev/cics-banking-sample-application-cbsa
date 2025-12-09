package com.ibm.cics.cip.cbsa.galasa.tests.manager;

import dev.galasa.zos3270.FieldNotFoundException;
import dev.galasa.zos3270.ITerminal;
import dev.galasa.zos3270.KeyboardLockedException;
import dev.galasa.zos3270.TerminalInterruptedException;
import dev.galasa.zos3270.TextNotFoundException;
import dev.galasa.zos3270.TimeoutException;
import dev.galasa.zos3270.spi.NetworkException;


public interface ICbsaTerminal extends ITerminal {

    public void loginToApplication() throws CbsaException;
    public void returnToMenu() throws TimeoutException, CbsaException;
    public ICbsaTerminal goToScreen(CbsaMenuItem menuItem) throws CbsaException;
    public ICbsaTerminal getCustomerByNumber(long customerNumber) throws CbsaException;
    public ICbsaTerminal getRandomCustomer() throws CbsaException;
    public ICbsaTerminal getLastCustomer() throws CbsaException;
    public String createCustomerAndReturnCustomerNumber() throws CbsaException;
    public ICbsaTerminal inputDataToCustomerCreationScreen(String[] customerData) throws FieldNotFoundException, KeyboardLockedException, TextNotFoundException, NetworkException, TerminalInterruptedException, CbsaException, TimeoutException;
    public ICbsaTerminal inputDataToAccountCreationScreen(String customerNumber, String accountType, String overdraftLimit) throws FieldNotFoundException, KeyboardLockedException, TextNotFoundException, NetworkException, TerminalInterruptedException, CbsaException, TimeoutException;
    public ICbsaTerminal getAccountByNumber(long customerNumber) throws CbsaException;
    public ICbsaTerminal getLastAccount() throws CbsaException;

    public static String removeZero(String str) {
        int i=0;
        while (i < str.length() && str.charAt(i) == '0'){i++;}
        StringBuffer sb = new StringBuffer(str);
        sb.replace(0, i, "");
        return sb.toString().trim();
    }

    public String getAccountValue(String accountNumber) throws CbsaException;
}
