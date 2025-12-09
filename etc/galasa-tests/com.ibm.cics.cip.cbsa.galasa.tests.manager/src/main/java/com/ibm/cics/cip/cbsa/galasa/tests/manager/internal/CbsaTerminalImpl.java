package com.ibm.cics.cip.cbsa.galasa.tests.manager.internal;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.ibm.cics.cip.cbsa.galasa.tests.manager.CbsaException;
import com.ibm.cics.cip.cbsa.galasa.tests.manager.CbsaMenuItem;
import com.ibm.cics.cip.cbsa.galasa.tests.manager.ICbsaCustomer;
import com.ibm.cics.cip.cbsa.galasa.tests.manager.ICbsaTerminal;

import dev.galasa.cicsts.CicstsManagerException;
import dev.galasa.cicsts.spi.CicsTerminalImpl;
import dev.galasa.cicsts.spi.ICicsRegionProvisioned;
import dev.galasa.cicsts.spi.ICicstsManagerSpi;

import dev.galasa.framework.spi.IFramework;

import dev.galasa.ipnetwork.IpNetworkManagerException;

import dev.galasa.textscan.spi.ITextScannerManagerSpi;

import dev.galasa.zos.IZosImage;
import dev.galasa.zos.ZosManagerException;

import dev.galasa.zos3270.FieldNotFoundException;
import dev.galasa.zos3270.KeyboardLockedException;
import dev.galasa.zos3270.TerminalInterruptedException;
import dev.galasa.zos3270.TextNotFoundException;
import dev.galasa.zos3270.TimeoutException;
import dev.galasa.zos3270.Zos3270Exception;
import dev.galasa.zos3270.Zos3270ManagerException;

import dev.galasa.zos3270.spi.NetworkException;

public class CbsaTerminalImpl extends CicsTerminalImpl implements ICbsaTerminal{

    private String customerNumber;

    private final static Log logger = LogFactory.getLog(CbsaTerminalImpl.class);

    public CbsaTerminalImpl(IFramework framework, ICicsRegionProvisioned cicsRegion, IZosImage zosImage, ICicstsManagerSpi cicstsManagerSpi, ITextScannerManagerSpi textScanManagerSpi, String loginCredentialsTag)
            throws Zos3270ManagerException, TerminalInterruptedException, ZosManagerException, IpNetworkManagerException, CbsaException, CicstsManagerException {

                super(
                    cicstsManagerSpi,
                    framework,
                    cicsRegion,
                    zosImage.getDefaultHostname(),
                    zosImage.getIpHost().getTelnetPort(),
                    zosImage.getIpHost().isTelnetPortTls(),
                    true,
                    textScanManagerSpi,
                    loginCredentialsTag
                );
    }

    @Override
    public String createCustomerAndReturnCustomerNumber() throws CbsaException {
        String[] ok = {"The Customer record has been successfully created"};
        String[] err = {"Valid titles are: Mr,Mrs,Miss,Ms,Dr,Drs,Professor,Lord,Sir,Lady", "Please supply a valid"};

        try {
            String[] customerData = ICbsaCustomer.generateRandomCustomerData();
            this.inputDataToCustomerCreationScreen(customerData);

            this.waitForTextInField(ok, err);

            customerNumber = this.retrieveFieldTextAfterFieldWithString("Customer Number").trim();

            return customerNumber;
        } catch (Zos3270Exception e) {
            throw new CbsaException("Could not create customer", e);
        }
    }

    @Override
    public ICbsaTerminal inputDataToCustomerCreationScreen(String[] customerData) throws FieldNotFoundException, KeyboardLockedException, TextNotFoundException, NetworkException, TerminalInterruptedException, CbsaException, TimeoutException {

		this.goToScreen(CbsaMenuItem.CREATE_CUSTOMER);

        String title = customerData[0];             // title
        String firstName = customerData[1];         // first name
        String middleInitials = customerData[2];    // middle initial
        String lastName = customerData[3];          // last name
        String addr1 = customerData[4];             // address line 1
        String addr2 = customerData[5];             // address line 2
        String addr3 = customerData[6];             // address line 3
        String dob = customerData[7];

        this.positionCursorToFieldContaining("Customer Title").tab().type(title);
        this.positionCursorToFieldContaining("First Name").tab().type(firstName);
        this.positionCursorToFieldContaining("Middle Initials").tab().type(middleInitials);
        this.positionCursorToFieldContaining("Family name").tab().type(lastName);
        this.positionCursorToFieldContaining("Customer Addr1").tab().type(addr1);
        this.positionCursorToFieldContaining("Customer Addr2").tab().type(addr2);
        this.positionCursorToFieldContaining("Customer Addr3").tab().type(addr3);
        this.positionCursorToFieldContaining("Customer D.O.B.").tab().type(dob.replace("/", "")).enter().waitForKeyboard();

        return this;
    }

    @Override
    public ICbsaTerminal inputDataToAccountCreationScreen(String customerNumber, String accountType, String overdraftLimit) throws FieldNotFoundException, KeyboardLockedException, TextNotFoundException, CbsaException, NetworkException, TerminalInterruptedException, TimeoutException {

        this.goToScreen(CbsaMenuItem.CREATE_ACCOUNT);

        this.positionCursorToFieldContaining("Customer number").tab().type(customerNumber);
        this.positionCursorToFieldContaining("Account Type").tab().type(accountType);
        this.positionCursorToFieldContaining("Overdraft Limit").tab().type(overdraftLimit).enter().waitForKeyboard();

        return this;
    }

    @Override
    public ICbsaTerminal goToScreen(CbsaMenuItem menuItem) throws CbsaException {
        try {
            if (!this.retrieveScreen().contains("BNK1MA")) {
                returnToMenu();
            }

            this.tab();

            if (menuItem == CbsaMenuItem.DISPLAY_CUSTOMER) {
                this.type("1").enter().waitForTextInField("BNK1DC").tab().wfk();
            }
            else if (menuItem == CbsaMenuItem.DISPLAY_ACCOUNT) {
                this.type("2").enter().waitForTextInField("BNK1DA").tab().wfk();
            }
            else if (menuItem == CbsaMenuItem.CREATE_CUSTOMER) {
                this.type("3").enter().waitForTextInField("BNK1CC").tab().wfk();
            }
            else if (menuItem == CbsaMenuItem.CREATE_ACCOUNT) {
                this.type("4").enter().waitForTextInField("BNK1CA").tab().wfk();
            }
            else if (menuItem == CbsaMenuItem.UPDATE_ACCOUNT) {
                this.type("5").enter().waitForTextInField("BNK1UA").tab().wfk();
            }
            else if (menuItem == CbsaMenuItem.CREDIT_FUNDS) {
                this.type("6").enter().waitForTextInField("BNK1CD").tab().wfk();
            }
            else if (menuItem == CbsaMenuItem.TRANSFER_FUNDS) {
                this.type("7").enter().waitForTextInField("BNK1TF").tab().wfk();
            }
            else if (menuItem == CbsaMenuItem.LOOKUP) {
                this.type("A").enter().waitForTextInField("BNK1ACC").tab().wfk();
            }

        } catch (Zos3270Exception e) {
            e.printStackTrace();
        }
        return this;
    }

    private String padCustomerNumber(long customerNumber) {
        return String.format("%10d", customerNumber);
    }

    private String padAccountNumber(long accountNumber) {
        return String.format("%8d", accountNumber);
    }

    @Override
    public ICbsaTerminal getCustomerByNumber(long customerNumber) throws CbsaException {
        try {
            this.goToScreen(CbsaMenuItem.DISPLAY_CUSTOMER).type(padCustomerNumber(customerNumber)).enter().waitForKeyboard();
            return this;
        } catch (Zos3270Exception e) {
            throw new CbsaException("Problem retrieving random customer", e);
        }
    }

    @Override
    public ICbsaTerminal getLastCustomer() throws CbsaException {
        this.getCustomerByNumber(9999999999L);
        return this;
    }

    @Override
    public ICbsaTerminal getAccountByNumber(long accountNumber) throws CbsaException {
        try {
            this.goToScreen(CbsaMenuItem.DISPLAY_ACCOUNT).type(padAccountNumber(accountNumber)).enter().waitForKeyboard();
            return this;
        } catch (Zos3270Exception e) {
            throw new CbsaException("Problem retrieving random customer", e);
        }
    }

    @Override
    public ICbsaTerminal getLastAccount() throws CbsaException {
        this.getAccountByNumber(9999999999L);
        return this;
    }

    @Override
    public ICbsaTerminal getRandomCustomer() throws CbsaException {
        this.getCustomerByNumber(0);
        return this;
    }

    public CbsaTerminalImpl connectToCics() throws CicstsManagerException {
        this.connectToCicsRegion();
        return this;
    }

    @Override
    public void loginToApplication() throws CbsaException{
            try {
                this.clear().waitForKeyboard().type("OMEN").enter().waitForTextInField("BNK1MA");
            } catch (Zos3270Exception e) {
                throw new CbsaException("Failed to login to application: ", e);
            }
    }

    @Override
    public void returnToMenu() throws CbsaException {
        try {
            // Assumes you're only one pf3 away from the main menu
            if (this.retrieveScreen().contains("CICS Banking Sample Application")) {
                if (this.retrieveScreen().contains("BNK1MA")){
                    return;
                } else {
                    this.pf3().waitForKeyboard().waitForTextInField("BNK1MA");
                }
            } else {
                this.connectToCics();
                this.loginToApplication();
            }
        } catch (Zos3270Exception | CicstsManagerException e) {
            throw new CbsaException("Failed to login to application: ", e);
        }
    }

    public void discardAccount(String accountNumber) throws CbsaException {
        try {
            this.waitForKeyboard().type("2").enter().waitForKeyboard().type(accountNumber).enter().waitForKeyboard().pf5().waitForKeyboard();
        } catch (TimeoutException | KeyboardLockedException | TerminalInterruptedException | NetworkException | FieldNotFoundException e) {
            throw new CbsaException("Failed to login to application: ", e);
        }
    }

    @Override
    public String getAccountValue(String accountNumber) throws CbsaException {
        try {
            this.goToScreen(CbsaMenuItem.DISPLAY_ACCOUNT);
			this.waitForKeyboard().positionCursorToFieldContaining("ACCOUNT NUMBER").tab().type(accountNumber).enter().waitForKeyboard();
            return this.retrieveFieldTextAfterFieldWithString("Actual Balance");
        } catch (TimeoutException | KeyboardLockedException | TerminalInterruptedException | NetworkException | FieldNotFoundException | TextNotFoundException e) {
            throw new CbsaException("Failed to login to application: ", e);
        }
    }
}
