package com.ibm.cics.cip.cbsa.galasa.tests.manager;

import java.util.Map;

import dev.galasa.selenium.IWebPage;

public interface ICbsaWebInterface extends IWebPage {

    public String getBirthdateFromAge(int age);
    public String deleteCustomer(String customerNumber, String xpath) throws Exception;
    public void returnHome() throws CbsaException;
    public Map<String, String> inputCreateCustomerDobTestData(String dob, Boolean expectAlert) throws Exception;
    public Map<String, String> createAccountForCustomer(String customerNumber) throws Exception;
    public Map<String, String> updateCustomer(String customerNumber) throws Exception;
    public Map<String, String> updateAccount(String accountNumber) throws Exception;
    public String deleteAccount(String accountNumber) throws Exception;

}
