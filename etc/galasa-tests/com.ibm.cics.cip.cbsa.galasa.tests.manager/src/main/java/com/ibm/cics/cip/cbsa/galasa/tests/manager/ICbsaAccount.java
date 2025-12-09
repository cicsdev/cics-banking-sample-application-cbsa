package com.ibm.cics.cip.cbsa.galasa.tests.manager;

public interface ICbsaAccount{

	public String getAccountNumber();
	public String getAccountType();
	public String getCustomerNumber();
	public String getAccountValue() throws CbsaException;
	public void discardAccount(String accountNumber) throws CbsaException;
}
