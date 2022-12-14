/*
 *
 *    Copyright IBM Corp. 2022
 *
 *
 */
package com.ibm.cics.cip.bank.springboot.customerservices.jsonclasses.createaccount;

import javax.validation.constraints.*;

public class CreateAccountForm {

    static final String COPYRIGHT =
      "Copyright IBM Corp. 2022";


    @NotNull
    @Size(max = 8)
    private String custNumber;

    @NotNull
    private AccountType accountType;

    private int overdraftLimit;
    
    private float interestRate;

    public int getOverdraftLimit() {
        return overdraftLimit;
    }

    public void setOverdraftLimit(int overdraftLimit) {
        this.overdraftLimit = overdraftLimit;
    }

    public float getInterestRate() {
        return interestRate;
    }

    public void setInterestRate(float interestRate) {
        this.interestRate = interestRate;
    }

    public CreateAccountForm() {

    }

    public String getCustNumber() {
        return custNumber;
    }

    public void setCustNumber(String custNumber) {
        this.custNumber = custNumber;
    }

    public AccountType getAccountType() {
        return accountType;
    }

    public void setAccountType(AccountType accountType) {
        this.accountType = accountType;
    }

    @Override
    public String toString() {
        return "CreateAccountForm [accountType=" + accountType + ", custNumber=" + custNumber + ", interestRate="
                + interestRate + ", overdraftLimit=" + overdraftLimit + "]";
    }
    
}
