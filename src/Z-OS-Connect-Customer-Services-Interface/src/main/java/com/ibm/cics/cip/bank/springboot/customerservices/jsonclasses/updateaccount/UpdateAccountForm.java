/*
 *
 *    Copyright IBM Corp. 2022
 *
 *
 */
package com.ibm.cics.cip.bank.springboot.customerservices.jsonclasses.updateaccount;

import javax.validation.constraints.NotNull;

import com.ibm.cics.cip.bank.springboot.customerservices.jsonclasses.createaccount.AccountType;

import org.hibernate.validator.constraints.Range;

public class UpdateAccountForm {

    static final String COPYRIGHT =
      "Copyright IBM Corp. 2022";


    private String custNumber;

    // Limiting length to 8
    @NotNull
    @Range(min = 1, max = 99999999)
    private int acctNumber;

    @NotNull(message = "You must choose an account type")
    private AccountType acctType;

    @NotNull
    private String acctInterestRate;

    @NotNull
    private String acctOverdraft;

    private String acctOpenedDate;

    private String acctLastStatementDate;

    private String acctNextStatementDate;

    private float acctAvailableBalance;

    private float acctActualBalance;

    public String getCustNumber() {
        return custNumber;
    }

    public void setCustNumber(String custNumber) {
        this.custNumber = custNumber;
    }

    public int getAcctNumber() {
        return acctNumber;
    }

    public void setAcctNumber(int acctNumber) {
        this.acctNumber = acctNumber;
    }

    public AccountType getAcctType() {
        return acctType;
    }

    public void setAcctType(AccountType acctType) {
        this.acctType = acctType;
    }
    
    // Interest Rate
    public String getAcctInterestRate() {
        return acctInterestRate;
    }

    public float getAcctInterestRateFloat() {
        return Float.parseFloat(acctInterestRate);
    }

    // Counting "" as null here, as an actual value needs to be entered
    public void setAcctInterestRate(String acctInterestRate) {
        this.acctInterestRate = acctInterestRate == "" ? null : acctInterestRate;
    }

    // Overdraft
    public String getAcctOverdraft() {
        return acctOverdraft;
    }

    public int getAcctOverdraftInt() {
        return Integer.parseInt(acctOverdraft);
    }

    // Counting "" as null here, as an actual value needs to be entered
    public void setAcctOverdraft(String acctOverdraft) {
        this.acctOverdraft = acctOverdraft == "" ? null : String.valueOf(Integer.parseInt(acctOverdraft));
    }

    public String getAcctOpenedDate() {
        return acctOpenedDate;
    }

    public void setAcctOpenedDate(String acctOpenedDate) {
        if (acctOpenedDate == "") {
            return;
        }
        this.acctOpenedDate = "";
        this.acctOpenedDate += acctOpenedDate.substring(8, 10) + acctOpenedDate.substring(5, 7)
                + acctOpenedDate.substring(0, 4);
    }

    public String getAcctLastStatementDate() {
        return acctLastStatementDate;
    }

    public void setAcctLastStatementDate(String acctLastStatementDate) {
        if (acctLastStatementDate == "") {
            return;
        }
        this.acctLastStatementDate = "";
        this.acctLastStatementDate += acctLastStatementDate.substring(8, 10) + acctLastStatementDate.substring(5, 7)
                + acctLastStatementDate.substring(0, 4);
    }

    public String getAcctNextStatementDate() {
        return acctNextStatementDate;
    }

    public void setAcctNextStatementDate(String acctNextStatementDate) {
        if (acctNextStatementDate == "") {
            return;
        }
        this.acctNextStatementDate = "";
        this.acctNextStatementDate += acctNextStatementDate.substring(8, 10) + acctNextStatementDate.substring(5, 7)
                + acctNextStatementDate.substring(0, 4);
    }

    public float getAcctAvailableBalance() {
        return acctAvailableBalance;
    }

    public void setAcctAvailableBalance(float acctAvailableBalance) {
        this.acctAvailableBalance = acctAvailableBalance;
    }

    public float getAcctActualBalance() {
        return acctActualBalance;
    }

    public void setAcctActualBalance(float acctActualBalance) {
        this.acctActualBalance = acctActualBalance;
    }

    @Override
    public String toString() {
        return "UpdateAccountForm [acctActualBalance=" + acctActualBalance + ", acctAvailableBalance="
                + acctAvailableBalance + ", acctInterestRate=" + acctInterestRate + ", acctLastStatementDate="
                + acctLastStatementDate + ", acctNextStatementDate=" + acctNextStatementDate + ", acctNumber="
                + acctNumber + ", acctOpenedDate=" + acctOpenedDate + ", acctOverdraft=" + acctOverdraft + ", acctType="
                + acctType + ", custNumber=" + custNumber + "]";
    }

}
