/*
 *
 *    Copyright IBM Corp. 2022
 *
 *
 */
package com.ibm.cics.cip.bank.springboot.customerservices.jsonclasses.createaccount;

import com.fasterxml.jackson.databind.annotation.JsonNaming;
import com.ibm.cics.cip.bank.springboot.customerservices.JsonPropertyNamingStrategy;

@JsonNaming(JsonPropertyNamingStrategy.class)
public class CreaccJson
{

	static final String COPYRIGHT = "Copyright IBM Corp. 2022";

	private String commAccType;
	private String commCustno;
	private String commEyecatcher;
	private CreaccKeyJson commKey;
	private float commInterestRate;
	private int commOpened;
	private float commOverdraftLimit;
	private int commLastStatementDate;
	private int commNextStatementDate;
	private float commAvailableBalance;
	private float commActualBalance;
	private String commSuccess;
	private String commFailCode;

	public CreaccJson(String accountType, String accountNumber, float overdraftLimit, float interestRate)
	{
		commAccType = String.format("%-8s", accountType);
		commCustno = String.format("%8s", accountNumber).replace(" ", "0");

		commOverdraftLimit = overdraftLimit;
		commInterestRate = interestRate;

		commEyecatcher = "    ";
		commKey = new CreaccKeyJson();

	}

	public CreaccJson()
	{

	}

	public String getCommEyecatcher()
	{
		return commEyecatcher;
	}

	public void setCommEyecatcher(String commEyecatcherIn)
	{
		commEyecatcher = commEyecatcherIn;
	}

	public String getCommCustno()
	{
		return commCustno;
	}

	public void setCommCustno(String commCustnoIn)
	{
		commCustno = commCustnoIn;
	}

	public CreaccKeyJson getCommKey()
	{
		return commKey;
	}

	public void setCommKey(CreaccKeyJson commKeyIn)
	{
		commKey = commKeyIn;
	}

	public String getCommAccType()
	{
		return commAccType;
	}

	public void setCommAccType(String commAccTypeIn)
	{
		commAccType = commAccTypeIn;
	}

	public float getCommInterestRate()
	{
		return commInterestRate;
	}

	public void setCommInterestRate(float commInterestRateIn)
	{
		commInterestRate = commInterestRateIn;
	}

	public int getCommOpened()
	{
		return commOpened;
	}

	public void setCommOpened(int commOpenedIn)
	{
		commOpened = commOpenedIn;
	}

	public float getCommOverdraftLimit()
	{
		return commOverdraftLimit;
	}

	public void setCommOverdraftLimit(float commOverdraftLimitIn)
	{
		commOverdraftLimit = commOverdraftLimitIn;
	}

	public int getCommLastStatementDate()
	{
		return commLastStatementDate;
	}

	public void setCommLastStatementDate(int setCommLastStatementDateIn)
	{
		commLastStatementDate = setCommLastStatementDateIn;
	}

	public int getCommNextStatementDate()
	{
		return commNextStatementDate;
	}

	public void setCommNextStatementDate(int commNextStatementDateIn)
	{
		commNextStatementDate = commNextStatementDateIn;
	}

	public float getCommAvailableBalance()
	{
		return commAvailableBalance;
	}

	public void setCommAvailableBalance(float commAvailableBalanceIn)
	{
		commAvailableBalance = commAvailableBalanceIn;
	}

	public float getCommActualBalance()
	{
		return commActualBalance;
	}

	public void setCommActualBalance(float commActualBalanceIn)
	{
		commActualBalance = commActualBalanceIn;
	}

	public String getCommSuccess()
	{
		return commSuccess;
	}

	public void setCommSuccess(String commSuccessIn)
	{
		commSuccess = commSuccessIn;
	}

	public String getCommFailCode()
	{
		return commFailCode;
	}

	public void setCommFailCode(String commFailCodeIn)
	{
		commFailCode = commFailCodeIn;
	}

}
