/*                                                                        */
/* Copyright IBM Corp. 2023                                               */
/*                                                                        */
package com.ibm.cics.cip.bank.springboot.customerservices.jsonclasses.createaccount;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.annotation.JsonNaming;
import com.ibm.cics.cip.bank.springboot.customerservices.JsonPropertyNamingStrategy;

@JsonNaming(JsonPropertyNamingStrategy.class)
public class CreaccJson
{



	@JsonProperty("COMM_ACC_TYPE")
	private String commAccType;

	@JsonProperty("COMM_CUSTNO")
	private String commCustno;

	@JsonProperty("COMM_EYECATCHER")
	private String commEyecatcher;

	@JsonProperty("COMM_KEY")
	private CreaccKeyJson commKey;

	@JsonProperty("COMM_INT_RT")
	private float commInterestRate;

	@JsonProperty("COMM_OPENED")
	private int commOpened;

	@JsonProperty("COMM_OVERDR_LIM")
	private float commOverdraftLimit;

	@JsonProperty("COMM_LAST_STMT_DT")
	private int commLastStatementDate;

	@JsonProperty("COMM_NEXT_STMT_DT")
	private int commNextStatementDate;

	@JsonProperty("COMM_AVAIL_BAL")
	private float commAvailableBalance;

	@JsonProperty("COMM_ACT_BAL")
	private float commActualBalance;

	@JsonProperty("COMM_SUCCESS")
	private String commSuccess;

	@JsonProperty("COMM_FAIL_CODE")
	private String commFailCode;


	public CreaccJson(String accountType, String accountNumber,
			float overdraftLimit, float interestRate)
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
