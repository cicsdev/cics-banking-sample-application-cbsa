/*                                                                        */
/* Copyright IBM Corp. 2023                                               */
/*                                                                        */
package com.ibm.cics.cip.bank.springboot.paymentinterface.jsonclasses.paymentinterface;

import com.fasterxml.jackson.databind.annotation.JsonNaming;
import com.ibm.cics.cip.bank.springboot.paymentinterface.JsonPropertyNamingStrategy;

@JsonNaming(JsonPropertyNamingStrategy.class)
public class DbcrJson
{



	private String commAccno;

	private float commAmt;

	private int commSortC = 0;

	private int commAvBal = 0;

	private int commActBal = 0;

	private OriginJson commOrigin;

	private String commSuccess = " ";

	private String commFailCode = " ";


	public DbcrJson()
	{

	}


	public DbcrJson(TransferForm transferForm)
	{
		commOrigin = new OriginJson(transferForm.getOrganisation());

		// accno
		// Pads out with zeroes to the length specified
		commAccno = String.format("%8s", transferForm.getAcctNumber())
				.replace(" ", "0");

		// Make the amount positive or negative based on wether debit or credit
		// is selected
		commAmt = transferForm.isDebit() ? (transferForm.getAmount() * -1)
				: transferForm.getAmount();
	}


	public String getCommAccno()
	{
		return commAccno;
	}


	public void setCommAccno(String commAccnoIn)
	{
		commAccno = commAccnoIn;
	}


	public float getCommAmt()
	{
		return commAmt;
	}


	public void setCommAmt(float commAmtIn)
	{
		commAmt = commAmtIn;
	}


	public int getCommSortC()
	{
		return commSortC;
	}


	public void setCommC(int commSortCodeIn)
	{
		commSortC = commSortCodeIn;
	}


	public int getCommAvBal()
	{
		return commAvBal;
	}


	public void setCommAvBal(int commAvBalIn)
	{
		commAvBal = commAvBalIn;
	}


	public int getCommActBal()
	{
		return commActBal;
	}


	public void setCommActBal(int commActBalIn)
	{
		commActBal = commActBalIn;
	}


	public OriginJson getCommOrigin()
	{
		return commOrigin;
	}


	public void setCommOrigin(OriginJson commOriginIn)
	{
		commOrigin = commOriginIn;
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


	@Override
	public String toString()
	{
		return "DbcrJson [CommAccno=" + commAccno + ", CommActBal="
				+ commActBal + ", CommAmt=" + commAmt + ", CommAvBal="
				+ commAvBal + ", CommFailCode=" + commFailCode
				+ ", CommOrigin=" + commOrigin.toString() + ", CommSortC="
				+ commSortC + ", CommSuccess=" + commSuccess + "]";
	}

}
