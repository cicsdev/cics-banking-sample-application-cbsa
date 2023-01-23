package com.ibm.cics.cip.bank.springboot.customerservices.controllers;

public class InvalidCustomerException extends Exception
{

	private final String errorMsg;
	/**
	 * General exception for a customer that does not meet standards
	 */
	private static final long serialVersionUID = 4059447992690613550L;

	InvalidCustomerException(String msg)
	{
		errorMsg = msg;
	}

	@Override
	public String getMessage()
	{
		return errorMsg;
	}

}
