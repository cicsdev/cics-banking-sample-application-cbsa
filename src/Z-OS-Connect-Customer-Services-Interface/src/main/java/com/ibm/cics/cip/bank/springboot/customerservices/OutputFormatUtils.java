/*
 *
 *    Copyright IBM Corp. 2022
 *
 *
 */
package com.ibm.cics.cip.bank.springboot.customerservices;

public class OutputFormatUtils
{

	static final String COPYRIGHT = "Copyright IBM Corp. 2022";


	private OutputFormatUtils()
	{
		throw new IllegalStateException("Static only");
	}


	public static String date(String date)
	{

		String unSlashedString = String.format("%8s", date).replace(" ", "0");
		return unSlashedString.substring(0, 2) + "/"
				+ unSlashedString.substring(2, 4) + "/"
				+ unSlashedString.substring(4, 8);
	}


	public static String date(int date)
	{
		return date(String.valueOf(date));
	}


	// Although using String.format directly might work sometimes, you still
	// need different formatting depending
	// on wether the input is a string or an int - this makes it slightly
	// quicker for account and customer numbers,
	// because the type returned can vary. Not as necessary for monetary values
	// since they're always a float or an int.
	public static String leadingZeroes(int amount, String input)
	{
		String formatString = "%" + amount + "s";
		return String.format(formatString, input).replace(" ", "0");
	}


	public static String leadingZeroes(int amount, int input)
	{
		return leadingZeroes(amount, String.valueOf(input));
	}
}
