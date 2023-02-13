/*
 *
 *    Copyright IBM Corp. 2023
 *
 */
package com.ibm.cics.cip.bankliberty.api.json;

import java.io.IOException;
import java.sql.Date;
import java.util.Calendar;
import java.util.Random;
import java.util.logging.LogManager;
import java.util.logging.Logger;

/**
 * This class describes the methods of the CreditScore, used to generate a
 * credit score for new customers
 * 
 */

public class CreditScore
{

	private CreditScore()
	{
		throw new IllegalStateException("Static only");
	}

	private static Logger logger = Logger
			.getLogger("com.ibm.cics.cip.bankliberty.api.json");


	public static CustomerJSON populateCreditScoreAndReviewDate(
			CustomerJSON customer)
	{
		// return true if it worked, false if it failed
		sortOutLogging();
		logger.entering("CreditScore",
				"populateCreditScoreAndReviewDate(CustomerJSON customer)");

		logger.fine(
				"CreditScore using credit agency to set credit score and review date");
		try
		{
			customer = CreditScoreCICS540
					.populateCreditScoreAndReviewDate(customer);
		}
		catch (java.lang.NoClassDefFoundError e)
		{
			int creditScoreTotal = 0;
			for (int i = 1; i < 5; i++)
			{
				creditScoreTotal = creditScoreTotal
						+ new Random(Calendar.getInstance().getTimeInMillis())
								.nextInt(999)
						+ 1;
			}
			customer.setCreditScore(Integer.toString(creditScoreTotal / 5));
			Calendar calendar = Calendar.getInstance();
			long nowMs = calendar.getTimeInMillis();
			int next21Days = new Random(calendar.getTimeInMillis()).nextInt(20);
			next21Days++;
			nowMs = nowMs + (1000L * 60L * 60L * 24L * next21Days);
			customer.setReviewDate(new Date(nowMs));
		}
		logger.exiting("CreditScore",
				"populateCreditScoreAndReviewDate(CustomerJSON customer)",
				customer);
		return customer;
	}


	private static void sortOutLogging()
	{
		try
		{
			LogManager.getLogManager().readConfiguration();
		}
		catch (SecurityException | IOException e)
		{
			logger.severe(e.toString());
		}
	}

}
