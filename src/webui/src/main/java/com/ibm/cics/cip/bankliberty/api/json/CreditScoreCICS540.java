/*
 *
 *    Copyright IBM Corp. 2022
 *
 */
package com.ibm.cics.cip.bankliberty.api.json;

import java.io.IOException;
import java.sql.Date;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;
import java.util.Random;
import java.util.concurrent.Future;
import java.util.logging.Level;
import java.util.logging.LogManager;
import java.util.logging.Logger;

import com.ibm.cics.cip.bankliberty.datainterfaces.CRECUST;
import com.ibm.cics.server.AsyncService;
import com.ibm.cics.server.AsyncServiceImpl;
import com.ibm.cics.server.CCSIDErrorException;
import com.ibm.cics.server.Channel;
import com.ibm.cics.server.ChannelErrorException;
import com.ibm.cics.server.ChildResponse;
import com.ibm.cics.server.CodePageErrorException;
import com.ibm.cics.server.Container;
import com.ibm.cics.server.ContainerErrorException;
import com.ibm.cics.server.InvalidRequestException;
import com.ibm.cics.server.InvalidTransactionIdException;
import com.ibm.cics.server.NotAuthorisedException;
import com.ibm.cics.server.NotFoundException;
import com.ibm.cics.server.ResourceDisabledException;
import com.ibm.cics.server.Task;
import com.ibm.cics.server.ChildResponse.CompletionStatus;

/**
 * This class is used with the jCICS ASYNC API to generate multiple credit scores
 * 
 */



public class CreditScoreCICS540 {

	static final String COPYRIGHT =
			"Copyright IBM Corp. 2022";

	private static Logger logger = Logger.getLogger("com.ibm.cics.cip.bankliberty.api.json");

	public static CustomerJSON populateCreditScoreAndReviewDate(CustomerJSON customer)
	{
		sortOutLogging();

		int creditAgencyCount = 5;

		/*                Set up a random CS review date within the next 21 days */

		Calendar calendar = Calendar.getInstance();
		long nowMs = calendar.getTimeInMillis();
		int next21Days = new Random(calendar.getTimeInMillis()).nextInt(20);
		next21Days++;
		nowMs = nowMs + (1000L * 60L * 60L * 24L * next21Days);
		customer.setReviewDate(new Date(nowMs));
		Channel myCreditScoreChannel = null;

		AsyncService asService = new AsyncServiceImpl();
		List<Future<ChildResponse>> children = new ArrayList<>();
		String[] containerID = new String[creditAgencyCount];
		int creditScoreTotal = 0;
		
		try 
		{
			myCreditScoreChannel = Task.getTask().createChannel("CIPCREDCHANN");

			String[] transactionID = new String[creditAgencyCount];

			for(int i=0;i < creditAgencyCount;i++)
			{
				transactionID[i] = "O" + "CR" + (i +1);
				containerID[i] = "CIP" + ((char)('A'+i));

				CRECUST myCRECUST = new CRECUST();
				myCRECUST.setCommAddress(customer.getCustomerAddress());
				Calendar myCalendar = Calendar.getInstance();
				myCalendar.setTime(customer.getDateOfBirth());
				myCRECUST.setCommBirthDay(myCalendar.get(Calendar.DAY_OF_MONTH));
				myCRECUST.setCommBirthMonth(myCalendar.get(Calendar.MONTH) + 1);
				myCRECUST.setCommBirthYear(myCalendar.get(Calendar.YEAR));
				myCRECUST.setCommName(customer.getCustomerName());
				myCRECUST.setCommNumber(Long.parseLong(customer.getId()));
				myCalendar.setTime(customer.getReviewDate());
				myCRECUST.setCommCsReviewDd(myCalendar.get(Calendar.DAY_OF_MONTH));
				myCRECUST.setCommCsReviewMm(myCalendar.get(Calendar.MONTH) + 1);
				myCRECUST.setCommCsReviewYyyy(myCalendar.get(Calendar.YEAR));
				myCRECUST.setCommSortcode(Integer.parseInt(customer.getSortCode()));

				Container myContainer = myCreditScoreChannel.createContainer(containerID[i]);
				myContainer.put(myCRECUST.getByteBuffer());

				children.add(asService.runTransactionId(transactionID[i],myCreditScoreChannel));

			}
		}
		catch (CCSIDErrorException | CodePageErrorException | ContainerErrorException | InvalidRequestException | InvalidTransactionIdException | NotAuthorisedException | ResourceDisabledException | ChannelErrorException e) 
		{
			logger.severe(e.toString());
			return null;
		} 

	int completedRequests = 0;
	while(completedRequests < creditAgencyCount)
	{
		ChildResponse anyOneWillDo = null;
		try 
		{
			anyOneWillDo = asService.getAny();

			if(anyOneWillDo.getCompletionStatus().equals(CompletionStatus.NORMAL))
			{
				Channel responseChannel = anyOneWillDo.getChannel();
				customer.setCreditScore("123");
				Container myContainer;
				boolean foundIt = false;
				for(int j = 0; !foundIt;j++)
				{
					if(anyOneWillDo.equals(children.get(j)))
					{
						myContainer = responseChannel.getContainer(containerID[j]);
						byte[] myContainerBytes = myContainer.get();
						CRECUST myCRECUST = new CRECUST(myContainerBytes);
						creditScoreTotal = creditScoreTotal + myCRECUST.getCommCreditScore();
						foundIt = true;
						completedRequests++;
					}
				}

			} 
			else
			{
				logger.log(Level.SEVERE,() -> "One of the agencies didn't work");
				creditAgencyCount--;
			}
		}
		catch (InvalidRequestException | NotFoundException | ChannelErrorException | CCSIDErrorException | CodePageErrorException | ContainerErrorException e) 
		{
			logger.severe(e.toString());
			Task.getTask().abend("CRDT");
		}

	}



	int creditScoreAverage = creditScoreTotal / creditAgencyCount;
	customer.setCreditScore(Integer.toString(creditScoreAverage));
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
