/*
 *
 *    Copyright IBM Corp. 2023
 *
 *
 */

package com.ibm.cics.cip.bankliberty.datainterfaces;

import com.ibm.jzos.fields.*;

// Generated by IBM Record Generator for Java V3.0.2 Build 20221206-1121 on: Tue Feb 14 07:05:37 CST 2023

public class CustomerControl
{
	protected static CobolDatatypeFactory factory = new CobolDatatypeFactory();
	static
	{
		factory.setStringTrimDefault(false);
	}

	/**
	 * <pre>
	 01 WS-CUSTOMER-AGE               PIC S9999.
	 * </pre>
	 */
	public static final int CUSTOMER_CONTROL_LEN = 259;

	/**
	 * <pre>
	 01 CUSTOMER-CONTROL.
	     COPY CUSTCTRL.
	*
	*
	*    Copyright IBM Corp. 2023
	*
	*
	*
	     03 CUSTOMER-CONTROL-RECORD.
	 * </pre>
	 */
	public static final int CUSTOMER_CONTROL_RECORD_LEN = 259;

	public static final int CUSTOMER_CONTROL_RECORD_OFFSET = factory
			.getOffset();

	/**
	 * <pre>
	******************************************************************
	     03 CUSTOMER-CONTROL-RECORD.
	 * </pre>
	 */
	protected static final StringField CUSTOMER_CONTROL_EYECATCHER = factory
			.getStringField(4);

	/**
	 * <pre>
	     03 CUSTOMER-CONTROL-RECORD.
	 * </pre>
	 */
	public static final String CUSTOMER_CONTROL_EYECATCHER_V = "CTRL";

	/**
	 * <pre>
	        05 CUSTOMER-CONTROL-EYECATCHER             PIC X(4).
	 * </pre>
	 */
	public static final int CUSTOMER_CONTROL_KEY_LEN = 16;

	public static final int CUSTOMER_CONTROL_KEY_OFFSET = factory.getOffset();

	/**
	 * <pre>
	           88 CUSTOMER-CONTROL-EYECATCHER-V        VALUE 'CTRL'.
	 * </pre>
	 */
	protected static final ExternalDecimalAsIntField CUSTOMER_CONTROL_SORTCODE = factory
			.getExternalDecimalAsIntField(6, false, false, false, false);

	/**
	 * <pre>
	        05 CUSTOMER-CONTROL-KEY.
	 * </pre>
	 */
	protected static final ExternalDecimalAsLongField CUSTOMER_CONTROL_NUMBER = factory
			.getExternalDecimalAsLongField(10, false, false, false, false);

	/**
	 * <pre>
	           07 CUSTOMER-CONTROL-SORTCODE        PIC 9(6) DISPLAY.
	 * </pre>
	 */
	protected static final ExternalDecimalAsLongField NUMBER_OF_CUSTOMERS = factory
			.getExternalDecimalAsLongField(10, false, false, false, false);

	/**
	 * <pre>
	           07 CUSTOMER-CONTROL-NUMBER          PIC 9(10) DISPLAY.
	 * </pre>
	 */
	protected static final ExternalDecimalAsLongField LAST_CUSTOMER_NUMBER = factory
			.getExternalDecimalAsLongField(10, false, false, false, false);

	/**
	 * <pre>
	        05 NUMBER-OF-CUSTOMERS                 PIC 9(10) DISPLAY.
	 * </pre>
	 */
	protected static final StringField CUSTOMER_CONTROL_SUCCESS_FLAG = factory
			.getStringField(1);

	/**
	 * <pre>
	        05 LAST-CUSTOMER-NUMBER                PIC 9(10) DISPLAY.
	 * </pre>
	 */
	public static final String CUSTOMER_CONTROL_SUCCESS = "Y";

	/**
	 * <pre>
	        05 CUSTOMER-CONTROL-SUCCESS-FLAG       PIC X.
	 * </pre>
	 */
	protected static final StringField CUSTOMER_CONTROL_FAIL_CODE = factory
			.getStringField(1);

	/**
	 * <pre>
	        88 CUSTOMER-CONTROL-SUCCESS VALUE 'Y'.
	 * </pre>
	 */
	protected static final StringField FILLER_1 = factory.getStringField(38);

	/**
	 * <pre>
	        05 CUSTOMER-CONTROL-FAIL-CODE PIC X.
	 * </pre>
	 */
	protected static final StringField FILLER_2 = factory.getStringField(160);

	/**
	 * <pre>
	        05 FILLER                              PIC X(38).
	 * </pre>
	 */
	protected static final ExternalDecimalAsIntField FILLER_3 = factory
			.getExternalDecimalAsIntField(8, false, false, false, false);

	/**
	 * <pre>
	        05 FILLER                              PIC X(160).
	 * </pre>
	 */
	protected static final ExternalDecimalAsIntField FILLER_4 = factory
			.getExternalDecimalAsIntField(3, false, false, false, false);

	/**
	 * <pre>
	        05 FILLER                              PIC 9(8).
	 * </pre>
	 */
	protected static final ExternalDecimalAsIntField FILLER_5 = factory
			.getExternalDecimalAsIntField(8, false, false, false, false);

	protected byte[] byteBuffer;

	// Instance variables used to cache field values
	protected String customerControlEyecatcher;

	protected int customerControlSortcode;

	protected boolean customerControlSortcodeIsSet;

	protected long customerControlNumber;

	protected boolean customerControlNumberIsSet;

	protected long numberOfCustomers;

	protected boolean numberOfCustomersIsSet;

	protected long lastCustomerNumber;

	protected boolean lastCustomerNumberIsSet;

	protected String customerControlSuccessFlag;

	protected String customerControlFailCode;

	protected String filler1;

	protected String filler2;

	protected int filler3;

	protected boolean filler3IsSet;

	protected int filler4;

	protected boolean filler4IsSet;

	protected int filler5;

	protected boolean filler5IsSet;


	public CustomerControl(byte[] buffer)
	{
		this.byteBuffer = buffer;
	}


	public CustomerControl()
	{
		this.byteBuffer = new byte[CUSTOMER_CONTROL_LEN];
	}


	public byte[] getByteBuffer()
	{
		return byteBuffer;
	}


	public String getCustomerControlEyecatcher()
	{
		if (customerControlEyecatcher == null)
		{
			customerControlEyecatcher = CUSTOMER_CONTROL_EYECATCHER
					.getString(byteBuffer);
		}
		return customerControlEyecatcher;
	}


	public void setCustomerControlEyecatcher(String customerControlEyecatcher)
	{
		if (CUSTOMER_CONTROL_EYECATCHER.equals(this.customerControlEyecatcher,
				customerControlEyecatcher))
		{
			return;
		}
		CUSTOMER_CONTROL_EYECATCHER.putString(customerControlEyecatcher,
				byteBuffer);
		this.customerControlEyecatcher = customerControlEyecatcher;
	}


	public boolean isCustomerControlEyecatcherV()
	{
		return getCustomerControlEyecatcher()
				.equals(CUSTOMER_CONTROL_EYECATCHER_V);
	}


	public int getCustomerControlSortcode()
	{
		if (!customerControlSortcodeIsSet)
		{
			customerControlSortcode = CUSTOMER_CONTROL_SORTCODE
					.getInt(byteBuffer);
			customerControlSortcodeIsSet = true;
		}
		return customerControlSortcode;
	}


	public void setCustomerControlSortcode(int customerControlSortcode)
	{
		if (customerControlSortcodeIsSet && CUSTOMER_CONTROL_SORTCODE
				.equals(this.customerControlSortcode, customerControlSortcode))
		{
			return;
		}
		CUSTOMER_CONTROL_SORTCODE.putInt(customerControlSortcode, byteBuffer);
		this.customerControlSortcode = customerControlSortcode;
		customerControlSortcodeIsSet = true;
	}


	public long getCustomerControlNumber()
	{
		if (!customerControlNumberIsSet)
		{
			customerControlNumber = CUSTOMER_CONTROL_NUMBER.getLong(byteBuffer);
			customerControlNumberIsSet = true;
		}
		return customerControlNumber;
	}


	public void setCustomerControlNumber(long customerControlNumber)
	{
		if (customerControlNumberIsSet && CUSTOMER_CONTROL_NUMBER
				.equals(this.customerControlNumber, customerControlNumber))
		{
			return;
		}
		CUSTOMER_CONTROL_NUMBER.putLong(customerControlNumber, byteBuffer);
		this.customerControlNumber = customerControlNumber;
		customerControlNumberIsSet = true;
	}


	public long getNumberOfCustomers()
	{
		if (!numberOfCustomersIsSet)
		{
			numberOfCustomers = NUMBER_OF_CUSTOMERS.getLong(byteBuffer);
			numberOfCustomersIsSet = true;
		}
		return numberOfCustomers;
	}


	public void setNumberOfCustomers(long numberOfCustomers)
	{
		if (numberOfCustomersIsSet && NUMBER_OF_CUSTOMERS
				.equals(this.numberOfCustomers, numberOfCustomers))
		{
			return;
		}
		NUMBER_OF_CUSTOMERS.putLong(numberOfCustomers, byteBuffer);
		this.numberOfCustomers = numberOfCustomers;
		numberOfCustomersIsSet = true;
	}


	public long getLastCustomerNumber()
	{
		if (!lastCustomerNumberIsSet)
		{
			lastCustomerNumber = LAST_CUSTOMER_NUMBER.getLong(byteBuffer);
			lastCustomerNumberIsSet = true;
		}
		return lastCustomerNumber;
	}


	public void setLastCustomerNumber(long lastCustomerNumber)
	{
		if (lastCustomerNumberIsSet && LAST_CUSTOMER_NUMBER
				.equals(this.lastCustomerNumber, lastCustomerNumber))
		{
			return;
		}
		LAST_CUSTOMER_NUMBER.putLong(lastCustomerNumber, byteBuffer);
		this.lastCustomerNumber = lastCustomerNumber;
		lastCustomerNumberIsSet = true;
	}


	public String getCustomerControlSuccessFlag()
	{
		if (customerControlSuccessFlag == null)
		{
			customerControlSuccessFlag = CUSTOMER_CONTROL_SUCCESS_FLAG
					.getString(byteBuffer);
		}
		return customerControlSuccessFlag;
	}


	public void setCustomerControlSuccessFlag(String customerControlSuccessFlag)
	{
		if (CUSTOMER_CONTROL_SUCCESS_FLAG.equals(
				this.customerControlSuccessFlag, customerControlSuccessFlag))
		{
			return;
		}
		CUSTOMER_CONTROL_SUCCESS_FLAG.putString(customerControlSuccessFlag,
				byteBuffer);
		this.customerControlSuccessFlag = customerControlSuccessFlag;
	}


	public boolean isCustomerControlSuccess()
	{
		return getCustomerControlSuccessFlag().equals(CUSTOMER_CONTROL_SUCCESS);
	}


	public String getCustomerControlFailCode()
	{
		if (customerControlFailCode == null)
		{
			customerControlFailCode = CUSTOMER_CONTROL_FAIL_CODE
					.getString(byteBuffer);
		}
		return customerControlFailCode;
	}


	public void setCustomerControlFailCode(String customerControlFailCode)
	{
		if (CUSTOMER_CONTROL_FAIL_CODE.equals(this.customerControlFailCode,
				customerControlFailCode))
		{
			return;
		}
		CUSTOMER_CONTROL_FAIL_CODE.putString(customerControlFailCode,
				byteBuffer);
		this.customerControlFailCode = customerControlFailCode;
	}


	public String getFiller1()
	{
		if (filler1 == null)
		{
			filler1 = FILLER_1.getString(byteBuffer);
		}
		return filler1;
	}


	public void setFiller1(String filler1Local)
	{
		if (FILLER_1.equals(this.filler1, filler1Local))
		{
			return;
		}
		FILLER_1.putString(filler1, byteBuffer);
		this.filler1 = filler1Local;
	}


	public String getFiller2()
	{
		if (filler2 == null)
		{
			filler2 = FILLER_2.getString(byteBuffer);
		}
		return filler2;
	}


	public void setFiller2(String filler2Local)
	{
		if (FILLER_2.equals(this.filler2, filler2Local))
		{
			return;
		}
		FILLER_2.putString(filler2Local, byteBuffer);
		this.filler2 = filler2Local;
	}


	public int getFiller3()
	{
		if (!filler3IsSet)
		{
			filler3 = FILLER_3.getInt(byteBuffer);
			filler3IsSet = true;
		}
		return filler3;
	}


	public void setFiller3(int filler3Local)
	{
		if (filler3IsSet && FILLER_3.equals(this.filler3, filler3Local))
		{
			return;
		}
		FILLER_3.putInt(filler3Local, byteBuffer);
		this.filler3 = filler3Local;
		filler3IsSet = true;
	}


	public int getFiller4()
	{
		if (!filler4IsSet)
		{
			filler4 = FILLER_4.getInt(byteBuffer);
			filler4IsSet = true;
		}
		return filler4;
	}


	public void setFiller4(int filler4Local)
	{
		if (filler4IsSet && FILLER_4.equals(this.filler4, filler4Local))
		{
			return;
		}
		FILLER_4.putInt(filler4Local, byteBuffer);
		this.filler4 = filler4Local;
		filler4IsSet = true;
	}


	public int getFiller5()
	{
		if (!filler5IsSet)
		{
			filler5 = FILLER_5.getInt(byteBuffer);
			filler5IsSet = true;
		}
		return filler5;
	}


	public void setFiller5(int filler5Local)
	{
		if (filler5IsSet && FILLER_5.equals(this.filler5, filler5Local))
		{
			return;
		}
		FILLER_5.putInt(filler5Local, byteBuffer);
		this.filler5 = filler5Local;
		filler5IsSet = true;
	}

}
