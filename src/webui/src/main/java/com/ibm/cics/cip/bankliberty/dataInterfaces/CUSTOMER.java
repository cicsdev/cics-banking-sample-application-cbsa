/*
 *
 *    Copyright contributors to the CICS Banking Sample Application (CBSA) project
 *
 */
package com.ibm.cics.cip.bankliberty.dataInterfaces;
import com.ibm.jzos.fields.*;

// Generated by IBM Record Generator for Java V3.0.0 Build 20170904-1704 on: Fri Feb 12 14:18:05 GMT 2021

public class CUSTOMER {

    static final String COPYRIGHT =
      "Copyright contributors to the CICS Banking Sample Application (CBSA) project.";

	protected static CobolDatatypeFactory factory = new CobolDatatypeFactory();
	static { factory.setStringTrimDefault(false); } 

	/** <pre>
	 01 COBOL-LANGUAGE-STRUCTURE. </pre> */
	public static final int COBOL_LANGUAGE_STRUCTURE_len = 259; 

	/** <pre>
	     03 CUSTOMER-RECORD. </pre> */
	public static final int CUSTOMER_RECORD_len = 259; 
	public static final int CUSTOMER_RECORD_offset = factory.getOffset();

	/** <pre>
	        05 CUSTOMER-EYECATCHER                 PIC X(4). </pre> */
	protected static final StringField CUSTOMER_EYECATCHER = factory.getStringField(4);

	/** <pre>
	           88 CUSTOMER-EYECATCHER-VALUE        VALUE 'CUST'. </pre> */
	public static final String CUSTOMER_EYECATCHER_VALUE = "CUST";

	/** <pre>
	        05 CUSTOMER-KEY. </pre> */
	public static final int CUSTOMER_KEY_len = 16; 
	public static final int CUSTOMER_KEY_offset = factory.getOffset();

	/** <pre>
	           07 CUSTOMER-SORTCODE                PIC 9(6) DISPLAY. </pre> */
	protected static final ExternalDecimalAsIntField CUSTOMER_SORTCODE = factory.getExternalDecimalAsIntField(6, false, false, false, false);

	/** <pre>
	           07 CUSTOMER-NUMBER                  PIC 9(10) DISPLAY. </pre> */
	protected static final ExternalDecimalAsLongField CUSTOMER_NUMBER = factory.getExternalDecimalAsLongField(10, false, false, false, false);

	/** <pre>
	        05 CUSTOMER-NAME                       PIC X(60). </pre> */
	protected static final StringField CUSTOMER_NAME = factory.getStringField(60);

	/** <pre>
	*          07 CUSTOMER-TITLE                   PIC X(8).
	*          07 CUSTOMER-GIVEN-NAME              PIC X(20).
	*          07 CUSTOMER-INITIALS                PIC X(10).
	*          07 CUSTOMER-FAMILY-NAME             PIC X(20).
	        05 CUSTOMER-ADDRESS                    PIC X(160). </pre> */
	protected static final StringField CUSTOMER_ADDRESS = factory.getStringField(160);

	/** <pre>
	*          07 CUSTOMER-STREET-ADDRESS          PIC X(50).
	*          07 CUSTOMER-ADDRESS-DISTRICT        PIC X(50).
	*          07 CUSTOMER-ADDRESS-TOWN            PIC X(50).
	*          07 CUSTOMER-POSTCODE-OR-ZIP         PIC X(10).
	        05 CUSTOMER-DATE-OF-BIRTH              PIC 9(8). </pre> */
	protected static final ExternalDecimalAsIntField CUSTOMER_DATE_OF_BIRTH = factory.getExternalDecimalAsIntField(8, false, false, false, false);

	/** <pre>
	        05 CUSTOMER-DOB-GROUP REDEFINES CUSTOMER-DATE-OF-BIRTH. </pre> */
	static { factory.pushOffset(); } 
	static { factory.setOffset(CUSTOMER_DATE_OF_BIRTH.getOffset()); } 
	public static final int CUSTOMER_DOB_GROUP_len = 8; 
	public static final int CUSTOMER_DOB_GROUP_offset = factory.getOffset();

	/** <pre>
	           07 CUSTOMER-BIRTH-DAY               PIC 99. </pre> */
	protected static final ExternalDecimalAsIntField CUSTOMER_BIRTH_DAY = factory.getExternalDecimalAsIntField(2, false, false, false, false);

	/** <pre>
	           07 CUSTOMER-BIRTH-MONTH             PIC 99. </pre> */
	protected static final ExternalDecimalAsIntField CUSTOMER_BIRTH_MONTH = factory.getExternalDecimalAsIntField(2, false, false, false, false);

	/** <pre>
	           07 CUSTOMER-BIRTH-YEAR              PIC 9999. </pre> */
	protected static final ExternalDecimalAsIntField CUSTOMER_BIRTH_YEAR = factory.getExternalDecimalAsIntField(4, false, false, false, false);
	static { factory.popOffset(); } 

	/** <pre>
	        05 CUSTOMER-CREDIT-SCORE               PIC 999. </pre> */
	protected static final ExternalDecimalAsIntField CUSTOMER_CREDIT_SCORE = factory.getExternalDecimalAsIntField(3, false, false, false, false);

	/** <pre>
	        05 CUSTOMER-CS-REVIEW-DATE             PIC 9(8). </pre> */
	protected static final ExternalDecimalAsIntField CUSTOMER_CS_REVIEW_DATE = factory.getExternalDecimalAsIntField(8, false, false, false, false);

	/** <pre>
	        05 CUSTOMER-CS-GROUP
	           REDEFINES CUSTOMER-CS-REVIEW-DATE. </pre> */
	static { factory.pushOffset(); } 
	static { factory.setOffset(CUSTOMER_CS_REVIEW_DATE.getOffset()); } 
	public static final int CUSTOMER_CS_GROUP_len = 8; 
	public static final int CUSTOMER_CS_GROUP_offset = factory.getOffset();

	/** <pre>
	           07 CUSTOMER-CS-REVIEW-DAY           PIC 99. </pre> */
	protected static final ExternalDecimalAsIntField CUSTOMER_CS_REVIEW_DAY = factory.getExternalDecimalAsIntField(2, false, false, false, false);

	/** <pre>
	           07 CUSTOMER-CS-REVIEW-MONTH         PIC 99. </pre> */
	protected static final ExternalDecimalAsIntField CUSTOMER_CS_REVIEW_MONTH = factory.getExternalDecimalAsIntField(2, false, false, false, false);

	/** <pre>
	           07 CUSTOMER-CS-REVIEW-YEAR          PIC 9999. </pre> */
	protected static final ExternalDecimalAsIntField CUSTOMER_CS_REVIEW_YEAR = factory.getExternalDecimalAsIntField(4, false, false, false, false);
	static { factory.popOffset(); } 

	protected byte[] _byteBuffer;
	// Instance variables used to cache field values
	protected String customerEyecatcher;
	protected Integer customerSortcode;
	protected Long customerNumber;
	protected String customerName;
	protected String customerAddress;
	protected Integer customerCreditScore;


	public CUSTOMER (byte[] buffer) {
		this._byteBuffer = buffer;
	}

	public CUSTOMER () {
		this._byteBuffer = new byte[COBOL_LANGUAGE_STRUCTURE_len];
	}

	public byte[] getByteBuffer() {
		return _byteBuffer;
	}


	public String getCustomerEyecatcher() {
		if (customerEyecatcher == null) {
			customerEyecatcher = CUSTOMER_EYECATCHER.getString(_byteBuffer);
		}
		return customerEyecatcher;
	}

	public void setCustomerEyecatcher(String customerEyecatcher) {
		if (CUSTOMER_EYECATCHER.equals(this.customerEyecatcher, customerEyecatcher)) {
			return;
		}
		CUSTOMER_EYECATCHER.putString(customerEyecatcher, _byteBuffer);
		this.customerEyecatcher = customerEyecatcher;
	}

	public boolean isCustomerEyecatcherValue() {
		return getCustomerEyecatcher().equals(CUSTOMER_EYECATCHER_VALUE);
	}

	public int getCustomerSortcode() {
		if (customerSortcode == null) {
			customerSortcode = new Integer(CUSTOMER_SORTCODE.getInt(_byteBuffer));
		}
		return customerSortcode.intValue();
	}

	public void setCustomerSortcode(int customerSortcode) {
		if (CUSTOMER_SORTCODE.equals(this.customerSortcode, customerSortcode)) {
			return;
		}
		CUSTOMER_SORTCODE.putInt(customerSortcode, _byteBuffer);
		this.customerSortcode = new Integer(customerSortcode);
	}

	public long getCustomerNumber() {
		if (customerNumber == null) {
			customerNumber = new Long(CUSTOMER_NUMBER.getLong(_byteBuffer));
		}
		return customerNumber.longValue();
	}

	public void setCustomerNumber(long customerNumber) {
		if (CUSTOMER_NUMBER.equals(this.customerNumber, customerNumber)) {
			return;
		}
		CUSTOMER_NUMBER.putLong(customerNumber, _byteBuffer);
		this.customerNumber = new Long(customerNumber);
	}

	public String getCustomerName() {
		if (customerName == null) {
			customerName = CUSTOMER_NAME.getString(_byteBuffer);
		}
		return customerName;
	}

	public void setCustomerName(String customerName) {
		if (CUSTOMER_NAME.equals(this.customerName, customerName)) {
			return;
		}
		CUSTOMER_NAME.putString(customerName, _byteBuffer);
		this.customerName = customerName;
	}

	public String getCustomerAddress() {
		if (customerAddress == null) {
			customerAddress = CUSTOMER_ADDRESS.getString(_byteBuffer);
		}
		return customerAddress;
	}

	public void setCustomerAddress(String customerAddress) {
		if (CUSTOMER_ADDRESS.equals(this.customerAddress, customerAddress)) {
			return;
		}
		CUSTOMER_ADDRESS.putString(customerAddress, _byteBuffer);
		this.customerAddress = customerAddress;
	}

	public int getCustomerDateOfBirth() {
		return CUSTOMER_DATE_OF_BIRTH.getInt(_byteBuffer);
	}

	public void setCustomerDateOfBirth(int customerDateOfBirth) {
		CUSTOMER_DATE_OF_BIRTH.putInt(customerDateOfBirth, _byteBuffer);
	}

	public int getCustomerBirthDay() {
		return CUSTOMER_BIRTH_DAY.getInt(_byteBuffer);
	}

	public void setCustomerBirthDay(int customerBirthDay) {
		CUSTOMER_BIRTH_DAY.putInt(customerBirthDay, _byteBuffer);
	}

	public int getCustomerBirthMonth() {
		return CUSTOMER_BIRTH_MONTH.getInt(_byteBuffer);
	}

	public void setCustomerBirthMonth(int customerBirthMonth) {
		CUSTOMER_BIRTH_MONTH.putInt(customerBirthMonth, _byteBuffer);
	}

	public int getCustomerBirthYear() {
		return CUSTOMER_BIRTH_YEAR.getInt(_byteBuffer);
	}

	public void setCustomerBirthYear(int customerBirthYear) {
		CUSTOMER_BIRTH_YEAR.putInt(customerBirthYear, _byteBuffer);
	}

	public int getCustomerCreditScore() {
		if (customerCreditScore == null) {
			customerCreditScore = new Integer(CUSTOMER_CREDIT_SCORE.getInt(_byteBuffer));
		}
		return customerCreditScore.intValue();
	}

	public void setCustomerCreditScore(int customerCreditScore) {
		if (CUSTOMER_CREDIT_SCORE.equals(this.customerCreditScore, customerCreditScore)) {
			return;
		}
		CUSTOMER_CREDIT_SCORE.putInt(customerCreditScore, _byteBuffer);
		this.customerCreditScore = new Integer(customerCreditScore);
	}

	public int getCustomerCsReviewDate() {
		return CUSTOMER_CS_REVIEW_DATE.getInt(_byteBuffer);
	}

	public void setCustomerCsReviewDate(int customerCsReviewDate) {
		CUSTOMER_CS_REVIEW_DATE.putInt(customerCsReviewDate, _byteBuffer);
	}

	public int getCustomerCsReviewDay() {
		return CUSTOMER_CS_REVIEW_DAY.getInt(_byteBuffer);
	}

	public void setCustomerCsReviewDay(int customerCsReviewDay) {
		CUSTOMER_CS_REVIEW_DAY.putInt(customerCsReviewDay, _byteBuffer);
	}

	public int getCustomerCsReviewMonth() {
		return CUSTOMER_CS_REVIEW_MONTH.getInt(_byteBuffer);
	}

	public void setCustomerCsReviewMonth(int customerCsReviewMonth) {
		CUSTOMER_CS_REVIEW_MONTH.putInt(customerCsReviewMonth, _byteBuffer);
	}

	public int getCustomerCsReviewYear() {
		return CUSTOMER_CS_REVIEW_YEAR.getInt(_byteBuffer);
	}

	public void setCustomerCsReviewYear(int customerCsReviewYear) {
		CUSTOMER_CS_REVIEW_YEAR.putInt(customerCsReviewYear, _byteBuffer);
	}

}