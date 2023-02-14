/*
 *
 *    Copyright IBM Corp. 2023
 *
 *
 */

package com.ibm.cics.cip.bankliberty.datainterfaces;
import com.ibm.jzos.fields.*;

// Generated by IBM Record Generator for Java V3.0.2 Build 20221206-1121 on: Tue Feb 14 06:51:17 CST 2023

public class CRECUST {
	protected static CobolDatatypeFactory factory = new CobolDatatypeFactory();
	static { factory.setStringTrimDefault(false); } 

	/** <pre>
	 LINKAGE SECTION. </pre> */
	public static final int DFHCOMMAREA_len = 261; 

	/** <pre>
	 01 DFHCOMMAREA.
	     COPY CRECUST.
    *
    *
    *    Copyright IBM Corp. 2023
    *
    *
    *
	    03 COMM-EYECATCHER                 PIC X(4). </pre> */
	protected static final StringField COMM_EYECATCHER = factory.getStringField(4);

	/** <pre>
	******************************************************************
	    03 COMM-EYECATCHER                 PIC X(4). </pre> */
	public static final int COMM_KEY_len = 16; 
	public static final int COMM_KEY_offset = factory.getOffset();

	/** <pre>
	    03 COMM-EYECATCHER                 PIC X(4). </pre> */
	protected static final ExternalDecimalAsIntField COMM_SORTCODE = factory.getExternalDecimalAsIntField(6, false, false, false, false);

	/** <pre>
	    03 COMM-KEY. </pre> */
	protected static final ExternalDecimalAsLongField COMM_NUMBER = factory.getExternalDecimalAsLongField(10, false, false, false, false);

	/** <pre>
	       05 COMM-SORTCODE                PIC 9(6) DISPLAY. </pre> */
	protected static final StringField COMM_NAME = factory.getStringField(60);

	/** <pre>
	       05 COMM-NUMBER                  PIC 9(10) DISPLAY. </pre> */
	protected static final StringField COMM_ADDRESS = factory.getStringField(160);

	/** <pre>
	    03 COMM-NAME                       PIC X(60). </pre> */
	protected static final ExternalDecimalAsIntField COMM_DATE_OF_BIRTH = factory.getExternalDecimalAsIntField(8, false, false, false, false);

	/** <pre>
	    03 COMM-ADDRESS                    PIC X(160). </pre> */
	static { factory.pushOffset(); } 
	static { factory.setOffset(COMM_DATE_OF_BIRTH.getOffset()); } 
	public static final int COMM_DOB_GROUP_len = 8; 
	public static final int COMM_DOB_GROUP_offset = factory.getOffset();

	/** <pre>
	    03 COMM-DATE-OF-BIRTH              PIC 9(8). </pre> */
	protected static final ExternalDecimalAsIntField COMM_BIRTH_DAY = factory.getExternalDecimalAsIntField(2, false, false, false, false);

	/** <pre>
	    03 COMM-DOB-GROUP REDEFINES COMM-DATE-OF-BIRTH. </pre> */
	protected static final ExternalDecimalAsIntField COMM_BIRTH_MONTH = factory.getExternalDecimalAsIntField(2, false, false, false, false);

	/** <pre>
	       05 COMM-BIRTH-DAY               PIC 99. </pre> */
	protected static final ExternalDecimalAsIntField COMM_BIRTH_YEAR = factory.getExternalDecimalAsIntField(4, false, false, false, false);
	static { factory.popOffset(); } 

	/** <pre>
	       05 COMM-BIRTH-MONTH             PIC 99. </pre> */
	protected static final ExternalDecimalAsIntField COMM_CREDIT_SCORE = factory.getExternalDecimalAsIntField(3, false, false, false, false);

	/** <pre>
	       05 COMM-BIRTH-YEAR              PIC 9999. </pre> */
	protected static final ExternalDecimalAsIntField COMM_CS_REVIEW_DATE = factory.getExternalDecimalAsIntField(8, false, false, false, false);

	/** <pre>
	    03 COMM-CREDIT-SCORE               PIC 999. </pre> */
	static { factory.pushOffset(); } 
	static { factory.setOffset(COMM_CS_REVIEW_DATE.getOffset()); } 
	public static final int COMM_CS_REVIEW_DATE_GROUP_len = 8; 
	public static final int COMM_CS_REVIEW_DATE_GROUP_offset = factory.getOffset();

	/** <pre>
	    03 COMM-CS-REVIEW-DATE             PIC 9(8). </pre> */
	protected static final ExternalDecimalAsIntField COMM_CS_REVIEW_DD = factory.getExternalDecimalAsIntField(2, false, false, false, false);

	/** <pre>
	    03 COMM-CS-REVIEW-DATE-GROUP REDEFINES COMM-CS-REVIEW-DATE. </pre> */
	protected static final ExternalDecimalAsIntField COMM_CS_REVIEW_MM = factory.getExternalDecimalAsIntField(2, false, false, false, false);

	/** <pre>
	       05 COMM-CS-REVIEW-DD            PIC 99. </pre> */
	protected static final ExternalDecimalAsIntField COMM_CS_REVIEW_YYYY = factory.getExternalDecimalAsIntField(4, false, false, false, false);
	static { factory.popOffset(); } 

	/** <pre>
	       05 COMM-CS-REVIEW-MM            PIC 99. </pre> */
	protected static final StringField COMM_SUCCESS = factory.getStringField(1);

	/** <pre>
	       05 COMM-CS-REVIEW-YYYY          PIC 9999. </pre> */
	protected static final StringField COMM_FAIL_CODE = factory.getStringField(1);

	protected byte[] _byteBuffer;
	// Instance variables used to cache field values
	protected String commEyecatcher;
	protected int commSortcode;
	protected boolean commSortcodeIsSet;
	protected long commNumber;
	protected boolean commNumberIsSet;
	protected String commName;
	protected String commAddress;
	protected int commCreditScore;
	protected boolean commCreditScoreIsSet;
	protected String commSuccess;
	protected String commFailCode;


	public CRECUST (byte[] buffer) {
		this._byteBuffer = buffer;
	}

	public CRECUST () {
		this._byteBuffer = new byte[DFHCOMMAREA_len];
	}

	public byte[] getByteBuffer() {
		return _byteBuffer;
	}


	public String getCommEyecatcher() {
		if (commEyecatcher == null) {
			commEyecatcher = COMM_EYECATCHER.getString(_byteBuffer);
		}
		return commEyecatcher;
	}

	public void setCommEyecatcher(String commEyecatcher) {
		if (COMM_EYECATCHER.equals(this.commEyecatcher, commEyecatcher)) {
			return;
		}
		COMM_EYECATCHER.putString(commEyecatcher, _byteBuffer);
		this.commEyecatcher = commEyecatcher;
	}

	public int getCommSortcode() {
		if (!commSortcodeIsSet) {
			commSortcode = COMM_SORTCODE.getInt(_byteBuffer);
			commSortcodeIsSet = true;
		}
		return commSortcode;
	}

	public void setCommSortcode(int commSortcode) {
		if (commSortcodeIsSet && COMM_SORTCODE.equals(this.commSortcode, commSortcode)) {
			return;
		}
		COMM_SORTCODE.putInt(commSortcode, _byteBuffer);
		this.commSortcode = commSortcode;
		commSortcodeIsSet = true;
	}

	public long getCommNumber() {
		if (!commNumberIsSet) {
			commNumber = COMM_NUMBER.getLong(_byteBuffer);
			commNumberIsSet = true;
		}
		return commNumber;
	}

	public void setCommNumber(long commNumber) {
		if (commNumberIsSet && COMM_NUMBER.equals(this.commNumber, commNumber)) {
			return;
		}
		COMM_NUMBER.putLong(commNumber, _byteBuffer);
		this.commNumber = commNumber;
		commNumberIsSet = true;
	}

	public String getCommName() {
		if (commName == null) {
			commName = COMM_NAME.getString(_byteBuffer);
		}
		return commName;
	}

	public void setCommName(String commName) {
		if (COMM_NAME.equals(this.commName, commName)) {
			return;
		}
		COMM_NAME.putString(commName, _byteBuffer);
		this.commName = commName;
	}

	public String getCommAddress() {
		if (commAddress == null) {
			commAddress = COMM_ADDRESS.getString(_byteBuffer);
		}
		return commAddress;
	}

	public void setCommAddress(String commAddress) {
		if (COMM_ADDRESS.equals(this.commAddress, commAddress)) {
			return;
		}
		COMM_ADDRESS.putString(commAddress, _byteBuffer);
		this.commAddress = commAddress;
	}

	public int getCommDateOfBirth() {
		return COMM_DATE_OF_BIRTH.getInt(_byteBuffer);
	}

	public void setCommDateOfBirth(int commDateOfBirth) {
		COMM_DATE_OF_BIRTH.putInt(commDateOfBirth, _byteBuffer);
	}

	public int getCommBirthDay() {
		return COMM_BIRTH_DAY.getInt(_byteBuffer);
	}

	public void setCommBirthDay(int commBirthDay) {
		COMM_BIRTH_DAY.putInt(commBirthDay, _byteBuffer);
	}

	public int getCommBirthMonth() {
		return COMM_BIRTH_MONTH.getInt(_byteBuffer);
	}

	public void setCommBirthMonth(int commBirthMonth) {
		COMM_BIRTH_MONTH.putInt(commBirthMonth, _byteBuffer);
	}

	public int getCommBirthYear() {
		return COMM_BIRTH_YEAR.getInt(_byteBuffer);
	}

	public void setCommBirthYear(int commBirthYear) {
		COMM_BIRTH_YEAR.putInt(commBirthYear, _byteBuffer);
	}

	public int getCommCreditScore() {
		if (!commCreditScoreIsSet) {
			commCreditScore = COMM_CREDIT_SCORE.getInt(_byteBuffer);
			commCreditScoreIsSet = true;
		}
		return commCreditScore;
	}

	public void setCommCreditScore(int commCreditScore) {
		if (commCreditScoreIsSet && COMM_CREDIT_SCORE.equals(this.commCreditScore, commCreditScore)) {
			return;
		}
		COMM_CREDIT_SCORE.putInt(commCreditScore, _byteBuffer);
		this.commCreditScore = commCreditScore;
		commCreditScoreIsSet = true;
	}

	public int getCommCsReviewDate() {
		return COMM_CS_REVIEW_DATE.getInt(_byteBuffer);
	}

	public void setCommCsReviewDate(int commCsReviewDate) {
		COMM_CS_REVIEW_DATE.putInt(commCsReviewDate, _byteBuffer);
	}

	public int getCommCsReviewDd() {
		return COMM_CS_REVIEW_DD.getInt(_byteBuffer);
	}

	public void setCommCsReviewDd(int commCsReviewDd) {
		COMM_CS_REVIEW_DD.putInt(commCsReviewDd, _byteBuffer);
	}

	public int getCommCsReviewMm() {
		return COMM_CS_REVIEW_MM.getInt(_byteBuffer);
	}

	public void setCommCsReviewMm(int commCsReviewMm) {
		COMM_CS_REVIEW_MM.putInt(commCsReviewMm, _byteBuffer);
	}

	public int getCommCsReviewYyyy() {
		return COMM_CS_REVIEW_YYYY.getInt(_byteBuffer);
	}

	public void setCommCsReviewYyyy(int commCsReviewYyyy) {
		COMM_CS_REVIEW_YYYY.putInt(commCsReviewYyyy, _byteBuffer);
	}

	public String getCommSuccess() {
		if (commSuccess == null) {
			commSuccess = COMM_SUCCESS.getString(_byteBuffer);
		}
		return commSuccess;
	}

	public void setCommSuccess(String commSuccess) {
		if (COMM_SUCCESS.equals(this.commSuccess, commSuccess)) {
			return;
		}
		COMM_SUCCESS.putString(commSuccess, _byteBuffer);
		this.commSuccess = commSuccess;
	}

	public String getCommFailCode() {
		if (commFailCode == null) {
			commFailCode = COMM_FAIL_CODE.getString(_byteBuffer);
		}
		return commFailCode;
	}

	public void setCommFailCode(String commFailCode) {
		if (COMM_FAIL_CODE.equals(this.commFailCode, commFailCode)) {
			return;
		}
		COMM_FAIL_CODE.putString(commFailCode, _byteBuffer);
		this.commFailCode = commFailCode;
	}

}
