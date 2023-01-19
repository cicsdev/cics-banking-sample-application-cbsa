/*
 *
 *    Copyright IBM Corp. 2022
 *
 */
package com.ibm.cics.cip.bankliberty.datainterfaces;

import com.ibm.jzos.fields.*;

// Generated by com.ibm.jzos.recordgen.cobol.RecordClassGenerator on Tue Nov 15 10:25:10 GMT 2016

public class GetCompany
{

	static final String COPYRIGHT = "Copyright IBM Corp. 2022";

	protected static CobolDatatypeFactory factory = new CobolDatatypeFactory();
	static
	{
		factory.setStringTrimDefault(false);
	}

	/**
	 * <pre>
	 01 COBOL-LANGUAGE-STRUCTURE.
	 * </pre>
	 */
	public static final int COBOL_LANGUAGE_STRUCTURE_LEN = 40;

	/**
	 * <pre>
	     03 GETCompanyOperation.
	 * </pre>
	 */
	public static final int GETCOMPANYOPERATION_LEN = 40;
	public static final int GETCOMPANYOPERATION_OFFSET = factory.getOffset();

	/**
	 * <pre>
	       06 company-name pic x(40).
	 * </pre>
	 */
	protected static StringField COMPANY_NAME = factory.getStringField(40);

	protected byte[] byteBuffer;

	public GetCompany(byte[] buffer)
	{
		this.byteBuffer = buffer;
	}

	public GetCompany()
	{
		this.byteBuffer = new byte[COBOL_LANGUAGE_STRUCTURE_LEN];
	}

	public byte[] getByteBuffer()
	{
		return byteBuffer;
	}

	public String getCompanyName()
	{
		return COMPANY_NAME.getString(byteBuffer);
	}

	public void setCompanyName(String companyName)
	{
		COMPANY_NAME.putString(companyName, byteBuffer);
	}

}
