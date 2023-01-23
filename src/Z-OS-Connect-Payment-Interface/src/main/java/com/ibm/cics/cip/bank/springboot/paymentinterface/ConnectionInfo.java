/*
 *
 *    Copyright IBM Corp. 2022
 *
 *
 */
package com.ibm.cics.cip.bank.springboot.paymentinterface;

import com.beust.jcommander.Parameter;

public class ConnectionInfo
{

	static final String COPYRIGHT = "Copyright IBM Corp. 2022";


	private ConnectionInfo()
	{
		throw new IllegalStateException("Static only");
	}

	@Parameter(names =
	{ "--port", "-p" }, description = "Port to connect with")
	private static int port = 30701;

	@Parameter(names =
	{ "--address", "--url", "-a", "-u" }, description = "Address to use")
	private static String address = "localhost";


	public static String getAddressAndPort()
	{
		return address + ":" + port;
	}


	public static int getPort()
	{
		return port;
	}


	public static String getPortString()
	{
		return Integer.toString(port);
	}


	public static void setPort(int port)
	{
		ConnectionInfo.port = port;
	}


	public static String getAddress()
	{
		return address;
	}


	public static void setAddress(String address)
	{
		ConnectionInfo.address = address;
	}

}
