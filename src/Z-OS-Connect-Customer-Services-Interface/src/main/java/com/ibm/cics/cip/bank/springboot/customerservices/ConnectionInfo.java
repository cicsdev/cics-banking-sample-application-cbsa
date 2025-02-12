/*                                                                        */
/* Copyright IBM Corp. 2023                                               */
/*                                                                        */
package com.ibm.cics.cip.bank.springboot.customerservices;

import com.beust.jcommander.Parameter;

public class ConnectionInfo
{



	@Parameter(names =
	{ "--scheme", "-s" }, description = "Scheme/protocol to connect with")
	private static String scheme = "http";

	@Parameter(names =
	{ "--port", "-p" }, description = "Port to connect with")
	private static int port = 38016;

	@Parameter(names =
	{ "--address", "--url", "-a", "-u" }, description = "Address to use")
	private static String address = "winmvsb0.hursley.ibm.com";


	private ConnectionInfo()
	{
		throw new IllegalStateException("Static only");
	}


	public static String getAddressAndPort()
	{
		return scheme + "://" + address + ":" + port;
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

	public static String getScheme()
	{
		return scheme;
	}


	public static void setScheme(String scheme)
	{
		ConnectionInfo.scheme = scheme;
	}

}
