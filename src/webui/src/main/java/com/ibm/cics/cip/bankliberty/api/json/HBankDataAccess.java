/*
 *
 *    Copyright IBM Corp. 2022
 *
 */
package com.ibm.cics.cip.bankliberty.api.json;

import java.io.IOException;
import java.sql.Connection;
import java.sql.SQLException;
import java.util.HashMap;
import java.util.logging.Level;
import java.util.logging.LogManager;
import java.util.logging.Logger;

import javax.naming.Context;
import javax.naming.InitialContext;
import javax.naming.NamingException;

import javax.sql.DataSource;

import com.ibm.cics.server.InvalidRequestException;
import com.ibm.cics.server.RolledBackException;
import com.ibm.cics.server.Task;

/**
 * This class is used to hold a HashTable which in turn holds DB2 Connections
 * 
 */

public class HBankDataAccess
{

	static final String COPYRIGHT = "Copyright IBM Corp. 2022";

	protected Connection conn = null;

	static int connectionCount = 0;

	private static final String DB2CONN = "DB2CONN";

	@SuppressWarnings("rawtypes")
	static HashMap cornedBeef = null;

	private static Logger logger = Logger
			.getLogger("com.ibm.cics.cip.bankliberty.api.json");


	public HBankDataAccess()
	{
		sortOutLogging();
		// If the hashmap 'cornedBeef' does not exist then create a new
		// hashtable
		if (cornedBeef == null)
		{
			logger.log(Level.FINE,
					() -> "HBankDataAccess creating new hashtable");
			HBankDataAccess.createHashMap();
		}
	}


	private static void createHashMap()
	{
		cornedBeef = new HashMap<>();

	}


	protected void openConnection()
	{
		// Open a connection to the DB2 database
		logger.entering(this.getClass().getName(), "openConnection()");

		Integer taskNumberInteger = Task.getTask().getTaskNumber();
		String db2ConnString = DB2CONN.concat(taskNumberInteger.toString());
		logger.log(Level.FINE,
				() -> "Attempting to get DB2CONN for task number "
						+ taskNumberInteger.toString());
		this.conn = (Connection) cornedBeef.get(db2ConnString);
		if (this.conn == null)
		{
			HBankDataAccess.incrementConnCount();
			logger.log(Level.FINE,
					() -> "Attempting to create DB2CONN for task number "
							+ taskNumberInteger.toString());
			// Attempt to open a connection
			openConnectionInternal();
			logger.log(Level.FINE,
					() -> "Creation succcessful for DB2CONN for task number "
							+ taskNumberInteger.toString());
		}
		else
		{
			logger.log(Level.FINE, () -> "Reusing DB2CONN for task number "
					+ taskNumberInteger.toString());

			try
			{
				// If the connection is closed, try to reopen the connection
				if (this.conn.isClosed())
				{
					logger.warning(
							"DB2 connection was closed, attempting to reopen");
					openConnectionInternal();
				}
			}
			catch (SQLException e)
			{
				logger.severe(e.getLocalizedMessage());
			}
		}

		logger.exiting(this.getClass().getName(), "openConnection()");
	}


	public void terminate()
	{
		// Close the connection
		closeConnection();
	}


	public void closeConnection()
	{
		// Close the connection to the DB2 database
		logger.entering(this.getClass().getName(), "closeConnection()");

		HBankDataAccess.decrementConnCount();

		Integer taskNumberInterger = Task.getTask().getTaskNumber();
		String db2ConnString = DB2CONN.concat(taskNumberInterger.toString());
		this.conn = (Connection) cornedBeef.get(db2ConnString);
		// If there is an open connection
		if (this.conn != null)
		{
			logger.log(Level.FINE, () -> "We have a DB2 connection to close");
			try
			{
				logger.log(Level.FINE, () -> "Syncpointing");
				Task.getTask().commit();
				// Close the connection
				this.conn.close();
				cornedBeef.remove(db2ConnString);
			}
			catch (SQLException e)
			{
				logger.severe(
						"SQLException in com.ibm.cics.cip.bankliberty.web.db2.Customer "
								+ e.getErrorCode() + "," + e.getSQLState() + ","
								+ e.getMessage());
			}
			catch (InvalidRequestException | RolledBackException e)
			{
				logger.severe(e.getLocalizedMessage());
			}
		}
		logger.exiting(this.getClass().getName(), "closeConnection()");
	}


	@SuppressWarnings("unchecked")
	void openConnectionInternal()
	{
		logger.entering(this.getClass().getName(), "openConnectionInternal");
		String jndiString = "jdbc/defaultCICSDataSource";
		Context ctx;

		try
		{
			ctx = new InitialContext();
			DataSource ds = (DataSource) ctx.lookup(jndiString);
			logger.log(Level.FINE, () -> "jndi string is " + jndiString);
			// If there is no current connection
			if (this.conn == null)
			{
				logger.log(Level.FINE,
						() -> "About to attempt to get DB2 connection");
				// Try and get a connection
				this.conn = ds.getConnection();
				this.conn.setTransactionIsolation(
						Connection.TRANSACTION_READ_UNCOMMITTED);
				Integer taskNumberInterger = Task.getTask().getTaskNumber();
				String db2ConnString = DB2CONN
						.concat(taskNumberInterger.toString());
				cornedBeef.put(db2ConnString, this.conn);
				HBankDataAccess.incrementConnCount();

			}
			else
			{
				// If the connection is closed, open a new connection
				if (this.conn.isClosed())
				{
					logger.log(Level.FINE,
							() -> "DB2 connection was closed, getting a new one");
					this.conn = ds.getConnection();
				}
			}
		}
		catch (NamingException | SQLException e)
		{
			logger.severe(e.getMessage());
			Task.getTask().abend("HDB2");
		}
	}


	protected void sortOutLogging()
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


	private static void incrementConnCount()
	{
		HBankDataAccess.connectionCount++;
	}


	private static void decrementConnCount()
	{
		HBankDataAccess.connectionCount--;
	}
}
