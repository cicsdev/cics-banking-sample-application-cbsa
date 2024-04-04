# CICS Banking Sample Application (CBSA) RESTful Interface Reference


## Accessing the RESTful Interface:

Behind the Carbon React UI is a RESTful interface which is accessed via HTTP requests. GET requests are easily performed through a web browser. PUT, POST and DELETE requests will require a HTTP client or plug-in.

The exact URL depends upon the host name and the port number chosen during installation. It
looks something like this:

[http://*your-allocated-host-name*:*your-port-number*/webui-1.0/banking/*resource*](http://your-allocated-host-name:your-port-number/webui-1.0/banking/*resource*)

#### Company Name

[http://*your-allocated-host-name*:*your-port-number*/webui-1.0/banking/companyName](http://your-allocated-host-name:your-port-number/webui-1.0/banking/companyName)

A HTTP GET for the Company Name resource returns HTTP 200 and a JSON response as follows:

{"companyName":"CICS Banking Sample Application"}

#### Sort Code

[http://*your-allocated-host-name*:*your-port-number*/webui-1.0/banking/sortCode](http://your-allocated-host-name:your-port-number/webui-1.0/banking/sortCode)

A HTTP GET for the Sort Code Name resource returns HTTP 200 and a JSON response as follows:

{"sortCode":"987654"}

#### Customer

##### GET (List or Read)

###### List

[http://*your-allocated-host-name*:*your-port-number*/webui-1.0/banking/customer](http://your-allocated-host-name:your-port-number/webui-1.0/banking/customer)

Returns details of all customers, in a JSON array. Depending on how many customers were created, this might be large. The default is 10,000 customers.

[http://*your-allocated-host-name*:*your-port-number*/webui-1.0/banking/customer](http://your-allocated-host-name:your-port-number/webui-1.0/banking/customer?countOnly=true)

Returns the total number of customers without any details.

{

	"numberOfCustomers": 10000

}



[http://*your-allocated-host-name*:*your-port-number*/webui-1.0/banking/customer?limit=*limitValue*](http://your-allocated-host-name:your-port-number/webui-1.0/banking/customer?limit=*limitValue*)

Returns details of customers in a JSON array up to a limit of *limitValue* customers. This number must be specified.

[http://*your-allocated-host-name*:*your-port-number*/webui-1.0/banking/customer?offset=*offsetValue*&limit=*limitValue*](http://your-allocated-host-name:your-port-number/webui-1.0/banking/customer?offset=*offsetValue*&limit=*limitValue*)

Returns details of customers in a JSON array up to a limit of *limitValue* customers. If *offsetValue* is zero then it starts at the first customer. Incrementing *offsetValue* with *limitValue* returns subsequent portions of the customer data.

{

	"customers":

	[

		{

			"customerAddress":"12 Acacia Avenue, Oswestry",

			"dateOfBirth":"1996-07-04",

			"id":"2",

			"sortCode":"987654",

			"customerName":"Mrs Belinda G Downton"

		},

		{

			"customerAddress":"34 Sycamore Rise, Winchester",

			"dateOfBirth":"1954-11-04",

			"id":"3",

			"sortCode":"987654",

			"customerName":"Dr Frederick R Corbett"

		}

	],

	"numberOfCustomers":2

}

###### List customers by name

This is currently unavailable.

###### List customers by town

This is currently unavailable.

###### List customers by age

This is currently unavailable.

###### Read

[http://*your-allocated-host-name*:*your-port-number*/webui-1.0/banking/customer/*number*](http://your-allocated-host-name:your-port-number/webui-1.0/banking/customer/*number*)

where *number* is a customer number. "A *number* value of 0 returns details of a random customer. A *number* value of 9999999999  returns the details of the customer with the highest customer number.

{

	"customerAddress":"12 Acacia Avenue, Oswestry",

	"dateOfBirth":"1996-07-04",

	"id":"2",

	"sortCode":"987654",

	"customerName":"Mrs Belinda G Downton"

}

##### POST (Create)

[http://*your-allocated-host-name*:*your-port-number*/webui-1.0/banking/customer](http://your-allocated-host-name:your-port-number/webui-1.0/banking/customer)

A POST to the above URL with the correct payload and "Content-Type: application/json" creates a new customer. Note that the customer number is not in the URL, nor in the payload, it is allocated by the application.

{

	"customerAddress":"12 Acacia Avenue, Oswestry",

	"dateOfBirth":"1996-07-04",

	"sortCode":"987654",

	"customerName":"Mrs Belinda G Downton"

}

If successful, a "HTTP 201 Created" response is returned, and the following data containing the customer number in the "id" field.

{

	"customerAddress":"12 Acacia Avenue, Oswestry",

	"dateOfBirth":"1996-07-04",

	"id":"2",

	"sortCode":"987654",

	"customerName":"Mrs Belinda G Downton"

}

##### PUT (Update)

[http://*your-allocated-host-name*:*your-port-number*/webui-1.0/banking/customer/*number*](http://your-allocated-host-name:your-port-number/webui-1.0/banking/customer/*number*)

A PUT to the above URL with a payload updates the customer. Not all fields are updatable, only the name and address. The customerName, customerAddress and sortCode fields are required for input.

Input is the customer details

{

"customerAddress":"12 Acacia Avenue, Oswestry",

"dateOfBirth":"1996-07-04",

"sortCode":"987654",

"customerName":"Mrs Belinda G Downton"

}

The updated record is returned with a HTTP 200 return code.

{

"customerAddress":"12 Acacia Avenue, Oswestry",

"dateOfBirth":"1996-07-04",

"sortCode":"987654",

"customerName":"Mrs Belinda G Downton",

"id":"2"

}


##### DELETE (Delete)

[http://*your-allocated-host-name*:*your-port-number*/webui-1.0/banking/customer/*number*](http://your-allocated-host-name:your-port-number/webui-1.0/banking/customer/*number*)

No other input is required. The specified record is deleted, AS WELL AS ALL THEIR ACCOUNTS. The deleted customer record is returned as a JSON record.

There is no global delete option.

#### Account

##### GET (List or Read)

###### List

[http://*your-allocated-host-name*:*your-port-number*/webui-1.0/banking/account](http://your-allocated-host-name:your-port-number/webui-1.0/banking/account)

A GET to the above URL returns a list of all accounts in a JSON array. Depending on how many accounts were created, this might be large. The default is 25,040 accounts.

[http://*your-allocated-host-name*:*your-port-number*/webui-1.0/banking/account](http://your-allocated-host-name:your-port-number/webui-1.0/banking/account?countOnly=true)

A GET to the above URL returns the number of accounts without any details.

{

  "numberOfAccounts":25040

}

[http://*your-allocated-host-name*:*your-port-number*/webui-1.0/banking/account](http://your-allocated-host-name:your-port-number/webui-1.0/banking/account?limit=*limitValue*)

This returns the first *limitValue* accounts as a JSON array.

[http://*your-allocated-host-name*:*your-port-number*/webui-1.0/banking/account](http://your-allocated-host-name:your-port-number/webui-1.0/banking/account?limit=*limitValue*&offset=*offsetValue*)

This returns *limitValue* accounts starting at record *offsetValue*. 0 is the first record. Incrementing *offsetValue* by *limitValue* returns subsequent sections of data.

{

	"numberOfAccounts":3,

	"accounts":

	[

		{

			"interestRate":2.10,

			"lastStatementDate":"2014-07-01",

			"nextStatementDate":"2014-08-01",

			"dateOpened":"2004-10-12",

			"actualBalance":108065.44,

			"overdraft":0,

			"accountType":"ISA",

			"id":"12",

			"customerNumber":"6",

			"sortCode":"987654",

			"availableBalance":108065.44

		},

		{

			"interestRate":1.75,

			"lastStatementDate":"2014-07-01",

			"nextStatementDate":"2014-08-01",

			"dateOpened":"2014-10-07",

			"actualBalance":497576.79,

			"overdraft":0,

			"accountType":"SAVING",

			"id":"13",

			"customerNumber":"6",

			"sortCode":"987654",

			"availableBalance":497576.79

		},

		{

			"interestRate":0.00,

			"lastStatementDate":"2014-07-01",

			"nextStatementDate":"2014-08-01",

			"dateOpened":"1996-09-10",

			"actualBalance":410929.96,

			"overdraft":100,

			"accountType":"CURRENT",

			"id":"14",

			"customerNumber":"6",

			"sortCode":"987654",

			"availableBalance":410929.96

		}
	]
}


###### Read

[http://*your-allocated-host-name*:*your-port-number*/webui-1.0/banking/account/*number*](http://your-allocated-host-name:your-port-number/webui-1.0/banking/account/*number*)

A GET to the above URL with *number* returns that account number if it exists. Account number 99999999 returns the account with the highest number.

{

	"interestRate":2.10,

	"lastStatementDate":"2014-07-01",

	"nextStatementDate":"2014-08-01",

	"dateOpened":"2004-10-12",

	"actualBalance":108065.44,

	"overdraft":0,

	"accountType":"ISA",

	"id":"12",

	"customerNumber":"6",

	"sortCode":"987654",

	"availableBalance":108065.44

}

###### Get accounts for a customer

[http://*your-allocated-host-name*:*your-port-number*/webui-1.0/banking/account/retrieveByCustomerNumber/*number*](http://*your-allocated-host-name*:*your-port-number*/webui-1.0/banking/account/retrieveByCustomerNumber/*number*)

A HTTP GET of the above URL returns a JSON array of all accounts belonging to customer *number* as a JSON array. Customers can have up to 10 accounts.

###### Get accounts above or below a specified balance

[http://*your-allocated-host-name*:*your-port-number*/webui-1.0/bbanking/account/balance?balance=*balance*&operator=*operator*
](http://*your-allocated-host-name*:*your-port-number*/webui-1.0/bbanking/account/balance?balance=*balance*&operator=*operator*)

A HTTP GET to the above returns a JSON array containing accounts with a that are greater than or less than (*operator* is > or <) the *balance*.

[http://*your-allocated-host-name*:*your-port-number*/webui-1.0/bbanking/account/balance?balance=*balance*&operator=*operator*&offset=*offsetValue*&limit=*limitValue*
](http://*your-allocated-host-name*:*your-port-number*/webui-1.0/bbanking/account/balance?balance=*balance*&operator=*operator*&offset=*offsetValue*&limit=*limitValue*)

A HTTP GET to the above returns a JSON array of up to *limitValue* accounts starting at offset *offsetValue* containing accounts that are greater than or less than (*operator* is > or <) the *balance*. *offsetValue* of zero is the first record. Incrementing *offsetValue* by *limitValue* returns subsequent sections of data.

##### POST (Create)

[http://*your-allocated-host-name*:*your-port-number*/webui-1.0/banking/account](http://*your-allocated-host-name*:*your-port-number*/webui-1.0/banking/account)

An HTTP POST to the above URL with a body of data as below with "Content-Type: application/json" creates a new account. Note that the account number is not in the URL, nor in the payload, it is allocated by the application. The customer number specified must exist.

{

	"interestRate":1.35,

	"lastStatementDate":"2014-07-01",

	"nextStatementDate":"2014-08-01",

	"dateOpened":"1973-06-18",

	"actualBalance":0,

	"overdraft":100,

	"accountType":"CURRENT",

	"customerNumber":"120",

	"sortCode":"987654",

	"availableBalance":0

}

Date Opened is set to today's date by the application. Last statement date and next statement date are also set.

A HTTP 201 response is returned if successful. Customers may only have ten accounts and attempting to create an eleventh account fails.

##### PUT (Update)

[http://*your-allocated-host-name*:*your-port-number*/webui-1.0/banking/account/*number*](http://*your-allocated-host-name*:*your-port-number*/webui-1.0/banking/account/*number*)

A PUT to the above URL with a payload such as follows updates the account *number*. Only certain fields can be updated, the type of account, the overdraft limit and the interest rate.

{

	"interestRate":1.9,

	"lastStatementDate":"2014-07-01",

	"nextStatementDate":"2018-08-01",

	"dateOpened":"1973-06-18",

	"actualBalance":71311.2,

	"overdraft":500,

	"accountType":"MORTGAGE",

	"id":"76",

	"customerNumber":"271",

	"sortCode":"987654",

	"availableBalance":0

}

###### Credit an account

[http://*your-allocated-host-name*:*your-port-number*/webui-1.0/banking/account/credit/*number*](http://*your-allocated-host-name*:*your-port-number*/webui-1.0/banking/account/credit/*number*)

A PUT to the above URL adds money to account *number*. It requires the following input:

{

	"amount":0.40

}

The updated account is returned.


###### Debit an account

[http://*your-allocated-host-name*:*your-port-number*/webui-1.0/banking/account/debit/*number*](http://*your-allocated-host-name*:*your-port-number*/webui-1.0/banking/account/debit/*number*)

A PUT to the above URL deducts money from account *number*. It requires the following input:

{

	"amount":0.40

}

The updated account is returned.

###### Transfer between accounts at this bank

[http://*your-allocated-host-name*:*your-port-number*/webui-1.0/banking/account/transfer/*number*](http://*your-allocated-host-name*:*your-port-number*/webui-1.0/banking/account/transfer/*number*)

A PUT to the above URL deducts money from account *number* and adds it to the account specified in the payload. It requires the following input:

{

	"amount":12.34,

	"targetAccount":122

}

Both accounts must exist.

##### DELETE (Delete)

[http://*your-allocated-host-name*:*your-port-number*/webui-1.0/banking/account/*number*](http://*your-allocated-host-name*:*your-port-number*/webui-1.0/banking/account/*number*)

A HTTP DELETE request to the above URL deletes that record if it exists. The deleted account is returned.

#### Processed Transactions

The bank keeps a record of all transactions in a processed transaction data store. This can be queried using the RESTful interface.

##### GET (List)

Listing all records is possible, with optional "offset" and "limit".

[http://*your-allocated-host-name*:*your-port-number*/webui-1.0/banking/processedTransaction](http://*your-allocated-host-name*:*your-port-number*/webui-1.0/banking/processedTransaction)

[http://*your-allocated-host-name*:*your-port-number*/webui-1.0/banking/processedTransaction?offset=*offsetValue*&limit=*limitValue*](http://*your-allocated-host-name*:*your-port-number*/webui-1.0/banking/processedTransaction?offset=*offsetValue*&limit=*limitValue*)


A JSON array is returned:

{
	"processedTransactions":
	[
		{
			"reference":"2826",

			"amount":0.00,

			"accountType":"MORTGAGE",

			"description":"0000000001MORTGAGE2711201727122017DELETE",

			"lastStatement":"Nov 27, 2017 12:00:00 AM",

			"nextStatement":"Dec 27, 2017 12:00:00 AM",

			"accountNumber":"2552",

			"type":"IDA",

			"sortCode":"987654",

			"timestamp":"Nov 28, 2017 3:25:56 PM",

			"customer":"1"

		},


		{

			"reference":"2826",

			"amount":0.00,

			"description":"4410010000000001Mrs Zsa G Hard1941-02-22",

			"dateOfBirth":"Feb 22, 1941 12:00:00 AM",

			"accountNumber":"0",

			"type":"IDC",

			"sortCode":"987654",

			"customerName":"Mrs Zsa G Hard",

			"timestamp":"Nov 28, 2017 3:25:56 PM",

			"customer":"1"

		},

	],

	"numberOfProcessedTransactionRecords":2,

	"success":"Y"
}

Each processed transaction has a reference number (the task that caused it to be written), a type, and then various information depending on the type.

  * IDC and ODC are delete customer records.

  * IDA and ODA are delete account records.

  * ICC and OCC are create customer records.

  * ICA and OCA are create account records.

  * CRE and DEB are credit and debit records.

  * TFR is a transfer record.
