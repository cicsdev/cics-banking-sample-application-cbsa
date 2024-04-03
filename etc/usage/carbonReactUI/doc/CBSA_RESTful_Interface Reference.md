# CICS Banking Sample Application (CBSA) RESTful Interface Reference


## Accessing the RESTful Interface:

Access to the React UI is via HTTP requests. GET requests can be easily performed through a web browser. PUT, POST and DELETE requests will require a HTTP client or plug-in.

The exact URL will depend upon the host name and the port number chosen during installation. It
will look something like this:

[http://*your-allocated-host-name*:*your-port-number*/webui-1.0/banking/*resource*](http://your-allocated-host-name:your-port-number/webui-1.0/banking/*resource*)

#### Company Name

[http://*your-allocated-host-name*:*your-port-number*/webui-1.0/banking/companyName](http://your-allocated-host-name:your-port-number/webui-1.0/banking/companyName)

A HTTP GET for the Company Name resource will return HTTP 200 and a JSON response as follows:

{"companyName":"CICS Banking Sample Application"}

#### Sort Code

[http://*your-allocated-host-name*:*your-port-number*/webui-1.0/banking/sortCode](http://your-allocated-host-name:your-port-number/webui-1.0/banking/sortCode)

A HTTP GET for the Sort Code Name resource will return HTTP 200 and a JSON response as follows:

{"sortCode":"987654"}

#### Customer

##### GET (Read or List)

[http://*your-allocated-host-name*:*your-port-number*/webui-1.0/banking/customer](http://your-allocated-host-name:your-port-number/webui-1.0/banking/customer/*number*)

where *number* is a customer number will return that customer number if it exists. Customer 0 will return a random customer, and customer 9999999999 will return the customer with the highest number.

{"customerAddress":"12 Acacia Avenue, Oswestry","dateOfBirth":"1996-07-04","id":"2","sortCode":"987654","customerName":"Mrs Belinda G Downton"}

[http://*your-allocated-host-name*:*your-port-number*/webui-1.0/banking/customer](http://your-allocated-host-name:your-port-number/webui-1.0/banking/customer)

will return a list of all customers, in a JSON array. Depending on how many customers were created, this might be large. The default is 10,000 customers.

[http://*your-allocated-host-name*:*your-port-number*/webui-1.0/banking/customer?limit=100](http://your-allocated-host-name:your-port-number/webui-1.0/banking/customer?limit=100)

will a return a list of customers in a JSON array up to a limit of 100 customers. This number can be changed.

[http://*your-allocated-host-name*:*your-port-number*/webui-1.0/banking/customer?offset=0&limit=100](http://your-allocated-host-name:your-port-number/webui-1.0/banking/customer?offset=0&limit=100)

will a return a list of customers in a JSON array up to a limit of 100 customers starting at the first customer (0). These number can be changed. Changing the offset will show different customers.

{"customers":[{"customerAddress":"12 Acacia Avenue, Oswestry","dateOfBirth":"1996-07-04","id":"2","sortCode":"987654","customerName":"Mrs Belinda G Downton"},{"customerAddress":"34 Sycamore Rise, Winchester","dateOfBirth":"1954-11-04","id":"3","sortCode":"987654","customerName":"Dr Frederick R Corbett"}]}

##### POST (Create)

[http://*your-allocated-host-name*:*your-port-number*/webui-1.0/banking/customer](http://your-allocated-host-name:your-port-number/webui-1.0/banking/customer)

A POST to the above URL with the correct payload and "Content-Type: application/json" will create a new customer. Note that the customer number is not in the URL, nor in the payload, it is allocated by the application.

{"customerAddress":"12 Acacia Avenue, Oswestry","dateOfBirth":"1996-07-04","sortCode":"987654","customerName":"Mrs Belinda G Downton"}

If successful, a HTTP 201 Created response will be returned, and the following data containing the customer number in the "id" field.

{"customerAddress":"12 Acacia Avenue, Oswestry","dateOfBirth":"1996-07-04","id":"2","sortCode":"987654","customerName":"Mrs Belinda G Downton"}

##### PUT (Update)

[http://*your-allocated-host-name*:*your-port-number*/webui-1.0/banking/customer/*number*](http://your-allocated-host-name:your-port-number/webui-1.0/banking/customer/*number*)

And a customer payload will update the customer. Not all fields are updatable, only the name and address.

Input is the customer details, and the updated record will be returned.


##### DELETE (Delete)

[http://*your-allocated-host-name*:*your-port-number*/webui-1.0/banking/customer/*number*](http://your-allocated-host-name:your-port-number/webui-1.0/banking/customer/*number*)

No other input is required. The relevant customer record will be deleted, AS WELL AS ALL THEIR ACCOUNTS. The deleted customer record is returned as a JSON record.

There is no global delete option.

#### Account

##### GET (Read or List)

[http://*your-allocated-host-name*:*your-port-number*/webui-1.0/banking/account/*number*](http://your-allocated-host-name:your-port-number/webui-1.0/banking/account/*number*)

where *number* is an account number will return that account number if it exists. Account number 99999999 will return the account with the highest number.

{"interestRate":2.10,"lastStatementDate":"2014-07-01","nextStatementDate":"2014-08-01","dateOpened":"2004-10-12","actualBalance":108065.44,"overdraft":0,"accountType":"ISA","id":"12","customerNumber":"6","sortCode":"987654","availableBalance":108065.44}

[http://*your-allocated-host-name*:*your-port-number*/webui-1.0/banking/account](http://your-allocated-host-name:your-port-number/webui-1.0/banking/account)

will return a list of all accounts, in a JSON array. Depending on how many accounts were created, this might be large. The default is 25,040 accounts.

[http://*your-allocated-host-name*:*your-port-number*/webui-1.0/banking/account](http://your-allocated-host-name:your-port-number/webui-1.0/banking/account?limit=100)

This will return the first 100 accounts.

[http://*your-allocated-host-name*:*your-port-number*/webui-1.0/banking/account](http://your-allocated-host-name:your-port-number/webui-1.0/banking/account?limit=100&offset=0)

This will return the first 100 accounts starting at the first record (offset 0).

{"numberOfAccounts":3,"accounts":[{"interestRate":2.10,"lastStatementDate":"2014-07-01","nextStatementDate":"2014-08-01","dateOpened":"2004-10-12","actualBalance":108065.44,"overdraft":0,"accountType":"ISA","id":"12","customerNumber":"6","sortCode":"987654","availableBalance":108065.44},{"interestRate":1.75,"lastStatementDate":"2014-07-01","nextStatementDate":"2014-08-01","dateOpened":"2014-10-07","actualBalance":497576.79,"overdraft":0,"accountType":"SAVING","id":"13","customerNumber":"6","sortCode":"987654","availableBalance":497576.79},{"interestRate":0.00,"lastStatementDate":"2014-07-01","nextStatementDate":"2014-08-01","dateOpened":"1996-09-10","actualBalance":410929.96,"overdraft":100,"accountType":"CURRENT","id":"14","customerNumber":"6","sortCode":"987654","availableBalance":410929.96}]}

###### Get accounts for a customer

[http://*your-allocated-host-name*:*your-port-number*/webui-1.0/banking/account/retrieveByCustomerNumber/*number*](http://*your-allocated-host-name*:*your-port-number*/webui-1.0/banking/account/retrieveByCustomerNumber/*number*)

This will return a JSON array of all accounts belonging to the supplied number. Customers can have up to 10 accounts.

###### Get accounts above or below a specified balance

[http://*your-allocated-host-name*:*your-port-number*/webui-1.0/bbanking/account/balance?balance=10000.00&operator=<
](http://*your-allocated-host-name*:*your-port-number*/webui-1.0/bbanking/account/balance?balance=10000.00&operator=<)

[http://*your-allocated-host-name*:*your-port-number*/webui-1.0/bbanking/account/balance?balance=10000.00&operator=>
](http://*your-allocated-host-name*:*your-port-number*/webui-1.0/bbanking/account/balance?balance=10000.00&operator=>)


offset and limit are optional parameters.

##### POST (Create)

[http://*your-allocated-host-name*:*your-port-number*/webui-1.0/banking/account](http://*your-allocated-host-name*:*your-port-number*/webui-1.0/banking/account)

An HTTP POST to the above URL with a body of data as below with "Content-Type: application/json" will create a new account. Note that the account number is not in the URL, nor in the payload, it is allocated by the application. The customer number specified must exist.

{"interestRate":1.35,"lastStatementDate":"2014-07-01","nextStatementDate":"2014-08-01","dateOpened":"1973-06-18","actualBalance":0,"overdraft":100,"accountType":"CURRENT","customerNumber":"120","sortCode":"987654","availableBalance":0}

Date Opened will be set to today's date by the application. Last statement date and next statement date are also set.

A HTTP 201 response will be returned if successful.

Customers may only have ten accounts.

##### PUT (Update)

[http://*your-allocated-host-name*:*your-port-number*/webui-1.0/banking/account/*number*](http://*your-allocated-host-name*:*your-port-number*/webui-1.0/banking/account/*number*)

A PUT to the above URL with a payload such as follows will update the account. Only certain fields can be updated, the type of account, the overdraft limit and the interest rate.

{"interestRate":1.9,"lastStatementDate":"2014-07-01","nextStatementDate":"2018-08-01","dateOpened":"1973-06-18","actualBalance":71311.2,"overdraft":500,"accountType":"MORTGAGE","id":"76","customerNumber":"271","sortC
ode":"987654","availableBalance":0}

###### Credit an account

[http://*your-allocated-host-name*:*your-port-number*/webui-1.0/banking/account/credit/*number*](http://*your-allocated-host-name*:*your-port-number*/webui-1.0/banking/account/credit/*number*)

This is accessed via HTTP PUT so is not suitable for access from a browser. It is used to add money to an account. It requires the following input:

{"amount":0.40}

The updated account will be returned.


###### Debit an account

[http://*your-allocated-host-name*:*your-port-number*/webui-1.0/banking/account/debit/*number*](http://*your-allocated-host-name*:*your-port-number*/webui-1.0/banking/account/debit/*number*)

This is accessed via HTTP PUT so is not suitable for access from a browser. It is used to deduct money from an account. It requires the following input:

{"amount":0.40}

The updated account will be returned.

###### Transfer between accounts at this bank

[http://*your-allocated-host-name*:*your-port-number*/webui-1.0/banking/account/transfer/*number*](http://*your-allocated-host-name*:*your-port-number*/webui-1.0/banking/account/transfer/*number*)

This is accessed via HTTP PUT and the following payload:

{"amount":12.34,"targetAccount":122}

This will deduct "amount" from the account number in the URL and add it to the "targetAccount". Both accounts must exist.

##### DELETE (Delete)

[http://*your-allocated-host-name*:*your-port-number*/webui-1.0/banking/account/*number*](http://*your-allocated-host-name*:*your-port-number*/webui-1.0/banking/account/*number*)

A HTTP DELETE request to the above URL will delete that record if it exists. The deleted account is returned.

#### Processed Transactions

The bank keeps a record of all transactions in a processed transaction data store. This can be queried using the RESTful interface.

##### GET (List)

Listing all records is possible, with optional "offset" and "limit".

[http://*your-allocated-host-name*:*your-port-number*/webui-1.0/banking/processedTransaction/](http://*your-allocated-host-name*:*your-port-number*/webui-1.0/banking/processedTransaction/)

A JSON array is returned:

{"processedTransactions":[
{"reference":"2826","amount":0.00,"accountType":"MORTGAGE","description":"0000000001MORTGAGE2711201727122017DELETE","lastStatement":"Nov 27, 2017 12:00:00 AM","nextStatement":"Dec 27, 2017 12:00:00 AM","accountNumber":"2552","type":"IDA","sortCode":"987654","timestamp":"Nov 28, 2017 3:25:56 PM","customer":"1"},
{"reference":"2826","amount":0.00,"description":"4410010000000001Mrs Zsa G Hard1941-02-22","dateOfBirth":"Feb 22, 1941 12:00:00 AM","accountNumber":"0","type":"IDC","sortCode":"987654","customerName":"Mrs Zsa G Hard","timestamp":"Nov 28, 2017 3:25:56 PM","customer":"1"},
],"numberOfProcessedTransactionRecords":2,"success":"Y"}

Each processed transaction has a reference number (the task that caused it to be written), a type, and then various information depending on the type.
IDC and ODC are delete customer records.
IDA and ODA are delete account records.
ICC and OCC are create customer records.
ICA and OCA are create account records.
CRE and DEB are credit and debit records.
TFR is a transfer record.
