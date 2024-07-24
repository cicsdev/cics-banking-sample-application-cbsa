# The Customer Services Interface User Guide

### 

### Introduction:

The Customer Services interface is a Spring Boot application, which can
drive back-end banking data, via a series of RESTful API calls, made to
a zOS Connect server, which in turn, routes the requests to the CICS
region. Please refer to the GitHub repo:

cicsdev/cics-banking-sample-application-cbsa/etc/usage/sprintBoot/doc

for the CBSA RESTful API guide.

### The business justification/usage scenario for the Customer Services interface:

The CBSA Customer Services interface will be used, in branch, to reduce
people's queueing time. Staff will be deployed as "queue busters" at peak
times. They will be armed with a tablet, and they will walk the queue
and perform some of the functions of the Teller. The bank doesn't want
the queue busters to become a target for robbery, so the tasks that they
can perform (via the Customer Services interface) are more
administrative in nature. For handling money (paying it in, taking it
out, transferring it or dealing with cheques) the customer will still
need to see a CBSA Bank Teller who will utilise the BMS or Liberty
interface to perform those functions.

Further information about the Teller's interfaces can be found in the
BMS User Guide in the GitHub repo:

> cicsdev/cics-banking-sample-application-cbsa/etc/usage/base/doc

and the Liberty UI User Guide:

> cicsdev/cics-banking-sample-application-cbsa/etc/usage/libertyUI/doc

Queue busters can attend to things like:

-   Account or customer enquiries

-   Updating customer details (e.g. changes of address, or name changes)

-   The creating of new customers

-   Deleting a customer (when a customer leaves or dies)

-   Updating account details (increasing overdraft limits etc.)

-   The opening of new accounts

-   Account deletion (if the customer wants to close any of their
    accounts)

The RESTful API can be utilised directly without using the Customer
Services interface and could, if required, be integrated with other
applications. The Customer Services interface has been provided, along
with the source code etc. to allow the RESTful API to be used straight
from the box.

The functionality in this interface, mimics most functions provided in
the BMS and Liberty UI interfaces (as used by the Teller).

### Accessing the Customer Services Interface:

It can be accessed via a URL structured as follows:\
\
[http://your-host-name:your-host-port-number/customerservices-1.0/](http://your-host-name:your-host-port-number/customerservices-1.0/)

(\* as part of the installation process, you will have previously assigned your own hostname and port number for the Customer Services interface to utilise).

### The Customer Services landing page:

![landing page](../doc/images/CustomerServiceUserGuide/Landing_Page.jpg)

The landing page is the start point for accessing all of the Customer
Service functionality. To initiate each function simply click on its
associated blue function button.

### *Create a Customer:*

Accounts may only exist if they belong to an associated customer. To
create a new customer, click on the "Create customer" button on the
landing page.

![landing page](../doc/images/CustomerServiceUserGuide/Landing_Page.jpg)

![Create customer start](../doc/images/CustomerServiceUserGuide/Create_Customer_START.jpg)

Next, provide a Customer Name (which is comprised of a title, first
name, middle initial, and surname), and the Customer's Address and their
Date of Birth (in DD/MM/YYYY format).

![Create customer entry](../doc/images/CustomerServiceUserGuide/Create_Customer_ENTRY.jpg)

Once you are happy, click on the "Submit" button.

Under the covers, validation will be carried out. Should something fail
a validation check, you will be prompted to correct any invalid data and
resubmit.\
\
\
Once the details have been validated, a RESTful API call gets executed
which initiates a call to one of CBSA's existing back-end programs, to
create the customer information. Assuming that the call was successful,
confirmation will be returned:

![create customer success](../doc/images/CustomerServiceUserGuide/Create_Customer_SUCCESS.jpg)


(If for some reason a call was unsuccessful, a suitable error reply will
be returned).

Upon completion of creating a new customer, simply press "Home" to
return to the landing page.

### *Customer Enquiry:*

To see customer details click the "View customer details" button on the
landing page:

![Landing page](../doc/images/CustomerServiceUserGuide/Landing_Page.jpg)

![customer enquiry start](../doc/images/CustomerServiceUserGuide/Customer_Enquiry_START.jpg)


Enter a customer number:

![customer enquiry entry](../doc/images/CustomerServiceUserGuide/Customer_Enquiry_ENTRY.jpg)

and click "Submit":

![customer enquiry success](../doc/images/CustomerServiceUserGuide/Customer_Enquiry_SUCCESS.jpg)

And the Customer details will be returned (see above).

### *Update a Customer:*

To amend customer details click the "Update customer details" button on
the landing page:

![landing page](../doc/images/CustomerServiceUserGuide/Landing_Page.jpg)

![update customer start](../doc/images/CustomerServiceUserGuide/Update_Customer_START.jpg)


Supply the customer number, and an amended customer name (to perform a
name change):

![update customer entry1](../doc/images/CustomerServiceUserGuide/Update_Customer_ENTRY1.jpg)


And press "Submit"

![update customer success1](../doc/images/CustomerServiceUserGuide/Update_Customer_SUCCESS1.jpg)

Or another valid combination might be to amend the customer's address.
Again, supply the customer number and the amended address details:

![update customer entry2](../doc/images/CustomerServiceUserGuide/Update_Customer_ENTRY2.jpg)


And click Submit:

![update customer success2](../doc/images/CustomerServiceUserGuide/Update_Customer_SUCCESS2.jpg)


Or the name and address can be updated at the same time, by providing
the customer number, and an amended name and an amended address all
together:

![update customer entry3](../doc/images/CustomerServiceUserGuide/Update_Customer_ENTRY3.jpg)


And click Submit:

![update customer success3](../doc/images/CustomerServiceUserGuide/Update_Customer_SUCCESS3.jpg)


The change/changes should be acknowledged (as shown above).

Clicking "Home" returns back to the landing page.

### *List Accounts belonging to a Customer:*

To display the accounts for a particular customer, click on "List
accounts belonging to customer":

![landing page](../doc/images/CustomerServiceUserGuide/Landing_Page.jpg)


![list accounts for customer start](../doc/images/CustomerServiceUserGuide/List_Accounts_for_Customer_START.jpg)


Supply a Customer number:

![list accounts for customer entry](../doc/images/CustomerServiceUserGuide/List_Accounts_for_Customer_ENTRY.jpg)


And click "Submit":

![list accounts for customer success](../doc/images/CustomerServiceUserGuide/List_Accounts_for_Customer_SUCCESS.jpg)


And the associated accounts will be returned. In this example, Customer
2345 has two accounts (7012 and 7013) associated with it .

Click "Home" to return to the landing page.

### *Customer removal/deletion:*

To remove a customer, click on "Delete customer" from the landing page:

![landing page](../doc/images/CustomerServiceUserGuide/Landing_Page.jpg)


![delete customer start](../doc/images/CustomerServiceUserGuide/Delete_Customer_START.jpg)


Supply the customer number:

![delete customer entry](../doc/images/CustomerServiceUserGuide/Delete_Customer_ENTRY.jpg)


And then click "Submit":

![delete customer success](../doc/images/CustomerServiceUserGuide/Delete_Customer_SUCCESS.jpg)


If the request to delete the customer was successful, you should get a
confirmation message (shown above). **NOTE** Deleting a customer also
removes all associated accounts for that customer.

Click "Home" to return to the landing page.

### *Account creation:*

To create a new account for a Customer, click on "Create account":

![landing page](../doc/images/CustomerServiceUserGuide/Landing_Page.jpg)


![create account start](../doc/images/CustomerServiceUserGuide/Create_Account_START.jpg)

Supply the customer number, and select a type of account from the radio
buttons provided, and set the Overdraft limit and Interest rate (if
applicable):

![create account entry](../doc/images/CustomerServiceUserGuide/Create_Account_ENTRY.jpg)

Then click Submit:

![create account success](../doc/images/CustomerServiceUserGuide/Create_Account_SUCCESS.jpg)

Associated details belonging to the new account get returned upon
successful completion (see above).

To return to the landing page click "Home".

### *Account enquiries:*

To enquire on an account, click on "View account details" from the
landing page:

![landing page](../doc/images/CustomerServiceUserGuide/Landing_Page.jpg)

![account enquiry start](../doc/images/CustomerServiceUserGuide/Account_Enquiry_START.jpg)


Supply an account number:

![account enquiry entry](../doc/images/CustomerServiceUserGuide/Account_Enquiry_ENTRY.jpg)


Then click "Submit":

![account enquiry success](../doc/images/CustomerServiceUserGuide/Account_Enquiry_SUCCESS.jpg)

The account information data is returned (as shown above).

To return to the landing page click "Home".

### *Update Account information:*

To amend account information, click on "Update account details" from the
landing page:

![landing page](../doc/images/CustomerServiceUserGuide/Landing_Page.jpg)


![update account start](../doc/images/CustomerServiceUserGuide/Update_Account_START.jpg)

Supply the account number that you wish to update, then you have the
option to amend the account type (for example to turn a loan account
into a mortgage account), along with updating the interest rate and
overdraft limit. You have the choice of changing the account type,
interest rate or overdraft limit individually or you can change any
combination together:

![update account entry](../doc/images/CustomerServiceUserGuide/Update_Account_ENTRY.jpg)


(in this example we change the account type and interest rate)

Click "Submit":

![update account success](../doc/images/CustomerServiceUserGuide/Update_Account_SUCCESS.jpg)

Confirmation of the change is provided upon successful completion.

To return to the landing page, click "Home".

### *Account Deletion:*

To remove an account click on "Delete account" from the landing page:

![landing page](../doc/images/CustomerServiceUserGuide/Landing_Page.jpg)


![delete account start](../doc/images/CustomerServiceUserGuide/Delete_Account_START.jpg)


Provide an account number:

![delete account entry](../doc/images/CustomerServiceUserGuide/Delete_Account_ENTRY.jpg)

And click "Submit":

![delete account success](../doc/images/CustomerServiceUserGuide/Delete_Account_SUCCESS.jpg)


Acknowledgement that the deletion has been successful is provided (see
above).

To return to the landing page click "Home".
