# CBSA base/COBOL Installation Instructions

##

## Introduction:

There are multiple parts to the CICS Bank Sample Application (CBSA).
There is the base/COBOL installation - which needs to be installed as a
minimum. Then there are additional, optional, installation instructions
for the Liberty UI and the Payment and Customer Services interfaces.

This document is the base/COBOL installation. It assumes that the
installer already has:

-   a CICS region (running CICS TS 6.1 or greater)

-   a Db2 subsystem (v12 or greater)

-   a z/OS Connect server.

These instructions detail the steps required to:

1.  Download the base JCL, source code, and maps.

2.  Compile/assemble, bind and link edit the source code to create the
    base load modules.

3.  Create all of the CICS CSD file definitions (to define all of the
    transactions, programs, and CICS resources for CBSA).

4.  Define all of the Db2 artefacts.

5.  Create and populate all of the underlying VSAM files and also
    populate the Db2 tables with data.

6.  Make the necessary changes to your CICS region start up JCL and zOS
    Connect server.xml file.


At the end, your CICS region should be capable of successfully executing
CBSA transactions.

##

## Setting up libraries on the host machine and downloading data into them from GitHub:

The CBSA GitHub repo can be found at:

> <https://github.com/cicsdev/cics-banking-sample-application-cbsa>

**Assumptions:**

There are some assumptions within this document, these are:

> a. That a CICS region and the underlying libraries for that region
> are already installed in the host environment.
>
> b. That a Db2 subsystem is available in the host environment. The Db2
> subsystem used, in this document, is called **DBCG**.
>
> c. If the restful API or the Payment and/or the Customer Services
> interfaces are required, it is assumed that a zOS Connect Server is
> already installed and available. For the sake of illustration, the one
> we use is installed in USS at:
>
> >/var/zosconnect/v3r0/servers/defaultServer/resources/zosconnect
>
> (\* the zOS Connect *server.xml* file for your own installation may
> be installed in a different location in USS and the instructions
> should be applied accordingly).

**NOTE 1** - The installation illustrated in this document replaces the
'default' zOS Connect **server.xml** with one which utilises the port
numbers ***30701*** (HTTP) and ***30702*** (HTTPS) for the CBSA restful
API. Should these port numbers not be appropriate in your host
environment then they may be changed in the **server.xml** member after
downloading it from the repo folder:

> cicsdev/cics-banking-sample-application-cbsa/etc/install/base/zosconnectserver

(see [Installation instructions](#installation-instructions) below for
more information).

**NOTE 2** - The last line in **server.xml** references the connection
between the z/OS Connect server and the CICS system and it
utilises port ***30709***:

>\<zosconnect_cicsIpicConnection id=\"cicsConn\"
host=\"[localhost]{.underline}\" port=\"30709\" /\>

If the 30709 port number needs to be changed, edit the **server.xml**
file and then edit the CSD definition for TCPIP Service definition
(ZOSEE), as shown below, which is provided in member BANK, after
downloading it from the repo folder:

> cicsdev/cics-banking-sample-application-cbsa/etc/install/base/installjcl

>![TCPIP def](../doc/images/Baseinstall/Baseinstall_TCPIPS_DEF.jpg)

>(see [Installation instructions](#installation-instructions) below for
more information)

##

### Installation instructions:

1. Create a PDSE on you host machine. In our example, we called it **CBSA.JCL.INSTALL** (this
should be capable of holding JCL members). It must be Fixed Block and with a record length of 80. Here is the dataset
information that we used:

>![PDSE](../doc/images/Baseinstall/Baseinstall_PDSE.jpg)

>Then copy all members from the /cicsdev/cics-banking-sample-application-cbsa/etc/install/base/installjcl folder in
the repo, into the PDSE **CBSA.JCL.INSTALL**

>This PDSE contains a set of jobs, which are used to create the necessary
libraries and perform necessary set up.

2. On the host machine, edit job CBSA.JCL.INSTALL(**CRELIBS**).

Change the line // JCLLIB ORDER=**CBSA.JCL.INSTALL** to refer to the dataset you just created.

Change the line // SET HLQ=**CBSA** to reflect the name of the dataset we have just created, but without "JCL.INSTALL". This will be the prefix for all the created datasets.

Then submit the job.

>This job creates the following additional empty PDSE/LIBRARYs on the
>host machine (which are all required later on):
>
>| PDSE/Library              | Description                                |
>| ------------              | -----------                                |
>| CBSA.DB2.JCL.INSTALL      | Will contain JCL members to set up DB2.    |
>| CBSA.CICSBSA.BUILDJCL     | Will contain the jobs (JCL) to compile all of the source code in CBSA. |
>| CBSA.CICSBSA.LOADLIB      | Will contain the LOAD modules which will be used by the CICS region running CBSA (these modules are created later during compilation).|
>| CBSA.CICSBSA.DBRM         | Will contain the Db2 DBRM modules (which are generated by the bind).|
>| CBSA.CICSBSA.LKED         | Will contain the Link Edit source entries.|
>| CBSA.CICSBSA.BMS          | Will contain the BMS (map) source code modules.|
>| CBSA.CICSBSA.ASM          | Will contain Assembler source modules required to assemble DFHNCOPT.|
>| CBSA.CICSBSA.CBSAMOD      | Will contain the output from the COBOL compilation.|
>| CBSA.CICSBSA.COBOL        | Will contain the COBOL source code.|
>| CBSA.CICSBSA.DSECT        | Will contain the copylib that is used by the COBOL programs and installation compile jobs (this folder contains Mapset DSECTs too).|

3. Once the CBSA.JCL.INSTALL(**CRELIBS**) job has completed and has created
the above libraries, it will be necessary to copy the content from each
of the following GitHub folders into each respective host library.

>| GitHub folder content                                                         | Host LIBRARY                                |
>| ---------------------                                                         | ------------                                |
>| /cicsdev/cics-banking-sample-application-cbsa/etc/install/base/db2jcl         | **CBSA.DB2.JCL.INSTALL**                    |
>| /cicsdev/cics-banking-sample-application-cbsa/etc/install/base/buildjcl       | **CBSA.CICSBSA.BUILDJCL**                   |
>| /cicsdev/cics-banking-sample-application-cbsa/etc/install/base/linkeditjcl    | **CBSA.CICSBSA.LKED**                       |
>| /cicsdev/cics-banking-sample-application-cbsa/src/base/bms_src                | **CBSA.CICSBSA.BMS**                        |
>| /cicsdev/cics-banking-sample-application-cbsa/src/base/asm_src                | **CBSA.CICSBSA.ASM**                        |
>| /cicsdev/cics-banking-sample-application-cbsa/src/base/cobol_src              | **CBSA.CICSBSA.COBOL**                      |
>| /cicsdev/cics-banking-sample-application-cbsa/src/base/cobol_copy             | **CBSA.CICSBSA.DSECT**                      |


>| GitHub folder content                                                         | USS Location on the host                    |
>| ---------------------                                                         | ------------------------                    |
>| /cicsdev/cics-banking-sample-application-cbsa/etc/install/base/aarfiles       | Copy all members/files into the location of the zOS Connect **apis** folder in USS, in our case it is /var/zosconnect/v3r0/servers/defaultServer/resources/zosconnect/**apis**/ (**Note** - if your zOS Connect server runs from a different location in USS, copy the content of the GitHub folder into the location of your own server's **apis** folder). |
>| /cicsdev/cics-banking-sample-application-cbsa/etc/install/base/sarfiles       | Copy all members/files into the location of the zOS Connect **services** folder in USS, in our case it is /var/zosconnect/v3r0/servers/defaultServer/resources/zosconnect/**services**/ (**Note** - if  your zOS Connect server runs from a different location in USS, copy the content of GitHub folder into the location of your own server's **services** folder). |



##

## Setting up the Db2 artefacts (STORGROUPS, TABLESPACES, TABLES, INDEXES):

The following job creates the ACCOUNT, PROCTRAN and CONTROL table Db2
artefacts used by CBSA. The Db2 ACCOUNT table holds the data for every
account at the bank. The PROCTRAN table holds information about
successfully processed banking transactions and the CONTROL table is
utilised to store the last account number in use.

**Note** - You need to tailor the following job for your Db2
subsystem and site standards. At the moment, "as is", it assumes an
existing Db2 subsystem called **DBCG,** and all of the tables are
prefixed **IBMUSER** to differentiate them from others that you may have
with similar names.

1. Edit CBSA.DB2.JCL.INSTALL(**DEFAULT**). This contains nine variables that need completing, which are used by the two jobs INSTDB2 and DB2BIND.

@DB2_HLQ@ is the high level qualifier of the Db2 datasets SDSNLOAD and SDSNEXIT. Depending on your installation, SDSNEXIT may have an additional qualifier which you will need to insert into the JCL yourself.

@DB2_SUBSYSTEM@ is the 1-4 character name of the Db2 subsystem you are using, which must be on the same MVS image as the JCL is submitted (and the CICS region).

@DB2_OWNER@ is the userid or group that is going to create the Db2 resources, and also the prefix for the resources. For example, IBMUSER.

@BANK_DBRMLIB@ is the dataset that is populated by COMPALL with Db2 information. For example, CBSA.CICSBSA.DBRM.

@BANK_PLAN@ is the Db2 plan you intend to use. This will need to match the Db2 definitions in the CICS region and CSD.  For example, CBSA.

@BANK_PACKAGE@ is the Db2 package you intend to bind the plan into. This is required as we also need to use Java. For example, PCBSA.

@DB2_DSNTEP_PLAN@ is the name of the plan that the Db2 utility program is bound with. This is installation dependent. You may need to ask your Db2 administrator, or use Db2 to inquire on possible plan names.

@DB2_DSNTEP_LOADLIB@ is the name of the dataset containing the Db2 utility program DSNTEP2. Typically this has "RUNLIB" as part of the name, but this is installation dependent.

@BANK_USER@ is the name of the user who is going to use Db2 in CICS, which is distinct from the user who is creating the resources. For example, CICSUSER.

2. Edit job CBSA.DB2.JCL.INSTALL(**INSTDB2**).

This creates the Db2 artefacts e.g. STORGROUPS, TABLESPACES, TABLES and INDEXES.

You need to change the JCLLIB to refer to the dataset you are currently using.

The VCAT value for the storage groups must be changed to match your installation's requirements.

(note at this time the ACCOUNT, PROCTRAN and CONTROL tables, required by CBSA, have no data on them yet - data population is performed in [Create the VSAM
files and populate the Db2 tables with
data.](#create-the-vsam-files-and-populate-the-db2-tables-with-data)).

Submit job **INSTDB2** to create the necessary Db2 artefacts.

##

## Compile/assemble, linkedit and bind the source code modules and BMS maps.

CBSA.CICSBSA.BUILDJCL(DEFAULT) contains default values for the high-level qualifiers for CICS, DB2, COBOL, Language Environment and CBSA itself. Change these if necessary.

1. Edit job CBSA.CICSBSA.BUILDJCL(**COMPALL**). Change the JCLLIB to refer to the "BUILDJCL" dataset. Submit to compile all of the
source code.

2. Execute job CBSA.DB2.JCL.INSTALL(**DB2BIND**) to BIND the programs to
Db2.

You need to change the JCLLIB to refer to the dataset you are currently using.


>> All load modules are created in CBSA.CICSBSA.LOADLIB.

##

## Create the VSAM files and populate the Db2 tables with data:

1. Execute job CBSA.JCL.INSTALL(**BANKDATA**) to create the ABNDFILE and
CUSTOMER VSAM files and to execute program BANKDATA which populates the
ACCOUNT and CONTROL Db2 tables with data (note the PROCTRAN table has no
data in it to start with).

##

## Set up the CICS region, SIT parameters and the DFHCSD used to contain all of the CBSA definitions:

1. Check CBSA.JCL.INSTALL(**BANK**). This contains all the resources for the CICS region in a group called BANK. There is a DB2CONN in the group. If your region already has a DB2CONN, then delete this. If not, verify that it connects to the correct Db2 subsystem.

There is a LIBRARY called CBSA that refers to DSNAME01 of CBSA.CICSBSA.LOADLIB. Correct this to what you have called your loadlib.

Change the port of the TCPIPSERVICE to the one you have used for the connection FROM z/OS Connect.


2. Execute job CBSA.JCL.INSTALL(**CBSACSD**) to update the DFHCSD with the
CBSA definitions and add the CSD GROUP(BANK) into a **LIST** called
**CICSTS61** (which is used in the SIT parms for the CICS region).


3. Check job CBSA.JCL.INSTALL(**CICSTS56**), this is an example CICS region.  You will have you own JCL or PROC
to start up your own CICS region. You may need to extrapolate
information from this member to include into your own CICS region
startup JCL. The most pertinent things are:

>> -   Inclusion of the DFHCSD containing the CBSA definitions created
>>     previously
>> -   Inclusion of the relevant Db2 datasets
>>

4. You may need to extrapolate SIT parm information from
CBSA.JCL.INSTALL(DFH\$SIP1) and include this in your own region. The
most pertinent things from the SIT are:

>> DB2CONN=YES
>>
>> GRPLIST=(XYZLIST,**CICSTS61**) - ensure that you include the CICSTS61
>> LIST, created above, into your GRPLIST.

##

## Make RACF and USS security changes

***Some RACF changes for Db2 authorisation and the zOS Connect server
will likely be required.***

1. Please review the content of member CBSA.JCL.INSTALL(**RACF001**). This
makes the RACF changes to ensure that the user **IBMUSER** utilised
within our CICS environment has the correct level of access to the Db2
subsystem **DBCG**. Similar RACF authorisations will need to be made to
the user and Db2 subsystem in use in your environment.

2. Please also review the content of member CBSA.JCL.INSTALL(**ZOSCSEC**).
This makes changes to the security required for zOS Connect. A
similar authorisation will need to be made to the location of the apis
and services folder in use by your own zOS Connect server (the apis
and services folder contains the aarfile and sarfiles respectively).

***Stop and Restart the CICS region.***

1. In order to pick up the changes to the SIT parms and the new CSD
definitions for CBSA, it is necessary to stop and restart your CICS
region at this point. The CICS region should be COLD started, to ensure
that all of the changes are pulled in.

***Stop and Restart the zOS Connect Server.***

1. In order to pick up the changes to the z/OS Connect Server (which
may already be running), it is necessary to stop and restart your zOS
Connect server at this point.

##

## Validating that the installation has worked:

***Checking the CUSTOMER VSAM KSDS:***

1. Logon/sign in to your CICS region. Clear the screen, Type OMEN and
you should be presented with the CBSA MAIN MENU:

![Main Menu](../doc/images/Baseinstall/Baseinstall_CBSA_MAIN_MENU.jpg)


2. Select option 1 (Display/Delete/Update CUSTOMER information) from
the main menu:

![Option 1 start](../doc/images/Baseinstall/Baseinstall_CBSA_option_1_START.jpg)

Then enter a Customer Number:

![Option 1 entry](../doc/images/Baseinstall/Baseinstall_CBSA_option_1_ENTRY.jpg)


If all is well, the details for that particular customer should be
returned:

![Option 1 success](../doc/images/Baseinstall/Baseinstall_CBSA_option_1_SUCCESS.jpg)


(this validates that customer information held on the CUSTOMER KSDS file
has been populated correctly).

***Checking the ACCOUNT data (Db2):***

3. Go back to the CBSA main menu (pf3):

![CBSA main menu](../doc/images/Baseinstall/Baseinstall_CBSA_MAIN_MENU.jpg)


And from here select option 2 (Display/Delete ACCOUNT information):

![CBSA option2 empty](../doc/images/Baseinstall/Baseinstall_CBSA_option_2_EMPTY.jpg)


The account number field should be 8 bytes long. Supply an account
number, for example:

![CBSA option2 entry](../doc/images/Baseinstall/BaseInstall_CBSA_option_2_ENTRY.jpg)

And this should return some data:

![CBSA option2 success](../doc/images/Baseinstall/BaseInstall_CBSA_option_2_SUCCESS.jpg)


If data is displayed, this validates that the Db2 ACCOUNT table has been
correctly populated.

***\
\
Checking the zOS Connect API:***

4. Confirm that the zOS Connect server has been restarted and is
executing.

5. Go to a web browser and put in this URL:

> [http://*your-host-name*:*your-port-number*/inqaccz/enquiry/42](http://your-host-name:your%20port-number/inqaccz/enquiry/42)

6. This should return something similar to this (enquiry on account
42):

![web browse returned data](../doc/images/Baseinstall/Baseinstall_CBSA_web_browser_returned_data.jpg)


If data is not returned, then check the port number and status of the
z/OS Connect server. If these are OK, check in the CICS region (using
the **CEMT I TCPIPS(ZOSEE)** command) that there is a TCPIP Service
called **ZOSEE** and that it has been installed and that the TCPIP
Service is Open. You should see something like this:

![TCPIPS active system](../doc/images/Baseinstall/Baseinstall_TCPIPS_in_active_system.jpg)

The above TCPIP Service uses port 30709 in our set up (yours may be
different) - this is the connection between the zOS Connect server
and the CICS region.

Finally, check the zOS Connect Server logs to ensure that the API
archive files (the aars), and the service archive files (the sars) were
all installed successfully.
