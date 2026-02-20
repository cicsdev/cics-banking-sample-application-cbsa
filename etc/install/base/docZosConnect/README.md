# CBSA z/OS Connect Installation Instructions

##

## Introduction:

This document is to assist with the installation of the z/OS Connect components. It assumes that the
installer already has:

-   a CICS region (running CICS TS 6.1 or greater)

-   a z/OS Connect server.

These instructions detail the steps required to:

1.  Download the z/OS Connect archive files into the correct location.

2.  Make note of the z/OS Connect server HTTP port.

3.  Configure the z/OS Connect server to talk to your CICS region.

4.  Configure your CICS region to receive requests from z/OS Connect.

5.  Validating that the installation has worked

6.  Troubleshooting

7.  Considerations for Db2.


At the end, z/OS Connect and your CICS region should be capable of successfully executing
requests.

## Download the z/OS Connect archive files into the correct location.

Your z/OS Connect server has a config directory that contains *server.xml*.

In the same directory is a sub-directory called *resources*.

Inside *resources* is a sub-directory called *zosconnect*.

This contains two sub-directories, *apis* and *services*.

Copy, as binary files, resources from your workstation to these directories. An example is shown below.

>| GitHub folder content                                                         | USS Location on the host                    |
>| ---------------------                                                         | ------------------------                    |
>| /cicsdev/cics-banking-sample-application-cbsa/etc/install/base/aarfiles       | Copy all members/files into the location of the z/OS Connect **apis** folder in USS, in our case it is /var/zosconnect/v3r0/servers/defaultServer/resources/zosconnect/**apis**/ (**Note** - if your z/OS Connect server runs from a different location in USS, copy the content of the GitHub folder into the location of your own server's **apis** folder). |
>| /cicsdev/cics-banking-sample-application-cbsa/etc/install/base/sarfiles       | Copy all members/files into the location of the z/OS Connect **services** folder in USS, in our case it is /var/zosconnect/v3r0/servers/defaultServer/resources/zosconnect/**services**/ (**Note** - if  your z/OS Connect server runs from a different location in USS, copy the content of GitHub folder into the location of your own server's **services** folder). |

## Make note of the z/OS Connect server HTTP port.

The *server.xml* file contains an *httpEndpoint* element, which has two attributes, *host* and *httpPort*. These are used to connect to the z/OS Connect server, either from a web browser or from the Spring Boot applications.

## Configure the z/OS Connect server to talk to your CICS region.

The z/OS Connect APIs connect to a CICS region. It is necessary to configure this in the z/OS Connect server's *server.xml*.

>\<zosconnect_cicsIpicConnection id=\"cicsConn\"
host=\"[localhost]{.underline}\" port=\"30709\" /\>

This is an example of what you might want to add. Note that the host and port number might be different to that printed above.

## Configure your CICS region to receive requests from z/OS Connect.

The CICS CSD contains a TCPIPSERVICE definition that listens on port 30709. If the 30709 port number has changed, edit the CSD definition for TCPIP Service definition
(ZOSEE), as shown below, which is provided in member BANK, after
downloading it from the repo folder:

> cicsdev/cics-banking-sample-application-cbsa/etc/install/base/installjcl

>![TCPIP def](../doc/images/Baseinstall/Baseinstall_TCPIPS_DEF.jpg)




## Validating that the installation has worked

1. Confirm that the z/OS Connect server has been restarted and is
executing.

2. Go to a web browser and put in this URL:

> [http://*your-host-name*:*your-port-number*/inqaccz/enquiry/42](http://your-host-name:your%20port-number/inqaccz/enquiry/42)

3. This should return something similar to this (enquiry on account
42):

![web browse returned data](../doc/images/Baseinstall/Baseinstall_CBSA_web_browser_returned_data.jpg)

## Troubleshooting

If data is not returned, then check the port number and status of the
z/OS Connect server. If these are OK, check in the CICS region (using
the **CEMT I TCPIPS(ZOSEE)** command) that there is a TCPIP Service
called **ZOSEE** and that it has been installed and that the TCPIP
Service is Open. You should see something like this:

![TCPIPS active system](../doc/images/Baseinstall/Baseinstall_TCPIPS_in_active_system.jpg)

The above TCPIP Service uses port 30709 in our set up (yours may be
different) - this is the connection between the zOS Connect server
and the CICS region.

Verify that the APIs have been installed correctly. Go to a web browser and put in this URL

> [http://*your-host-name*:*zos-connect-port-number*/zosConnect/apis](http://*your-host-name*:*zos-connect-port-number*/zosConnect/apis)

A list of APIs should be presented. There should be nine APIs for Customer Services, and one for the Payment Interface.

Verify that the Services have been installed correctly.

> [http://*your-host-name*:*zos-connect-port-number*/zosConnect/services](http://*your-host-name*:*zos-connect-port-number*/zosConnect/services)

A list of Services should be presented. There should be nine services for Customer Services, and one for the Payment Interface.

If any are missing, check the z/OS Connect server logs, which are in ASCII. It is important that the aar and sar files are transferred to the correct directories as binary files, without conversion to EBCDIC.

## Considerations for Db2

The z/OS Connect APIs use COBOL programs running in your CICS region. These programs access the customer data, held on a VSAM file, and account data, held in a Db2 table. These COBOL programs expect to be accessed using the Db2 plan specified during the base installation, the default being CBSA.

If this Db2 plan is specified on the Db2 connection used by your CICS region, then requests from z/OS Connect will work correctly.

If the plan is different, then the requests from z/OS Connect will fail. This is because the requests will run under the default *mirror transaction* CSMI, and this will run under the default plan associated with the Db2 connection. The 3270 terminal requests succeed because they run under transactions associated with *Db2 transactions* which relate to a *Db2 entry* which runs under the correct plan, CBSA.

If you change the default plan on the Db2 connection to be CBSA, then this will let your requests work. However, other requests and programs that rely on the old plan will now fail.

You can create a *Db2 transaction* for CSMI and associate it with the existing *Db2 entry*. Examples can be found in RDO group BANK in the CSD. However, other requests entering the CICS region, either from other CICS regions or other z/OS Connect servers will now use the new plan and may well fail.

You can create a new *mirror transaction*, for example *ZSMI*. Copy the existing definition of CSMI from group DFHISC to BANK and rename it as *ZSMI*. Copy a Db2 transaction and rename it. Be aware that the Db2 transaction contains a transaction attribute.

In the z/OS Connect server.xml, on the CICS connection element, add a *transid=ZSMI* attribute. This means that requests from THIS z/OS Connect server to your region will now use the new transaction ZSMI, and will now use the CBSA plan.
