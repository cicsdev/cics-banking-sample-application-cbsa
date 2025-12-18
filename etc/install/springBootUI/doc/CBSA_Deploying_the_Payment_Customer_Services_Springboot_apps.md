# CBSA Deploying the Payment and Customer Services Spring Boot apps

### Introduction:

CBSA has two Spring Boot applications provided, for the Customer
Services and Payment interfaces. These applications allow easy use of
the RESTful API in CBSA with simple clean interfaces. There are multiple
steps (listed below) that need to be actioned in order to get them up
and running.

These instructions detail the steps required to:

1.  Set up the port for the z/OS Connect server.

2.  Update the JVM Profile with the correct port and hostname for the z/OS Connect server.

3.  Build the Java components using with Maven.

4.  Copy the WAR file to the apps directory.

5.  Update the JVM Server.

6.  Access the applications.

### Assumptions:

-   A CICS region (running CICS TS 6.1 with APAR PH60795 applied or later) is available, and the
    CBSA base/COBOL installation has been performed.

-   A Db2 subsystem (v12 or greater) is available.

-   A z/OS Connect server is available and set up as part of the
    base/COBOL install, especially the .aar and .sar files.

-   A Liberty JVM is running inside of the CICS region - this is set up
    as part of the Carbon React UI installation process.

-   Java 17.



### CICS Bank Sample Application Architecture:

For architecture information please refer to the GitHub repo:

> cicsdev/cics-banking-sample-application-cbsa/doc

##

## Changing the port to match z/OS Connect:

The Spring Boot applications run inside a WebSphere Liberty Profile JVM
server inside CICS, but also communicate with z/OS Connect. It is
important to make sure that the connection information to the z/OS
Connect server is correct. As a default, these are set to port 30701
and host localhost, if you utilised a different port number or hostname
during the z/OS Connect setup (as part of the base/COBOL installation)
then please substitute the default values with yours.

Should you need to change these, they are configured by parameters that you will need to set in the JVM Profile as given in the examples below.

> -DCBSA_ZOSCONN_PORT=30701

> -DCBSA_ZOSCONN_HOST=localhost

##

## Maven:

Maven is a dependency management tool which is provided by Apache. A wrapper for Maven is provided for your convenience.

Start the command prompt.

Change directory to the base directory.

Issue the following Maven command:

> mvnw clean package

This creates 3 folders called target, one inside each of the Java application directories within src. Inside of each target is a .war file.

##

## Export to apps directory:

Copy the war file from the two target directories into the apps directory as binary files.

##

## Updating the JVM server:

### JVM Profile

The JVM server, provided in the CICS region, may need to be updated to
provide more time for the application to start. The JVM profile lives in
the JVMPROFILEDIR specified on CICS start-up, in this example it is this
one:

> /S0W1/var/cics/JVMProfiles/DFHWLP.jvmprofile

At the bottom, add the following timeout overrides.

> -Dcom.ibm.cics.jvmserver.controller.timeout=900000

> -Dcom.ibm.cics.jvmserver.wlp.bundlepart.timeout=900000



### server.xml

Then update the server.xml which controls the operation of the WebSphere
Liberty Application Server. In this example, it is here, and this is an
ASCII file. You need to use 3.17 (the Unix Directory List Facility) and
the EA (Edit Ascii File) option to edit it.

> /u/cicsuser/CICSTS61/CBSAWLP/wlp/usr/servers/defaultServer/server.xml


Add the two applications by adding these lines:

>    \<webApplication location=\"customerservices-1.0.war\"/\>

>    \<webApplication location=\"paymentinterface-1.1.war\"/\>


Restart the JVM server to allow these new changes to come into effect.

The WAR files have installed and now we must wait for the Spring Boot
applications to start. Check the *messages.log* file, in this example
these are found in:

> /u/cicsuser/CICSTS61/CBSAWLP/wlp/usr/servers/defaultServer/logs
>
> ![Spring](../doc/images/springBootUI/SpringBoot_Spring.jpg)

This is a sign that things are working, and the applications are
starting.

Message CWWKZ0001I is issued several minutes later at which point you
can use the applications.

> Root WebApplicationContext: initialization completed in 180700 ms
>
> Root WebApplicationContext: initialization completed in 180395 ms

##

## Access to the application/interfaces:

### The Customer Service interface
To access the Customer Services interface utilise the following URL,
note that you will need to amend the URL in this example to reflect your
own hostname and port number:

> <http://your-chosen-host-name:19080/customerservices-1.0/>

**NOTE** - Port number 19080 is the JVM Server port number which was
allocated as part of the Carbon React UI installation process. If you elected
to use a different port number, you should substitute it with your own
one.

A user guide for the Customer Services interface can be found in the
repo:

> cicsdev/cics-banking-sample-application-cbsa/etc/usage/springBoot/doc


### The Payment interface
Access to the Payment interface is via the following URL, note that you
will need to amend the URL in this example to reflect your own hostname
and port number:

> <http://your-chosen-host-name:19080/paymentinterface-1.1/>

**NOTE** - Port number 19080 is the JVM Server port number which was
allocated as part of the Liberty UI installation process. If you elected
to use a different port number, you should substitute it with your own
one.

A user guide for the Payment interface can be found in the
repo:

> cicsdev/cics-banking-sample-application-cbsa/etc/usage/springBoot/doc.
