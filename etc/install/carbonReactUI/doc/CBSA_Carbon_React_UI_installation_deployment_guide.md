# Carbon React UI installation/deployment guide

## Introduction:

There are multiple parts to the CICS Banking Sample Application (CBSA).
The base/COBOL installation needs to be installed first, to define all
of the necessary files, data etc.
This document is for the Carbon React UI deployment/installation. It
assumes that the installer already has:

-   a CICS region (running CICS TS 6.1 with APAR PH60795 applied, or later)

-   a Db2 subsystem (v12 or greater)

-   the CBSA base/COBOL installed in the CICS region already

-   Java 17

These instructions detail the steps required to:
- [Set up the OMVS Segment](#OMVS-Segment)

- [Set up the JVM profile](#JVM-profile)

- [Set up the JVM server](#JVM-server)

- [Edit the server.xml](#edit-serverxml)

- [Deploy the Carbon React UI](#deploy)

- [Checking the Carbon React UI](#checking)

- [Further Development](#development)

### Assumptions:

-   These instructions utilise CICS TS 6.1 and therefore all directory
    names used etc. are based around that, they will need to be
    amended accordingly for different versions of CICS. In particular, verify the "BOM" for CICS is correct
    for your version of CICS.

- Java 17 on the workstation

-   The hostname, port number, userid and CICS TS version may be
    different when you install/deploy into your own environment. We
    have highlighted, within these instructions, where such things may
    need to be amended accordingly.

-   The User ID running the CICS region that we are using throughout
    this document is called **CICSUSER**. You may wish to utilise a
    different User ID and should amend references to CICSUSER
    accordingly.

## OMVS-Segment

If you have already installed the Spring Boot UI, you can skip this step.

It may be necessary to update the USERID being utilised in your
environment to allow the USERID running the CICS region to have access
to everything it needs. In this example, the USER running the CICS
region is called **CICSUSER**. It needs to have access to the "HOME"
directory.

1.  Issue the commands from the TSO Shell.

`LISTUSER CICSUSER OMVS NORACF`

This returns:

```

USER=CICSUSER

OMVS INFORMATION

UID= 0000990018

HOME= /u

PROGRAM= /bin/sh

CPUTIMEMAX= NONE

ASSIZEMAX= NONE

FILEPROCMAX= NONE

PROCUSERMAX= NONE

THREADSMAX= NONE

MMAPAREAMAX= NONE

```

2.  The "HOME" directory is set to /u and the USERID running the CICS
    region (CICSUSER) may not have access to that. You fix that by
    issuing the following:

`ALTUSER CICSUSER OMVS(HOME(/u/cicsuser))`

This fixes it, but that directory doesn't exist yet.

3.  Create a new folder for /u/cicsuser in Remote Systems Explorer.

This may cause permissions problems. So amend the permissions as follows:

![permissions](../doc/images/CarbonReactUIinstall/CarbonReactUI_permissions_START.jpg)
And then change the owner:

`chown CICSUSER .`


## JVM profile
If you have already installed the Spring Boot UI, you can skip this step. Carry on from ![Edit server.xml](#Edit server.xml)

We need a JVMSERVER resource.

1.  Create a new directory called

`/var/cics/JVMProfiles/`

2.  Copy the CICS supplied JVM profile called DFHWLP in to this new
    directory. In this case, copy from the following into the new **JVM Profiles** directory:

`/usr/lpp/cicsts/cicsts61/JVMProfiles/DFHWLP.jvmprofile`

3.  To utilise a JVMSERVER within CICS, you will we need a JVMPROFILEDIR
    SIT parameter in the CICS. Specify the following in the CICS
    region SIT parameters:

`JVMPROFILEDIR=/var/cics/JVMProfiles/`

4.  The copied DFHWLP.jvmprofile will need to be edited.

  a.  Ensure that JAVA_HOME is set to the appropriate level of java (in
      our case Java 17):

`JAVA_HOME=/usr/lpp/java/current_64/`

b.  Ensure that autoconfigure is set to true:

`-Dcom.ibm.cics.jvmserver.wlp.autoconfigure=true`

c.  The timeout value should be set to a large value.:

`-Dcom.ibm.cics.jvmserver.controller.timeout=900000`

 **Please note:** that if the execution host machine is not very powerful, that Java may take a significant time to start.

d.  Add the following to the JVM profile to add support for Db2:


`-Dcom.ibm.cics.jvmserver.wlp.jdbc.driver.location=/usr/lpp/db2c12/jdbc/`

`-Ddb2.jcc.currentSchema=IBMUSER`

 **Please note**: You should verify the location of the JDBC driver and
 amend the location (above) according to your own site installation
 location. Ensure that the currentSchema is the "DB2OWNER" from the Db2 setup.


e. Add the following to choose the correct HTTP port.
`-Dcom.ibm.cics.jvmserver.wlp.server.http.port=**19080**`


Port **19080** is used in these instructions, you may wish to utilise a
different port number and should specify your chosen port number
(above) instead.

f.  WORK_DIR must be set to a directory that the CICS region userid
    "CICSUSER" has access to. For example:

`/u/cicsuser/`

g. Ensure that the Time Zone is set correctly, otherwise Java and COBOL will not be using the same clock. This is done by specifying TZ and then the correct value. The correct value can be obtained by entering UNIX Systems Services and issuing the command:

`echo $TZ`

 This would return CST6CDT, which you would set in the JVM profile as below:

`TZ=CST6CDT`




## JVM server

1.  Create a JVM server definition in CSD GROUP CBSAWLP

`CEDA DEFINE GROUP(CBSAWLP) JVMSERVER(CBSAWLP) JVMPROFILE(DFHWLP)`

 This is a JVM server using the JVM profile we copied earlier.

2.  Add the group CBSAWLP to a list installed on a cold start, for
    example CICSTS61.

3.  Create and install this and it will create a JVM server. It may take
    a few minutes to become enabled.

`CEDA INSTALL GROUP(CBSAWLP)`

You should also add the group to the list installed on a cold start.

`CEDA ADD GROUP(CBSAWLP) LIST(CICSTS61) AFTER(BANK)`

 This will create a server.xml file in

`/u/cicsuser/CICSTS61/CICSTS61/CBSAWLP/wlp/usr/servers/defaultServer/`



## Edit server.xml

1.  Edit server.xml so that the application "webUI" is defined.

`<webApplication location="webui-1.0.war"/>`


2.  Add the following attribute to the **properties.db2.jcc** element. Replace IBMUSER with the Db2 owner chosen using the base install.

`currentSchema="IBMUSER"`

 So it now looks like this:

`<properties.db2.jcc driverType="2" currentSchema="IBMUSER"/>`

 **Note** -- "IBMUSER" is utilised in this example and relates to the
 user id assigned to the Db2 environment setup.

3.  Within the `<featureManager>` tag, add the following lines:

`<feature>jakartaee-10.0</feature>`
`<feature>json-1.0</feature>`


## Deploy

Maven is a dependency management tool which is provided by Apache. A wrapper for Maven is provided.

Start the command prompt.

The following process will build both the Carbon React UI and the two SpringBoot applications.

Change directory to the cics-banking-sample-application-cbsa folder.

Issue the following Maven command:

`mvnw clean package`

This will create "webui-1.0.war" file in a new "target" directory,
`/src/webui/target`

## Create an apps directory in your JVM server directory

This directory will be where our applications will be stored.

## Export to apps directory

Copy the war file from the target directory into the apps directory of the JVM server. You may need to alter permissions on the war file to let the CICS region userid read it.


## Checking

 Access the Carbon React UI using the following URL, you should utilise the
 hostname and port number that you have assigned:

> [http://*your-chosen-host-name*:*your-chosen-port-number*/webui-1.0/](http://your-chosen-host-name:your-chosen-port-number/webui-1.0/)

 **Please note** that on lower powered machines, it may take up
 to 5 minutes for the JVM server to restart.

 Once the JVM server has restarted you should be presented with the
 Carbon React UI main menu.

 CICS Banking Sample Application Main Menu:

 ![Carbon React Main Menu](../doc/images/CarbonReactUIinstall/CarbonReactUI_MainMenu.png)


 For a detailed description of the functionality provided by this
 interface please refer to the GitHub repo:

[How to use](/etc/usage/carbonReactUI/doc/CBSA_Carbon_React_UI_User_Guide.md)

## Development

The Carbon React UI is an application that consists of back-end Java code and front-end JavaScript.
You can make changes to the back-end Java code in the application which is in

[src/webui/src/main/java](/src/webui/src/main/java)

 After making changes, you need to use Maven to rebuild.

Issue the following Maven command from the base directory:

`mvnw clean package`

This will create "webui-1.0.war" file into the "target" directory. Copy this war file to the "apps" folder as before.

You can make changes to the front end (browser interface) which is written using the Carbon Design Framework on top of React. This is more involved. You do NOT need to do this to use the Carbon React UI, only if you wish to change the web pages.

The source for this is found here:

[/src/bank-application-frontend/](/src/bank-application-frontend/)

After making changes, you need to use a tool called "yarn" to build and package. "Yarn" is installed via the npm command

`npm install yarn`

You then issue the

`yarn build package`

 command from the directory

`/src/bank-application-frontend/`

This will compress the JavaScript into a new "build" directory.

Copy the contents of the build directory and paste them in to the following directory. It is a good idea to delete the "static" folder from WebContent before doing so. This is because "yarn build package" produces JavaScript files with randomly generated names that are quite large, and these will make your "war" file larger. You only need the latest ones produced by the build.

`/src/webui/WebContent`

After making changes, you need to use Maven to rebuild.

Issue the following Maven command from the base directory:

`mvnw clean package`

This will create "webui-1.0.war" file into the "src/webui/target" directory. Copy this war file to the "apps" folder as before.
