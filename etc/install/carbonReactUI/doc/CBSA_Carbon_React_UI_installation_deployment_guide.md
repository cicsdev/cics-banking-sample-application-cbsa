# Carbon React UI installation/deployment guide

## Introduction:

> There are multiple parts to the CICS Bank Sample Application (CBSA).
> The base/COBOL installation needs to be installed first, to define all
> of the necessary files, data etc.
>
> This document is for the Liberty UI deployment/installation. It
> assumes that the installer already has:

-   a CICS region (running CICS TS 5.4 or greater)

-   a Db2 subsystem (v12 or greater)

-   the CBSA base/COBOL installed in the CICS region already

-   Java SE 11.0.17 or later. It is possible to use with Java 11.0.16 but this requires a workaround which is detailed below.

> These instructions detail the steps required to:

1.  Create and authorise a JVM profile.

2.  Define the JVM CICS resources.

3.  Edit the server.xml with the appropriate properties.

4.  Deploy the Carbon React UI application.

### Assumptions: 

-   These instructions utilise CICS TS 6.1 and therefore all directory
    names used etc. are based around that, they will need to be
    amended accordingly for different versions of CICS. In particular, verify the "BOM" for CICS is correct
    for your version of CICS.

- Java SE 11 on the workstation

-   The hostname, port number, userid and CICS TS version may be
    different when you install/deploy into your own environment. We
    have highlighted, within these instructions, where such things may
    need to be amended accordingly.

-   The User ID running the CICS region that we are using throughout
    this document is called **CICSUSER**. You may wish to utilise a
    different User ID and should amend references to CICSUSER
    accordingly.

## Update the CICSUSER Userid:

If you have already installed the Liberty UI, you can skip this step.

> It may be necessary to update the USERID being utilised in your
> environment to allow the USERID running the CICS region to have access
> to everything it needs. In this example, the USER running the CICS
> region is called **CICSUSER**. It needs to have access to the "HOME"
> directory.

1.  Issue the commands from the TSO Shell.

> **LISTUSER CICSUSER OMVS NORACF**
>
> This returns:
>
>> USER=CICSUSER
>>
>> OMVS INFORMATION
>>
>> \-\-\-\-\-\-\-\-\-\-\-\-\-\-\--
>>
>> UID= 0000990018
>>
>> HOME= /u
>>
>> PROGRAM= /bin/sh
>>
>> CPUTIMEMAX= NONE
>>
>> ASSIZEMAX= NONE
>>
>> FILEPROCMAX= NONE
>>
>> PROCUSERMAX= NONE
>>
>> THREADSMAX= NONE
>>
>> MMAPAREAMAX= NONE

2.  The "HOME" directory is set to /u and the USERID running the CICS
    region (CICSUSER) may not have access to that. You fix that by
    issuing the following:

> ALTUSER CICSUSER OMVS(HOME(/u/cicsuser))
>
> This fixes it, but that directory doesn't exist yet.

3.  Create a new folder for /u/cicsuser in Remote Systems Explorer.

> This may cause permissions problems. So amend the permissions as
> follows:
>
> ![permissions](../doc/images/LibertyUIinstall/LibertyUI_permissions_START.jpg)
>
>
> And then change the owner:
>
> chown CICSUSER .

## 

## Create a JVM server:
If you have already installed the Liberty UI, you can skip this step.

> We need a JVMSERVER resource.

1.  Create a new directory called

> /var/cics/JVMProfiles/

2.  Copy the CICS supplied JVM profile called DFHWLP in to this new
    directory. In this case, copy from the following into the new **JVM Profiles** directory:

> /usr/lpp/cicsts/cicsts61/JVMProfiles/DFHWLP.jvmprofile

3.  To utilise a JVMSERVER within CICS, you will we need a JVMPROFILEDIR
    SIT parameter in the CICS. Specify the following in the CICS
    region SIT parameters:

> JVMPROFILEDIR=/var/cics/JVMProfiles/

4.  The copied DFHWLP.jvmprofile will need to be edited:

> 1.  Ensure that JAVA_HOME is set to the appropriate level of java (in
      our case Java 11):
> 
>> JAVA_HOME=/usr/lpp/java/current_64/
>
>2.  Ensure that autoconfigure is set to true:

>> -Dcom.ibm.cics.jvmserver.wlp.autoconfigure=true

>3.  The timeout value should be set to a large value.:

>> -Dcom.ibm.cics.jvmserver.controller.timeout=900000
>>
>> **Please note:** that if the execution host machine is not very
>> powerful, that Java may take a significant time to start.

>4.  Add the following to the JVM profile to add support for Db2:


>> -Dcom.ibm.cics.jvmserver.wlp.jdbc.driver.location=/usr/lpp/db2c10/jdbc/
>> -Ddb2.jcc.currentSchema=IBMUSER
>> **Please note**: You should verify the location of the JDBC driver and
>> amend the location (above) according to your own site installation
>> location. Ensure that the currentSchema is the "DB2OWNER" from the Db2 setup.
>>

>5. Add the following to choose the correct HTTP port.
>> -Dcom.ibm.cics.jvmserver.wlp.server.http.**port=19080**
>>

>> Port **19080** is used in these instructions, you may wish to utilise a
>> different port number and should specify your chosen port number
>> (above) instead.

>6.  WORK_DIR must be set to a directory that the CICS region userid
    "CICSUSER" has access to. For example:

>> /u/cicsuser/

>7. At CICS TS 5.5 and CICS TS 5.6, add the following to the JVM profile to prevent CICS adding "wab" support.

>> -Dcom.ibm.cics.jvmserver.wlp.wab=false

>8. Ensure that the Time Zone is set correctly, otherwise Java and COBOL will not be using the same clock. This is done by specifying TZ and then the correct value. The correct value can be obtained by entering UNIX Systems Services and issuing the command:

>> echo $TZ

> This would return CST6CDT, which you would set in the JVM profile as below:

>> TZ=CST6CDT

>9. If you are using Java 11.0.16, you need to add Db2 libraries to your LIBPATH. An example is shown below:

>># Specify any directories that contain DLLs required at run time.
>>LIBPATH_SUFFIX=/usr/lpp/db2d10/jdbc/lib


## 

## CICS Resources:

1.  Create a JVM server definition in CSD GROUP CBSAWLP

> CEDA DEFINE GROUP(CBSAWLP) JVMSERVER(CBSAWLP) JVMPROFILE(DFHWLP)
>
> This is a JVM server using the JVM profile we copied earlier.
>
> CEDA DEFINE GROUP(CBSAWLP) URIMAP(CBSAWLP) USAGE(JVMSERVER) HOST(\*)
> PORT(**19080**) PATH(\*) USERID(CICSUSER)
>
> Amend the above command accordingly, to represent your chosen port
> number and userid.

2.  Add the group CBSAWLP to a list installed on a cold start, for
    example CICSTS61.

3.  Create and install this and it will create a JVM server. It may take
    a few minutes to become enabled.

> CEDA INSTALL GROUP(CBSAWLP)

You should also add the group to the list installed on a cold start.

> CEDA ADD GROUP(CBSAWLP) LIST(CICSTS61) AFTER(BANK)

> This will create a server.xml file in
>
> /u/cicsuser/CICSTS61/CICSTS61/CBSAWLP/wlp/usr/servers/defaultServer/


##

## Edit server.xml:

1.  Edit server.xml so that the application "webUI" is defined.

> \<webApplication location=\"webui-1.0.war\"/\>


2.  Add the following attribute to the **properties.db2.jcc** element:

> currentSchema=\"IBMUSER\"
>
> So it now looks like this:
>
> \<properties.db2.jcc driverType=\"2\" currentSchema=\"IBMUSER\"/\>
>
> **Note** -- "IBMUSER" is utilised in this example and relates to the
> user id assigned to the Db2 environment setup.

3.  Within the \<featureManager\> tag, add the following lines:

> \<feature\>jaxrs-2.1\</feature\>
>
> \<feature\>json-1.0\</feature\>
>
> \<feature\>jsp-2.3\</feature\>
>
> \<feature\>jdbc-4.0\</feature\>


## 

## Deploying the application:

You need Maven installed on your laptop. Maven is a dependency
management tool which is provided by Apache.

> <https://maven.apache.org/install.html>

Start the command prompt.

Change directory to the cics-banking-sample-application-cbsa/src/webui/ folder

At this point we are missing the java archive (jar) for the IBM JZOS Toolkit API. 

You need to goto your installation of IBM Java for z/OS, typically this can be found in:

> /usr/lpp/java/lib/ext

Copy the ibmjzos.jar to your workstation and make a note of the location.

Issue the following command from the command line to add the IBM JZOS Toolkit API jar:

> mvn install:install-file -Dfile="*download location*/ibmjzos.jar" -DgroupId=jzos -DartifactId=ibmjzos -Dversion=1.0.0 -Dpackaging=jar

Issue the following Maven command:

> mvn clean package

This will create "webui-1.0.war" file in a new "target" directory.

## Create an apps directory in your JVM server directory

## Export to apps directory:

Copy the war file from the target directory into the apps directory of the JVM server. 

## 

## Checking the Carbon React UI:

> Access the Carbon React UI using the following URL, you should utilise the
> hostname and port number that you have assigned:
>
> [http://*your-chosen-host-name*:*your-chosen-port-number*/webui-1.0/](http://your-chosen-host-name:your-chosen-port-number/webui-1.0/)
>
> **Please note** that on lower powered machines, it may take up
> to 5 minutes for the JVM server to restart.
>
> Once the JVM server has restarted you should be presented with the
> Carbon React UI main menu.
>
> CICS Bank Sample Application Main Menu:
>
> ![Liberty Main Menu](../doc/images/CarbonReactUIinstall/CarbonReactUI_MAINMENU.jpg)
>
>
> For a detailed description of the functionality provided by this
> interface please refer to the GitHub repo:
>
> cicsdev/cics-banking-sample-application-cbsa/etc/usage/carbonReactUI/doc

## Further development

> The Carbon React UI is an application that consists of back-end Java code and front-end JavaScript. 
> You can make changes to the back-end Java code in the application which is in 
>
> cicsdev/cics-banking-sample-application-cbsa/src/webui/src/main/java
>
> After making changes, you need to use Maven to rebuild.

Issue the following Maven command:

> mvn clean package

This will create "webui-1.0.war" file into the "target" directory. Copy this war file to the "apps" folder as before.

You can make changes to the front end (browser interface) which is written using the Carbon Design Framework on top of React. This is more involved.

The source for this is found here:

> cicsdev/cics-banking-sample-application-cbsa/src/bank-application-frontend/src

After making changes, you need to use a tool called "yarn" to build and package. "Yarn" is installed via the npm command

npm install yarn

You then issue the yarn build package command from this directory

> cicsdev/cics-banking-sample-application-cbsa/src/bank-application-frontend

This will compress the Javascript into a new "build" directory.

Copy the contents of the build directory and paste them in to this directory:

> cicsdev/cics-banking-sample-application-cbsa/src/webui/WebContent

> After making changes, you need to use Maven to rebuild.

Issue the following Maven command:

> mvn clean package

This will create "webui-1.0.war" file into the "target" directory. Copy this war file to the "apps" folder as before.