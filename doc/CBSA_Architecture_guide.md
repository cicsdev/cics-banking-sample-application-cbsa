# CICS Bank Sample Application (CBSA) Architecture

## Introduction:

There are multiple parts to the CICS Bank Sample Application (CBSA).
There is:

-   The base/COBOL installation - which needs to be installed first.

-   The Carbon React Interface - which builds upon the base/COBOL
    installation.

-   The Payment/Customer Services User Interfaces - these too build
    upon the base/COBOL interface.

The Carbon React UI and the Payment/Customer Services UIs are distinct
interfaces separate from each other. These can be installed later if
required, both execute in a Liberty JVM inside of the CICS region.
However, to get all of the features and the full functionality of CBSA
we strongly recommend installing all three parts.

## CBSA base/COBOL installation architecture diagram:

This is the minimum installation required to get CBSA up and running.
The architecture diagram for base/COBOL looks as follows:

![base cobol architecture diagram](../doc/images/Architecture/Base_cobol_CBSA_architecture_diagram.jpg)

### Assumptions & Requirements for base/COBOL:

The base/COBOL installation assumes that the installer has:

-   A CICS region (running CICS TS 6.1 or greater)

-   A Db2 subsystem (v12 or greater)

-   A z/OS Connect server

For the CBSA base offering the CICS region does not need to have a JVM
server, although for additional/optional CBSA installation offerings
e.g. the Carbon React UI and the Payment/Customer Service UIs, a JVM will be
required. The base/COBOL installation utilises BMS maps:

![bms main menu](../doc/images/Architecture/Baseinstall_CBSA_MAIN_MENU.jpg)


All CBSA resource definitions for the CBSA base offering are supplied
via the CICS region's DFHCSD file - installation jobs are provided to
update this file.

The data utilised by CBSA is spread over a mixture of Db2 tables and
VSAM files. For example, the Account information is held on a Db2 table
called ACCOUNT, internal control information for CBSA is held on the
CONTROL table and the PROCTRAN table (Process Transactions) holds all
successfully processed banking transactions. All of these tables get set
up and populated during base CBSA installation.

There are a couple of VSAM files utilise within CBSA. The first is the
CUSTOMER file which holds CUSTOMER information, and the second file an
internal file call ABNDFILE, which is used for abend processing. These
files are defined and populated (where applicable) during installation
too.

Please note that whilst the zOS Connect EE server is setup during the
base/COBOL installation, it only gets utilised for RESTful API calls and
by the Payment and Customer Services interfaces.

For more information please refer to:

> https://github.com/cicsdev/cics-banking-sample-application-cbsa/etc/install/base/doc

for the base/COBOL installation documentation.

## The Carbon React UI installation architecture diagram:

The diagram below shows the addition of the Carbon React UI. This utilises a
JVM server running in the CICS region.

![libertyUI architecture diagram](../doc/images/Architecture/LibertyUI_CBSA_architecture_diagram.jpg)

### Assumptions & Requirements for the Carbon React UI:

The Carbon React interface assumes:

-   That the base/COBOL installation has already been successfully
    installed.

-   That there is a Liberty JVM server executing in the CICS region

-   Java 17 or later

For more information please refer to:

> <https://github.com/cicsdev/cics-banking-sample-application-cbsa/etc/install/carbonReactUI/doc>

for the Carbon React UI installation documentation and:

> <https://github.com/cicsdev/cics-banking-sample-application-cbsa/etc/usage/carbonReactUI/doc>

for the Carbon React UI guide.

## The Payment and Customer Services (Spring Boot) User Interface installation architecture diagram:

The diagram below shows the addition of the Spring Boot Payment and
Customer Services user interfaces. These utilise the RESTful APIs which
interface with a z/OS Connect server and from there talk to the CICS
region.

![Payment and CS architecture diagram](../doc/images/Architecture/Payment_and_Customer_Services_UI_CBSA_architecture_diagram.jpg)

## Assumptions & Requirements for the Payment and Customer UIs:

The Payment and Customer Services UI assumes:

-   That the base/COBOL installation has been successfully completed

-   That there is a Liberty JVM (installed as part of the Carbon React UI installation)

-   That there is a z/OS Connect server (installed as part of the base installation)

-   Java 17

If you wish to utilise the RESTful API via the Spring Boot Payment or
Customer services UI or from a web browser, you will need a z/OS Connect
EE server. The installation instructions for the z/OS Connect Server
are included in the base/COBOL CBSA documentation (please refer to
<https://github.com/cicsdev/cics-banking-sample-application-cbsa/etc/install/base/doc>

For more information about installing the Payment and Customer Services
UI (Spring Boot) please refer to:

<https://github.com/cicsdev/cics-banking-sample-application-cbsa/etc/install/springBootUI/doc>

for installation instructions and:

> <https://github.com/cicsdev/cics-banking-sample-application-cbsa/etc/usage/sprintBoot/doc>

for the Customer Services and Payment user guides, and the RESTful API
guide.
