# 🧪 CBSA End-to-End Test Suite with Galasa
This repository contains automated tests for the **CICS Banking Sample Application (CBSA)**, using the [Galasa Framework](https:/galasa.dev/) to validate both its **mainframe** and **web UI** components. It uses version 0.44.0 of Galasa.
---
## 📋 Table of Contents
- [Overview](#overview)
- [Project Structure](#project-structure)
- [Technologies Used](#technologies-used)
- [Setup and Installation](#setup-and-installation)
- [Configuration](#configuration)
- [Running the Tests](#running-the-tests)
- [Test Examples](#test-examples)
- [Artifacts and Logs](#artifacts-and-logs)
- [Troubleshooting](#troubleshooting)
- [Contributing](#contributing)
- [License](#license)
---
## 📘 Overview
This repository contains automated tests for the **CICS Banking Sample Application (CBSA)** — a hybrid enterprise application that combines:
- A **modern React-based web user interface**
- A **mainframe-based 3270 terminal interface**
The tests are written using the [Galasa Framework](https://galasa.dev/), which allows seamless integration of both mainframe and web UI testing in a single, unified framework.
This suite uses:
- **Selenium WebDriver** to test the React web UI of CBSA
- **c3270 terminal emulation** to test the green-screen (3270) portion of CBSA
The goal is to validate full banking workflows that span both platforms, enabling robust end-to-end regression testing for CBSA.
---
## 📁 Project Structure
Only the core source and configuration files are included in this repository. Build outputs (e.g. `/build`) are excluded.
```
.
├── .galasa/
├── build.sh
├── gradlew
├── gradlew.bat
├── settings.gradle
├── gradle/
│   └── wrapper/
│       ├── gradle-wrapper.jar
│       └── gradle-wrapper.properties
├── com.ibm.cics.cip.cbsa.galasa.tests.install/
│   ├── bnd.bnd
│   ├── build.gradle
│   └── src/
│       └── main/
│           ├── java/com/ibm/cics/cip/cbsa/galasa/tests/install/
│           └── resources/
├── com.ibm.cics.cip.cbsa.galasa.tests.manager/
│   ├── bnd.bnd
│   ├── build.gradle
│   └── src/
│       └── main/
│           └── java/com/ibm/cics/cip/cbsa/galasa/tests/manager/
├── com.ibm.cics.cip.cbsa.galasa.tests.obr/
│   ├── build.gradle
```
- `tests.install`: Contains Galasa test classes that drive test cases against CBSA
- `tests.manager`: Holds reusable managers and interfaces for interacting with CBSA systems (e.g., terminal, web UI, etc)
- `tests.obr`: OBR packaging for test deployment
- `run.sh`, `build.sh`: Convenience scripts for local execution
- `gradlew`: Gradle wrapper scripts to build without installing Gradle manually
### ℹ️ Note on `.galasa/`
The `.galasa/` directory contains local Galasa configuration, including runtime settings and credentials. It is created when you initialize Galasa on your machine.
For setup instructions, please refer to the official documentation:
- [Installing the Galasa CLI Tool](https://galasa.dev/docs/cli-command-reference/installing-cli-tool/)
- [Initializing the Galasa Home Folder](https://galasa.dev/docscli-command-reference/initialising-home-folder#setting-the-galasa-home-folder-optional)
This directory is user-specific and usually **excluded from version control**.
---
## 🛠️ Technologies Used
- **[Galasa](https://galasa.dev/)** – Enterprise test automation framework
- **c3270** – Terminal emulation for mainframe sessions
- **Selenium WebDriver** – UI automation for web apps
- **Java (17+)** – Primary test implementation language (Java 17 or higher recommended)
- **Gradle** – Build and dependency management tool
---
## 🚀 Setup and Installation
### Pre-requisites
#### Local Development and Test Execution
- Java 17 or higher
- Gradle 8.3 (usually included with the project via the Gradle Wrapper)
- [Galasa CLI Installation](https://galasa.dev/docscli-command-reference/installing-cli-tool/) and
[Initializing Galasa Home Folder](https://galasa.dev/docs/cli-command-reference/initialising-home-folder/) (required for local test execution and setup)
- Browser drivers for Selenium (e.g., ChromeDriver)
- Access to mainframe systems (e.g., IP/hostname, user credentials)
- A CICS region with CBSA installed is required for running **c3270** tests
- The React Web UI of CBSA must be installed and accessible for running **Selenium** Web UI tests
- For detailed CBSA installation instructions, please refer to the [CBSA Installation Documentation](#) *(replace `#` with actual link if available)*
#### Ecosystem Deployment (CI/CD)
- Maven Repository
- Kubernetes cluster
- Helm package manager
---
### 🔧 Required Galasa Properties
To run the CBSA Galasa test suite, several system properties must be configured in your `.galasa/` environment (typically in `localproperties` or defined via your ecosystem configuration).
Below is a sample configuration with **placeholders** — you must update the values based on your own mainframe, z/OS image, CBSAregion, and WebUI setup.
### 🔐 Credentials Configuration
Galasa uses a `credentials.properties` file to securely reference system login details by tag. This allows your tests to authenticatewith z/OS systems, CBSA regions, and other secured components withouthardcoding usernames and passwords into test logic.
#### ✅ Example Format
```properties
secure.credentials.YOUR_CREDENTIAL_TAG.username=your-username
secure.credentials.YOUR_CREDENTIAL_TAG.password=your-password
```
#### ✅ Sample Property Configuration
```properties
# Default CICS logon details
cicsts.default.logon.gm.text=******
cicsts.default.logon.initial.text=ENTER
# Provisioning
cicsts.provision.type=dse
# z/OS cluster and image setup
zos.cluster.DEFAULT.images=YOUR_IMAGE_ID
zos.dse.tag.PRIMARY.clusterid=YOUR_CLUSTER_ID
zos.dse.tag.PRIMARY.imageid=YOUR_IMAGE_ID
zos.dse.tag.RUN.clusterid=YOUR_CLUSTER_ID
zos.dse.tag.RUN.imageid=YOUR_IMAGE_ID
# Image credentials and access
zos.image.YOUR_IMAGE_ID.credentials=YOUR_CREDENTIAL_TAG
zos.image.YOUR_IMAGE_ID.default.hostname=your.host.name
zos.image.YOUR_IMAGE_ID.ipv4.hostname=your.host.name
zos.image.YOUR_IMAGE_ID.javahome=JAVA_HOME
zos.image.YOUR_IMAGE_ID.liberty.install.dir=LIBERTY_INSTALL_DIR
zos.image.YOUR_IMAGE_ID.max.slot=HI
zos.image.YOUR_IMAGE_ID.max.slots=50
zos.image.YOUR_IMAGE_ID.ports=20{3-9}{0-9}{0-9},21{0-3}{0-9}{0-9},21{0-2}{0-9}
zos.image.YOUR_IMAGE_ID.sysplex=YOUR_SYS_PLEX
zos.image.YOUR_IMAGE_ID.telnet.port=TELNET_PORT
zos.image.YOUR_IMAGE_ID.telnet.tls=false
# Terminal emulator settings
zos3270.terminal.output=json,png
# Console and batch job settings
zosbatch.batchjob.YOUR_IMAGE_ID.restrict.to.image=false
zosconsole.console.YOUR_IMAGE_ID.restrict.to.image=false
# z/OSMF config
zosmf.image.YOUR_IMAGE_ID.servers=YOUR_SERVER_ID
zosmf.server.YOUR_SERVER_ID.image=YOUR_IMAGE_ID
zosmf.server.YOUR_IMAGE_ID.https=true
zosmf.server.YOUR_IMAGE_ID.port=PORT_NUMBER
zosmf.sysplex.YOUR_SYS_PLEX.default.servers=YOUR_IMAGE_ID
# CBSA-specific tags and region info
cbsa.cbsa.zos.image.tag=PRIMARY
cbsa.cbsa.region.image.tag=PRIMARY
cbsa.region.applid=YOUR_CBSA_APPLID
cbsa.region.credentials.tag=YOUR_CREDENTIAL_TAG
# WebUI endpoint (React app)
cbsa.webui.url=http://your-webui-host:PORT/webui-1.0/#/profile/Admin
# Selenium config (for local or grid execution)
# For Local
selenium.local.driver.FIREFOX.path=LOCAL_PATH
selenium.driver.type=FIREFOX,CHROME etc
# For Grid / Standalone Deployment
selenium.driver.type=grid
selenium.default.driver=grid
selenium.grid.endpoint=ENDPOINT_URL
# Galasa OBR/Test Stream config (based on Maven Repo)
framework.test.stream.main.repo=https://your-repo-host/
framework.test.stream.main.obr=mvn:com.yourorg/your.obr.artifact/0.0.1/obr
framework.test.stream.main.location=https://your-repo-host/path/toyour/testcatalog.json
```
---
## Create the main test stream
Create a GalasaStream YAML resource, like this:
```apiVersion: galasa-dev/v1alpha1
kind: GalasaStream
metadata:
    name: main
    description: CBSA Galasa tests
data:
    isEnabled: true
    repository:
        url: https://your-maven-repo/path/to/cbsa/test/material
    obrs:
        - group-id: com.ibm.cics
          artifact-id: com.ibm.cics.cip.cbsa.galasatests.obr
          version: 0.0.1
    testCatalog:
        url: https://your-maven-repo/path/to/testcatalog.json
```
Then apply to the Galasa ecosystem by running:

```
galasactl resources apply -f <your-galasa-stream-file>.yaml --bootstrap [bootstrap] --log -
```
## ▶️ Running the Tests
### Run Tests Locally
```bash
./gradlew clean build publishToMavenLocal
galasactl runs submit local --class com.ibm.cics.cip.cbsa.galasatests.install/com.ibm.cics.cip.cbsa.galasa.tests.install[TestClassName] --obr mvn:com.ibm.cics/com.ibm.cics.cip.cbsa.galasatests.obr/0.0.1/obr --log -
```
### Run Tests within Ecosystem
```bash
galasactl runs prepare \
        --stream "main" \
	--bundle "com.ibm.cics.cip.cbsa.galasa.tests.install" \
        --portfolio "portfolio.yaml" \
        --bootstrap [bootstrap] \
        --log "-"
galasactl runs submit \
        --portfolio "portfolio.yaml" \
        --reportjson "report.json" \
        --noexitcodeontestfailures \
        --bootstrap [bootstrap] \
        --log "-"
```
---
## ✅ Test Examples
### c3270 Test
```java
    @Test
    public void updateCustomerWithInvalidTitle() throws Exception {
        String[] customerData = ICbsaCustomergenerateRandomCustomerData();
        String name = customerData[1];
        try {
            terminal.goToScreen(CbsaMenuItem.DISPLAY_CUSTOMER);
            terminal.waitForKeyboard().type(customer.getCustomerNumbe()).enter().waitForKeyboard().pf10().waitForKeyboard()eraseInput().type("Super " + name + " K Walker").enter()waitForKeyboard();
            assertThat(terminal.retrieveScreen()).contains("Validtitles are:");
        } catch (Exception e) {
            throw e;
        }
    }
```
### Selenium Test
```java
@Test
    public void createCustomer() throws Exception {
        Map<String, String> result = webuiinputCreateCustomerDobTestData(dateOfBirth,false);
        newCustomerNumber = result.get("customerNumber");
        String messageText = result.get("messageText");
        assertThat(newCustomerNumber).isNotNull();
        assertThat(messageText).contains(customerCreationSuccessMessage);
    }
```
---
## 📦 Artifacts and Logs
After each test run, Galasa stores:
- Console logs (in .galasa/ras)
- Screenshots (for Selenium - can be added where you like using thedriver functions)
- Terminal screen dumps (for 3270)
---
## 🤝 Contributing
We welcome improvements! Please fork this repo and raise a pullrequest.
Before contributing:
- Follow naming conventions
- Include comments and Javadoc where helpful
- Make sure your changes pass all existing tests
---

---
## 📚 References
- [Galasa Docs](https://galasa.dev/docs/)
