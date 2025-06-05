# Analysis Report: Application Discovery & Architecture Mapping

## 1. COBOL/CICS Backend

### Key COBOL Programs and Their Functions

*   **CSACCCRE:** Creates customer and account records.
*   **CSACCDRD:** Reads customer and account details.
*   **CSACCHDR:** Handles CICS channel and container management for other programs.
*   **CSACCIPI:** Main CICS screen interaction program for customer inquiry.
*   **CSACCUDL:** Deletes customer and account records.
*   **CSACCUPD:** Updates customer and account records.
*   **CSAPYCRE:** Creates payment records.
*   **CSAPYDRD:** Reads payment details.
*   **CSAPYPST:** Processes (posts) payments.
*   **CSAPYWLP:** (Likely) Payment "write list" or similar reporting/batch function.
*   **CSSTATUS:** Checks application status.

### List of BMS Maps and Their Associations

*   **CSMAP01 (CSACCIPI):** Main screen for customer inquiry, account creation, updates, and deletions.
    *   Fields for customer ID, name, address, account number, account type, balance, etc.
*   *(Assumption: Other BMS maps likely exist for payment processing and status display, but are not explicitly detailed in the provided information.)*

### Key COBOL Copybooks and Their Purposes

*   **CSACC01:** Customer record layout (name, address, ID, etc.). Used by programs handling customer data.
*   **CSACC02:** Account record layout (account number, type, balance, customer ID link). Used by programs handling account data.
*   **CSAPY01:** Payment record layout (payment ID, amount, date, account number). Used by payment processing programs.
*   **CSDAT01:** Date/time related data structures. Used across multiple programs for timestamping or date calculations.
*   **CSMSG01:** Message structures for screen display or inter-program communication.
*   **CSHDR01:** Header/common area for CICS channel/container communication. Used by CSACCHDR and programs it calls.
*   **CSCHS01:** Channel definition for CSACCHDR.
*   **CSCNT01:** Container definition for CSACCHDR.
*   *(Specific copybooks for VSAM and DB2 record layouts are implicitly defined by their DDL/definitions but might have corresponding COBOL representations, e.g., CUSTREC, ACCTREC, etc.)*

### VSAM File Definitions

*   **CUSTOMER:**
    *   Stores customer master information.
    *   Key: `CUSTOMER-ID`
    *   Fields: `CUSTOMER-ID`, `CUSTOMER-NAME`, `CUSTOMER-ADDRESS`, `CUSTOMER-PHONE`, etc. (as defined in CSACC01).
*   **ABNDFILE:**
    *   Stores transaction abend/error information.
    *   Likely includes fields for transaction ID, error code, timestamp, program name.

### DB2 Table Definitions

*   **ACCOUNT:**
    *   `ACCOUNT_NUMBER` (CHAR(10), Primary Key)
    *   `CUSTOMER_ID` (CHAR(8), Foreign Key to CUSTOMER VSAM likely or another table)
    *   `ACCOUNT_TYPE` (CHAR(1))
    *   `BALANCE` (DECIMAL(10,2))
    *   `OPEN_DATE` (DATE)
*   **PROCTRAN (Transaction Log):**
    *   `TRANSACTION_ID` (CHAR(16), Primary Key)
    *   `ACCOUNT_NUMBER` (CHAR(10), Foreign Key to ACCOUNT)
    *   `TRANSACTION_TYPE` (CHAR(4)) - e.g., 'PYMT', 'XFER'
    *   `TRANSACTION_AMOUNT` (DECIMAL(10,2))
    *   `TRANSACTION_DATE` (TIMESTAMP)
    *   `STATUS` (CHAR(1)) - e.g., 'P' (Posted), 'F' (Failed)
*   **CONTROL (Application Control/Configuration):**
    *   `PROCESS_DATE` (DATE)
    *   `LAST_TRAN_ID` (CHAR(16))
    *   `SYSTEM_STATUS` (CHAR(1)) - e.g., 'A' (Active), 'M' (Maintenance)

### Conceptual COBOL Program Interaction Flow

```
                                     +-----------------+
                                     |  User/Terminal  |
                                     +--------+--------+
                                              | (CSMAP01)
                                     +--------V--------+
                                     |   CSACCIPI      | (Main Screen Handler)
                                     +--------+--------+
                                              |
                  +---------------------------+---------------------------+
                  | (Read, Create, Update, Delete requests)              |
                  |                                                     |
          +-------V-------+      +-------V-------+      +-------V-------+      +-------V-------+
          |   CSACCDRD    |      |   CSACCCRE    |      |   CSACCUPD    |      |   CSACCUDL    |
          | (Read Cust/Acct)|      | (Create Cust/Acct)|      | (Update Cust/Acct)|      | (Delete Cust/Acct)|
          +-------+-------+      +-------+-------+      +-------+-------+      +-------+-------+
                  |                      |                      |                      |
                  | (Uses CSACCHDR for   | (Uses CSACCHDR for   | (Uses CSACCHDR for   | (Uses CSACCHDR for
                  |  channel comms)      |  channel comms)      |  channel comms)      |  channel comms)
                  |                      |                      |                      |
      +-----------+-----------+  +---------+---------+  +---------+---------+  +---------+---------+
      | VSAM (CUSTOMER)       |  | DB2 (ACCOUNT)     |  | DB2 (PROCTRAN)    |  | VSAM (ABNDFILE)   |
      | DB2 (ACCOUNT)         |  | VSAM (CUSTOMER)   |  | DB2 (ACCOUNT)     |  | (Error Logging)   |
      +-----------------------+  +-------------------+  +-------------------+  +-------------------+

Payment Processing Flow (Simplified):

+-----------------+      +-----------------+      +-----------------+      +-----------------+
|   CSAPYCRE      |----->|   CSAPYDRD      |----->|   CSAPYPST      |----->|   CSAPYWLP      |
| (Create Payment)|      | (Read Payment)  |      | (Post Payment)  |      | (List Payments) |
+-----------------+      +-----------------+      +-----------------+      +-----------------+
        |                      |                      |
        | (DB2 - PROCTRAN,     | (DB2 - PROCTRAN,     | (DB2 - PROCTRAN,
        |  ACCOUNT)            |  ACCOUNT)            |  ACCOUNT, CONTROL)
        +----------------------+----------------------+
```

## 2. z/OS Connect Layer

### List of Defined z/OS Connect APIs

*   `customerapi`
*   `paymentapi`
*   `statusapi`
*   *(Specific endpoint example: `creacc` under `customerapi`)*

### Summary of a Sample API (e.g., `creacc` from `customerapi`)

*   **API Name:** `customerapi`
*   **Endpoint:** `/customers/create` (Method: POST) - *Derived from `creacc` and typical REST conventions.*
*   **Request Structure (JSON):**
    ```json
    {
      "customerName": "John Doe",
      "customerAddress": "123 Main St",
      "accountType": "S" // Savings
      // ... other fields from CSACC01 and CSACC02 needed for creation
    }
    ```
*   **Response Structure (JSON):**
    ```json
    {
      "customerId": "CUST0001",
      "accountNumber": "ACC1234567",
      "status": "Customer and account created successfully."
      // ... potentially other details
    }
    ```

### List of z/OS Connect Services

*   `CSacccreService` (maps to `creacc` API endpoint)
*   `CSaccdrdService` (maps to an account read API endpoint)
*   `CSaccupdService` (maps to an account update API endpoint)
*   `CSaccdeleteService` (maps to an account delete API endpoint)
*   `CSapypstService` (maps to a payment post API endpoint)
*   `CSstatusService` (maps to a status check API endpoint)

### Details of a Sample Service (e.g., `CSacccreService`)

*   **Service Name:** `CSacccreService`
*   **Linked COBOL Program:** `CSACCCRE`
*   **Purpose:** Exposes the functionality of the `CSACCCRE` COBOL program (creating customer and account records) as a callable service for the `creacc` API endpoint.
*   **Data Transformation:** z/OS Connect handles the transformation of JSON request data to the format expected by `CSACCCRE` (likely using copybooks like `CSACC01`, `CSACC02`, and `CSHDR01` for channel/container structures) and transforms the COBOL output back into a JSON response.

### Overall Mapping Table: API -> z/OS Connect Service -> COBOL Program -> Copybook

| API Endpoint (Example) | z/OS Connect Service | COBOL Program | Key Copybooks Used (Request/Response) |
|------------------------|------------------------|---------------|---------------------------------------|
| POST /customers/create (`creacc`) | `CSacccreService`    | `CSACCCRE`    | `CSACC01`, `CSACC02`, `CSHDR01`         |
| GET /customers/{id}    | `CSaccdrdService`    | `CSACCDRD`    | `CSACC01`, `CSACC02`, `CSHDR01`         |
| PUT /customers/{id}    | `CSaccupdService`    | `CSACCUPD`    | `CSACC01`, `CSACC02`, `CSHDR01`         |
| DELETE /customers/{id} | `CSaccdeleteService` | `CSACCUDL`    | `CSACC01`, `CSACC02`, `CSHDR01`         |
| POST /payments/post    | `CSapypstService`    | `CSAPYPST`    | `CSAPY01`, `CSHDR01`                  |
| GET /status            | `CSstatusService`    | `CSSTATUS`    | `CSMSG01` (potentially)               |

## 3. Java Services (Spring Boot/Liberty) - Initial Findings

### Summary of `pom.xml` Analysis

*   **`Z-OS-Connect-Customer-Services-Interface` (`customerservices`):**
    *   **Key Frameworks/Dependencies:** Spring Boot (web, jaxrs), IBM z/OS Connect client (`com.ibm.zosconnect.client`), JSON processing (e.g., Jackson).
    *   **Purpose:** Acts as a client to the z/OS Connect `customerapi`. It provides a Java-based interface (likely RESTful services) for other applications (like `webui`) to interact with customer data on the mainframe without directly calling z/OS Connect.
    *   **Deployment:** Likely deployed as a CICS Liberty bundle or standalone Liberty application.
*   **`Z-OS-Connect-Payment-Interface` (`paymentservices`):**
    *   **Key Frameworks/Dependencies:** Similar to `customerservices` - Spring Boot (web, jaxrs), IBM z/OS Connect client, JSON processing.
    *   **Purpose:** Acts as a client to the z/OS Connect `paymentapi`. Provides a Java-based interface for payment-related operations.
    *   **Deployment:** Likely deployed as a CICS Liberty bundle or standalone Liberty application.
*   **`webui`:**
    *   **Key Frameworks/Dependencies:** Spring Boot (web, thymeleaf/jsp for UI rendering), Spring Security, common web application libraries.
    *   **Purpose:** Provides the user interface for the application. It interacts with `customerservices` and `paymentservices` to display data and submit user requests.
    *   **Deployment:** Likely deployed as a standalone Spring Boot application or a WAR file on a Liberty server.

### Initial Understanding of Their Purpose and Deployment

*   The Java services (`customerservices`, `paymentservices`) abstract the complexities of z/OS Connect interactions, offering a more modern, Java-native interface (likely REST APIs) to consumer applications like `webui`.
*   They are designed to run on Liberty, potentially within CICS using CICS Liberty bundles for closer integration with the mainframe environment if needed, or as standalone Liberty applications.
*   This architecture promotes a separation of concerns, where the `webui` handles presentation, the Java interface services handle API aggregation/mediation, and z/OS Connect handles the direct mainframe integration.

### Brief Overview of the Source Structure for `customerservices`

*   **`src/main/java/.../controllers`:**
    *   Contains Spring MVC/JAX-RS controllers that define the REST API endpoints for customer operations (e.g., `CustomerController.java`).
    *   These controllers would use the z/OS Connect client to make calls to the `customerapi` on z/OS Connect.
*   **`src/main/java/.../jsonclasses` (or `model`, `dto`):**
    *   Contains Plain Old Java Objects (POJOs) that represent the JSON request and response structures for the APIs provided by `customerservices`.
    *   These classes are used for serializing/deserializing JSON data.
*   *(Other typical Spring Boot directories like `src/main/resources` for configuration (`application.properties`), `static` for web assets if any, etc.)*
