---
trigger: always_on
---

# Minimal Architecture Understanding
*Essential context for agents working on COBOL-to-Java migration*

## 🏦 Application Overview
**CICS Banking Sample Application (CBSA)** - Enterprise banking system with 29 COBOL programs being migrated to Java Spring Boot + SQLite.

## 📊 Technology Stack
**Java 11, Spring Boot 2.7.18, SQLite, Maven, JUnit 5, H2 (testing), Lombok, SpringDoc OpenAPI, JaCoCo, SonarQube**

## 🏗️ Layered Architecture

### **Core Package Structure: `com.cbsa.migration.*`**
- **`config/`** - Spring Boot configuration (database, OpenAPI)
- **`controller/`** - REST API endpoints exposing COBOL program functionality
- **`dto/`** - Data Transfer Objects with validation and API contracts
- **`model/`** - Domain entities matching COBOL data structures
- **`repository/`** - Data access interfaces + JDBC implementations
- **`service/`** - Business logic layer (COBOL program translations)

### **Critical Configuration Files:**
- **`schema.sql`** ⚠️ - Single source of truth for database structure
- **`application.properties`** - Spring Boot configuration (port 8085)
- **`test-schema.sql`** - H2-compatible version for testing
- **`pom.xml`** - Maven dependencies with JaCoCo coverage (50% threshold)

## 📦 DTO Architecture

### **Data Transfer Objects Layer**
**Purpose**: Clean separation between internal domain models and external API contracts

### **DTO Types**
1. **Request DTOs** - Input validation and API contracts
   - `CustomerRequestDto` - Customer creation/update validation
   - `AccountRequestDto` - Account creation validation  
   - `TransactionRequestDto` - Transaction input validation

2. **Response DTOs** - Clean API output without internal fields
   - `CustomerResponseDto` - Customer data with account summaries
   - `AccountResponseDto` - Account details with computed status
   - `TransactionResponseDto` - Transaction data with derived fields
   - `AccountSummaryDto` - Lightweight account overview

3. **Mapping Layer**
   - `DtoMapper` - Bidirectional conversion between domain models and DTOs
   - Handles derived fields (status, account summaries)
   - Encapsulates business logic for API responses

### **Benefits**
- **Security**: Hides internal database fields from API
- **Stability**: API contracts independent of domain model changes
- **COBOL Compatibility**: Matches expected data structures from legacy programs
- **Validation**: Input validation at API boundary

## 🗄️ Database Architecture

### **Multi-Tier Database Strategy**
1. **Production SQLite** (`banking.db`): Live application data
2. **H2 In-Memory** (testing): Fast test execution with H2-compatible schema

### **Schema Consistency Principle** ⚠️
- **Production Schema**: `src/main/resources/db/schema.sql` (SQLite syntax)
- **Test Schema**: `src/test/resources/db/test-schema.sql` (H2-compatible syntax)
- **Same Structure**: Identical tables, columns, constraints - only syntax differs
- **Prevents**: Schema drift between SQLite (prod) and H2 (test) environments

### **Database Tables (COBOL → SQL Translation)**
1. **`control`** - System counters (customer_count, last_customer_number, etc.)
2. **`customer`** - From CUSTOMER.cpy VSAM → SQLite with eye_catcher, sort_code PK
3. **`account`** - From ACCOUNT.cpy DB2 → SQLite with foreign key to customer
4. **`bank_transaction`** - From PROCTRAN.cpy DB2 → SQLite with logical delete flag

### **Key COBOL Data Patterns Preserved**
- **Eye-catcher fields**: `CHECK(eye_catcher = 'CUST')` constraints
- **Composite primary keys**: `(sort_code, customer_number)`
- **Logical deletes**: `logically_deleted INTEGER` instead of physical deletes
- **Date handling**: TEXT fields in ISO format (SQLite limitation)

## 🎯 Core COBOL Program Categories
1. **Customer Management**: CRECUST, INQCUST, UPDCUST, DELCUS (4 programs)
2. **Account Management**: CREACC, INQACC, UPDACC, DELACC (4 programs)  
3. **Account-Customer Integration**: INQACCCU (1 program)
4. **Financial Transactions**: XFRFUN, DBCRFUN (2 programs)
5. **Credit Agency Services**: CRDTAGY1-5 (5 programs)
6. **UI/Menu System**: BNKMENU, BNK1* screens (9 programs)
7. **Utilities**: GETCOMPY ✅, GETSCODE ✅, BANKDATA, ABNDPROC (4 programs)

## 🔑 Key Business Entities
1. **Customer** (VSAM): Personal info, credit score, demographics
2. **Account** (DB2): Balance, type, rates, overdraft limits  
3. **Transaction**: Debits, credits, transfers between accounts
4. **Control**: System counters for customer/account numbering

## ⚡ Critical Success Patterns
1. **Working Examples**: Follow GETCOMPY/GETSCODE patterns for new migrations
2. **Single Package**: Keep `com.cbsa.migration.*` hierarchy (no duplicates)
3. **Schema Consistency**: Use schema files, not code-based table creation
4. **Repository Pattern**: Interface + JDBC implementation for testability
5. **Service Layer**: Simple services, avoid over-engineering
6. **Test Isolation**: H2 in-memory database with compatible schema

## 🚨 Architectural Governance Rules
1. **NO Duplicate Packages**: Avoid `com.bank.cbsa.*` or similar hierarchies
2. **NO Duplicate Entities**: Single Account, Customer, Transaction model classes  
3. **NO Complex Patterns**: Keep services simple, repositories straightforward
4. **Database Config**: Use Spring @Value injection, no hardcoded paths
5. **Test Database**: Use same schema.sql, different database file

## 🎦 Application Ports & URLs
- **Application**: `http://localhost:8085` (configurable via `server.port`)
- **REST API**: `/api/status`, `/api/utility/company-name`, `/api/utility/sortcode`
- **Documentation**: `/swagger-ui/index.html` (OpenAPI 3.0), `/v3/api-docs` (JSON spec)
- **Database**: `banking.db` (SQLite prod), H2 in-memory (tests)

## 🧪 Test Framework Status
**CURRENT STATE**: Functional testing framework with 39 passing tests
- ✅ **Working Tests**: Model, repository, and service layer coverage
- ✅ **H2 Integration**: In-memory database for fast test execution
- ✅ **JaCoCo Coverage**: 31.9% overall (targeting 50% threshold)
- ⚠️ **Coverage Gaps**: Major repositories need comprehensive testing
- 🎯 **Priority**: Add tests for AccountRepository, CustomerRepository, TransactionRepository

---
*This document provides the minimum viable understanding for agents to work effectively on the COBOL-to-Java banking migration project.*
