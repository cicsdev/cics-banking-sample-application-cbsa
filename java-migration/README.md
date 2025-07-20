# CICS Banking Sample Application - Java Migration

## 🏦 Overview
Modern Java implementation of the CICS Banking Sample Application (CBSA), migrating 29 COBOL programs to Spring Boot microservices architecture.

## 📊 Technology Stack
- **Java 11** with Spring Boot 2.7.18
- **SQLite Database** with JDBC (no ORM complexity)
- **Maven** build system
- **JUnit 5** + Mockito for testing
- **OpenAPI/Swagger** for API documentation

## 🎯 Current Migration Status
### ✅ **Completed COBOL Programs**
1. **GETCOMPY.cbl** → `CompanyInfoService` - Returns company information
2. **GETSCODE.cbl** → `SortCodeService` - Returns bank sort code

### 🔄 **In Progress**
- Test framework implementation
- Additional COBOL program migrations (27 remaining)

## 🏗️ Architecture Overview

### **Maven Project Structure**
```
java-migration/
├── pom.xml                                    # Maven dependencies & build config
├── src/main/java/com/cbsa/migration/
│   ├── BankingApplication.java               # Spring Boot entry point  
│   ├── config/
│   │   └── DatabaseConfig.java              # SQLite connection setup
│   ├── controller/
│   │   ├── StatusController.java            # Health check endpoints
│   │   └── UtilityController.java           # COBOL program REST endpoints
│   ├── model/
│   │   ├── Account.java                     # 12-field COBOL account entity
│   │   ├── Customer.java                    # Customer entity
│   │   ├── Transaction.java                 # Banking transaction records
│   │   ├── Control.java                     # System control records
│   │   └── *Response.java                   # JSON response DTOs
│   ├── repository/
│   │   ├── *Repository.java                 # Repository interfaces
│   │   └── jdbc/*Repository.java            # SQLite implementations
│   ├── service/
│   │   ├── CompanyInfoService.java          # GETCOMPY business logic
│   │   └── SortCodeService.java             # GETSCODE business logic
│   └── util/
│       └── CobolConverter.java              # COBOL data conversions
└── src/main/resources/
    ├── application.properties               # Spring Boot config (port 8085)
    └── db/
        └── schema.sql                       # Database table definitions
```

### **Database Architecture**
- **Production SQLite** (`banking.db`) - Live application data
- **Schema Definition** - Single source of truth in `schema.sql`
- **COBOL Data Patterns Preserved**:
  - Eye-catcher fields with CHECK constraints
  - Composite primary keys
  - Logical delete flags
  - ISO date format handling

## 🚀 Getting Started

### **Prerequisites**
- Java 11 (OpenJDK or Oracle)
- Maven 3.6+

### **Quick Start**
```bash
# Clone and navigate to project
cd java-migration

# Build the application
mvn clean compile

# Run with test data generation
mvn spring-boot:run -Dspring-boot.run.arguments="--generate-test-data=true --customer-count=10 --accounts-per-customer=2 --transactions-per-account=5 --reset-database=true"

# Or run without test data
mvn spring-boot:run
```

### **Application Access**
- **Base URL**: http://localhost:8085
- **Health Check**: http://localhost:8085/status
- **API Documentation**: http://localhost:8085/swagger-ui.html

## 🔗 API Endpoints

### **COBOL Program Endpoints**
```bash
# Company Information (GETCOMPY migration)
GET /api/company-name
Response: {"companyName": "CBSA", "timestamp": "2024-01-01T12:00:00"}

# Sort Code Information (GETSCODE migration)  
GET /api/sort-code
Response: {"sortCode": "123456", "timestamp": "2024-01-01T12:00:00"}

# System Status
GET /status
Response: {"status": "UP", "database": "connected", "tables": {...}}
```

## 🗄️ Database Schema

### **Core Tables**
1. **`control`** - System counters and configuration
2. **`customer`** - Customer information (from CUSTOMER.cpy)
3. **`account`** - Account details (from ACCOUNT.cpy) 
4. **`bank_transaction`** - Transaction records (from PROCTRAN.cpy)

### **Key COBOL Translations**
- **VSAM → SQLite** with preserved eye-catcher patterns
- **DB2 → SQLite** with foreign key relationships
- **Packed Decimal → INTEGER/REAL** with precision handling

## 🧪 Testing

### **Run Tests**
```bash
# Unit and integration tests
mvn test

# With coverage reporting (when configured)
mvn clean verify
```

## 📋 Development Notes

### **COBOL Migration Patterns**
- **Service Layer** - Contains translated COBOL program logic
- **Controller Layer** - REST API wrapper around COBOL services
- **Repository Layer** - Database access with COBOL data patterns
- **Model Layer** - Java POJOs matching COBOL copybook structures

### **Configuration**
- **Port**: 8085 (configured to avoid conflicts)
- **Database**: SQLite file-based (portable, no server required)
- **Logging**: Debug level for migration package

## 🎯 Next Steps
1. Implement remaining 27 COBOL program migrations
2. Enhance test coverage with JaCoCo reporting
3. Add integration tests for complex business workflows
4. Performance optimization for high-volume transactions

## 📞 Support
- **Project Type**: COBOL-to-Java Migration
- **Architecture**: Spring Boot + SQLite + REST APIs
- **Status**: Foundation complete, active development

---
*Generated from CICS Banking Sample Application migration project*
