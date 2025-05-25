# CBSA Migration Project

## Overview
This project contains the Java implementation of the CICS Banking Sample Application (CBSA) as part of the COBOL to Java migration effort.

## Technology Stack
- Java 11
- Spring Boot 2.7.x
- SQLite Database
- Plain JDBC (no ORM)
- Maven

## Project Structure

```
src/main/java/com/cbsa/migration/
├── model/         # Java classes representing data structures (mapped from COBOL copybooks)
├── repository/    # Database access layer (JDBC implementations)
├── service/       # Business logic implementation
├── controller/    # REST API endpoints
└── BankingApplication.java  # Main application entry point
```

## Getting Started

### Prerequisites
- Java 11 (OpenJDK)
- Maven 3.x

### Building the Application
```bash
mvn clean package
```

### Running the Application
```bash
mvn spring-boot:run
```
Or after building:
```bash
java -jar target/banking-app-migration-0.0.1-SNAPSHOT.jar
```

## Development Process
This migration follows an incremental approach where:
1. We start with simple components first
2. Each component is migrated, tested, and documented before moving to more complex ones
3. We focus on functional equivalence rather than architectural perfection

## Testing
- Unit tests are written using JUnit 5 and Mockito
- Integration tests validate end-to-end functionality
- Run tests with: `mvn test`
