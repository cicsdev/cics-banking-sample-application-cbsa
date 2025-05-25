# COBOL to Java Migration Plan

Date: May 24, 2025
Last Updated: May 24, 2025

## 1. Introduction

This document outlines the detailed plan for migrating the CICS Banking Sample Application (CBSA) from COBOL/CICS to a Java-based implementation. The plan takes into account our limited COBOL/CICS knowledge and prioritizes simpler components for initial migration to build momentum and understanding.

## 2. Migration Approach

We'll adopt an **incremental migration** approach with the following characteristics:

- **Start Small**: Begin with simpler, self-contained programs before tackling complex ones
- **Incremental Delivery**: Migrate and test one component at a time
- **Knowledge Building**: Use early migrations to establish patterns for later, more complex programs
- **Focus on Learning**: Document discoveries about the COBOL structure to aid future migrations

## 3. Technology Stack

### Target Architecture

- **Runtime**: Java 11 with Spring Boot
- **API Layer**: RESTful services that mirror the functionality of COBOL programs
- **Database**: SQLite with JDBC (plain SQL, no ORM)
- **Testing**: JUnit 5 + Mockito
- **Build System**: Maven
- **Deployment**: Self-contained JAR

## 4. Migration Phases

### Phase 1: Environment Setup and Data Model (Weeks 1-2) - ✅ MAIN TASKS COMPLETED

1. **Set up Java/Spring Boot Development Environment** ✅

   1. Set up Maven project structure
   2. Configure Spring Boot starter dependencies

2. **Define Java Data Models** ✅

   1. Create Java classes for core entities based on COBOL copybooks:
      1. `Account` (from ACCOUNT.cpy)
      2. `Customer` (from CUSTOMER.cpy)
      3. `Transaction` (from PROCTRAN.cpy)
      4. `Control` (from CONTROLI.cpy)
   2. Include proper validation annotations

3. **Create SQLite Database Schema** ✅

   1. Design relational schema based on COBOL data structures
   2. Create SQL scripts for table creation
   3. Implement basic JDBC repository classes
   4. Created REST API endpoints for status and data management

4. **Develop Test Data Generator** ✅

   1. Create script to generate realistic test data
   2. Include capabilities for database reset
   3. Generate sample customer and account data
   4. Successfully tested with command: `mvn spring-boot:run -Dspring-boot.run.arguments="--generate-test-data=true --customer-count=10 --accounts-per-customer=2 --transactions-per-account=5 --reset-database=true" -Dspring-boot.run.jvmArguments="-Dserver.port=8085"`

5. **Phase 1 Completion Tasks** 🔄

   1. Add basic integration tests for the repository layer
   2. Review SQL schema for any needed optimizations
   3. Document existing API endpoints with Swagger/OpenAPI

### Phase 1 Technical Insights

1. **SQLite Implementation Discoveries**

   1. SQLite has reserved keywords (like "transaction") that required table name adjustments
   2. Spring Data JDBC doesn't work well with SQLite; plain JDBC repositories are more reliable
   3. SQLite data types differ from standard SQL, requiring careful mapping

2. **Java Implementation Successes**

   1. REST API structure with separate controllers for status and data management works well
   2. Application runs successfully as a backend service with API endpoints
   3. Test data generation is critical for validating functionality

3. **Architecture Validation**

   1. The Spring Boot architecture with separate controllers, services, and repositories provides a clean separation of concerns
   2. Java model classes successfully map to COBOL data structures
   3. REST API approach is suitable for replacing CICS transaction processing

### Phase 2: Utility Functions Migration (Weeks 3-4) - 🔄 NEXT

### Phase 2 Focus

1. **Set Migration Priorities**

   1. Start with GETSCODE.cbl and GETCOMPY.cbl as first conversions
   2. These provide simple, testable functionality with minimal CICS interactions

### Phase 2 Implementation Tasks

1. **Migrate Simple Utility Programs**

   1. Start with smallest, self-contained programs like:
      1. GETSCODE.cbl (retrieve sort codes)
      2. GETCOMPY.cbl (retrieve company information)
   2. Create equivalent Java service methods
   3. Write unit tests to verify behavior

2. **Establish Common Helper Functions**

   1. Date/time conversions between COBOL and Java formats
   2. Field validations and formatting
   3. Error handling utilities

3. **Define REST API Controllers**

   1. Create initial REST endpoints for utility functions
   2. Document API using Swagger/OpenAPI
   3. Write integration tests for endpoints

### Phase 3: Basic Account Functions (Weeks 5-8)

1. **Account Inquiry**

   1. Migrate INQACC.cbl (account inquiry)
   2. Implement account lookup functionality
   3. Create read-only endpoints first

2. **Account Creation**

   1. Migrate CREACC.cbl (account creation)
   2. Implement account creation service
   3. Include business validation rules

3. **Account Update and Deletion**

   1. Migrate UPDACC.cbl (account update)
   2. Migrate DELACC.cbl (account deletion)
   3. Ensure proper validation and error handling

### Phase 4: Customer Management (Weeks 9-12)

1. **Customer Inquiry**

   1. Migrate INQCUST.cbl (customer inquiry)
   2. Implement customer lookup functionality
   3. Create read-only endpoints first

2. **Customer Creation and Management**

   1. Migrate CRECUST.cbl (customer creation)
   2. Migrate UPDCUST.cbl (customer update)
   3. Migrate DELCUS.cbl (customer deletion)

### Phase 5: Financial Operations (Weeks 13-16)

1. **Transaction Processing**

   1. Migrate DBCRFUN.cbl (debit/credit functionality)
   2. Implement transaction posting logic

2. **Transfer Functions**

   1. Migrate XFRFUN.cbl (transfer functionality)
   2. Implement transfer between accounts
   3. Ensure proper transaction handling

### Phase 6: Menu and Integration (Weeks 17-20)

1. **Main Menu Functionality**

   1. Migrate BNKMENU.cbl to Java controller
   2. Connect UI components to backend services

2. **Integration Testing**

   1. Develop end-to-end test scenarios
   2. Validate complete business processes
   3. Test boundary conditions and error handling

3. **Performance Testing**

   1. Benchmark Java implementation
   2. Identify and resolve bottlenecks

### Phase 7: Data Migration and Final Deployment (Weeks 21-24)

1. **Data Migration Tools**

   1. Create scripts to migrate data from COBOL structures to SQLite
   2. Verify data integrity after migration

2. **Deployment Process**

   1. Finalize deployment approach
   2. Create deployment documentation
   3. Set up continuous integration pipeline

3. **Transition Planning**

   1. Define detailed component transition steps
   2. Create verification procedures
   3. Establish success criteria for each component

## 5. Common COBOL to Java Conversion Patterns

### Data Type Conversions

| COBOL Type | Java Type | Notes |
|------------|-----------|-------|
| PIC X(n) | String | Fixed-length character fields |
| PIC 9(n) | Integer, Long | Numeric fields without decimals |
| PIC 9(n)V9(m) | BigDecimal | Numeric fields with decimals |
| PIC S9(n)V9(m) | BigDecimal | Signed numeric fields with decimals |
| COMP-3 | BigDecimal | Packed decimal format |
| 88-level | Boolean or enum | Condition names translated to constants |

### COBOL Structural Elements

| COBOL Concept | Java Equivalent | Implementation Approach |
|---------------|-----------------|-------------------------|
| COPY | Import/Include | Java class imports |
| REDEFINES | Multiple classes/views | Separate classes with converters |
| OCCURS | Arrays/Lists | Java collections |
| WORKING-STORAGE | Class fields | Static or instance variables |
| PROCEDURE DIVISION | Methods | Service and controller methods |
| CICS Commands | Spring components | REST controllers, services, etc. |

## 6. Testing Strategy

### Unit Testing

1. Each Java service method will have comprehensive unit tests
2. Use JUnit 5 for test framework
3. Use Mockito to mock dependencies
4. Target high test coverage (>80%)

### Integration Testing

1. Test complete business processes
2. Validate API endpoints with Spring MockMvc
3. Use SQLite in-memory database for tests

### Test Data Management

1. Develop data generation scripts for all entities
2. Create known test scenarios with predictable outcomes
3. Include boundary conditions and error cases

## 7. Documentation Requirements

1. **Code Documentation**

   1. Javadoc comments for all classes and methods
   2. Clear mapping between COBOL programs and Java components
   3. Document business rules and validation logic

2. **API Documentation**

   1. OpenAPI/Swagger documentation for all endpoints
   2. Include example requests and responses

3. **Architecture Documentation**

   1. Component diagrams
   2. Data flow descriptions
   3. Integration points

## 8. Risks and Mitigation

| Risk | Impact | Likelihood | Mitigation |
|------|--------|------------|------------|
| Incomplete understanding of COBOL logic | High | High | Start with simpler components, document findings, seek expert consultation when needed |
| Data conversion issues | High | Medium | Create validation tools, test with representative data samples |
| Performance problems in Java implementation | Medium | Low | Include performance tests, optimize critical paths |
| Missing edge cases in business logic | Medium | Medium | Thorough testing, document all business rules |
| Dependency on external systems | Low | Low | Identify and isolate external dependencies early |

## 9. Key Success Metrics

1. **Functional Completeness**

   1. All COBOL program functionality successfully migrated
   2. No regressions in business logic

2. **Code Quality**

   1. Test coverage percentage
   2. Static analysis results
   3. Documentation completeness

3. **Performance**

   1. Response time comparable to or better than COBOL version
   2. Resource utilization within acceptable limits

## 10. Next Steps

1. **Immediate Actions**

   1. Set up Java/Spring Boot development environment
   2. Create initial project structure
   3. Begin analyzing COBOL data structures for Java model design
   4. Develop test data generation scripts

2. **First Milestone Target (2 weeks)**

   1. Complete Phase 1: Environment Setup and Data Model
   2. Demonstrate basic CRUD operations with SQLite database
   3. Present initial Java data models mapped from COBOL structures
