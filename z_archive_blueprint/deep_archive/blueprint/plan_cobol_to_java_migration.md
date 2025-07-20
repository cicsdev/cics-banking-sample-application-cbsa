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

### Phase 1: Environment Setup and Data Model (Weeks 1-2) - ✅ COMPLETED

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

5. **Phase 1 Completion Tasks** ✅

   1. Added basic integration tests for the repository layer (Customer and Account)
   2. Reviewed SQL schema and implemented optimizations with additional indexes
   3. Documented existing API endpoints with Swagger/OpenAPI

### Phase 1 Technical Insights

1. **SQLite Implementation Discoveries**

   1. SQLite has reserved keywords (like "transaction") that required table name adjustments
   2. Spring Data JDBC doesn't work well with SQLite; plain JDBC repositories are more reliable
   3. SQLite data types differ from standard SQL, requiring careful mapping
   4. H2 in-memory database works well for integration tests but has compatibility issues with SQLite

2. **Java Implementation Successes**

   1. REST API structure with separate controllers for status and data management works well
   2. Application runs successfully as a backend service with API endpoints
   3. Test data generation is critical for validating functionality
   4. Swagger/OpenAPI documentation provides clear API visibility
   5. Integration tests verify critical repository operations

3. **Architecture Validation**

   1. The Spring Boot architecture with separate controllers, services, and repositories provides a clean separation of concerns
   2. Java model classes successfully map to COBOL data structures
   3. REST API approach is suitable for replacing CICS transaction processing
   4. Integration testing validates database interactions
   5. Maven provides reliable build and test execution

### Phase 2: Utility Functions Migration (Weeks 3-4) - 🚵 Current Work

### Phase 2 Focus

1. **Set Migration Priorities**

   1. ✔ Migrated GETSCODE.cbl and GETCOMPY.cbl to Java (`SortCodeService`, `CompanyInfoService`)
   2. These provide simple, testable functionality with minimal CICS interactions

### Phase 2 Implementation Tasks

1. **Migrate Simple Utility Programs**

   1. Start with smallest, self-contained programs like:
      1. GETSCODE.cbl (retrieve sort codes)
      2. GETCOMPY.cbl (retrieve company information)
   2. Create equivalent Java service methods
   3. Write unit tests to verify behavior

### Phase 3: Basic Account Functions (Weeks 5-8)

1. **Workflow and Rules Development**

   1. ✔ Drafted **crucial starter rules** (type mapping + minimal naming) in `.windsurf/rules/`
   2. Authored initial `cobol_function_migration` workflow (proved too large in practice)
   3. ✔ Ran workflow on INQACC.cbl; captured learnings that led to pivot
   4. Pivot → designing piece-wise workflows (see `plan/plan_breakdown_workflows.md`) and refining rules accordingly
   5. TODO — create the piece-wise workflow markdown templates under `.windsurf/workflows/`

2. **Account Inquiry**

   1. PENDING — migrate INQACC.cbl (account inquiry) with the new piece-wise workflow (`/java_function_piece_*` commands)
   2. Implement account lookup functionality
   3. Create read-only endpoints first
   4. Document workflow effectiveness and challenges

3. **Account Creation**

   1. Refine the workflow based on INQACC.cbl migration experience
   2. Migrate CREACC.cbl (account creation) following the updated workflow
   3. Implement account creation service
   4. Include business validation rules
   5. Update workflows and rules based on new learnings

4. **Account Update and Deletion**

   1. Migrate UPDACC.cbl (account update) following the workflow
   2. Migrate DELACC.cbl (account deletion) following the workflow
   3. Ensure proper validation and error handling
   4. Create integration tests for repository and controller layers
   5. Document APIs with Swagger/OpenAPI annotations
   6. Finalize Phase 3 updates to workflows and rules

### Phase 4: Customer Management (Weeks 9-12)

1. **Workflow and Rules Refinement**

   1. Review and update workflows based on Phase 3 experience
   2. Refine rules for consistency across all migrated components
   3. Document common patterns and challenges identified in Phase 3

2. **Customer Inquiry**

   1. Migrate INQCUST.cbl (customer inquiry) following the refined workflow
   2. Implement customer lookup functionality
   3. Create read-only endpoints first
   4. Document any new workflow insights

3. **Customer Creation and Management**

   1. Migrate CRECUST.cbl (customer creation) following the workflow
   2. Migrate UPDCUST.cbl (customer update) following the workflow
   3. Migrate DELCUS.cbl (customer deletion) following the workflow
   4. Add integration tests for all customer operations
   5. Document APIs with Swagger/OpenAPI annotations
   6. Update workflows and rules with any new patterns discovered

### Phase 5: Financial Operations (Weeks 13-16)

1. **Workflow and Rules Assessment**

   1. Perform comprehensive assessment of workflow effectiveness
   2. Identify any areas for improvement in complex financial operations
   3. Update workflows and rules for transaction-heavy operations

2. **Transaction Processing**

   1. Migrate DBCRFUN.cbl (debit/credit functionality) following the workflow
   2. Implement transaction posting logic
   3. Document specific workflow adaptations for financial transactions

3. **Transfer Functions**

   1. Migrate XFRFUN.cbl (transfer functionality) following the workflow
   2. Implement transfer between accounts
   3. Ensure proper transaction handling
   4. Refine workflows based on complex financial operation experience

### Phase 6: Menu and Integration (Weeks 17-20)

1. **Workflow Finalization for UI Components**

   1. Extend workflows to cover UI component migration
   2. Update rules for UI-specific considerations
   3. Document UI migration patterns

2. **Main Menu Functionality**

   1. Migrate BNKMENU.cbl to Java controller following the workflow
   2. Connect UI components to backend services
   3. Document specific UI migration challenges and solutions

3. **Integration Testing**

   1. Develop end-to-end test scenarios
   2. Validate complete business processes
   3. Test boundary conditions and error handling
   4. Update workflows with integration testing insights

4. **Performance Testing**

   1. Benchmark Java implementation
   2. Identify and resolve bottlenecks
   3. Document performance optimization patterns in workflows

### Phase 7: Data Migration and Final Deployment (Weeks 21-24)

1. **Data Migration Workflow Implementation**

   1. Implement the Data Migration Workflow documented in `plan_workflow_rules_development.md`
   2. Create scripts to migrate data from COBOL structures to SQLite
   3. Verify data integrity after migration
   4. Document effectiveness of the Data Migration Workflow

2. **Workflow and Rules Documentation Finalization**

   1. Compile all workflow and rules insights from Phases 1-7
   2. Create comprehensive final documentation of successful patterns
   3. Document lessons learned and recommendations for future projects

3. **Deployment Process**

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
