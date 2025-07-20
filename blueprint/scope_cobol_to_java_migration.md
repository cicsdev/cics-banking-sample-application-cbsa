# COBOL to Java Migration Project Scope

Date: May 24, 2025

## Project Overview

This document outlines the scope and approach for migrating the CICS Banking Sample Application (CBSA) from COBOL/CICS to a modern Java-based implementation. The goal is to eliminate dependencies on mainframe technologies while maintaining full feature parity.

## Migration Approach

- **Strategy**: Incremental migration of COBOL programs to Java, starting with simpler components
- **Timeline**: Progressive delivery with prioritization of simpler components first
- **Risk Mitigation**: Comprehensive testing of each component before moving to the next

## Technical Scope

### Components to Migrate

1. **COBOL Programs**: All 29 COBOL programs, including:
   - Main menu controller (BNKMENU)
   - Account management (CREACC, INQACC, UPDACC, DELACC)
   - Customer management (CRECUST, INQCUST, UPDCUST, DELCUS)
   - Data initialization (BANKDATA)
   - Financial operations (XFRFUN, DBCRFUN)

2. **Data Access Layer**:
   - Migrate DB2 table access (ACCOUNT, CONTROL, PROCTRAN)
   - Convert VSAM file access (CUSTOMER, ABNDFILE) to relational model

3. **Transaction Processing**:
   - Replace CICS transaction handling with Java service methods
   - Implement all business rules and validation logic in Java

## Technical Approach

### UI Strategy

- Reuse existing components from `/src/webui/` and `/src/bank-application-frontend/`
- Connect these UI components to new Java backend via REST APIs
- No UI redesign - focus exclusively on backend migration

### Data Architecture

- **Database**: SQLite file-based database
- **Access Method**: Plain JDBC (no ORM)
- **Data Migration**: VSAM and DB2 data will be unloaded into SQLite relational tables
- **Rationale**: Simple, lightweight solution appropriate for teller-style workload volumes

### Runtime & Deployment

- **Runtime**: Java 11 Spring Boot application
- **Packaging**: Self-contained uber-JAR with embedded servlet container
- **Hosting**: Cloud VM (AWS/Azure/GCP)
- **Infrastructure**: Standard Linux + Java stack
- **Dependencies**: No IBM-specific dependencies or mainframe connectivity required

### Integration Strategy

- **Approach**: Complete native Java rewrites of all CICS transactions
- **API Layer**: REST endpoints for all business functions
- **External Systems**: No z/OS Connect bridge or screen-scraping layer

## Testing Strategy

1. **Integration Tests**:
   - Spring Boot tests against SQLite test database
   - Validate end-to-end business processes

2. **Test Data Management**:
   - Data generator script to seed test databases with realistic accounts and balances
   - Reset capability for clean test runs

3. **Unit Testing**:
   - JUnit 5 + Mockito for isolated component testing
   - Focus on business logic validation

4. **Continuous Integration**:
   - GitHub Actions for automated build and test
   - Test coverage reporting (enforcement in future phases)

## Success Criteria

- Complete elimination of COBOL/CICS dependencies
- 100% functional parity with existing application
- Self-contained, vendor-agnostic Java implementation
- Successful execution of all test scenarios
- Reliable deployment process to target environment

## Project Logistics

### Resources

- **Team Size**: Single developer project
- **Technical Background**: Primary expertise in Python; both COBOL and Java are new technologies

### Timeline Considerations

- No strict deadline pressure
- Focus on simplicity and straightforward implementation
- Prioritize maintainable code over complex optimizations
- Iterative learning approach to both source (COBOL) and target (Java) technologies

### Work Minimization Strategy

- Leverage existing patterns and examples where possible
- Focus on functional equivalence rather than architectural perfection
- Use simple, well-documented approaches to minimize learning curve
- Prioritize code readability and maintainability over technical elegance