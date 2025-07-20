---
trigger: model_decision
description: When making a plan to migrate cobol program to java
---

# COBOL-to-Java Migration Rules

## Program Classification & Mapping

### Screen Programs (BMS maps, CICS SEND/RECEIVE MAP)
- **Create**: REST Controller in `com.cbsa.migration.controller`
- **Naming**: `{ProgramName}Controller` 
- **Exception**: Skip if screen logic is simple data display only

### Business Logic Programs (CRUD, validation, calculations)
- **Create**: Service class in `com.cbsa.migration.service`
- **Naming**: `{DomainName}Service` (not program name)
- **Exception**: Create utility class if program only contains static functions

### Data Access Programs (VSAM/DB2 operations)
- **Create**: Repository interface + JDBC implementation in `com.cbsa.migration.repository`
- **Naming**: `{EntityName}Repository` + `Jdbc{EntityName}Repository`
- **Exception**: Add methods to existing repository if same entity

## Data Structure Mapping

### COBOL Copybooks → Java Entities
- **Create**: Domain model in `com.cbsa.migration.model`
- **Naming**: Match copybook name (e.g., `CUSTOMER.cpy` → `Customer.java`)
- **Exception**: Use existing entity if copybook is subset of existing fields

### COMMAREA Structures → DTOs
- **Create**: DTO in `com.cbsa.migration.dto`
- **Naming**: `{ProgramName}Request/Response`
- **Exception**: Reuse existing DTO if structure is identical

## Package Placement Rules

- **Controllers**: `com.cbsa.migration.controller`
- **Services**: `com.cbsa.migration.service`  
- **Repositories**: `com.cbsa.migration.repository` (interfaces) + `com.cbsa.migration.repository.jdbc` (implementations)
- **Models**: `com.cbsa.migration.model`
- **DTOs**: `com.cbsa.migration.dto`
- **Exceptions**: `com.cbsa.migration.exception`

## Quick Decision Tree

1. **Does COBOL program handle screens?** → Controller + DTOs
2. **Does it perform file operations?** → Repository interface + JDBC impl
3. **Does it contain business logic?** → Service class
4. **Does it use copybooks?** → Domain models
5. **Does it call other programs?** → Service collaboration

## Critical Exceptions

- **Don't create** separate artifacts for simple utility functions (add to existing service)
- **Don't create** controllers for batch programs (use scheduled services)
- **Don't create** new entities for control/config copybooks (use existing Control class)
- **Don't create** repositories for read-only reference data (use configuration properties)

## Common Pitfalls & Gotchas

### Transaction Boundaries
- **COBOL**: Implicit transaction per program execution
- **Java**: Must explicitly add `@Transactional` to service methods
- **Gotcha**: Missing transactions cause partial updates

### Data Type Mismatches
- **COBOL**: COMP-3 packed decimals, PIC 9(8) numerics
- **Java**: Use `BigDecimal` for financial data, not `double`
- **Gotcha**: Precision loss with floating point types

### Pseudo-Conversational Pattern
- **COBOL**: Programs terminate and restart between user interactions
- **Java**: REST APIs are stateless - don't maintain conversation state
- **Gotcha**: Trying to preserve session data in instance variables

### Error Code Translation
- **COBOL**: RESP codes, ABEND codes, condition flags
- **Java**: Proper exception hierarchies with meaningful messages
- **Gotcha**: Returning error codes instead of throwing exceptions

### File vs Database Thinking
- **COBOL**: Sequential file processing, VSAM key access
- **Java**: Set-based SQL operations, JPA entity management
- **Gotcha**: Translating file browse loops to inefficient SQL queries

### Naming Conflicts
- **COBOL**: Hyphenated names (CUSTOMER-RECORD)
- **Java**: camelCase conventions (CustomerRecord)
- **Gotcha**: Reserved words like `class`, `interface`, `package`