# Final Tests Before Continuing Migration

## Coverage Analysis (SonarQube)
**Current**: 31.9% overall coverage (279/396 uncovered lines)

## Critical Priority Tests (0% Coverage)

### 1. JdbcTransactionRepository.java - CRITICAL
- 90 uncovered lines, complexity 17
- Risk: Transaction integrity, money movement
- Focus: Transaction CRUD, balance calculations, error handling

### 2. JdbcAccountRepository.java - CRITICAL  
- 65 uncovered lines, complexity 19 (highest)
- Risk: Account data integrity, balance management
- Focus: Account operations, balance updates, data consistency

### 3. JdbcCustomerRepository.java - HIGH
- 46 uncovered lines, complexity 13
- Risk: Customer data integrity
- Focus: Customer lookup, COBOL data mapping, error handling

### 4. Controllers - MEDIUM
- StatusController: 19 uncovered lines
- UtilityController: 10 uncovered lines
- Focus: API endpoints, health checks

## Well-Tested 
- JdbcControlRepository: 98.4% coverage
- Services: 100% coverage

## Missing DTO Tests (New Implementation)

### 5. DTO Layer Tests - NEW
- **DtoMapperTest.java** - Conversion logic between domain models and DTOs
- **CustomerRequestDtoTest.java** - Validation tests for customer input
- **AccountRequestDtoTest.java** - Validation tests for account creation
- **TransactionRequestDtoTest.java** - Validation tests for transaction input
- Focus: Field mapping accuracy, validation rules, edge cases

### 6. Future Controller Integration Tests - PLANNED
- **Controller tests using DTOs** (when controllers are updated)
- **API contract tests** (request/response DTO validation)
- **End-to-end HTTP tests** with proper DTO serialization
- Focus: API layer reliability, DTO integration

## Strategy
1. **Repository layer first** (highest business risk)
2. **Error scenarios** (migration failure points)
3. **Data integrity** (preserve banking rules)
4. **DTO tests** (new layer validation)
5. **Integration tests** for controllers