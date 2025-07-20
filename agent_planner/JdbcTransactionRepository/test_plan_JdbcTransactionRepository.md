# JdbcTransactionRepository Test Plan

**Core Function**: Handles banking transaction CRUD operations with composite ID parsing and logical deletes - essential for transaction integrity and audit trails.

## Test Strategy
- **Database**: H2 in-memory (following JdbcControlRepositoryTest pattern)
- **Approach**: Integration tests using `@JdbcTest` with real SQL execution
- **Scope**: Core functionality only, edge cases deferred

## High-Priority Test Methods

### 1. **Save Operations** (Highest Risk)
- `testSave_NewTransaction_InsertsRecord()` - Verify INSERT path works
- `testSave_ExistingTransaction_UpdatesRecord()` - Verify UPDATE path works
- `testSave_WithValidData_ReturnsTransaction()` - Data integrity check

### 2. **Find Operations** (Critical for Queries)
- `testFindById_WithValidCompositeId_ReturnsTransaction()` - Core lookup functionality
- `testFindById_WithInvalidCompositeId_ReturnsEmpty()` - ID parsing validation
- `testFindByAccount_WithTransactions_ReturnsOrderedList()` - Account transaction history

### 3. **Delete Operations** (Banking Audit Requirements)
- `testDeleteById_WithValidId_ReturnsTrue()` - Physical delete functionality
- `testMarkAsDeleted_WithValidId_SetsLogicalFlag()` - Logical delete (audit trail)
- `testMarkAsDeleted_PreservesTransactionData()` - Ensure data retention

### 4. **Repository Utility Methods**
- `testFindAll_ReturnsAllTransactions()` - Bulk operations
- `testCount_ReturnsCorrectCount()` - Reporting functionality

### 5. **Integration Test**
- `testCompleteTransactionLifecycle_CreateUpdateDelete()` - End-to-end workflow

## Test Data Strategy
- **Basic Valid Transactions**: Credit, Debit, Transfer types
- **Composite ID Format**: "987654123456789-2024-01-15-12:30:45-123456"
- **Date/Time**: Standard banking hours, ISO format
- **Amounts**: Typical banking amounts (avoid boundary testing for now)

## Success Criteria
- Cover all public methods with core happy path scenarios
- Achieve 80%+ line coverage for JdbcTransactionRepository
- Verify SQL operations execute correctly against H2
- Ensure composite ID parsing works reliably
- Validate logical vs physical delete behavior

## Deferred for Later Pass
- Edge cases (invalid dates, extreme amounts, malformed IDs)  
- Error handling (database connection failures, constraint violations)
- Performance testing with large datasets
- Concurrent transaction scenarios
