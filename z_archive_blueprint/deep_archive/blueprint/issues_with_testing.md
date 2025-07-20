# Plan to Fix Scaffolding Issues

# Most Recently Discovered Issues - Start Here

### Executive Summary

Our investigation revealed critical insights about the migration status:

1. **GETCOMPY & GETSCODE**: ✅ Actually working and production-ready
2. **INQACC**: ❌ Multiple serious architectural issues beyond initial assessment
3. **Root Cause**: Package structure inconsistencies and disconnected components

## Detailed Findings

### ✅ Programs That Actually Work

#### GETCOMPY.cbl → CompanyInfoService
- **Status**: FULLY FUNCTIONAL
- **Evidence**: Live endpoint returns `{"companyName": "CICS Bank Sample Application"}`
- **Architecture**: Proper service → controller → endpoint integration
- **Package**: `com.cbsa.migration.*` (correctly scanned by Spring Boot)

#### GETSCODE.cbl → SortCodeService  
- **Status**: FULLY FUNCTIONAL
- **Evidence**: Live endpoint returns `{"sortCode": "987654"}`
- **Architecture**: Proper service → controller → endpoint integration
- **Package**: `com.cbsa.migration.*` (correctly scanned by Spring Boot)

### ❌ INQACC: Critical Issues Discovered

#### Issue 1: Package Scanning Problem
- **Problem**: `InquiryAccountController` is in `com.bank.cbsa.inqacc.*`
- **Root Cause**: Spring Boot's `@SpringBootApplication` only scans `com.cbsa.migration.*`
- **Impact**: Controller not registered, endpoint returns 404
- **Evidence**: `curl http://localhost:8085/api/inqacc` → 404 Not Found

#### Issue 2: Controller Implementation
- **Problem**: Controller is a complete stub returning `null` with `NOT_IMPLEMENTED`
- **Code**: `@ResponseStatus(HttpStatus.NOT_IMPLEMENTED)` with TODO comment
- **Impact**: Even if scanned, would return HTTP 501

#### Issue 3: Repository Pattern Duplication
- **Problem**: Two different `AccountRepository` interfaces:
  - `com.cbsa.migration.repository.AccountRepository` ✅ (working with JdbcAccountRepository)
  - `com.bank.cbsa.inqacc.repository.AccountRepository` ❌ (stub interface)
- **Impact**: Service implementation can't connect to working repository

#### Issue 4: Testing Deception
- **Problem**: Unit tests pass because they mock the missing components
- **Impact**: Tests don't reflect actual HTTP endpoint functionality
- **Evidence**: Tests pass but real endpoint fails
- **SonarQube Confirmation**: 0% test coverage despite 24 passing tests (255ms execution time)
- **Root Cause**: Tests use mocks exclusively, never execute production code paths

## Fix Strategy: Two Approaches

### Approach A: Quick Bridge Fix (Recommended)
**Timeline**: 2-3 hours  
**Risk**: Low  
**Goal**: Get INQACC working immediately

1. **Fix Package Scanning**
   - Add `@ComponentScan(basePackages = {"com.cbsa.migration", "com.bank.cbsa.inqacc"})` to `BankingApplication`
   - Alternative: Move INQACC controller to `com.cbsa.migration.controller` package

2. **Implement Controller Method**
   - Replace stub implementation with actual service call
   - Remove `@ResponseStatus(HttpStatus.NOT_IMPLEMENTED)`
   - Add proper error handling for account not found scenarios

3. **Bridge Repository Pattern**
   - Create adapter to connect INQACC service to working `JdbcAccountRepository`
   - Maintain existing `InquiryAccountService` interface

4. **Add Integration Tests**
   - Create `@SpringBootTest` tests that verify actual HTTP endpoints
   - Test with real database queries, not mocks

### Approach B: Architectural Cleanup (Long-term)
**Timeline**: 1-2 weeks  
**Risk**: Medium  
**Goal**: Standardize architecture across all modules

1. **Choose Single Repository Pattern**
   - Standardize on `com.cbsa.migration.repository.*` pattern
   - Migrate all services to use consistent data access layer

2. **Unify Package Structure**
   - Move all controllers to `com.cbsa.migration.controller`
   - Move all services to `com.cbsa.migration.service`
   - Update imports and dependencies

3. **Enhance Data Models**
   - Expand `AccountDto` with all COBOL fields
   - Create comprehensive mapping between database and DTO models

4. **Comprehensive Testing Strategy**
   - Replace mock-heavy unit tests with integration tests
   - Add end-to-end HTTP endpoint testing
   - Verify COBOL business logic compatibility

## Recommended Implementation Plan

### ✅ Phase 1: Immediate Fix (COMPLETED) 
1. ✅ Chose Approach A for quick resolution
2. ✅ INQACC endpoint now works with real data
3. ✅ Verified no regression in GETCOMPY/GETSCODE

**Implementation Summary:**
- Added `@ComponentScan(basePackages = {"com.cbsa.migration", "com.bank.cbsa.inqacc"})` to BankingApplication  
- Replaced controller stub with `return service.inquireAccount(sortCode, accountNumber);`
- Renamed INQACC repository bean to avoid conflicts
- **Result**: INQACC went from 404 errors to functional JSON responses

### 🔄 Phase 2: Integration Testing (CURRENT PRIORITY)
**Goal**: Address 0% SonarQube test coverage issue
1. **Add integration tests** using `@SpringBootTest` for actual HTTP endpoints
2. **Create end-to-end tests** with TestRestTemplate for INQACC, GETCOMPY, GETSCODE  
3. **Test real database scenarios** instead of mock-only tests
4. **Validate COBOL business logic parity** with actual data

### Phase 3: Architecture Review (Next Sprint)
1. Decide on long-term package structure
2. Plan migration of future COBOL programs  
3. Create architectural guidelines for consistency

## Success Criteria

### Immediate Success (Phase 1) - ✅ COMPLETED
- ✅ `curl http://localhost:8085/api/inqacc?sortCode=987654&accountNumber=1000000000` returns account data
- ✅ `curl http://localhost:8085/api/inqacc?sortCode=987654&accountNumber=9999999999` returns "not found" response
- ✅ GETCOMPY and GETSCODE endpoints still work
- ✅ All existing tests continue to pass

### **NEW: Phase 2 Success Criteria**
- [ ] SonarQube test coverage improves from 0% to >50%
- [ ] Integration tests for all 3 working endpoints (INQACC, GETCOMPY, GETSCODE)
- [ ] End-to-end database testing with real data scenarios
- [ ] Tests validate actual HTTP responses, not mocks

## Key Learnings

1. **"Passing Tests" ≠ "Working Software"**: Mock-heavy tests can hide integration issues ✅ **CONFIRMED**
2. **Package Structure Matters**: Spring Boot component scanning affects what gets registered ✅ **CONFIRMED**  
3. **End-to-End Testing Essential**: HTTP endpoint testing reveals real functionality ✅ **CONFIRMED**
4. **Architecture Consistency Critical**: Multiple repository patterns create confusion ✅ **CONFIRMED**
5. **Verification Over Assumption**: Always test actual endpoints, not just unit tests ✅ **CONFIRMED**
6. **NEW: SonarQube Coverage Requires Integration Tests**: Unit tests with mocks don't improve coverage metrics

## Next Steps (Updated)

**IMMEDIATE (This Session):**
1. **Create integration tests** for INQACC endpoint to improve SonarQube coverage
2. **Add database integration tests** to validate actual account lookup scenarios
3. **Test edge cases** like invalid accounts, missing data, COBOL sentinel values

**THIS WEEK:**
1. **Expand integration testing** to GETCOMPY and GETSCODE endpoints
2. **Create comprehensive test scenarios** covering all business logic paths
3. **Validate test coverage improvement** in SonarQube (target: >50%)

**ARCHITECTURAL (Next Sprint):**
1. **Decide package structure standard** for future COBOL migrations
2. **Document integration testing approach** as template for remaining 26 programs
3. **Plan next COBOL program migration** using lessons learned

#TODO ^this
