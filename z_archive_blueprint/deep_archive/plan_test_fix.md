# Mini Test Framework Rebuild Plan

## Context
**Status**: Clean slate achieved - all broken tests deleted + test data infrastructure cleared  
**Goal**: Build minimal, solid, extensible foundation for 26 COBOL program migrations  
**Approach**: "Mini version" with at least one example of each test type

**Recent Cleanup**:
- ✅ Deleted `TestDataGenerator.java` (583 lines with architectural violations)
- ✅ Deleted `DataController.java` (test data REST API)
- ✅ Deleted `TestDataGenerationRunner.java` (CLI test data runner)
- ✅ Clean foundation ready for proper schema.sql-based approach

## Framework Components

### 1. Test Infrastructure Setup
1. Add JaCoCo Maven plugin for coverage reporting
2. Add SonarQube Maven plugin for coverage monitoring
3. **Multi-Tier Database Strategy**:
   - **H2 In-Memory**: Fast unit repository tests, CI pipeline
   - **Test SQLite** (`test-banking.db`): Integration tests, migration verification
   - **Production SQLite** (`banking.db`): Read-only manual testing only
4. **Test DB Setup Consistency**: Use same `schema.sql` approach as production (not duplicate `create*Table()` methods)
5. Create `TestDataGeneratorConfig` (adapt existing generator)
6. Create hybrid base test classes:
   - `BaseIntegrationTest` (shared DB setup)
   - `BaseUnitTest` (lightweight, no DB)

### 2. Four Test Type Examples
1. **Unit Test**: `CompanyInfoServiceTest` ✅ **COMPLETED**
   - Pure logic testing (COBOL GETCOMPY service)
   - No database dependencies
   - **Status**: Test created, passing, logged in mapping_log.md
   - **Coverage Impact**: 0.2%

2. **Integration Test**: `StatusControllerIntegrationTest` ⏳ **NEXT**
   - Real HTTP endpoints + SQLite + all repositories
   - Full end-to-end validation
   - **Blocker**: Need test database setup strategy

3. **Utility Test**: `CobolConverterTest` 🎯 **SIMPLE ALTERNATIVE**
   - Pure utility functions (no database needed)
   - COBOL data conversion logic testing
   - **Benefit**: Higher coverage gain, no DB complexity

4. **Repository Test**: `JdbcAccountRepositoryTest` ⏳ **LATER**
   - SQLite + SQL execution validation
   - Database layer testing
   - **Dependency**: Requires test DB infrastructure

### 3. Test Data Strategy
1. **CREATE** new `TestDataGenerator` from scratch:
   - Use `schema.sql` for table creation (NO duplicate `create*Table()` methods)
   - Focus only on realistic data generation
   - Preserve business logic patterns from deleted version (interest rates, overdraft limits, etc.)
2. **Mirror Production Database Management**:
   - Same schema initialization approach (`schema.sql` + `ScriptUtils.executeSqlScript`)
   - Same table structures, constraints, and relationships
   - Consistent data types and field validations
   - Prevents schema drift between test and production environments
3. Create test-specific configuration
4. Add cleanup/setup methods for test isolation
5. Separate test database prevents production data conflicts

## Implementation Order
1. **Maven Configuration**: Add JaCoCo plugin for coverage ✅ **DONE**
2. **First Test**: `CompanyInfoServiceTest` (simple unit test) ✅ **DONE**
3. **Mapping Log**: Track COBOL→Java mappings ✅ **DONE**
4. **Next Simple Test**: `CobolConverterTest` (utility functions) ⏳ **CURRENT**
5. **Test Infrastructure**: Base classes and database configuration ⏳ **UPCOMING**
6. **Database Tests**: Integration and Repository tests ⏳ **LATER**
7. **Coverage Verification**: Ensure robust JaCoCo reporting ⏳ **ONGOING**
8. **Documentation**: Patterns for future COBOL migrations tests ⏳ **CONTINUOUS**

## Success Criteria
- ✅ JaCoCo coverage reporting functional (0.2% baseline)
- ✅ First unit test working (`CompanyInfoServiceTest`)
- ✅ Mapping log created and maintained
- ⏳ At least 4 different test types working
- ⏳ Coverage > 5% (meaningful baseline)
- ⏳ Clean test isolation patterns established
- ⏳ Foundation ready for 26 COBOL program migrations
- ✅ Clean test isolation
- ✅ Extensible patterns established
- ✅ Foundation ready for 26 COBOL program migrations

**Timeline**: Build foundation now before application complexity increases