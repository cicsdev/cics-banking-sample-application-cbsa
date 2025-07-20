---
trigger: model_decision
description: Hard-won guidelines for maintaing a testing framework that holds our migration effort together.
---

# Testing Pitfalls Prevention Guide
*For COBOL-to-Java Migration Developers*

## 🚨 **Critical Testing Pitfalls to Avoid**

### 1. **Mock-Heavy Testing Trap**
**❌ What to Watch For:**
- Tests that use `@MockBean` for everything
- SonarQube showing 0% coverage despite "passing" tests
- Tests that never touch real database or HTTP endpoints

**✅ Prevention:**
- If testing a controller, use `@SpringBootTest` with real HTTP calls
- If testing a repository, use real database (H2 or test SQLite)
- Mock only external dependencies (APIs, file systems), not your own code

### 2. **Schema Drift Between Test and Production**
**❌ What to Watch For:**
If new test data needed: FIRST try using existing schema with @BeforeEach setup. ONLY if you need new COBOL-mapped fields: (1) Add columns with DEFAULT values, (2) Update both schema files identically, (3) Test with mvn test before touching production, (4) Document the COBOL mapping reason."

Red Flags - DON'T DO THIS:
❌ Adding columns just for test convenience
❌ Removing existing columns
❌ Changing data types of existing fields
❌ Adding NOT NULL without DEFAULT
❌ Schema changes for "cleaner" test data

If after thinking about it you still need a schema change:
Update both schema files, then either (1) restart app with mvn spring-boot:run to auto-apply schema changes, or (2) use dev profile with separate DB file, or (3) just run mvn test to verify H2 tests work with new schema."

### 3. **Test Database Configuration Chaos**
**❌ What to Watch For:**
- Hardcoded database paths in test code
- Tests interfering with each other (shared state)
- Production database being modified by tests

**✅ Prevention:**
- Configure via Spring profiles: `@ActiveProfiles("test")`
- Clean database state between tests with `@Transactional` + `@Rollback`

---

## 🎯 **Test Strategy Decision Matrix**

### **When to Use Unit Tests:**
- **Pure business logic** (no database, no HTTP)
- **Utility functions** (`CobolConverter`, date formatters)
- **Service classes** with simple logic (`CompanyInfoService`)
- **Model validation** (getters/setters, validation annotations)

**Example**: `CompanyInfoServiceTest` - tests COBOL logic conversion without database

### **When to Use Integration Tests:**
- **Repository classes** (SQL execution, data mapping)
- **Controller endpoints** (HTTP requests/responses)
- **Database constraints** (foreign keys, unique constraints)
- **End-to-end COBOL program equivalence**

**Example**: `JdbcControlRepositoryTest` - tests real SQL against H2 database

### **When to Use Both (Hybrid Approach):**
Most Java artifacts in this migration will need **both** because they bridge COBOL business logic with modern database/web patterns.

**Typical Pattern for Repository Classes:**
```java
// Unit tests for business logic methods
@Test void validateAccountNumber() { /* no database */ }

// Integration tests for database operations  
@Test void findAccountById_WithRealDatabase() { /* H2 or SQLite */ }
```

---

## 🗄️ **Database Testing Strategy**

### **H2 In-Memory vs Test SQLite Decision Tree:**

**Use H2 In-Memory When:**
- ✅ Fast unit tests for repository methods
- ✅ CI pipeline (no file system dependencies)
- ✅ Testing SQL logic without SQLite-specific features
- ✅ Parallel test execution

**Use Test SQLite When:**
- ✅ Integration tests that must match production exactly
- ✅ Testing SQLite-specific constraints/behaviors
- ✅ COBOL program migration verification
- ✅ Manual debugging of test data

### **Test Database Update Process:**
**⚠️ CRITICAL: Never modify test database directly**

**Correct Process:**
1. Update `src/main/resources/db/schema.sql` (single source of truth)
2. Delete existing test database file: `rm test-banking.db`
3. Run tests - Spring Boot will recreate from schema.sql
4. Verify tests still pass with new schema

**Wrong Process (Causes Schema Drift):**
- ❌ Manually editing test database with SQL tools
- ❌ Creating separate test schema files
- ❌ Using different table structures in tests

---

## 📋 **Testing Checklist for New COBOL Migrations**

### **Before Writing Tests:**
- [ ] Identify which layer you're testing (service, repository, controller)
- [ ] Check if similar test already exists (follow established patterns)
- [ ] Verify database schema is up-to-date in `schema.sql`

### **During Test Development:**
- [ ] Use appropriate test type (unit vs integration)
- [ ] Follow naming convention: `ClassNameTest` or `ClassNameIntegrationTest`
- [ ] Test both happy path and error conditions
- [ ] Verify test data doesn't leak between tests

### **After Writing Tests:**
- [ ] Run SonarQube analysis to verify real coverage increase
- [ ] Check that tests pass in isolation (`mvn test -Dtest=YourTest`)
- [ ] Verify production behavior matches test behavior

---

## 🎯 **COBOL-to-Java Specific Testing Patterns**

### **Pattern 1: COBOL Program Equivalence Testing**
```java
@Test
void cobolProgramEquivalence_GETCOMPY() {
    // Test that Java service returns same result as COBOL program
    String javaResult = companyInfoService.getCompanyName();
    assertEquals("CBSA Bank", javaResult); // Known COBOL output
}
```

### **Pattern 2: Database Migration Verification**
```java
@Test
void cobolDataStructure_AccountEntity() {
    // Verify Java entity matches COBOL copybook structure
    Account account = new Account();
    // Test all 12 fields from ACCOUNT.cpy are present
    assertNotNull(account.getAccountNumber());
    assertNotNull(account.getSortCode());
    // ... verify all COBOL fields mapped correctly
}
```

### **Pattern 3: Multi-Tier Database Testing**
```java
@SpringBootTest
@ActiveProfiles("test")
class AccountRepositoryIntegrationTest {
    
    @Test
    void findAccount_MatchesCobolVsamBehavior() {
        // Test that SQL query returns same data structure as COBOL VSAM
        Account account = accountRepository.findById(123);
        // Verify eye-catcher, composite keys, logical delete patterns
    }
}
```

---

## 🚨 **Red Flags to Stop and Fix Immediately**

1. **Coverage Metrics Don't Make Sense**
   - SonarQube shows 0% but tests are "passing"
   - Coverage decreases after adding tests

2. **Test Database Issues**
   - Tests fail when run in different order
   - Production database gets test data
   - Schema errors only in production

3. **Architectural Violations**
   - Multiple Account/Customer/Transaction models
   - Tests importing from different package hierarchies
   - Duplicate repository implementations

4. **Mock Overuse**
   - Every dependency is mocked
   - Tests pass but real endpoints fail
   - No integration between layers tested

---

## 📚 **Reference Patterns**

**✅ Good Examples in Codebase:**
- `CompanyInfoServiceTest` - Clean unit test
- `JdbcControlRepositoryTest` - Proper integration test with H2
- `schema.sql` - Single source of truth for database structure

**❌ Avoid These Patterns:**
- Mock-heavy controller tests
- Duplicate table creation in test code
- Hardcoded database paths
- Tests that don't increase real coverage

---

*This guide should be updated as we discover new pitfalls during the remaining 26 COBOL program migrations.*