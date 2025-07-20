# Architectural Governance for COBOL-to-Java Migration

## Overview
This document outlines key architectural governance questions and potential solutions to prevent the duplication and architectural chaos discovered in the current CBSA migration project.

## Critical Governance Questions & Solutions

### 1. **Scope & Enforcement Level**

**QUESTION:** Do you want **mandatory checkpoints** that block the developer from proceeding, or **advisory guidelines** with rationale? Should these steps be integrated into the build process (Maven plugin, pre-commit hooks) or remain as documentation?

**POSSIBLE SOLUTIONS:**

**Option A: Mandatory Build Integration**
- Maven plugin that scans for architectural violations before compilation
- Pre-commit Git hooks that check package structure and naming conventions
- CI/CD pipeline gates that fail builds on governance violations
- **Pros:** Enforces compliance, prevents technical debt accumulation
- **Cons:** May slow development, requires tooling investment

**Option B: Advisory Guidelines with Reviews**
- Documentation-based guidelines with architectural review checkpoints
- Peer review process focusing on architectural compliance
- Regular architectural debt reviews and cleanup sprints
- **Pros:** Flexible, educational, less disruptive to development flow
- **Cons:** Relies on discipline, violations may slip through

**RECOMMENDED APPROACH:** Start with Option B (advisory) during active migration, transition to Option A (mandatory) once patterns are established.

---

### 2. **Package Structure Strategy**

**QUESTION:** Should all new COBOL program migrations use the **existing** `com.cbsa.migration.*` pattern, or create **program-specific** packages like `com.cbsa.migration.{program-name}.*`? How do you want to handle the existing `com.bank.cbsa.inqacc.*` pattern?

**POSSIBLE SOLUTIONS:**

**Option A: Unified Migration Package**
```
com.cbsa.migration/
├── model/           # All shared entities (Account, Customer, etc.)
├── repository/      # All data access interfaces
├── repository/jdbc/ # All JDBC implementations
├── service/         # All business services
└── controller/      # All REST endpoints
```
- **Pros:** Simple, consistent, easier Spring Boot scanning
- **Cons:** Potential for large packages, naming conflicts

**Option B: Program-Specific Sub-packages**
```
com.cbsa.migration/
├── shared/          # Common models, utilities
├── inqacc/          # INQACC-specific components
├── getcompy/        # GETCOMPY-specific components
└── proctran/        # PROCTRAN-specific components
```
- **Pros:** Clear separation, easier to understand program boundaries
- **Cons:** More complex Spring configuration, potential duplication

**Option C: Hybrid Approach**
```
com.cbsa.migration/
├── shared/
│   ├── model/       # Shared entities (Account, Customer)
│   ├── repository/  # Shared data access
│   └── service/     # Common business logic
└── program/
    ├── inqacc/      # Program-specific controllers, DTOs
    ├── getcompy/    # Program-specific logic
    └── proctran/    # Program-specific components
```
- **Pros:** Best of both worlds, clear shared vs specific separation
- **Cons:** Most complex, requires discipline

**RECOMMENDED APPROACH:** Option C (Hybrid) - promotes reuse while maintaining clear boundaries.

**Existing `com.bank.cbsa.inqacc.*` Handling:**
- **Migrate Gradually:** Move components to new structure during next refactoring
- **Update Component Scanning:** Ensure `@ComponentScan` includes all patterns during transition
- **Set Deprecation Timeline:** 2-3 sprints to complete migration

---

### 3. **Shared Entity Governance**

**QUESTION:** When migrating a new COBOL program that touches **existing entities** (like Account, Customer), should developers always extend the existing model, create program-specific DTOs, or both?

**POSSIBLE SOLUTIONS:**

**Option A: Master Entity Pattern**
- Single `Account` model with all fields from all COBOL programs
- All repositories return the master entity
- Program-specific views created through DTOs/mappers
- **Pros:** Single source of truth, no data inconsistency
- **Cons:** Large entities, potential for unused fields

**Option B: Program-Specific Entities with Mapping**
- Each program maintains its own entity model
- Mapping layer converts between models and database
- Shared database schema with comprehensive fields
- **Pros:** Clear program boundaries, smaller entities
- **Cons:** Mapping complexity, potential inconsistencies

**Option C: Core + Extension Pattern**
```java
// Core shared entity
public class Account {
    // Common fields used by 80%+ of programs
    private String sortCode;
    private String accountNumber;
    private BigDecimal balance;
}

// Program-specific extensions
public class InqaccAccount extends Account {
    // INQACC-specific fields
    private String customField;
}
```
- **Pros:** Balances reuse and specialization
- **Cons:** Inheritance complexity, database mapping challenges

**RECOMMENDED APPROACH:** Option A (Master Entity) with careful field analysis and optional field patterns.

**Implementation Guidelines:**
1. **Field Addition Criteria:** Field must be used by 2+ COBOL programs OR core to business logic
2. **Optional Field Handling:** Use `@Nullable` annotations and null checks
3. **Version Control:** Track which program added which fields
4. **Regular Reviews:** Quarterly reviews to consolidate or remove unused fields

---

### 4. **Repository Pattern Standardization**

**QUESTION:** Should all repositories follow the **interface + JDBC implementation** pattern, or allow direct implementations for simple cases? Should method naming follow **JPA conventions** even when using plain JDBC?

**POSSIBLE SOLUTIONS:**

**Option A: Strict Interface + Implementation Pattern**
```java
public interface AccountRepository {
    Optional<Account> findBySortCodeAndAccountNumber(String sortCode, String accountNumber);
}

@Repository
public class JdbcAccountRepository implements AccountRepository {
    // JDBC implementation
}
```
- **Pros:** Testable, mockable, consistent patterns
- **Cons:** More boilerplate for simple cases

**Option B: Allow Direct Implementation for Simple Cases**
```java
@Repository
public class SimpleDataRepository {
    // Direct implementation for read-only/simple operations
    public String getCompanyName() { return "CICS Bank"; }
}
```
- **Pros:** Less boilerplate, faster development
- **Cons:** Inconsistent patterns, harder to test

**Option C: Complexity-Based Pattern Selection**
- **Simple/Static Data:** Direct implementation (like CompanyInfoService)
- **CRUD Operations:** Interface + implementation pattern
- **Complex Business Logic:** Interface + implementation with separate service layer

**RECOMMENDED APPROACH:** Option C with clear criteria:
- **0-1 Database Tables + Simple Logic:** Direct implementation
- **2+ Database Tables OR Business Logic:** Interface + implementation
- **Cross-Program Shared Data:** Always interface + implementation

**Method Naming Standards:**
- Use JPA conventions even with JDBC: `findBy...`, `save()`, `deleteBy...`
- Program-specific methods: `findAccountForInqacc()` vs generic `findAccount()`
- Consistent parameter naming: `sortCode`, `accountNumber` (never `sort_code`, `acct_num`)

---

### 5. **Integration Points & Endpoint Management**

**QUESTION:** How should developers handle **controller/endpoint conflicts** when multiple programs might expose similar functionality? Should there be a **shared service layer** for common banking operations?

**POSSIBLE SOLUTIONS:**

**Option A: Program-Specific Endpoints**
```
GET /api/inqacc/account/{sortCode}/{accountNumber}
GET /api/proctran/account/{sortCode}/{accountNumber}
GET /api/viewacct/account/{sortCode}/{accountNumber}
```
- **Pros:** Clear program boundaries, no conflicts
- **Cons:** API proliferation, client confusion

**Option B: Unified Business APIs**
```
GET /api/accounts/{sortCode}/{accountNumber}
GET /api/customers/{sortCode}/{customerNumber}
POST /api/transactions
```
- **Pros:** Clean client API, business-focused
- **Cons:** Complex routing, business logic consolidation needed

**Option C: Hybrid Program + Business APIs**
```
# Program-specific (for migration/testing)
GET /api/program/inqacc/account/{sortCode}/{accountNumber}

# Business-unified (for production clients)
GET /api/accounts/{sortCode}/{accountNumber}
```
- **Pros:** Supports both migration needs and production APIs
- **Cons:** Dual maintenance, potential confusion

**RECOMMENDED APPROACH:** Option C during migration, transition to Option B for production.

**Shared Service Layer Strategy:**
```java
// Business service layer
@Service
public class AccountService {
    public Account getAccount(String sortCode, String accountNumber) {
        // Unified business logic
    }
}

// Program-specific services delegate to business services
@Service
public class InqaccService {
    @Autowired
    private AccountService accountService;
    
    public AccountDto inquireAccount(String sortCode, String accountNumber) {
        Account account = accountService.getAccount(sortCode, accountNumber);
        return mapToInqaccDto(account); // Program-specific transformation
    }
}
```

---

### 6. **Testing Requirements & Quality Gates**

**QUESTION:** What level of **integration testing** should be mandatory vs optional? Should mock-heavy testing be discouraged in favor of real endpoint testing?

**POSSIBLE SOLUTIONS:**

**Option A: Comprehensive Integration Testing**
- Every endpoint must have integration test with real database
- Repository tests must use actual database connections
- Service tests must use real dependencies
- **Pros:** High confidence, catches integration issues
- **Cons:** Slower test execution, more complex setup

**Option B: Layered Testing Strategy**
- **Unit Tests:** Mock heavy for business logic
- **Integration Tests:** Real database for repository layer
- **End-to-End Tests:** Full application stack for critical paths
- **Pros:** Balanced speed vs confidence
- **Cons:** Requires discipline to maintain all layers

**Option C: Risk-Based Testing**
- **High-Risk Components:** Full integration testing
- **Simple Components:** Unit tests with mocks
- **Shared Components:** Mandatory integration tests
- **Program-Specific:** Developer choice

**RECOMMENDED APPROACH:** Option B with specific requirements:

**Mandatory Testing Requirements:**
1. **Repository Layer:** Integration tests with real database
2. **Shared Services:** Integration tests (impacts multiple programs)
3. **REST Endpoints:** At least one happy path integration test
4. **Data Migration:** Full end-to-end validation

**Optional but Recommended:**
1. **Unit Tests:** For complex business logic
2. **Mock Tests:** For external dependency scenarios
3. **Performance Tests:** For high-volume endpoints

**Test Quality Gates:**
- **Minimum Coverage:** 70% line coverage
- **Integration Coverage:** 100% of repository methods
- **Endpoint Coverage:** 100% of public endpoints
- **No @Disabled Tests:** In main branch (use feature branches for WIP)

---

### 7. **Migration Dependencies & Sequencing**

**QUESTION:** Should developers **wait for shared entity consolidation** before starting their migration, or proceed with temporary duplication that gets cleaned up later?

**POSSIBLE SOLUTIONS:**

**Option A: Consolidation-First Approach**
1. Pause new migrations
2. Consolidate existing Account/Customer/Transaction models
3. Establish standard patterns
4. Resume migrations with clean foundation
- **Pros:** Clean foundation, no future consolidation debt
- **Cons:** Blocks progress, delays business value

**Option B: Parallel Migration with Cleanup Sprints**
1. Continue migrations with temporary duplication
2. Track architectural debt in backlog
3. Scheduled consolidation sprints every 2-3 programs
4. Maintain working code throughout
- **Pros:** Maintains velocity, delivers business value
- **Cons:** Accumulates technical debt, consolidation complexity

**Option C: Controlled Duplication Strategy**
1. Allow duplication for new entities only
2. Require extending existing shared entities
3. Program-specific DTOs allowed for view concerns
4. Monthly architectural reviews
- **Pros:** Balances progress with governance
- **Cons:** Requires strong architectural discipline

**RECOMMENDED APPROACH:** Option C with clear rules:

**Duplication Rules:**
- ✅ **ALLOWED:** Program-specific DTOs, view models, controllers
- ✅ **ALLOWED:** New entities not used by existing programs
- ❌ **FORBIDDEN:** Duplicate repositories for same database table
- ❌ **FORBIDDEN:** Duplicate core business models (Account, Customer, Transaction)
- ❌ **FORBIDDEN:** Duplicate database access patterns

**Migration Sequence Strategy:**
1. **Phase 1:** Simple programs (GETCOMPY, GETSCODE) - establish patterns
2. **Phase 2:** Account-touching programs (INQACC, VIEWACCT) - consolidate Account model
3. **Phase 3:** Transaction programs (PROCTRAN, UPDACCT) - establish transaction patterns
4. **Phase 4:** Complex programs (remaining 20+) - use established patterns

---

## Implementation Checklist

### Before Starting Any COBOL Program Migration:

1. **📋 Entity Analysis**
   - [ ] Identify all COBOL copybooks/data structures used
   - [ ] Check if corresponding Java entities already exist
   - [ ] Document entity relationships and dependencies

2. **🏗️ Architecture Planning**
   - [ ] Choose package structure based on program complexity
   - [ ] Plan repository interfaces and implementation strategy
   - [ ] Design service layer integration points
   - [ ] Map REST endpoint structure

3. **🔍 Conflict Detection**
   - [ ] Scan for existing similar endpoints
   - [ ] Check for bean naming conflicts
   - [ ] Verify component scanning includes new packages
   - [ ] Review database schema for table conflicts

4. **🧪 Testing Strategy**
   - [ ] Plan repository integration tests
   - [ ] Design endpoint integration tests
   - [ ] Identify shared service testing needs
   - [ ] Document test data requirements

5. **📖 Documentation**
   - [ ] Document architectural decisions made
   - [ ] Update shared entity documentation
   - [ ] Record any temporary technical debt created
   - [ ] Plan future consolidation if needed

### During Development:

6. **✅ Development Standards**
   - [ ] Follow established naming conventions
   - [ ] Use standard Spring Boot annotations
   - [ ] Implement proper error handling
   - [ ] Add appropriate logging

7. **🔗 Integration Verification**
   - [ ] Verify Spring component scanning works
   - [ ] Test database connectivity and transactions
   - [ ] Validate endpoint responses
   - [ ] Check for memory leaks or resource issues

### After Migration Completion:

8. **🏁 Quality Gates**
   - [ ] All tests pass (unit + integration)
   - [ ] Code coverage meets minimum requirements
   - [ ] No disabled tests in main branch
   - [ ] Performance benchmarks acceptable

9. **📚 Knowledge Transfer**
   - [ ] Update system documentation
   - [ ] Record lessons learned
   - [ ] Share architectural patterns with team
   - [ ] Plan technical debt paydown if needed

---

## Conclusion

These governance questions and solutions provide a framework for preventing the architectural chaos discovered in the current CBSA migration. The key is balancing **migration velocity** with **architectural quality**, using a pragmatic approach that allows controlled technical debt while preventing systemic duplication.

**Next Steps:**
1. Review and discuss these options with the development team
2. Select preferred approaches for each governance area
3. Update this document with team decisions
4. Implement chosen governance mechanisms
5. Begin applying governance to remaining COBOL program migrations

__

Creating an audit of sorts?