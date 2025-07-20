# Code Coverage Strategy Plan

## 🎯 **CURRENT PROGRESS UPDATE**

**✅ MILESTONE ACHIEVED:** First Database Integration Test Complete!
- **JdbcControlRepository:** 98.4% coverage, 100% line coverage (49/49 lines)
- **Overall Project Coverage:** 31.9% (up from baseline)
- **Test Infrastructure:** H2 in-memory database setup ✅
- **Test Schema:** `/src/test/resources/db/test-schema.sql` ✅
- **12 comprehensive integration tests** passing ✅

---

## Testing Priorities (Start Small, Build Up)

### 1. **LOW-HANGING FRUIT** (Done!) 🍎
1. **CompanyInfoService** - Already has test, verify coverage
2. **SortCodeService** - Simple utility, easy to test
3. **Model Classes** - Account, Customer, Transaction (getters/setters)

### 2. **SINGLE-RESPONSIBILITY TESTS** (Database Integration) 🎯
1. **JdbcControlRepository** ✅ **COMPLETE** (98.4% coverage) - Database patterns established!
2. **Individual JdbcAccountRepository methods** (one at a time):
   1. `findById()` - Basic lookup (H2 test)
   2. `findByCustomerNumber()` - Query with parameters
   3. `count()` - Simple aggregation
   4. `save()` - Insert/update logic (most complex)
   5. `deleteById()` - Delete operations

### 3. **CONTROLLER LAYER** (Integration Focus) 🌐
1. **StatusController** - Database connectivity + JSON response
2. **UtilityController** - GETCOMPY/GETSCODE endpoints
3. **API endpoint testing** - HTTP requests/responses

### 4. **COMPLEX INTEGRATIONS** (Later) ⚡
1. **Full JdbcAccountRepository** - All methods together
2. **JdbcCustomerRepository** - Customer operations
3. **JdbcTransactionRepository** - Transaction handling
4. **End-to-end migration verification** - COBOL vs Java output

## Complete Java Source Structure Analysis

### **📁 FULL DIRECTORY BREAKDOWN** 
`java-migration/src/main/java/com/cbsa/migration/`

| Package | Files | Description | Test Priority |
|---------|-------|-------------|---------------|
| **📱 Main Application** | `BankingApplication.java` | Spring Boot main class | 🟡 MEDIUM |
| **⚙️ Config** | `DatabaseConfig.java`, `OpenApiConfig.java` | Configuration classes | 🟢 HIGH |
| **🌐 Controller** | `StatusController.java`, `UtilityController.java` | REST endpoints | 🟢 HIGH |
| **📋 Model** | 6 files (Account, Customer, etc.) | Data models | 🟢 HIGH |
| **🔌 Repository Interfaces** | 4 interfaces | Repository contracts | 🟡 MEDIUM |
| **💾 JDBC Implementations** | 4 implementations | Database access | 🔴 VARIES |
| **⚡ Service** | `CompanyInfoService.java`, `SortCodeService.java` | Business logic | 🟢 HIGH |
| **🔧 Util** | `CobolConverter.java` | Utility functions | 🟢 HIGH |

### **📋 MODEL CLASSES** 
`java-migration/src/main/java/com/cbsa/migration/model/`

| File | Purpose | Test Type | Priority | Notes |
|------|---------|-----------|----------|-------|
| **Account.java** | Account entity | Unit | 🟢 HIGH | Getters/setters, validation |
| **Customer.java** | Customer entity | Unit | 🟢 HIGH | Data model testing |
| **Transaction.java** | Transaction entity | Unit | 🟢 HIGH | Core business object |
| **Control.java** | Control records | Unit | 🟢 HIGH | Sequence numbers |
| **CompanyInfoResponse.java** | API response | Unit | 🟡 MEDIUM | JSON serialization |
| **SortCodeResponse.java** | API response | Unit | 🟡 MEDIUM | Response mapping |

### **⚙️ CONFIG CLASSES** 
`java-migration/src/main/java/com/cbsa/migration/config/`

| File | Purpose | Test Type | Priority | Notes |
|------|---------|-----------|----------|-------|
| **DatabaseConfig.java** | DB configuration | Integration | 🟢 HIGH | Connection setup |
| **OpenApiConfig.java** | Swagger/API docs | Unit | 🟡 MEDIUM | Documentation config |

### **🔌 REPOSITORY INTERFACES** 
`java-migration/src/main/java/com/cbsa/migration/repository/`

| File | Purpose | Test Type | Priority | Notes |
|------|---------|-----------|----------|-------|
| **AccountRepository.java** | Account contract | Unit | 🟡 MEDIUM | Interface validation |
| **CustomerRepository.java** | Customer contract | Unit | 🟡 MEDIUM | Method signatures |
| **TransactionRepository.java** | Transaction contract | Unit | 🟡 MEDIUM | Repository interface |
| **ControlRepository.java** | Control contract | Unit | 🟡 MEDIUM | Control operations |

### **💾 JDBC REPOSITORY IMPLEMENTATIONS**
`java-migration/src/main/java/com/cbsa/migration/repository/jdbc/`

| File | Size | Lines | Complexity | Test Priority | Notes |
|------|------|-------|------------|---------------|---------|
| **JdbcControlRepository.java** | 5,331 bytes | ~150 | ✅ **COMPLETE** | **98.4% COVERAGE** | Smallest, control records |
| **JdbcCustomerRepository.java** | 4,883 bytes | ~140 | 🟡 MEDIUM | **PHASE 2** | Customer operations |
| **JdbcAccountRepository.java** | 7,011 bytes | 186 | 🔴 HIGH | **PHASE 3** | Most complex, many methods |
| **JdbcTransactionRepository.java** | 9,419 bytes | ~250 | 🔴 VERY HIGH | **PHASE 4** | Largest, transaction logic |

## Database Strategy Breakdown

### **🚫 UNIT TESTS (No Database)**
| Component | Type | Why No DB | Priority |
|-----------|------|-----------|----------|
| **Model Classes** | Pure POJOs | Getters/setters only | 🟢 **START HERE** |
| **CompanyInfoService** | Hardcoded logic | Returns constants | 🟢 **EXISTING** |
| **SortCodeService** | Utility logic | String manipulation | 🟢 **HIGH** |
| **CobolConverter** | Utility functions | Data conversion | 🟢 **HIGH** |
| **Repository Interfaces** | Contract validation | Interface testing | 🟡 MEDIUM |
| **OpenApiConfig** | Configuration | Spring config beans | 🟡 MEDIUM |

### **💾 H2 IN-MEMORY TESTS (First Integration Tests)**
| Component | Database Type | Why H2 | Priority |
|-----------|---------------|--------|----------|
| **JdbcControlRepository** | H2 In-Memory | Fast CRUD testing | ✅ **COMPLETE (98.4%)** |
| **JdbcCustomerRepository** | H2 In-Memory | Repository patterns | 🟡 SECOND |
| **JdbcAccountRepository.findById()** | H2 In-Memory | Single method focus | 🟡 THIRD |
| **JdbcAccountRepository.save()** | H2 In-Memory | Complex insert/update | 🔴 LATER |
| **JdbcTransactionRepository** | H2 In-Memory | Full repository testing | 🔴 COMPLEX |

### **🗄️ TEST SQLITE DB (First Full Stack Tests)**
| Component | Database Type | Why Test DB | Priority |
|-----------|---------------|-------------|----------|
| **DatabaseConfig** | test-banking.db | Real connection config | 🟢 **FIRST TEST DB** |
| **StatusController** | test-banking.db | Full HTTP + DB stack | 🟡 **FIRST API+DB** |
| **UtilityController** | test-banking.db | REST endpoints + DB | 🟡 SECOND API |
| **End-to-end API tests** | test-banking.db | Complete integration | 🔴 LATER |

### **🏦 REAL DATABASE (Migration Verification)**
| Component | Database Type | Why Real DB | Priority |
|-----------|---------------|-------------|----------|
| **GetcompyMigrationTest** | banking.db (read-only) | COBOL vs Java accuracy | 🔴 **FIRST MIGRATION TEST** |
| **Account data verification** | banking.db (read-only) | Production data validation | 🔴 LATER |
| **Customer migration accuracy** | banking.db (read-only) | Legacy data comparison | 🔴 LATER |
| **Transaction consistency** | banking.db (read-only) | Financial data integrity | 🔴 COMPLEX |

## Test Strategy by Component

### **Coverage Goals by Phase**

#### **Phase 1: Foundation** (Target: 40% coverage)
1. All service classes (CompanyInfoService, SortCodeService)
2. All utility classes (CobolConverter)
3. One simple repository method (findById)
4. Model class validation

#### **Phase 2: Core Operations** (Target: 70% coverage) 
1. Primary repository CRUD operations
2. Controller endpoints with mocked dependencies
3. Database connection verification
4. Error handling paths

#### **Phase 3: Integration** (Target: 85% coverage)
1. Full repository test suites
2. End-to-end API testing
3. Migration accuracy verification
4. Edge cases and error scenarios

## 🚀 **RECOMMENDED NEXT STEPS** (Priority Order)

### **🎯 PHASE 2A: Quick Coverage Wins** (Recommended Next!)
1. **🟢 PRIORITY 1: SortCodeService test** - ~15 lines, pure utility, easy 100% coverage
2. **🟢 PRIORITY 2: Model Classes tests** - Account, Customer, Transaction (getters/setters)
3. **🟢 PRIORITY 3: CobolConverter test** - utility methods, no dependencies

### **🎯 PHASE 2B: Continue Database Integration**
1. **🟡 JdbcCustomerRepository** - Next smallest repository (~140 lines)
2. **🟡 Individual JdbcAccountRepository methods** - Start with `findById()`
3. **🟡 StatusController** - First API + Database integration

### **❌ Avoid Initially:**
1. ❌ Full JdbcAccountRepository (186 lines, too complex)
2. ❌ JdbcTransactionRepository (9,419 bytes, massive)
3. ❌ Complex migration verification tests
4. ❌ Multi-repository integration scenarios

### **💡 Strategic Recommendation:**
**Go for SortCodeService next!** It's a utility class with ~15 lines that will give us another quick coverage win and continue building momentum before tackling more complex database integrations.

## Success Metrics

1. **Each test runs independently** - No test pollution
2. **Fast feedback loop** - Tests complete in <30 seconds
3. **Clear failure messages** - Easy to debug when tests break
4. **Incremental coverage** - Each test adds measurable coverage
5. **SonarQube integration** - Coverage reports in dashboard

## Questions to Resolve

1. **H2 vs SQLite for repository tests?** 
   - H2: Faster, pure in-memory
   - SQLite: More production-like

2. **Mock vs Real database for controller tests?**
   - Mock: Faster, isolated
   - Real: Integration confidence

3. **Test data strategy?**
   - Generated: Clean, predictable
   - Fixed: Realistic, edge cases