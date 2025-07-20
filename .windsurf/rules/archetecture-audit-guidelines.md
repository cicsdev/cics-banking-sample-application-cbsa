---
trigger: model_decision
description: When performing audit or quality of new migration work
---

# SonarQube Architecture Governance

## ⚠️ **Architectural Pitfalls SonarQube Won't Catch**
*Tactical advice for COBOL-to-Java migration developers*

### **Package Structure Discipline**
- **Stick to `com.cbsa.migration.*`** - Don't create new package hierarchies
- **One Account model** - Use existing `com.cbsa.migration.model.Account` (12-field version)
- **Component scanning** - New packages won't be auto-discovered by Spring Boot

### **Repository Pattern Consistency**
- **Use existing JDBC repositories** - Don't create duplicate interfaces
- **Follow GETCOMPY/GETSCODE pattern** - Proven working architecture
- **Single data access layer** - Don't mix repository implementations

### **Service Layer Standards**
- **Direct `@Service` classes** - Avoid Interface+Impl complexity
- **Hardcoded constants OK** - For simple COBOL program migrations
- **Business logic in services** - Keep controllers thin

### **Database Access Rules**
- **Use existing `schema.sql`** - Don't create duplicate table definitions
- **Spring `@Value` injection** - No hardcoded database paths
- **Test database isolation** - Use `test-banking.db` for tests

## 🔍 Key Issues SonarQube Detects

### **Code Quality**
- **Duplication**: 0% ✅ (excellent)
- **Complexity**: High = refactor candidates
- **Technical Debt**: Maintenance burden
- **Code Smells**: Anti-patterns

### **Security & Reliability**
- **Null Pointer Risks**: `queryForObject()` without checks
- **Generic Exceptions**: `RuntimeException` vs specific types
- **Resource Leaks**: Unclosed connections

### **Banking-Specific Issues**
- **Missing `@Transactional`** on financial operations
- **Data Exposure**: `findAll()` without access controls
- **No Audit Logging** for money movements
- **Missing Input Validation** on financial data

## 🎯 Quick Fixes

```java
// ❌ Generic Exception
throw new RuntimeException("Database error");
// ✅ Specific Exception
throw new DatabaseInitializationException("Schema failed");

// ❌ NPE Risk
return jdbcTemplate.queryForObject("SELECT COUNT(*)", Integer.class);
// ✅ Null Safe
Integer count = jdbcTemplate.queryForObject("SELECT COUNT(*)", Integer.class);
return count != null ? count : 0;

// ❌ Security Risk
public List<Account> findAll() // Exposes all accounts
// ✅ Access Control
public List<Account> findByCustomerId(Long id, String role)
```

## 🚨 Banking Red Flags
1. **0% Test Coverage** on financial operations
2. **Generic Exceptions** in money movements
3. **Missing Audit Logs** for transactions
4. **Unrestricted Data Access** methods
