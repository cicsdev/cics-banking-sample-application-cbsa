---
trigger: model_decision
description: Apply this rule at the very start of implementing any new component—design it for easy, isolated testing before you write your first test.
---

# Testable Code Design Guide
*Write code that's easy to test BEFORE writing tests*

## 🎯 **Core Principle**
**Design decisions during initial coding have 10x more impact on testability than any testing framework.**


## 📋 **Quick Decision Guide**

**When designing a new class, ask:**

1. **"Can I test this without starting Spring?"** → If no, reduce dependencies
2. **"Can I predict the output?"** → If no, remove hidden state
3. **"Does this do one thing?"** → If no, split the class/method
4. **"Can I mock the dependencies?"** → If no, use dependency injection
5. **"Will this match COBOL behavior?"** → If unsure, design for easy comparison

---

*Follow these patterns and your tests will write themselves.*

## ✅ **Testable Code Patterns**

### **Database Layer**
```java
// ✅ GOOD - Constructor injection, single responsibility
@Repository
public class JdbcAccountRepository {
    private final JdbcTemplate jdbcTemplate;
    
    public JdbcAccountRepository(@Value("${database.path}") String dbPath) {
        // Testable with different database paths
    }
    
    public Optional<Account> findById(String id) {
        // Single SQL operation, easy to test with H2
    }
}

// ❌ BAD - Hardcoded paths, multiple responsibilities
public class AccountRepository {
    private static final String DB_PATH = "/prod/banking.db";
    
    public Map<String, Object> getAccountAndUpdateLastAccess(String id) {
        // Hard to test: static path, multiple operations, generic return
    }
}
```

### **Service Layer**
```java
// ✅ GOOD - Stateless, pure business logic
@Service
public class AccountService {
    private final AccountRepository repository;
    private final CobolConverter converter;
    
    public AccountResponseDto getAccount(String accountNumber) {
        Account account = repository.findById(accountNumber);
        return converter.toResponseDto(account); // Testable separately
    }
}

// ❌ BAD - Stateful, mixed concerns
@Service
public class AccountService {
    private Account currentAccount; // State makes testing hard
    
    public void processAccount(String id) {
        // Void method with side effects - hard to verify
        this.currentAccount = repository.findById(id);
        updateDatabase();
        sendEmail();
    }
}
```

### **Controller Layer**
```java
// ✅ GOOD - DTOs, validation, thin controller
@RestController
public class AccountController {
    private final AccountService service;
    
    @PostMapping("/accounts")
    public AccountResponseDto createAccount(@Valid @RequestBody AccountRequestDto request) {
        return service.createAccount(request); // Easy to test with mock service
    }
}

// ❌ BAD - Direct repository access, no validation
@RestController
public class AccountController {
    private final AccountRepository repository;
    
    @PostMapping("/accounts")
    public Account createAccount(@RequestBody Map<String, Object> data) {
        // Hard to test: no validation, generic input, direct DB access
        return repository.save(new Account(data.get("number").toString()));
    }
}
```

---

## 🚨 **Pre-Test Code Review Checklist**

### **Before Writing Any Test, Verify:**

**✅ Single Responsibility**
- [ ] Method does one thing only
- [ ] Class has one reason to change
- [ ] No methods longer than 20 lines

**✅ Dependency Injection**
- [ ] Constructor injection (not field injection)
- [ ] No hardcoded paths/URLs/values
- [ ] External dependencies can be mocked

**✅ Predictable Outputs**
- [ ] Same input always produces same output
- [ ] No hidden side effects
- [ ] Clear return types (not `Object` or `Map`)

**✅ Error Handling**
- [ ] Specific exception types
- [ ] Error conditions are testable
- [ ] No swallowed exceptions

---

## 🎯 **COBOL Migration Specific Patterns**

### **Separate COBOL Logic from Database Operations**
```java
// ✅ GOOD - Testable COBOL conversion
public class CobolConverter {
    public static String formatAccountNumber(String cobolFormat) {
        // Pure function - easy unit test
        return cobolFormat.trim().toUpperCase();
    }
}

@Service
public class InqaccService {
    public AccountResponseDto inquireAccount(String accountNumber) {
        Account account = repository.findById(accountNumber);
        String formattedNumber = CobolConverter.formatAccountNumber(account.getNumber());
        return new AccountResponseDto(formattedNumber, account.getBalance());
    }
}
```

### **Design for COBOL Program Equivalence Testing**
```java
// ✅ GOOD - Easy to compare with COBOL output
@Service
public class GetcompyService {
    public String getCompanyName() {
        return "CBSA Bank"; // Matches COBOL GETCOMPY.cbl output
    }
}

// Test becomes simple:
@Test
void matchesCobolOutput() {
    assertEquals("CBSA Bank", service.getCompanyName());
}
```

---

## 🚨 **Red Flags - Refactor Before Testing**

### **Immediate Refactoring Needed:**
- Methods with `void` return type that have side effects you need to verify
- Any use of `static` for business logic
- Classes with more than 3 constructor parameters
- Nested conditionals more than 2 levels deep
- Methods that both query AND modify data

### **Architecture Smells:**
- Services that import `javax.servlet` (HTTP concerns)
- Repositories that contain business logic
- Controllers that import `java.sql` (database concerns)
- Any class that knows about both HTTP requests AND database schemas

---

## 🏆 **Testability Success Metrics**

**You've designed for testability when:**
- [ ] Unit tests don't need `@SpringBootTest`
- [ ] Test setup is under 5 lines of code
- [ ] Tests run in under 100ms each
- [ ] Error conditions are easy to trigger in tests
- [ ] Mock setup is minimal and obvious

---


