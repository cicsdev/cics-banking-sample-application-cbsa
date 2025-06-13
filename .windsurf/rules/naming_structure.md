---
trigger: model_decision
description: Minimal naming conventions for generated Java code and packages.
---

# Java Naming Structure Rules

## 1. Package Roots
1. All generated Java source lives under the base package `com.bank.cbsa` (CBSA = *CICS Banking Sample Application*).
2. Domain-specific sub-packages are created directly beneath the base, e.g. `com.bank.cbsa.account`, `com.bank.cbsa.customer`.
3. Tests mirror the production package but reside under `src/test/java`.

## 2. Standard Layer Suffixes
1. **DTOs** – Classes that mirror COBOL data structures **end with** `Dto` (e.g. `AccountSummaryDto`).
2. **Service Interfaces** – Business service contracts **end with** `Service` (e.g. `InquireAccountService`).
3. **Service Implementations** – Concrete classes **end with** `ServiceImpl` and **implement exactly one** `*Service` interface (e.g. `InquireAccountServiceImpl`).
4. **Repositories** – Spring Data/JDBC persistence contracts **end with** `Repository` (e.g. `AccountRepository`).
5. **Controllers** – REST entry points **end with** `Controller` (e.g. `InquireAccountController`).
6. **Mappers** – MapStruct mapping helpers **end with** `Mapper` (e.g. `AccountMapper`).
7. **Configuration** – Spring configuration classes **end with** `Config` (e.g. `DatabaseConfig`).

## 3. Directory Layout (src/main/java)
1. `controller` – REST controllers.
2. `service` – Interfaces and implementations (implementation may live in nested `impl`).
3. `repository` – Repository interfaces + SQL mapper XML (if MyBatis) or SQL files.
4. `model` – DTOs and domain models.
5. `mapper` – MapStruct mappers.
6. `config` – Spring configuration classes.

Example path:
```
com/bank/cbsa/account/service/impl/InquireAccountServiceImpl.java
```

## 4. Naming Collision Guidance
1. If a class name already exists, append the COBOL program suffix (e.g. `InquireAccountDtoInqacc`).
2. Avoid abbreviations; favor descriptive names that match COBOL paragraph/section names where practical.

## 5. Scope
1. These conventions apply to **all** Java files generated or edited by Cascade during COBOL → Java migration.
2. The rule aims to provide just enough structure for Phase 3 starter workflow; it will be refined after the first migration run.
