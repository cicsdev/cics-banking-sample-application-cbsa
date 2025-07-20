---
description: DO NOT USE -- TOO LONG Unified COBOL business-function migration workflow
---

# Unified COBOL Business-Function Migration Workflow

## Purpose
1. Provide a repeatable sequence to migrate a single COBOL business program (e.g., **INQACC.cbl**) to Spring-based Java.
2. Ensure all generated code follows rule **@cobol_java_type_mapping.md** and rule **@naming_structure.md** rules.
3. Produce runnable, test-covered Java components within the existing CBSA project.

## Inputs & Preconditions
1. COBOL source file to migrate (`*.cbl`).
2. Verified SQLite schema or other target datastore is available.
3. Rules in place:
   1. @cobol_java_type_mapping.md
   2. @naming_structure.md
4. Local dev environment with JDK 17+, Maven 3.9+, and MapStruct annotation processor.

## Workflow Steps

1. Pre-migration Analysis
   1. Review **IDENTIFICATION DIVISION** comments to confirm program purpose.
   2. Extract **WORKING-STORAGE** and **LINKAGE SECTION** structures.
   3. Map each COBOL field to Java types using rule **@cobol_java_type_mapping.md**; capture in a table (`plan/type_maps/<program>-type-map.md`).
   4. Decide base package and class names per rule **@naming_structure.md** (e.g., `com.bank.cbsa.account`).

2. Skeleton Generation
   1. Create DTOs mirroring each COBOL data structure; suffix with `Dto`.
   2. Create `*Service` interface and `*ServiceImpl` class with Spring `@Service`.
   3. If data persistence is required, create a `*Repository` (Spring Data JPA).
   4. Create REST controller (`*Controller`) exposing required endpoints.
   5. Generate MapStruct `*Mapper` classes for DTO ↔ entity mapping.
   6. Annotate development-only beans (e.g., data generators, test-data controllers) with `@Profile("!test")` so they are excluded during automated tests.
   7. Add `src/test/resources/application-test.properties` template (H2 URL, schema & data locations) if it does not already exist.

3. Core Logic Translation
   1. Port each **PROCEDURE DIVISION** paragraph into descriptive service methods.
   2. Replace CICS file operations with repository calls. Use transactions where COBOL used `EXEC CICS SYNCPOINT`.
   3. Convert conditionals and loops to idiomatic Java; log decisions.
   4. Leave `TODO` comments where behavior is uncertain; mark for review.

4. Test Scaffolding
   1. Create JUnit 5 unit tests for every public service method.
   2. Create SpringBootTest for repository + service integration (H2 in-memory).
   3. Create Rest-Assured API test hitting the new controller endpoint(s).
   4. Ensure `src/test/resources/data.sql` is **idempotent** — either pre-cleans tables (`DELETE FROM ...`) or uses `INSERT OR IGNORE` to avoid primary-key clashes when the context reloads.

5. Build, Run & Verify
   1. Compile without tests:

       ```shell
       mvn clean package -DskipTests
       ```

   2. Execute full test suite:

       ```shell
       mvn test
       mvn verify -Pintegration
       ```

   3. Run the application with generated test data:

       ```shell
       mvn spring-boot:run -Dspring-boot.run.arguments="--generate-test-data=true --customer-count=10 --accounts-per-customer=2 --transactions-per-account=5 --reset-database=true --server.port=8085"
       ```

   4. Confirm service health:

       ```shell
       curl http://localhost:8085/actuator/health
       ```

   5. Manually hit newly created endpoint(s) or run Postman collection to verify functional parity.

6. Iteration & Refinement
   1. Compare Java output with original COBOL test results; document mismatches.
   2. Update code, type mappings, or naming rule if gaps discovered.
   3. Commit refined rules/workflow alongside code changes.

## Program-Specific Addenda
1. Special constants / flags  
   1. Document magic values such as `99999999` representing “last record”.  
   2. Capture them in `docs/<program>-constants.md` and reference within unit/integration tests.  
2. Abend / error mapping rules  
   1. Create a table mapping COBOL SQLCODE ranges and ABEND codes to Java exceptions and HTTP statuses.  
   2. Store the table in `docs/<program>-error-map.md` and ensure the mapping is wired into `*ServiceImpl`.  
3. Data-format conversion helpers  
   1. Identify non-standard date or numeric formats (e.g., DDMMYYYY, packed decimals).  
   2. Implement reusable converter utilities in `com.bank.cbsa.common.format.*`.  
   3. Add focused unit tests verifying each helper’s round-trip accuracy.  

## Completion Criteria
1. All unit, integration, and API tests pass.
2. Endpoint returns expected data matching COBOL behavior.
3. Code complies with naming and type-mapping rules (spot-check via package scan).
4. Documentation (`docs/`) updated with mapping table and migration notes.