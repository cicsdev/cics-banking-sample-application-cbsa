---
description: Verify integration across all implemented Java pieces for a migrated COBOL program.
---

1. **Slash-command**: `/java_function_integration_verify`

2. **Preconditions**
   1. Verify that **both** files below are supplied via `@[file]` mentions in the invocation.  
      i. `1_cobol_function_analysis_<PROGRAM>.md`  
      ii. `<PROGRAM>.cbl`  
   2. If either is missing, write `workflow_planner/<PROGRAM>/_iterative/5_integration_verify_<PIECE?>_<PROGRAM>.md` with an error summary and the line `STATUS: FAIL`, **then stop**.

3. **Environment Boot**
   1. // turbo  
      Run `mvn spring-boot:run -q -Dspring-boot.run.arguments="--server.port=8085 --generate-test-data=true --customer-count=10 --accounts-per-customer=2 --transactions-per-account=5 --reset-database=true"`.
   2. Wait for Spring context to load successfully.

4. **Execute Integration Tests**
   1. // turbo  
      Run `mvn -q verify -Pintegration` (assumes integration-tests are grouped under this Maven profile).

5. **Results Handling**
   1. If tests fail, for each failure, log offending `<PIECE>`/artifact, stack trace summary, and suggestions.  
   2. Write or update `workflow_planner/<PROGRAM>/_iterative/5_integration_verify_<PIECE>_<PROGRAM>.md` with details and `STATUS: FAIL`.  
   3. Stop and prompt the user to revisit the corresponding piece workflows.

6. **Success Recording**
   1. On success, write/update `workflow_planner/<PROGRAM>/_iterative/5_integration_verify_all_<PROGRAM>.md` with execution log and `STATUS: PASS`.  
   2. Optionally copy artifacts to `plan/completed_migrations/<PROGRAM>/`.

7. **Follow-up**
   1. If all integration tests pass, migration for `<PROGRAM>` can be marked complete in CI.
