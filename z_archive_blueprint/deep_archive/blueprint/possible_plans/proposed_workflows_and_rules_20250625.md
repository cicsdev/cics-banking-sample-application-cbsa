---
description: Proposed generalized workflows & automation rules for COBOL→Java migration
---

# 1. Overview

This document distills lessons captured under `plan/historical_learnings` into reusable **workflows** and **execution rules** that can be applied to *any* COBOL program being migrated to Java/Spring Boot.

# 2. Proposed Workflows

## 2.1 `/cobol-program-analysis` *(refines existing /one-cobol-program-analysis)*
1. Use `codebase_search` to locate the target `.cbl` & copybooks.
2. Parse FILE-SECTIONS to identify:
   1. i. External resource usage (CICS, DB2, VSAM).
   2. ii. Data structures (`PIC`, `REDEFINES`, `COMP-3`).
3. Auto-generate an **analysis report** under `plan/program_analysis/{program}.md` summarising:
   1. i. I/O patterns.
   2. ii. Data items & suggested Java types.
   3. iii. Control flow that maps to REST, service, or batch semantics.
4. Persist `type_map` fragments to `plan/type_maps/{program}.md` for later reuse.
5. Append an entry to `plan/historical_learnings` (auto-step, see §3.4).

## 2.2 `/cobol-java-scaffold` *(generalises /two-java-scaffold)*
1. Create DTOs & entity classes using the type map produced in 2.1.
2. Generate Mapper interfaces (MapStruct) or manual converters if `REDEFINES` detected.
3. Establish Spring Data repository *only* when table access exists; otherwise skip.
4. Produce stub Service & Controller classes with TODO placeholders.
5. Write slice tests (`@WebMvcTest`, `@DataJdbcTest`) verifying scaffolding compiles.
6. Commit changes; generate migration branch `feature/{program}-scaffold`.

## 2.3 `/cobol-java-implementation`
1. Fill TODOs layer-by-layer **in this order**:
   1. i. DTO/Mapper.
   2. ii. Repository (if needed).
   3. iii. Service logic.
   4. iv. Controller endpoint.
   5. v. Profile configuration (`@Profile("!test")`).
   6. vi. Idempotent seed SQL under `src/main/resources/db/data.sql`.
2. Add/extend `CobolConverter` utilities for any new type conversions.
3. Run unit + slice tests; ensure green.
4. Commit; open PR tagged `phase:implementation`.

## 2.4 `/cobol-java-verification` *(extends /five-java-test-verify)*
1. Generate integration tests exercising REST endpoints using Testcontainers (H2 by default).
2. Execute legacy COBOL regression suite (if runnable) **in parallel** to compare outputs.
3. Run `mvn test`; fail the workflow if discrepancies detected.
4. Produce **verification report** under `plan/verification/{program}.md`.

## 2.5 `/seed-sql-idempotentize`
1. Scan `data.sql` using `grep_search` for plain `INSERT INTO`.
2. Replace with `INSERT OR IGNORE` (SQLite) and `MERGE` (PostgreSQL) templates.
3. Verify reload succeeds twice without PK errors.

## 2.6 `/auto-learnings-log` (meta-workflow)
1. At the end of every workflow execution capture key metrics:
   1. i. Runtime.
   2. ii. Green/failed steps.
   3. iii. Notable manual interventions.
2. Append a new dated markdown file under `plan/historical_learnings/` following the format of existing entries.

# 3. Cross-Cutting Automation Rules

1. **Directory conventions**
   1. i. Store all generated *type maps* under `plan/type_maps/`.
   2. ii. Place analysis reports under `plan/program_analysis/`.
2. **Database profiles**
   1. i. Use SQLite for dev profile, H2 for tests.
   2. ii. Annotate data-generator beans & dev-only controllers with `@Profile("!test")`.
3. **Seed data**
   1. i. All `data.sql` files must be idempotent. Use `INSERT OR IGNORE` or equivalent.
4. **Historical learnings**
   1. i. Every workflow automatically triggers `/auto-learnings-log`.
   2. ii. The learning file must include: Program, Workflow, Pain-points, Fixes, Timestamp.
5. **Testing strategy**
   1. i. Slice tests for scaffold verification.
   2. ii. Full integration tests before merge.
6. **COBOL→Java type mapping**
   1. i. Maintain a central glossary `plan/type_maps/global.md` accumulating common patterns.
7. **Naming & branches**
   1. i. Branch names follow `feature/{program}-{stage}`.
   2. ii. PR labels: `phase:analysis`, `phase:scaffold`, `phase:implementation`, `phase:verification`.
8. **CI pipeline**
   1. i. Run both legacy COBOL regression (if dockerised) and new Java tests.
   2. ii. Trigger on PR and nightly.

# 4. Next Actions

1. Review & refine these workflows.
2. Convert approved ones into `.windsurf/workflows/*.md` files (numbered lists, Cascade tooling usage; no shell `grep`/`cat`).
3. Update CI to invoke workflows using slash-commands.
