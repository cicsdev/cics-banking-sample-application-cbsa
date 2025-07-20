Workflow unified_cobol_business_function_migration.md
was used to migrate
INQACC.cbl

Here is what we learned from the experience and how we would propose improving the workflow:

# Retro: First Run of unified_cobol_business_function_migration (INQACC)

1. What Went Well
   1. Skeleton generation (DTO, Mapper, Service, Controller) completed without manual scaffolding.
   2. Special–flag logic (`99999999` → last-record) captured in service layer exactly as in COBOL.
   3. Repository extension (`findBySortCode`) integrated cleanly.
   4. Build passes with focused INQACC integration tests; endpoints verified live.

2. Pain Points / Breakdowns
   1. Implementation step too coarse––we bounced between layers and config fixes.
   2. Legacy integration tests clashed with new H2 profile; we had to exclude them.
   3. Seed `data.sql` was not idempotent → duplicate PK errors on context reload.
   4. Data-generator beans executed in `test` profile until `@Profile("!test")` added.
   5. Type-map markdown saved in `doc/` but convention should be `plan/type_maps/`.

3. Quick Fix Opportunities
   1. Break workflow “Implementation” into: i. DTO/Mapper ii. Repository iii. Service iv. Controller v. Profile config vi. Idempotent seed data vii. Tests.
   2. Add workflow step: annotate data-gen beans & dev-only controllers with `@Profile("!test")`.
   3. Pre-clean tables or use `INSERT OR IGNORE` in seed SQL to guarantee idempotence.
   4. Update rule: type-maps stored under `plan/type_maps/`.
   5. Provide template `application-test.properties` & Surefire include/exclude guidance.

4. Larger Improvements To Schedule
   1. Create Maven `fast-test` profile using slice tests for quicker feedback loops.
   2. Auto-generate historical-learning file at end of workflow with timestamp & key points.
   3. CI pipeline to run both legacy suite (SQLite) and new H2-based tests separately until full migration.

_Date: 2025-06-04_