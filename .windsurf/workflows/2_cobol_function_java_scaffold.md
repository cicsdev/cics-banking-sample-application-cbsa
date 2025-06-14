---
description: 2 of 5: COBOL Function → Java Scaffold Workflow
---

1. Preconditions & Input Definition
   1. Verify that both the COBOL analysis markdown (`1_cobol_function_analysis_<PROGRAM>.md`) and the original COBOL source (`<PROGRAM>.cbl`) are supplied via `@[file]` mentions in the same slash-command invocation.  
   2. If either file is missing, write `2_cobol_function_java_scaffold_<PROGRAM>.md` containing an error summary and the line `STATUS: FAIL`, then stop.  
   3. `COBOL_SRC` – absolute path to the COBOL `.cbl` source, supplied via `@[file]` mention when the slash-command is invoked.  
   4. `ANALYSIS_MD` – path to the previously generated `cobol_function_analysis_<PROGRAM>.md`, also supplied via `@[file]` mention.  
   5. `PROGRAM` – inferred from `ANALYSIS_MD` file name (uppercase basename without extension, e.g. `INQACC`).

2. Establish naming context.
   1. Read `.windsurf/rules/naming_structure.md` for base package & suffix conventions.
   2. Compute `BASE_PACKAGE` = `com.bank.cbsa` (per rule).
   3. Compute `PACKAGE_ROOT` = `${BASE_PACKAGE}.<programLower>` where `<programLower>` is `PROGRAM` lower-cased.
   4. Determine target directory: `src/main/java/${PACKAGE_ROOT//./}/`.

// turbo
3. Verify optional dependencies (warn only—do **not** modify `pom.xml`).
   1. Run:
      ```bash
      grep -E "(mapstruct|lombok|spring-boot-starter-data-jpa)" -n pom.xml || true
      ```
   2. If any of the three strings are **absent**, emit a warning block in the workflow log: "⚠ Missing dependency: <name> – features may be stubbed."  
      This step may auto-run.

4. Parse `ANALYSIS_MD` → collect **Recommended Java Artifacts** table (section 7.3 in the standard output).
   1. For each recommended artifact, determine its type (DTO, Entity, Repository, Service, Mapper, Controller).
   2. Derive canonical name using `naming_structure.md` suffix rules if not already present.

5. Create package directories under `PACKAGE_ROOT` following rule §3 directory layout.
   1. `controller/`  
   2. `service/` and `service/impl/`  
   3. `repository/`  
   4. `model/`  
   5. `mapper/`  
   6. `config/` (only if needed)

6. Generate Java skeletons.
   1. **DTO & Entity classes** – Fields remain empty; include `// TODO` markers to copy field list from analysis output. If Lombok present, annotate with `@Data`; otherwise add getters/setters stubs.
   2. **Repository interfaces** – Extend `JpaRepository<DomainEntity, IdType>` when JPA starter present; otherwise leave interface body empty with `// TODO`.
   3. **Service interfaces** – Declare method stubs reflecting COBOL paragraphs of interest (copy names verbatim as camelCase); add javadoc linking back to `COBOL_SRC` line numbers.
   4. **ServiceImpl classes** – Annotate with `@Service`; inject repository/mapper as needed; stub method bodies with `// TODO`.
   5. **Mapper interfaces** – If MapStruct available, annotate with `@Mapper(componentModel = "spring")`; else stub and note missing dependency.
   6. **Controller classes** – Annotate with `@RestController`; expose minimal sample endpoint `/api/<programLower>` returning HTTP 501.

7. Persist scaffold artefacts.
   1. Write all Java files to disk under the computed directories.
   2. Produce documentation file `2_cobol_function_java_scaffold_${PROGRAM}.md` alongside the Java sources containing:
      1. Overview of generated classes.
      2. Outstanding TODOs & dependency warnings.
      3. Next workflows to run (e.g., `/java_function_piece_plan`).

8. Output summary.
   1. Display the list of created files.
   2. Append `STATUS: PASS` to `2_cobol_function_java_scaffold_<PROGRAM>.md`.
   3. Emit reminders:
      1. Tests are **not** generated here – they belong to later workflows.
      2. Address dependency warnings before compiling.