---
description: one of five workflows for migrating cobol to java
---

1. Define inputs.
   1. `COBOL_SRC` – absolute path to the COBOL `.cbl` file (supplied by the slash-command).
   2. `PROGRAM` – uppercase basename of `COBOL_SRC` (e.g., `INQACC`).
   3. Ensure `cobol_java_type_mapping.md` and `naming_structure.md` are present in the repo.
2. // turbo
   Run the following shell command to capture external calls:
   ```bash
   grep -n -E "EXEC[ ]+(CICS|SQL)" "$COBOL_SRC" > /tmp/${PROGRAM}_exec_calls.txt
   ```
3. From COBOL SRC find
   1. Extract `DATA DIVISION` structures → preliminary DTO / Entity rows.
   2. Extract `PROCEDURE DIVISION` paragraphs and labels.
   3. Collect constant assignments (`MOVE`, `SET`) for enum candidates.
4. Apply mapping rules.
   1. Use rule @cobol_java_type_mapping to translate PIC/GROUP types.
   2. Apply rule @naming_structure to derive Java-style names.
5. Detect ambiguities.
   1. Flag unknown PIC clauses.
   2. Note name collisions or conflicting length semantics.
6. Generate output file `plan/workflow_planner/${PROGRAM}/one_cobol_function_analysis_${PROGRAM}.md` with the following sections:
   1. **Overview** – purpose and high-level summary.
   2. **Data Types** – table mapping COBOL fields → Java types.
   3. **External Calls** – contents of `/tmp/${PROGRAM}_exec_calls.txt` with context.
   4. **Ambiguities** – open questions for reviewer resolution.
   5. **Recommended Java Artifacts** – DTOs, repositories, services, mappers.