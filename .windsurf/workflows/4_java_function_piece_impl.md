---
description: Piece-wise Java function implementation for a single COBOL “piece”.
---

1. **Slash-command**: `/java_function_piece_impl`

2. **Preconditions**
   1. Verify that **both** files below are supplied in the same invocation via `@[file]` mentions.  
      i. `1_cobol_function_analysis_<PROGRAM>.md`  
      ii. `<PROGRAM>.cbl` (original COBOL source)  
   2. If either file is missing, write or update `workflow_planner/<PROGRAM>/_iterative/4_java_function_piece_<PIECE>_<PROGRAM>.md` containing an error summary and the line `STATUS: FAIL`, **then stop**.

3. **Parameter Extraction**
   1. Derive `<PROGRAM>` from the filenames provided.  
   2. Derive `<PIECE>` from the iterative result-file path or prompt context.

4. **Implementation Steps**
   1. Parse the COBOL analysis markdown to locate the target paragraph(s) and corresponding Java artifacts.  
   2. Generate or modify Java source, DTOs, and mappers for `<PIECE>` following `cobol_java_type_mapping.md` and `naming_structure.md`.  
   3. Update or create piece-specific unit tests in `src/test/java/`.

5. // turbo
   Run `mvn -q test -Dtest=*<PIECE>*` to compile and execute only the unit tests associated with the piece.

6. **Failure Loop**
   1. If tests fail, append the Maven surefire summary to the iterative markdown, commit code fixes, and repeat Step&nbsp;5.  
   2. Continue until tests pass or user aborts.

7. **Success Recording**
   1. Append a concise changelog, key learnings, and `STATUS: PASS` to `workflow_planner/<PROGRAM>/_iterative/4_java_function_piece_<PIECE>_<PROGRAM>.md`.  
   2. Ensure the checklist in `3_java_function_piece_plan_<PROGRAM>.md` is updated (if applicable).

8. **Next Action**
   1. When all pieces A–N show `STATUS: PASS`, trigger `/java_function_integration_verify`.
