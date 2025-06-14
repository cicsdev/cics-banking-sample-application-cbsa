---
description: 3 of 5: Generate checklist of piece boundaries for Java function port
---

1. Preconditions
   1. Verify that both the COBOL analysis markdown (`1_cobol_function_analysis_<PROGRAM>.md`) and the original COBOL source (`<PROGRAM>.cbl`) are supplied via `@[file]` mentions in the same slash-command invocation.
   2. If either file is missing, write the result file (see Step 5) with an error summary and the line `STATUS: FAIL`, then stop.

2. Load Inputs
   1. Read the PROCEDURE DIVISION of the COBOL program from the supplied source file.
   2. Parse global metadata (data-structure mapping, constants, etc.) from the analysis markdown.

3. Determine Piece Boundaries
   1. Apply the following Piece-Boundary Heuristics **embedded here for self-containment**:
      i. COBOL paragraph boundary – each paragraph/section becomes one piece unless it is only an `EXIT.` line.
      ii. External interaction – any block that opens/closes a cursor or performs `EXEC SQL`, VSAM, or `EXEC CICS` I/O is isolated in its own piece.
      iii. Loop or conditional complexity – if a paragraph exceeds 40 lines or contains deeply nested control flow, split the inner loop into its own helper piece.
      iv. Shared Working-Storage structures – paragraphs operating on the same WS fields (mapping to one DTO/entity) stay together; otherwise they split.
      v. Magic-value branches – sentinel branches (e.g., `ACCNO = 99999999`) become dedicated pieces so tests can explicitly target each rule.
      vi. Cross-cutting utilities – generic routines (`POPULATE-TIME-DATE`, storm-drain checks) migrate once into shared utility classes (one piece, reused project-wide).
      vii. ABEND / error flows – all ABEND handling paragraphs are grouped into a common `*AbendHandler` utility rather than per-piece code.

4. Produce Checklist Table
   1. Create a markdown table with columns: Piece | COBOL paragraph(s) | Java artifact(s) | Primary inputs | Primary outputs | External touch-points.
   2. Derive Java artifact names using the conventions in `naming_structure.md`.

5. Write Result File
   1. Path: `workflow_planner/<PROGRAM>/3_java_function_piece_plan_<PROGRAM>.md` (create directories if necessary).
   2. If all parsing and table generation succeed, write the checklist table followed by the line `STATUS: PASS`.
   3. If any error occurs, write an error summary followed by the line `STATUS: FAIL`. **The file must always be created** so reviewers and CI can inspect the outcome.

6. Completion
   1. Echo a concise summary (either `3_java_function_piece_plan SUCCESS` or `3_java_function_piece_plan FAILURE`) to stdout so CI logs reflect the result.

Slash-command: `/java_function_piece_plan` (no positional parameters; rely on `@[file]` mentions).