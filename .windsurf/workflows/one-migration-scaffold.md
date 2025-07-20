---
description: workflow to generate java skeletons
---


1. Generate Java skeletons.
   a. Thoroughly review the COBOL source to determine what the program does in the overall application context.
   b. Thoroughly review the archetecture and relevant patterns in java-migration directory to determine possible places that will be impacted by the migration of our target cobol program.
   c. Base your thinking off of the rule cobol-to-java-migration-rules-of-thumb.md`.
   d. Check for optional dependencies in pom.xml and warn if missing (mapstruct, lombok, spring-boot-starter-data-jpa).
   e. At this point you should be forming a strong hypothesis about what the java skeleton will look like -- but you will likely have a few assumptions you need to make or questions you need to ask for clarification on what decisions to make where they are not obvious.
   Pause to ask these questions to me.
   f. After I have answered your questions, propose a skeleton for each artifact that will be impacted by the migration of our target cobol program - DON'T IMPLEMENT ANYTHING, this proposal will be delivered by the following table:
   IMPORTANT -MAIN DELIVERABLE OF THIS WORKFLOW: Produce Checklist Table
     1. Create a markdown table with columns: Piece | COBOL paragraph(s) | Java artifact(s) | Primary inputs | Primary outputs | External touch-points.
     2. Append to `agent_planner/<PROGRAM>/one_cobol_function_java_scaffold_<PROGRAM>.md`

2. Output summary.
   Write a summary of any open questions, forseen gotchas, and other important things to flag before me the developer moves onto the next workflow.  Append to the same file you edited earlier `agent_planner/<PROGRAM>/one_cobol_function_java_scaffold_<PROGRAM>.md`.
   