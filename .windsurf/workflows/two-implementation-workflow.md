---
description: tactical workflow to implement java code from scaffold plan with retro
---

## Pre-flight Check
Verify `agent_planner/<PROGRAM>/one_cobol_function_java_scaffold_<PROGRAM>.md` exists with completed checklist table.

## 1. Implementation Planning
   a. Review scaffold checklist table from `agent_planner/<PROGRAM>/one_cobol_function_java_scaffold_<PROGRAM>.md`
   b. Apply testable code design patterns from `.windsurf/rules/testable-code-expectations.md`
   c. Create implementation plan file: `agent_planner/<PROGRAM>/two_implementation_plan_<PROGRAM>.md`
   d. For each Java artifact, plan:
      - Constructor dependencies (for DI and testing)
      - Single responsibility focus
      - Testable design (no hardcoded values, predictable outputs)
      - Test strategy (unit vs integration)

## 2. Code Implementation
   a. Implement Java artifacts in order of dependencies (repositories → services → controllers)
   b. Follow testable patterns: constructor injection, single responsibility, pure functions where possible
   c. Run the java code to ensure it can run
   d. IMPORTANT**: Don't implement tests. Test will be written in the next workflow

## 3. Retrospective
   **Append retrospective to same plan file (`agent_planner/<PROGRAM>/two_implementation_plan_<PROGRAM>.md`):**
   - What worked well in the implementation?
   - What challenges emerged?
   - Any deviations from the original scaffold plan?
   - Are there any gotchas or issues that need to be flagged before the developer moves onto the next workflow?
