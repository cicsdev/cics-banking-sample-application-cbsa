---
description: Build Tests for New Java Artifacts and Log Mappings
---

## Pre-flight Check
Verify `agent_planner/<PROGRAM>/two_implementation_plan_<PROGRAM>.md` exists with completed implementation.

1.  **Study existing tests**  
    Inspect `/java-migraion/src/test` to learn the current style.
    
2.  **Check current coverage**  
Run sonarqube
    ```bash
mvn clean test jacoco:report sonar:sonar -Dsonar.projectKey=cobol-demo -Dsonar.projectName='cobol-demo' -Dsonar.host.url=http://localhost:9000 -Dsonar.token=sqp_965bb7c45a7bb43c8f544e2193eec858e11dae9c
```
    Then use SonarQube MCP and capture coverage for the target artifacts.
    
3.  **Draft a test plan**
    Consider together what you learned from analyzing relevant files and IMPORTANT the testing-expectations rule -- after considering those sources, take the time to think about it strategically and propose a testing strategy and list the high-value tests you’d add.
    Ask clarifying questions to learn about what scenarios I think we need to test and the level of testing im ok with (h2, test db, or mocking - the pros and cons of each decision)

4. After clarifying questions are asked save your plan to `agent_planner/<PROGRAM>/three_test_plan_<PROGRAM>.md` At the top of this file add a short sentence on what the code does and what is the most crucial expected behavior to test. This should be curt one sentence at the top no fluff.
    
5.  **Update `blueprint/mapping_log.md`**  
    Add one line per **〈Java artifact〉 / 〈test name〉** pair—use a real test-method identifier, e.g.  
    `CompanyInfoService.getCompanyName() | getCompanyName_returnsValidCompanyName` and another row is `CompanyInfoService.getCompanyName() | getCompanyName_returnsOnlyOneValue`
    
6.  **Get approval, then execute**  
    After I sign off on `mapping_log.md`, write the tests, run them, and report results.
    
7.  **Verify improvement**  

```
mvn clean test jacoco:report sonar:sonar -Dsonar.projectKey=cobol-demo -Dsonar.projectName='cobol-demo' -Dsonar.host.url=http://localhost:9000 -Dsonar.token=sqp_965bb7c45a7bb43c8f544e2193eec858e11dae9c
```
    Re-run SonarQube and then use MCP to confirm coverage has increased.

8.  **Retrospective**  
    **Append retrospective to same test plan file (`agent_planner/<PROGRAM>/three_test_plan_<PROGRAM>.md`):**
    
    -   **What you shipped**—one-liner per feature/test.
        
    -   **Coverage delta**—SonarQube before → after.
    -   **Re-assess plan**—Is there anything in the original test plan you weren't able to address?
        
    -   **New Tech Debt**—new tech-debt that results from this work
        
    -   **Self-critique**—gotchas, shortcuts, anything you would flag to the developer about what you analyzed, searched and built.