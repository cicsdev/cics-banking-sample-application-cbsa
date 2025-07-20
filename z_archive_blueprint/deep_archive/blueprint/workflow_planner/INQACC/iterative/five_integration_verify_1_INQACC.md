---
description: INQACC piece 1 integration verification results
---

1. Offending Artifact
   1. Integration tests for piece `1` (`InqAccServiceIntegrationTest`).

2. Failure Summary
   1. Assertion failure: `success` flag expected `true` but was `false` in `givenLastRecord_whenInquire_thenReturnsHighestAccount`.
   2. Assertion failure: `success` flag expected `true` but was `false` in `givenNormalAccount_whenInquire_thenSuccessTrue`.

3. Stack-Trace Excerpts
   1. `InqAccServiceIntegrationTest.givenLastRecord_whenInquire_thenReturnsHighestAccount(InqAccServiceIntegrationTest.java:38)`
   2. `InqAccServiceIntegrationTest.givenNormalAccount_whenInquire_thenSuccessTrue(InqAccServiceIntegrationTest.java:27)`

4. Suggestions
   1. Verify repository/service logic sets the `success` flag correctly when an account is found.
   2. Ensure `InquiryAccountMapper` correctly maps the `success` flag based on business rules.
   3. Add or update unit tests for service and mapper layers, then re-run them before executing integration tests again.

5. Status
   STATUS: FAIL
