# Three-Java Piece Plan – INQACC

## 1. Checklist Table

| Piece | COBOL paragraph(s) / section(s) | Java artifact(s) (proposed) | Primary inputs | Primary outputs | External touch-points |
|-------|---------------------------------|-----------------------------|----------------|-----------------|-----------------------|
| 1 | `PREMIERE SECTION` – `A010`, `A999` | `InquiryAccountServiceImpl.handleInquiry()` | DFHCOMMAREA fields: `INQACC-ACCNO`, `SORTCODE` | Populated `AccountDto` + `success` flag in COMMAREA | `EXEC CICS HANDLE ABEND`, PERFORM calls to DB2 reader pieces |
| 2 | `READ-ACCOUNT-DB2 SECTION` – `RAD010`/`RAD999` + `FETCH-DATA SECTION` – `FD010`/`FD999` | `AccountDb2Dao.readAccount()` + mapper to `AccountEntity` | Account number, sort code | Single `AccountEntity` | `EXEC SQL OPEN / FETCH` on `ACC-CURSOR` (DB2) |
| 3 | `READ-ACCOUNT-LAST SECTION` – `RAN010`/`RAN999` + `GET-LAST-ACCOUNT-DB2 SECTION` | `AccountDb2Dao.readLastAccount()` | Sentinel account number `99999999` | Last account details | `EXEC SQL` SELECT (DB2) |
| 4 | `CHECK-FOR-STORM-DRAIN-DB2 SECTION` – `CFSDCD010`/`CFSDCD999` | `StormDrainService.checkStormDrain()` | `SQLCODE` after DB2 ops | Storm-drain decision flag | None (pure logic) |
| 5 | `ABEND-HANDLING SECTION` – `AH010`/`AH999` | `CicsAbendHandler.handle()` | Abend code from CICS | Structured `AbendInfo` | `EXEC CICS ASSIGN / ABEND`, logging |
| 6 | `POPULATE-TIME-DATE SECTION` – `PTD010`/`PTD999` | `DateTimeUtil.populate()` | None – uses `EXEC CICS ASKTIME` | Formatted date / time strings | `EXEC CICS ASKTIME`, `FORMATTIME` |
| 7 | `GET-ME-OUT-OF-HERE SECTION` – `GMOFH010`/`GMOFH999` | handled implicitly by normal Java return flow; no dedicated artifact | N/A | N/A | `EXEC CICS RETURN` |

## 2. Notes

1. Pieces 2 and 3 deliberately isolate DB2 interactions per heuristic (ii).
2. Piece 4 separates storm-drain evaluation logic so it can be unit-tested independently.
3. Cross-cutting utilities (Pieces 5 & 6) are migrated to shared helpers per heuristic (vi/vii).
4. `GMOFH` simply issues `RETURN`; in Java a normal method return suffices, so no standalone artifact was planned.

STATUS: PASS
