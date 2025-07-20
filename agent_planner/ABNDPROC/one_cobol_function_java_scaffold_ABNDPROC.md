# ABNDPROC.cbl Migration Scaffold Plan

## Program Analysis
**ABNDPROC.cbl** - Centralized error handling and logging utility (177 lines)

**Purpose**: Processes application abends and writes them to centralized datastore for monitoring
**Input**: Error information via DFHCOMMAREA (transaction details, error codes, diagnostic info)
**Processing**: Writes error record to VSAM KSDS file using CICS operations
**Output**: Error logging with basic write failure handling

## Architecture Decision Summary
- **Error Storage**: ApplicationError entity + repository (mirrors COBOL approach)
- **API Design**: REST endpoint `POST /api/errors` for explicit error logging
- **Error Data**: Simplified Java exception information (no CICS-specific fields)
- **Integration**: Explicit service calls when catching exceptions

## Java Artifacts Scaffold

| Piece | COBOL paragraph(s) | Java artifact(s) | Primary inputs | Primary outputs | External touch-points |
|-------|-------------------|------------------|----------------|-----------------|----------------------|
| Error Data Structure | ABNDINFO copybook | `ApplicationError.java` (model) | Error details, timestamp, program name | Entity for database persistence | SQLite database |
| Error Persistence | CICS WRITE FILE('ABNDFILE') | `ApplicationErrorRepository.java` + `JdbcApplicationErrorRepository.java` | ApplicationError entity | Database INSERT result | SQLite `application_error` table |
| Error Logging API | PREMIERE SECTION A010 | `ErrorController.java` | POST request with error details | HTTP 201/500 response | REST API clients, exception handlers |
| Error Processing Logic | Main procedure logic | `ErrorLoggingService.java` | Error data from controller | Formatted error record | Repository layer, validation |
| Error Request/Response | DFHCOMMAREA structure | `ErrorRequestDto.java`, `ErrorResponseDto.java` | JSON error payload | JSON response with error ID | REST API contract |

## Database Schema Addition
New table required: `application_error`
```sql
CREATE TABLE application_error (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    timestamp TEXT NOT NULL,
    application_id TEXT,
    transaction_id TEXT,
    error_code TEXT,
    program_name TEXT NOT NULL,
    error_message TEXT,
    stack_trace TEXT,
    created_at TEXT DEFAULT CURRENT_TIMESTAMP
);
```

## Integration Pattern
Other components will call: `POST /api/errors` when catching exceptions
```java
// Example usage in catch blocks:
try {
    // business logic
} catch (Exception e) {
    errorLoggingService.logError("PROGRAM_NAME", e);
    // continue with error handling
}
```

## Open Questions
- Should we include correlation IDs to track errors across service calls?
- Do we need error severity levels (INFO, WARN, ERROR, FATAL)?
- Should errors be exposed via a separate inquiry endpoint for monitoring?

## Foreseen Gotchas
1. **Schema Migration**: Need to update both `schema.sql` (SQLite) and `test-schema.sql` (H2)
2. **Error ID Generation**: COBOL uses UTIME+TASKNO composite key, we'll use AUTO_INCREMENT
3. **Transaction Handling**: COBOL writes immediately, Spring may need @Transactional considerations
4. **Error Response**: Need to handle case where error logging itself fails (error-while-logging-error)
5. **Testing**: Mock external error conditions and verify error logging doesn't interfere with main logic

## Implementation Priority
1. Create ApplicationError model and repository (core functionality)
2. Add ErrorLoggingService (business logic)
3. Create ErrorController with DTOs (API layer)
4. Update database schema files
5. Add comprehensive tests for error scenarios
