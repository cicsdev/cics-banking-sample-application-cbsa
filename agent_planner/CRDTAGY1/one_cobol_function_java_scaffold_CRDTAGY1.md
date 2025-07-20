# CRDTAGY1 Migration Scaffold

## COBOL Program Analysis
- **Program**: CRDTAGY1 - Credit Agency Service
- **Purpose**: Simulate credit agency processing with random delay and credit score generation
- **Pattern**: Async-called service that receives customer data, generates credit score, returns updated data
- **Key Operations**: Container data exchange, random delay simulation, credit score generation, error handling

## Java Migration Checklist

| Piece | COBOL paragraph(s) | Java artifact(s) | Primary inputs | Primary outputs | External touch-points |
|-------|-------------------|------------------|----------------|-----------------|---------------------|
| Credit Score Request Processing | A010 (main logic) | `CreditAgencyController.processCredit()` | Customer data via REST | Updated customer with credit score | REST API endpoint |
| Customer Data Input/Output | WS-CONT-IN structure | `CreditScoreRequestDto`, `CreditScoreResponseDto` | Customer demographics, existing score | Updated customer data with new score | DTO validation |
| Credit Score Generation | Random score calculation | `CreditAgencyService.generateCreditScore()` | Customer data | Random score (1-999) | Random number generation |
| Processing Delay Simulation | Random delay (0-3 seconds) | `CreditAgencyService.simulateProcessingDelay()` | None | Delay execution | Thread.sleep() or async delay |
| Customer Data Update | Container PUT operation | `CreditAgencyService.updateCustomerCreditScore()` | Customer ID, new score | Updated customer record | CustomerRepository |
| Error Handling | ABNDPROC integration | `ErrorLoggingService` (existing) | Error details, ABEND codes | Error logs | Existing error logging |
| Container/Channel Management | CICS container operations | REST request/response handling | HTTP request/response | JSON data exchange | Spring Boot REST framework |

## Architecture Integration Points

### New Artifacts to Create:
1. **CreditAgencyController** - REST endpoint for credit scoring
2. **CreditAgencyService** - Business logic for credit processing
3. **CreditScoreRequestDto** - Input validation and contract
4. **CreditScoreResponseDto** - Output data structure
5. **CreditAgencyServiceTest** - Unit tests for service logic
6. **CreditAgencyControllerTest** - Integration tests for REST endpoint

### Existing Artifacts to Leverage:
1. **Customer** (model) - Already has creditScore and creditScoreReviewDate fields
2. **CustomerRepository** - For updating customer records
3. **ErrorLoggingService** - For error handling integration
4. **DtoMapper** - For Customer <-> DTO conversion

### Configuration Properties:
```properties
# Credit agency simulation settings
credit-agency.simulation.delay.enabled=true
credit-agency.simulation.delay.max-seconds=3
credit-agency.simulation.score.min=1
credit-agency.simulation.score.max=999
```

## REST API Design

### Endpoint: `POST /api/credit-agency/score`

**Request:**
```json
{
  "sortCode": "987654",
  "customerNumber": 123456789,
  "name": "John Doe",
  "address": "123 Main St, City, State",
  "dateOfBirth": "1980-01-15",
  "currentCreditScore": 650
}
```

**Response:**
```json
{
  "sortCode": "987654", 
  "customerNumber": 123456789,
  "updatedCreditScore": 742,
  "scoreReviewDate": "2024-01-15",
  "processingTimeMs": 1250,
  "success": true
}
```

## Key Business Logic

1. **Input Validation**: Validate customer data matches existing records
2. **Delay Simulation**: Random delay 0-3000ms (configurable)
3. **Score Generation**: Random integer 1-999
4. **Customer Update**: Update creditScore and creditScoreReviewDate
5. **Response Assembly**: Return updated customer data with processing metadata

## Error Handling Strategy

- **Customer Not Found**: Return 404 with error details
- **Invalid Input**: Return 400 with validation errors
- **Database Errors**: Log via ErrorLoggingService, return 500
- **Processing Timeout**: Configurable timeout, return 408

## Testing Strategy

1. **Unit Tests**: Service logic, score generation, delay simulation
2. **Integration Tests**: REST endpoint, database interactions
3. **Performance Tests**: Validate delay simulation works correctly
4. **Error Handling Tests**: Validate all error scenarios

## Summary & Implementation Notes

### Open Questions:
1. **Async vs Sync Trade-off**: COBOL version was called asynchronously, but REST API is synchronous. Consider if true async processing is needed for production scalability.
2. **Credit Score Business Rules**: COBOL generates random scores (1-999). Should we implement actual credit scoring algorithms or maintain simulation?
3. **Security**: REST endpoint needs authentication/authorization - should it use existing security patterns?
4. **Rate Limiting**: Credit agency calls should probably be rate-limited to prevent abuse.

### Foreseen Gotchas:
1. **Thread Safety**: Random number generation and delay simulation need to be thread-safe for concurrent requests.
2. **Configuration Management**: Delay settings should be externally configurable (dev vs prod environments).
3. **Database Transactions**: Customer updates need proper transaction management to prevent partial updates.
4. **Error Code Mapping**: COBOL ABEND codes need mapping to HTTP status codes and meaningful error messages.
5. **Date Handling**: COBOL date format (DDMMYYYY) vs Java LocalDate - ensure proper conversion.
6. **Container Size Limits**: COBOL had container length management - REST payload size limits may be needed.

### Implementation Priority:
1. **High**: CreditAgencyService with core business logic
2. **High**: Customer repository integration and transaction management  
3. **Medium**: REST controller and DTO structure
4. **Medium**: Configuration properties and delay simulation
5. **Low**: Advanced error handling and monitoring integration

### Critical Success Factors:
- Maintain existing Customer entity structure (no schema changes needed)
- Leverage existing ErrorLoggingService patterns
- Follow established DTO mapping patterns with DtoMapper
- Ensure thread safety for concurrent credit score requests
- Implement proper transaction boundaries for data consistency
