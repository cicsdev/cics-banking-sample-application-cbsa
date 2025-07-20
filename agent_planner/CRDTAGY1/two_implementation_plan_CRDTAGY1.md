# CRDTAGY1 Implementation Plan

## Pre-flight Check ✅
- Scaffold checklist completed in `one_cobol_function_java_scaffold_CRDTAGY1.md`
- COBOL source analyzed: Credit agency service with random delay and score generation
- Architecture integration points identified

## 1. Implementation Planning

### COBOL Program Analysis Summary:
- **Purpose**: Simulate credit agency processing with random delay (0-3 seconds) and credit score generation (1-999)
- **Input**: Customer data via CICS container (WS-CONT-IN structure)
- **Processing**: Random delay, generate random credit score, update container
- **Output**: Updated customer data with new credit score via container PUT
- **Error Handling**: ABNDPROC integration for CICS errors

### Java Artifacts Implementation Order:

#### 1. DTOs (Data Transfer Objects)
- **CreditScoreRequestDto** - Input validation and API contract
- **CreditScoreResponseDto** - Output data structure
- **Dependencies**: None (pure data structures)
- **Testability**: Simple validation tests, field mapping tests

#### 2. Service Layer
- **CreditAgencyService** - Core business logic
- **Dependencies**: CustomerRepository (for data updates), configuration properties
- **Testability**: Constructor injection, configurable delays, mockable repository
- **Design Patterns**: 
  - Single responsibility (credit scoring only)
  - Pure functions where possible (score generation)
  - Configurable behavior (delay simulation)

#### 3. Controller Layer  
- **CreditAgencyController** - REST API endpoint
- **Dependencies**: CreditAgencyService, DtoMapper
- **Testability**: MockMvc integration tests, service mocking

### Testable Design Decisions:

#### CreditAgencyService Design:
```java
@Service
public class CreditAgencyService {
    private final CustomerRepository customerRepository;
    private final Random random;
    private final boolean delayEnabled;
    private final int maxDelaySeconds;
    private final int minScore;
    private final int maxScore;
    
    // Constructor injection for all dependencies
    public CreditAgencyService(
        CustomerRepository customerRepository,
        @Value("${credit-agency.simulation.delay.enabled:true}") boolean delayEnabled,
        @Value("${credit-agency.simulation.delay.max-seconds:3}") int maxDelaySeconds,
        @Value("${credit-agency.simulation.score.min:1}") int minScore,
        @Value("${credit-agency.simulation.score.max:999}") int maxScore
    ) {
        this.customerRepository = customerRepository;
        this.random = new Random();
        this.delayEnabled = delayEnabled;
        this.maxDelaySeconds = maxDelaySeconds;
        this.minScore = minScore;
        this.maxScore = maxScore;
    }
    
    // Testable methods with predictable behavior
    public int generateCreditScore() { /* configurable range */ }
    public void simulateProcessingDelay() { /* configurable delay */ }
    public Customer updateCustomerCreditScore(String sortCode, Long customerNumber, int newScore) { /* single responsibility */ }
}
```

#### Benefits of This Design:
1. **Constructor injection** - Easy to mock dependencies in tests
2. **Configurable behavior** - Can disable delays in tests, control score ranges
3. **Single responsibility** - Each method does one thing
4. **Predictable outputs** - Score generation uses configurable ranges
5. **No hidden state** - All behavior controlled by constructor parameters

### Test Strategy:

#### Unit Tests:
- **CreditAgencyService**: Score generation, delay simulation, customer updates
- **DTOs**: Validation rules, field mapping
- **Controller**: Request/response handling, error scenarios

#### Integration Tests:
- **REST endpoint**: Full request/response cycle
- **Database interactions**: Customer record updates
- **Error handling**: Invalid customer data, database errors

## 2. Configuration Properties

Add to `application.properties`:
```properties
# Credit agency simulation settings
credit-agency.simulation.delay.enabled=true
credit-agency.simulation.delay.max-seconds=3
credit-agency.simulation.score.min=1
credit-agency.simulation.score.max=999
```

## 3. Implementation Notes

### Key COBOL Behaviors to Preserve:
1. **Random delay**: 0-3 seconds (configurable in Java)
2. **Credit score range**: 1-999 (configurable in Java)
3. **Customer data update**: Update creditScore and creditScoreReviewDate
4. **Error handling**: Proper error responses for invalid data

### Deviations from COBOL:
1. **Synchronous REST API** instead of async CICS container processing
2. **JSON request/response** instead of CICS container data
3. **HTTP status codes** instead of CICS response codes
4. **Spring Boot error handling** instead of ABNDPROC

### Architecture Integration:
- Leverages existing Customer model and CustomerRepository
- Uses existing DtoMapper for Customer <-> DTO conversion
- Follows established REST API patterns in the application
- Integrates with existing error handling framework

## Implementation Ready ✅
All dependencies identified, testable design planned, ready for code implementation.
