# Tech Debt - Java Migration

## Data Safety
- [ ] Add null checks for `BigDecimal.toString()` calls in repositories
- [ ] Handle null dates in row mappers consistently
- [ ] Validate ResultSet values before mapping

## Transaction Management
- [ ] Add `@Transactional` to repository save operations
- [ ] Wrap multi-step operations in transactions (test data generation)

## Error Handling
- [ ] Replace generic RuntimeException wrapping in DatabaseConfig
- [ ] Add global exception handler for API validation errors
- [ ] Implement consistent error responses

## Validation
- [ ] Add `@Valid` annotations to controller request bodies
- [ ] Implement request validation at API level

## Performance
- [ ] Consider connection pooling for production
- [ ] Add query performance monitoring
- [ ] Optimize SELECT queries (avoid SELECT *)

---
*Foundation is solid - these are production-readiness improvements*