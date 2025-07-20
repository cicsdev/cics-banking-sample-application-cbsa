# COBOL Programs Main Migration List

## Summary
**Total COBOL Programs to Migrate: 29**
**Total COBOL Copybooks: 37**

## COBOL Programs (.cbl files)

### 1. Core Banking Operations (19 programs)
1. **BNK1CAC.cbl** - Account Creation
2. **BNK1CCA.cbl** - Credit Card Account
3. **BNK1CCS.cbl** - Credit Card Services
4. **BNK1CRA.cbl** - Credit Account
5. **BNK1DAC.cbl** - Debit Account
6. **BNK1DCS.cbl** - Debit Card Services
7. **BNK1TFN.cbl** - Transfer Function
8. **BNK1UAC.cbl** - Update Account
9. **BNKMENU.cbl** - Banking Menu
10. **CREACC.cbl** - Create Account
11. **CRECUST.cbl** - Create Customer
12. **DELACC.cbl** - Delete Account
13. **DELCUS.cbl** - Delete Customer
14. **INQACC.cbl** - Inquire Account
15. **INQACCCU.cbl** - Inquire Account Customer
16. **INQCUST.cbl** - Inquire Customer
17. **UPDACC.cbl** - Update Account
18. **UPDCUST.cbl** - Update Customer
19. **XFRFUN.cbl** - Transfer Function

### 2. Credit Agency Services (5 programs)
20. **CRDTAGY1.cbl** - Credit Agency 1
21. **CRDTAGY2.cbl** - Credit Agency 2
22. **CRDTAGY3.cbl** - Credit Agency 3
23. **CRDTAGY4.cbl** - Credit Agency 4
24. **CRDTAGY5.cbl** - Credit Agency 5

### 3. Utility Programs (3 programs)
25. **GETCOMPY.cbl** - Get Company Information
26. **GETSCODE.cbl** - Get Sort Code
27. **DBCRFUN.cbl** - Debit/Credit Function

### 4. System Programs (2 programs)
28. **ABNDPROC.cbl** - Abend Processing
29. **BANKDATA.cbl** - Bank Data Processing

## COBOL Copybooks (.cpy files) - 37 total

### Data Structures (12 copybooks)
1. **ACCOUNT.cpy** - Account data structure
2. **CUSTOMER.cpy** - Customer data structure
3. **CONTROLI.cpy** - Control information
4. **ABNDINFO.cpy** - Abend information
5. **PAYDBCR.cpy** - Payment debit/credit
6. **PROCTRAN.cpy** - Process transaction
7. **PROCISRT.cpy** - Process insert
8. **RESPSTR.cpy** - Response structure
9. **SORTCODE.cpy** - Sort code structure
10. **STCUSTNO.cpy** - Store customer number
11. **NEWACCNO.cpy** - New account number
12. **NEWCUSNO.cpy** - New customer number

### Database Interfaces (4 copybooks)
13. **ACCDB2.cpy** - Account DB2 interface
14. **CONTDB2.cpy** - Control DB2 interface
15. **PROCDB2.cpy** - Process DB2 interface
16. **WAZI.cpy** - Wazi interface

### Control Structures (4 copybooks)
17. **ACCTCTRL.cpy** - Account control
18. **CUSTCTRL.cpy** - Customer control
19. **BNK1DDM.cpy** - Bank DDM
20. **BANKMAP.cpy** - Bank mapping

### Screen Maps (2 copybooks)
21. **CUSTMAP.cpy** - Customer mapping
22. **BANKMAP.cpy** - Bank mapping (duplicate entry - see Control Structures)

### Function-Specific Copybooks (15 copybooks)
23. **CREACC.cpy** - Create account
24. **CRECUST.cpy** - Create customer
25. **DELACC.cpy** - Delete account
26. **DELACCZ.cpy** - Delete account (variant)
27. **DELCUS.cpy** - Delete customer
28. **GETCOMPY.cpy** - Get company
29. **GETSCODE.cpy** - Get sort code
30. **INQACC.cpy** - Inquire account
31. **INQACCCU.cpy** - Inquire account customer
32. **INQACCCZ.cpy** - Inquire account (variant)
33. **INQACCZ.cpy** - Inquire account (variant)
34. **INQCUST.cpy** - Inquire customer
35. **INQCUSTZ.cpy** - Inquire customer (variant)
36. **UPDACC.cpy** - Update account
37. **UPDCUST.cpy** - Update customer
38. **XFRFUN.cpy** - Transfer function

## Migration Priority (Based on Complexity Analysis)

### Phase 1 - Simple Programs (Recommended Start)
1. **GETCOMPY.cbl** (44 lines) - Returns hardcoded company name
2. **GETSCODE.cbl** (47 lines) - Returns sort code value
3. **ABNDPROC.cbl** (177 lines) - Abend processing
4. **CRDTAGY1.cbl** (~150+ lines) - Credit agency with containers

### Phase 2 - Medium Complexity Programs
- Core banking operations (CREACC, CRECUST, INQACC, INQCUST, etc.)
- Account management programs (UPDACC, UPDCUST, DELACC, DELCUS)

### Phase 3 - Complex Programs
- Menu and navigation programs (BNKMENU)
- Complex transaction programs (BNK1* series)
- Transfer and financial operations (XFRFUN, DBCRFUN)

### Phase 4 - Credit Agency Suite
- CRDTAGY2 through CRDTAGY5 programs
- Integration with external credit services

## Notes
- All programs are located in `/src/base/cobol_src/`
- All copybooks are located in `/src/base/cobol_copy/`
- Migration should follow the established Spring Boot + SQLite architecture
- Each program will need corresponding Java service classes, controllers, and data models
- Copybooks will be converted to Java POJOs and DTOs
- Database operations will use JDBC with SQLite instead of DB2/VSAM

## Dependencies
- Many programs share common copybooks (ACCOUNT.cpy, CUSTOMER.cpy, etc.)
- Database interface copybooks (ACCDB2.cpy, CONTDB2.cpy) will need significant refactoring
- Screen mapping copybooks may not be needed in REST API implementation

## Estimated Migration Effort
- **Simple Programs (4)**: 1-2 weeks
- **Medium Programs (15)**: 6-8 weeks  
- **Complex Programs (6)**: 4-6 weeks
- **Credit Agency Suite (4)**: 3-4 weeks
- **Total Estimated Time**: 14-20 weeks

*Last Updated: July 2, 2025*
