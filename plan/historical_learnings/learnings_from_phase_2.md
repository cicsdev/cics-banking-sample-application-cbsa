# COBOL to Java Migration - Phase 2 Documentation

## 1. Overview

This document details the Phase 2 migration effort, which focused on migrating utility functions from the original COBOL programs to Java services. Specifically, this phase addressed the migration of:

1. `GETSCODE.cbl` - A COBOL program that returns a bank sort code
2. `GETCOMPY.cbl` - A COBOL program that returns the company name

## 2. COBOL to Java Mapping

### 2.1. GETSCODE.cbl to SortCodeService.java

#### 2.1.1. COBOL Implementation

```cobol
       CBL CICS('SP,EDF')
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GETSCODE.

       WORKING-STORAGE SECTION.
       COPY SORTCODE REPLACING ==SORTCODE== BY ==LITERAL-SORTCODE==.

       LINKAGE SECTION.
       01 DFHCOMMAREA.
           COPY GETSCODE.

       PROCEDURE DIVISION USING DFHCOMMAREA.
       PREMIERE SECTION.
       A010.
           MOVE LITERAL-SORTCODE
           TO SORTCODE OF DFHCOMMAREA.

           EXEC CICS RETURN
           END-EXEC.

           GOBACK.
```

With SORTCODE.cpy containing:
```cobol
77 SORTCODE           PIC 9(6) VALUE 987654.
```

And GETSCODE.cpy containing:
```cobol
03 GETSORTCODEOperation.
  06 SORTCODE pic xXXXXX.
```

#### 2.1.2. Java Implementation

```java
package com.cbsa.migration.service;

import org.springframework.stereotype.Service;

@Service
public class SortCodeService {

    private static final String SORT_CODE = "987654";

    public String getSortCode() {
        return SORT_CODE;
    }
}
```

### 2.2. GETCOMPY.cbl to CompanyInfoService.java

#### 2.2.1. COBOL Implementation

```cobol
       CBL CICS('SP,EDF')
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GETCOMPY.

       LINKAGE SECTION.
       01 DFHCOMMAREA.
           COPY GETCOMPY.

       PROCEDURE DIVISION USING DFHCOMMAREA.
       PREMIERE SECTION.
       A010.
           move 'CICS Bank Sample Application' to COMPANY-NAME.

           EXEC CICS RETURN
           END-EXEC.

           GOBACK.
```

With GETCOMPY.cpy containing:
```cobol
03 GETCompanyOperation.
  06 company-name pic x(40).
```

#### 2.2.2. Java Implementation

```java
package com.cbsa.migration.service;

import org.springframework.stereotype.Service;

@Service
public class CompanyInfoService {

    private static final String COMPANY_NAME = "CICS Bank Sample Application";

    public String getCompanyName() {
        return COMPANY_NAME;
    }
}
```

## 3. REST API Design

The original COBOL programs were accessed via CICS transactions. In our Java migration, we've implemented a REST API layer to expose these functions:

```java
@RestController
@RequestMapping("/api/utility")
public class UtilityController {

    private final SortCodeService sortCodeService;
    private final CompanyInfoService companyInfoService;

    @Autowired
    public UtilityController(SortCodeService sortCodeService, CompanyInfoService companyInfoService) {
        this.sortCodeService = sortCodeService;
        this.companyInfoService = companyInfoService;
    }

    @GetMapping("/sortcode")
    public ResponseEntity<SortCodeResponse> getSortCode() {
        String sortCode = sortCodeService.getSortCode();
        SortCodeResponse response = new SortCodeResponse(sortCode);
        return ResponseEntity.ok(response);
    }

    @GetMapping("/company-name")
    public ResponseEntity<CompanyInfoResponse> getCompanyName() {
        String companyName = companyInfoService.getCompanyName();
        CompanyInfoResponse response = new CompanyInfoResponse(companyName);
        return ResponseEntity.ok(response);
    }
}
```

## 4. Data Model Conversion

### 4.1. COBOL to Java Data Type Mapping

| COBOL Data Type | Java Data Type | Notes |
|----------------|----------------|-------|
| PIC X(n) | String | Fixed-length character field |
| PIC 9(n) | String or int/long | Numeric field, depending on size |
| PIC S9(n) | int/long | Signed numeric field |
| PIC S9(n)V9(m) | BigDecimal | Decimal numeric field |
| COMP-3 | BigDecimal | Packed decimal field |

### 4.2. Helper Utilities

The CobolConverter utility class was created to handle common COBOL-to-Java data conversion tasks:

1. Date format conversion between COBOL (YYYYMMDD) and Java (LocalDate)
2. String padding for fixed-length fields (PIC X(n))
3. Numeric padding with leading zeros (PIC 9(n))
4. Handling of COMP-3 (packed decimal) fields

## 5. Testing Strategy

### 5.1. Unit Tests

Unit tests were created for each service to verify that they return the expected constant values:

1. SortCodeServiceTest - verifies that the sort code value matches the original COBOL value
2. CompanyInfoServiceTest - verifies that the company name matches the original COBOL value
3. CobolConverterTest - verifies the utility methods for data conversion

### 5.2. Integration Tests

Integration tests were created for the REST controller to verify that the endpoints return the expected responses:

1. UtilityControllerTest - verifies both the /api/utility/sortcode and /api/utility/company-name endpoints

## 6. Design Decisions

1. **Service Layer**: Created dedicated service classes for each COBOL program to maintain separation of concerns
2. **REST API**: Implemented a REST API layer to make the functionality accessible via HTTP
3. **Response Models**: Created structured response models instead of using simple maps
4. **Utility Class**: Created a CobolConverter utility class to handle common data conversion tasks
5. **Constants**: Used constants for fixed values to maintain the same behavior as the original COBOL programs

## 7. Lessons Learned

1. **Simplicity**: The migrated COBOL programs were very simple, serving as utilities that return constant values. This made the migration straightforward.
2. **Data Type Conversion**: COBOL has specific data types (like PIC X(n), PIC 9(n), COMP-3) that need careful handling in Java.
3. **Testing**: Comprehensive testing is crucial to ensure that the migrated code behaves the same way as the original COBOL code.
4. **Documentation**: Documenting the mapping between COBOL and Java implementations helps maintain the system and supports future migration efforts.

## 8. Next Steps

The following items are recommended for the next phase of the migration:

1. Migrate more complex COBOL programs with database interactions
2. Implement transaction management for database operations
3. Create a more comprehensive CobolConverter utility with additional data conversion functions
4. Develop a consistent strategy for error handling and logging
5. Implement security measures for the REST API
