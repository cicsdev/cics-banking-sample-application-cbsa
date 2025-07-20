# COBOL Program Analysis: GETCOMPY

## Overview
GETCOMPY is a simple CICS program that returns the company name "CICS Bank Sample Application" to the caller. The program has minimal functionality - it only sets a value in the COMPANY-NAME field and returns control to CICS.

## Data Types

| COBOL Field | COBOL Definition | Java Type | Java Field Name | Notes |
|-------------|------------------|-----------|----------------|-------|
| COMPANY-NAME | PIC X(40) | String | companyName | Part of GETCompanyOperation structure |

## External Calls
```
40:           EXEC CICS RETURN
```

The program contains a single CICS call to return control to the caller. This is a standard termination call in CICS programs.

## Ambiguities
No ambiguities detected. The program is straightforward with minimal complexity.

## Recommended Java Artifacts

### 1. DTO
```java
package com.bank.cbsa.company.dto;

public class CompanyDto {
    private String companyName;
    
    // Getters and setters
    public String getCompanyName() {
        return companyName;
    }
    
    public void setCompanyName(String companyName) {
        this.companyName = companyName;
    }
}
```

### 2. Service Interface
```java
package com.bank.cbsa.company.service;

import com.bank.cbsa.company.dto.CompanyDto;

public interface CompanyService {
    CompanyDto getCompanyInfo();
}
```

### 3. Service Implementation
```java
package com.bank.cbsa.company.service.impl;

import com.bank.cbsa.company.dto.CompanyDto;
import com.bank.cbsa.company.service.CompanyService;
import org.springframework.stereotype.Service;

@Service
public class CompanyServiceImpl implements CompanyService {
    
    @Override
    public CompanyDto getCompanyInfo() {
        CompanyDto companyDto = new CompanyDto();
        companyDto.setCompanyName("CICS Bank Sample Application");
        return companyDto;
    }
}
```

### 4. Controller (Optional)
```java
package com.bank.cbsa.company.controller;

import com.bank.cbsa.company.dto.CompanyDto;
import com.bank.cbsa.company.service.CompanyService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/company")
public class CompanyController {
    
    private final CompanyService companyService;
    
    @Autowired
    public CompanyController(CompanyService companyService) {
        this.companyService = companyService;
    }
    
    @GetMapping
    public CompanyDto getCompanyInfo() {
        return companyService.getCompanyInfo();
    }
}
```

### Migration Notes
1. This is a very simple program that only returns a static company name.
2. No database interactions are required.
3. The Java implementation can be a simple service that returns a hardcoded value.
4. No special error handling is required as the original program has no error paths.
