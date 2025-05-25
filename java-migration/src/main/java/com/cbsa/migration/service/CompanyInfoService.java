package com.cbsa.migration.service;

import org.springframework.stereotype.Service;

/**
 * This service replicates the functionality of the GETCOMPY COBOL program.
 * It provides the company name, which in the original COBOL application
 * was a fixed value of 'CICS Bank Sample Application'.
 */
@Service
public class CompanyInfoService {

    private static final String COMPANY_NAME = "CICS Bank Sample Application";

    /**
     * Gets the company name.
     *
     * @return The company name as a string
     */
    public String getCompanyName() {
        return COMPANY_NAME;
    }
}
