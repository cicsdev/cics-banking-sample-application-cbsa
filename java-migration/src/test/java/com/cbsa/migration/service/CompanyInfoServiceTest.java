package com.cbsa.migration.service;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Unit tests for the CompanyInfoService class.
 */
public class CompanyInfoServiceTest {

    @Test
    public void testGetCompanyName() {
        // 1. Arrange
        CompanyInfoService service = new CompanyInfoService();
        String expectedCompanyName = "CICS Bank Sample Application";
        
        // 2. Act
        String actualCompanyName = service.getCompanyName();
        
        // 3. Assert
        assertEquals(expectedCompanyName, actualCompanyName, "Company name should match the value from COBOL GETCOMPY.cbl");
    }
}
