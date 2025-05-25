package com.cbsa.migration.service;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Unit tests for the SortCodeService class.
 */
public class SortCodeServiceTest {

    @Test
    public void testGetSortCode() {
        // 1. Arrange
        SortCodeService service = new SortCodeService();
        String expectedSortCode = "987654";
        
        // 2. Act
        String actualSortCode = service.getSortCode();
        
        // 3. Assert
        assertEquals(expectedSortCode, actualSortCode, "Sort code should match the value from COBOL SORTCODE.cpy");
    }
}
