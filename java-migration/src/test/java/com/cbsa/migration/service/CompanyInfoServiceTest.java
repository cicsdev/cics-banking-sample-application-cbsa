package com.cbsa.migration.service;

import org.junit.jupiter.api.Test;
import static org.assertj.core.api.Assertions.assertThat;

class CompanyInfoServiceTest {

    @Test
    void shouldReturnCorrectCompanyName() {
        // Given
        CompanyInfoService service = new CompanyInfoService();
        
        // When  
        String result = service.getCompanyName();
        
        // Then
        assertThat(result).isEqualTo("CICS Bank Sample Application");
    }
}
