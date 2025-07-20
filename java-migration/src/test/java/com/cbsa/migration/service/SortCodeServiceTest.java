package com.cbsa.migration.service;

import org.junit.jupiter.api.Test;
import static org.assertj.core.api.Assertions.assertThat;

class SortCodeServiceTest {

    @Test
    void shouldReturnCorrectSortCode() {
        // Given
        SortCodeService service = new SortCodeService();
        
        // When
        String result = service.getSortCode();
        
        // Then
        assertThat(result).isEqualTo("987654");
    }
    
    @Test
    void shouldReturnSameValueOnMultipleCalls() {
        // Given
        SortCodeService service = new SortCodeService();
        
        // When
        String result1 = service.getSortCode();
        String result2 = service.getSortCode();
        
        // Then
        assertThat(result1).isEqualTo(result2);
        assertThat(result1).isEqualTo("987654");
    }
}
