package com.cbsa.migration.model;

import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class ApplicationErrorTest {

    @Test
    void testConstructor_withMinimalFields_createsValidObject() {
        // Given
        String applicationId = "TESTAPP";
        String errorCode = "ABND";
        
        // When
        ApplicationError error = new ApplicationError();
        error.setApplicationId(applicationId);
        error.setErrorCode(errorCode);
        
        // Then
        assertThat(error).isNotNull();
        assertThat(error.getApplicationId()).isEqualTo(applicationId);
        assertThat(error.getErrorCode()).isEqualTo(errorCode);
        assertThat(error.getId()).isNull(); // Not set for new objects
    }

    @Test
    void testAllArgsConstructor_setsAllProperties() {
        // Given
        Long id = 1L;
        String timestamp = "2023-12-25T10:30:00";
        String applicationId = "BNKAPP";
        String transactionId = "TRAN001";
        String errorCode = "ABND";
        String programName = "ABNDPROC";
        String errorMessage = "Test error message";
        String stackTrace = "Stack trace here";
        String createdAt = "2023-12-25T10:30:00";
        String responseCode = "12";
        String response2Code = "34";
        String sqlCode = "-803";
        String freeformText = "Database constraint violation";
        
        // When
        ApplicationError error = new ApplicationError(id, timestamp, applicationId, transactionId,
            errorCode, programName, errorMessage, stackTrace, createdAt,
            responseCode, response2Code, sqlCode, freeformText);
        
        // Then
        assertThat(error.getId()).isEqualTo(id);
        assertThat(error.getApplicationId()).isEqualTo(applicationId);
        assertThat(error.getTransactionId()).isEqualTo(transactionId);
        assertThat(error.getTimestamp()).isEqualTo(timestamp);
        assertThat(error.getErrorCode()).isEqualTo(errorCode);
        assertThat(error.getProgramName()).isEqualTo(programName);
        assertThat(error.getResponseCode()).isEqualTo(responseCode);
        assertThat(error.getResponse2Code()).isEqualTo(response2Code);
        assertThat(error.getSqlCode()).isEqualTo(sqlCode);
        assertThat(error.getFreeformText()).isEqualTo(freeformText);
    }

    @Test
    void testIsCritical_withAbendCode_returnsTrue() {
        // Given
        ApplicationError error = new ApplicationError();
        error.setErrorCode("ABND");
        
        // When
        boolean result = error.isCritical();
        
        // Then
        assertThat(result).isTrue();
    }

    @Test
    void testIsCritical_withAsraCode_returnsTrue() {
        // Given
        ApplicationError error = new ApplicationError();
        error.setErrorCode("ASRA");
        
        // When
        boolean result = error.isCritical();
        
        // Then
        assertThat(result).isTrue();
    }

    @Test
    void testIsCritical_withNormalCode_returnsFalse() {
        // Given
        ApplicationError error = new ApplicationError();
        error.setErrorCode("INFO");
        
        // When
        boolean result = error.isCritical();
        
        // Then
        assertThat(result).isFalse();
    }

    @Test
    void testGetErrorSummary_containsEssentialFields() {
        // Given
        ApplicationError error = new ApplicationError();
        error.setApplicationId("TESTAPP");
        error.setTransactionId("TRAN123");
        error.setErrorCode("ABND");
        error.setProgramName("TESTPROG");
        error.setErrorMessage("Test error message");
        
        // When
        String result = error.getErrorSummary();
        
        // Then
        assertThat(result).contains("ABND");
        assertThat(result).contains("TESTPROG");
        assertThat(result).contains("TRAN123");
        assertThat(result).contains("Test error message");
    }
}
