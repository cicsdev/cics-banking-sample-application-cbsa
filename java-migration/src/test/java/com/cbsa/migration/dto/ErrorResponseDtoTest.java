package com.cbsa.migration.dto;

import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * Unit tests for ErrorResponseDto functionality.
 */
class ErrorResponseDtoTest {

    @Test
    void testSuccessResponse_createsCorrectDto() {
        // Given
        Long errorId = 123L;
        String timestamp = "2023-12-25T10:30:00";

        // When
        ErrorResponseDto dto = ErrorResponseDto.success(errorId, timestamp);

        // Then
        assertThat(dto.getErrorId()).isEqualTo(123L);
        assertThat(dto.getStatus()).isEqualTo("SUCCESS");
        assertThat(dto.getMessage()).isEqualTo("Error record successfully logged");
        assertThat(dto.getTimestamp()).isEqualTo("2023-12-25T10:30:00");
    }

    @Test
    void testValidationErrorResponse_createsCorrectDto() {
        // Given
        String message = "Invalid input data";
        String timestamp = "2023-12-25T10:30:00";

        // When
        ErrorResponseDto dto = ErrorResponseDto.validationError(message, timestamp);

        // Then
        assertThat(dto.getErrorId()).isNull();
        assertThat(dto.getStatus()).isEqualTo("VALIDATION_ERROR");
        assertThat(dto.getMessage()).isEqualTo("Invalid input data");
        assertThat(dto.getTimestamp()).isEqualTo("2023-12-25T10:30:00");
    }

    @Test
    void testFailureResponse_createsCorrectDto() {
        // Given
        String message = "Database connection failed";
        String timestamp = "2023-12-25T10:30:00";

        // When
        ErrorResponseDto dto = ErrorResponseDto.failure(message, timestamp);

        // Then
        assertThat(dto.getErrorId()).isNull();
        assertThat(dto.getStatus()).isEqualTo("FAILURE");
        assertThat(dto.getMessage()).isEqualTo("Database connection failed");
        assertThat(dto.getTimestamp()).isEqualTo("2023-12-25T10:30:00");
    }

    @Test
    void testConstructor_createsCorrectDto() {
        // Given
        Long errorId = 456L;
        String status = "CUSTOM_STATUS";
        String message = "Custom message";
        String timestamp = "2023-12-25T11:45:00";

        // When
        ErrorResponseDto dto = new ErrorResponseDto(errorId, status, message, timestamp);

        // Then
        assertThat(dto.getErrorId()).isEqualTo(456L);
        assertThat(dto.getStatus()).isEqualTo("CUSTOM_STATUS");
        assertThat(dto.getMessage()).isEqualTo("Custom message");
        assertThat(dto.getTimestamp()).isEqualTo("2023-12-25T11:45:00");
    }

    @Test
    void testNoArgsConstructor_createsEmptyDto() {
        // When
        ErrorResponseDto dto = new ErrorResponseDto();

        // Then
        assertThat(dto.getErrorId()).isNull();
        assertThat(dto.getStatus()).isNull();
        assertThat(dto.getMessage()).isNull();
        assertThat(dto.getTimestamp()).isNull();
    }

    @Test
    void testSetters_updateFieldsCorrectly() {
        // Given
        ErrorResponseDto dto = new ErrorResponseDto();

        // When
        dto.setErrorId(789L);
        dto.setStatus("TEST_STATUS");
        dto.setMessage("Test message");
        dto.setTimestamp("2023-12-25T12:00:00");

        // Then
        assertThat(dto.getErrorId()).isEqualTo(789L);
        assertThat(dto.getStatus()).isEqualTo("TEST_STATUS");
        assertThat(dto.getMessage()).isEqualTo("Test message");
        assertThat(dto.getTimestamp()).isEqualTo("2023-12-25T12:00:00");
    }

    @Test
    void testLombokMethods_workCorrectly() {
        // Given
        ErrorResponseDto dto1 = new ErrorResponseDto(123L, "SUCCESS", "Message", "2023-12-25T10:30:00");
        ErrorResponseDto dto2 = new ErrorResponseDto(123L, "SUCCESS", "Message", "2023-12-25T10:30:00");

        // When & Then
        assertThat(dto1).isEqualTo(dto2);
        assertThat(dto1.hashCode()).isEqualTo(dto2.hashCode());
        assertThat(dto1.toString()).contains("123");
        assertThat(dto1.toString()).contains("SUCCESS");
        assertThat(dto1.toString()).contains("Message");
    }

    @Test
    void testSuccessResponseWithNullValues_handlesGracefully() {
        // When
        ErrorResponseDto dto = ErrorResponseDto.success(null, null);

        // Then
        assertThat(dto.getErrorId()).isNull();
        assertThat(dto.getStatus()).isEqualTo("SUCCESS");
        assertThat(dto.getMessage()).isEqualTo("Error record successfully logged");
        assertThat(dto.getTimestamp()).isNull();
    }

    @Test
    void testValidationErrorWithNullValues_handlesGracefully() {
        // When
        ErrorResponseDto dto = ErrorResponseDto.validationError(null, null);

        // Then
        assertThat(dto.getErrorId()).isNull();
        assertThat(dto.getStatus()).isEqualTo("VALIDATION_ERROR");
        assertThat(dto.getMessage()).isNull();
        assertThat(dto.getTimestamp()).isNull();
    }

    @Test
    void testFailureWithNullValues_handlesGracefully() {
        // When
        ErrorResponseDto dto = ErrorResponseDto.failure(null, null);

        // Then
        assertThat(dto.getErrorId()).isNull();
        assertThat(dto.getStatus()).isEqualTo("FAILURE");
        assertThat(dto.getMessage()).isNull();
        assertThat(dto.getTimestamp()).isNull();
    }
}
