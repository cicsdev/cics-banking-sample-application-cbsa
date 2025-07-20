package com.cbsa.migration.dto;

import org.junit.jupiter.api.Test;

import javax.validation.Validation;
import javax.validation.Validator;
import javax.validation.ValidatorFactory;
import javax.validation.ConstraintViolation;
import java.util.Set;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * Unit tests for ErrorRequestDto validation and functionality.
 */
class ErrorRequestDtoTest {

    private final ValidatorFactory factory = Validation.buildDefaultValidatorFactory();
    private final Validator validator = factory.getValidator();

    @Test
    void testValidErrorRequest_passesValidation() {
        // Given
        ErrorRequestDto dto = new ErrorRequestDto();
        dto.setProgramName("TESTPROG");
        dto.setApplicationId("TESTAPP");
        dto.setTransactionId("T001");
        dto.setErrorCode("ABND");
        dto.setFreeformText("Test error message");

        // When
        Set<ConstraintViolation<ErrorRequestDto>> violations = validator.validate(dto);

        // Then
        assertThat(violations).isEmpty();
    }

    @Test
    void testMissingProgramName_failsValidation() {
        // Given
        ErrorRequestDto dto = new ErrorRequestDto();
        dto.setApplicationId("TESTAPP");
        dto.setFreeformText("Test error");

        // When
        Set<ConstraintViolation<ErrorRequestDto>> violations = validator.validate(dto);

        // Then
        assertThat(violations).hasSize(1);
        assertThat(violations.iterator().next().getMessage()).isEqualTo("Program name is required");
    }

    @Test
    void testBlankProgramName_failsValidation() {
        // Given
        ErrorRequestDto dto = new ErrorRequestDto();
        dto.setProgramName("");
        dto.setApplicationId("TESTAPP");
        dto.setFreeformText("Test error");

        // When
        Set<ConstraintViolation<ErrorRequestDto>> violations = validator.validate(dto);

        // Then
        assertThat(violations).hasSize(1);
        assertThat(violations.iterator().next().getMessage()).isEqualTo("Program name is required");
    }

    @Test
    void testProgramNameTooLong_failsValidation() {
        // Given
        ErrorRequestDto dto = new ErrorRequestDto();
        dto.setProgramName("TOOLONGPROGRAMNAME"); // 18 characters, max is 8
        dto.setApplicationId("TESTAPP");
        dto.setFreeformText("Test error");

        // When
        Set<ConstraintViolation<ErrorRequestDto>> violations = validator.validate(dto);

        // Then
        assertThat(violations).hasSize(1);
        assertThat(violations.iterator().next().getMessage()).isEqualTo("Program name must not exceed 8 characters");
    }

    @Test
    void testTransactionIdTooLong_failsValidation() {
        // Given
        ErrorRequestDto dto = new ErrorRequestDto();
        dto.setProgramName("TESTPROG");
        dto.setTransactionId("TOOLONG"); // 7 characters, max is 4
        dto.setFreeformText("Test error");

        // When
        Set<ConstraintViolation<ErrorRequestDto>> violations = validator.validate(dto);

        // Then
        assertThat(violations).hasSize(1);
        assertThat(violations.iterator().next().getMessage()).isEqualTo("Transaction ID must not exceed 4 characters");
    }

    @Test
    void testFreeformTextTooLong_failsValidation() {
        // Given
        ErrorRequestDto dto = new ErrorRequestDto();
        dto.setProgramName("TESTPROG");
        dto.setFreeformText("A".repeat(601)); // 601 characters, max is 600

        // When
        Set<ConstraintViolation<ErrorRequestDto>> violations = validator.validate(dto);

        // Then
        assertThat(violations).hasSize(1);
        assertThat(violations.iterator().next().getMessage()).isEqualTo("Freeform text must not exceed 600 characters");
    }

    @Test
    void testFromException_createsValidDto() {
        // Given
        String programName = "TESTPROG";
        Exception exception = new RuntimeException("Test exception message");

        // When
        ErrorRequestDto dto = ErrorRequestDto.fromException(programName, exception);

        // Then
        assertThat(dto.getProgramName()).isEqualTo("TESTPROG");
        assertThat(dto.getErrorCode()).isEqualTo("JAVA");
        assertThat(dto.getErrorMessage()).isEqualTo("Test exception message");
        assertThat(dto.getStackTrace()).isNotNull();
        assertThat(dto.getStackTrace()).contains("RuntimeException: Test exception message");
    }

    @Test
    void testFromExceptionWithContext_includesAllFields() {
        // Given
        String programName = "TESTPROG";
        String applicationId = "TESTAPP";
        String transactionId = "T001";
        Exception exception = new RuntimeException("Test exception message");

        // When
        ErrorRequestDto dto = ErrorRequestDto.fromException(programName, applicationId, transactionId, exception);

        // Then
        assertThat(dto.getProgramName()).isEqualTo("TESTPROG");
        assertThat(dto.getApplicationId()).isEqualTo("TESTAPP");
        assertThat(dto.getTransactionId()).isEqualTo("T001");
        assertThat(dto.getErrorCode()).isEqualTo("JAVA");
        assertThat(dto.getErrorMessage()).isEqualTo("Test exception message");
        assertThat(dto.getStackTrace()).isNotNull();
    }

    @Test
    void testFromExceptionWithNullMessage_handlesGracefully() {
        // Given
        String programName = "TESTPROG";
        Exception exception = new RuntimeException((String) null);

        // When
        ErrorRequestDto dto = ErrorRequestDto.fromException(programName, exception);

        // Then
        assertThat(dto.getProgramName()).isEqualTo("TESTPROG");
        assertThat(dto.getErrorCode()).isEqualTo("JAVA");
        assertThat(dto.getErrorMessage()).isNull();
        assertThat(dto.getStackTrace()).isNotNull();
    }

    @Test
    void testLombokMethods_workCorrectly() {
        // Given
        ErrorRequestDto dto1 = new ErrorRequestDto();
        dto1.setProgramName("TESTPROG");
        dto1.setApplicationId("TESTAPP");

        ErrorRequestDto dto2 = new ErrorRequestDto();
        dto2.setProgramName("TESTPROG");
        dto2.setApplicationId("TESTAPP");

        // When & Then
        assertThat(dto1).isEqualTo(dto2);
        assertThat(dto1.hashCode()).isEqualTo(dto2.hashCode());
        assertThat(dto1.toString()).contains("TESTPROG");
        assertThat(dto1.toString()).contains("TESTAPP");
    }
}
