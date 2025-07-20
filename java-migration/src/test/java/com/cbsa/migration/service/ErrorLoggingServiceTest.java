package com.cbsa.migration.service;

import com.cbsa.migration.dto.ErrorRequestDto;
import com.cbsa.migration.dto.ErrorResponseDto;
import com.cbsa.migration.model.ApplicationError;
import com.cbsa.migration.repository.ApplicationErrorRepository;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.jdbc.Sql;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Tests for ErrorLoggingService
 * Includes both unit tests (with mocked repository) and integration tests
 */
class ErrorLoggingServiceTest {

    // Unit tests with mocked repository
    @ExtendWith(MockitoExtension.class)
    static class UnitTests {

        @Mock
        private ApplicationErrorRepository errorRepository;

        @InjectMocks
        private ErrorLoggingService errorLoggingService;

        @Test
        void testLogError_validRequest_callsRepositorySave() {
            // Given
            ErrorRequestDto request = new ErrorRequestDto();
            request.setApplicationId("TESTAPP");
            request.setTransactionId("TRAN001");
            request.setErrorCode("ABND");
            request.setProgramName("TESTPROG");
            request.setResponseCode("12");
            request.setResponse2Code("34");
            request.setSqlCode("-803");
            request.setFreeformText("Test error message");

            when(errorRepository.save(any(ApplicationError.class))).thenReturn(1L);

            // When
            ErrorResponseDto response = errorLoggingService.logError(request);

            // Then
            verify(errorRepository).save(any(ApplicationError.class));
            assertThat(response).isNotNull();
            assertThat(response.getErrorId()).isEqualTo(1L);
            assertThat(response.getStatus()).isEqualTo("SUCCESS");
            assertThat(response.getMessage()).isNotNull();
        }

        @Test
        void testLogError_nullRequest_throwsIllegalArgumentException() {
            // When/Then
            assertThatThrownBy(() -> errorLoggingService.logError(null))
                .isInstanceOf(IllegalArgumentException.class)
                .hasMessage("Error request cannot be null");
        }

        @Test
        void testLogError_emptyApplicationId_throwsValidationException() {
            // Given
            ErrorRequestDto request = new ErrorRequestDto();
            request.setApplicationId("");  // Empty application ID
            request.setErrorCode("ABND");

            // When/Then
            assertThatThrownBy(() -> errorLoggingService.logError(request))
                .isInstanceOf(IllegalArgumentException.class)
                .hasMessage("Application ID cannot be null or empty");
        }
    }

    // Integration test with real repository and database
    @SpringBootTest
    @ActiveProfiles("test")
    @Sql("/db/test-schema.sql")
    @Transactional
    static class IntegrationTest {

        @Autowired
        private ErrorLoggingService errorLoggingService;

        @Autowired
        private ApplicationErrorRepository errorRepository;

        @Test
        void testLogError_endToEnd_persistsAndRetrievesCorrectly() {
            // Given
            ErrorRequestDto request = new ErrorRequestDto();
            request.setApplicationId("INTEGRATION_TEST");
            request.setTransactionId("TRAN_INT_001");
            request.setErrorCode("ABND");
            request.setProgramName("INTTEST");
            request.setResponseCode("99");
            request.setResponse2Code("88");
            request.setSqlCode("-123");
            request.setFreeformText("Integration test error message");

            // When
            ErrorResponseDto response = errorLoggingService.logError(request);

            // Then
            assertThat(response).isNotNull();
            assertThat(response.getErrorId()).isNotNull();
            assertThat(response.getErrorId()).isGreaterThan(0L);
            assertThat(response.getStatus()).isEqualTo("SUCCESS");
            assertThat(response.getMessage()).isNotNull();
            assertThat(response.getTimestamp()).isNotNull();

            // Verify data was actually persisted
            var persistedError = errorRepository.findById(response.getErrorId());
            assertThat(persistedError).isPresent();
            assertThat(persistedError.get().getApplicationId()).isEqualTo("INTEGRATION_TEST");
            assertThat(persistedError.get().getFreeformText()).isEqualTo("Integration test error message");
        }
    }
}
