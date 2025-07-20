package com.cbsa.migration.repository.jdbc;

import com.cbsa.migration.model.ApplicationError;
import com.cbsa.migration.repository.JdbcApplicationErrorRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.jdbc.JdbcTest;
import org.springframework.context.annotation.Import;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.test.context.jdbc.Sql;

import java.util.List;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * Integration tests for JdbcApplicationErrorRepository
 * Tests database interactions using H2 in-memory database
 */
@JdbcTest
@Import(JdbcApplicationErrorRepository.class)
@Sql("/db/test-schema.sql")
public class JdbcApplicationErrorRepositoryTest {

    @Autowired
    private JdbcTemplate jdbcTemplate;

    @Autowired
    private JdbcApplicationErrorRepository errorRepository;

    @BeforeEach
    void setUp() {
        // Clean up any existing error records
        jdbcTemplate.update("DELETE FROM application_error");
    }

    @Test
    void testSave_newError_insertsAndReturnsGeneratedId() {
        // Given
        ApplicationError error = new ApplicationError();
        error.setApplicationId("TESTAPP");
        error.setTransactionId("TRAN001");
        error.setTimestamp("2023-12-25T10:30:00");
        error.setErrorCode("ABND");
        error.setProgramName("TESTPROG");
        error.setResponseCode("12");
        error.setResponse2Code("34");
        error.setSqlCode("-803");
        error.setFreeformText("Test error message");

        // When
        Long savedId = errorRepository.save(error);

        // Then
        assertThat(savedId).isNotNull();
        assertThat(savedId).isGreaterThan(0L);
        
        // Verify the error was saved with correct ID
        Optional<ApplicationError> savedError = errorRepository.findById(savedId);
        assertThat(savedError).isPresent();
        assertThat(savedError.get().getId()).isEqualTo(savedId);
        assertThat(savedError.get().getApplicationId()).isEqualTo("TESTAPP");
    }

    @Test
    void testSave_secondError_insertsNewRecord() {
        // Given - Insert initial record
        ApplicationError firstError = new ApplicationError();
        firstError.setTimestamp("2023-12-25T10:30:00");
        firstError.setApplicationId("TESTAPP");
        firstError.setProgramName("TESTPROG");
        firstError.setErrorCode("ABND");
        firstError.setFreeformText("First error message");
        
        Long firstId = errorRepository.save(firstError);
        
        ApplicationError secondError = new ApplicationError();
        secondError.setTimestamp("2023-12-25T10:31:00");
        secondError.setApplicationId("TESTAPP");
        secondError.setProgramName("TESTPROG");
        secondError.setErrorCode("UPDT");
        secondError.setFreeformText("Second error message");

        // When
        Long secondId = errorRepository.save(secondError);

        // Then - Should insert new record with different ID
        assertThat(secondId).isNotEqualTo(firstId);
        assertThat(secondId).isGreaterThan(firstId);
        
        // Verify both records exist
        Optional<ApplicationError> first = errorRepository.findById(firstId);
        Optional<ApplicationError> second = errorRepository.findById(secondId);
        assertThat(first).isPresent();
        assertThat(second).isPresent();
        assertThat(first.get().getFreeformText()).isEqualTo("First error message");
        assertThat(second.get().getFreeformText()).isEqualTo("Second error message");
    }

    @Test
    void testFindById_existingRecord_returnsApplicationError() {
        // Given - Insert test record
        jdbcTemplate.update(
            "INSERT INTO application_error (application_id, transaction_id, timestamp, " +
            "error_code, program_name, response_code, response2_code, sql_code, freeform_text) " +
            "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)",
            "TESTAPP", "TRAN001", "2023-12-25T10:30:00", "ABND", "TESTPROG", 
            "12", "34", "-803", "Test error message"
        );
        
        Long errorId = jdbcTemplate.queryForObject(
            "SELECT id FROM application_error WHERE application_id = ?",
            Long.class, "TESTAPP");

        // When
        Optional<ApplicationError> result = errorRepository.findById(errorId);

        // Then
        assertThat(result).isPresent();
        ApplicationError error = result.get();
        assertThat(error.getId()).isEqualTo(errorId);
        assertThat(error.getApplicationId()).isEqualTo("TESTAPP");
        assertThat(error.getTransactionId()).isEqualTo("TRAN001");
        assertThat(error.getErrorCode()).isEqualTo("ABND");
        assertThat(error.getProgramName()).isEqualTo("TESTPROG");
        assertThat(error.getResponseCode()).isEqualTo("12");
        assertThat(error.getResponse2Code()).isEqualTo("34");
        assertThat(error.getSqlCode()).isEqualTo("-803");
        assertThat(error.getFreeformText()).isEqualTo("Test error message");
    }

    @Test
    void testFindById_nonExistentRecord_returnsEmpty() {
        // When
        Optional<ApplicationError> result = errorRepository.findById(999L);

        // Then
        assertThat(result).isEmpty();
    }

    @Test
    void testFindByProgramName_returnsMatchingErrors() {
        // Given - Insert multiple test records
        jdbcTemplate.update(
            "INSERT INTO application_error (timestamp, application_id, program_name, error_code, freeform_text) VALUES (?, ?, ?, ?, ?)",
            "2023-12-25T10:30:00", "APP1", "PROG1", "ERR1", "Error 1"
        );
        jdbcTemplate.update(
            "INSERT INTO application_error (timestamp, application_id, program_name, error_code, freeform_text) VALUES (?, ?, ?, ?, ?)",
            "2023-12-25T10:31:00", "APP2", "PROG1", "ERR2", "Error 2"
        );
        jdbcTemplate.update(
            "INSERT INTO application_error (timestamp, application_id, program_name, error_code, freeform_text) VALUES (?, ?, ?, ?, ?)",
            "2023-12-25T10:32:00", "APP3", "PROG2", "ERR3", "Error 3"
        );

        // When
        List<ApplicationError> result = errorRepository.findByProgramName("PROG1");

        // Then
        assertThat(result).hasSize(2);
        assertThat(result).allMatch(error -> "PROG1".equals(error.getProgramName()));
        assertThat(result).extracting(ApplicationError::getErrorCode)
            .containsExactlyInAnyOrder("ERR1", "ERR2");
    }

    @Test
    void testFindByApplicationAndTransaction_returnsMatchingErrors() {
        // Given - Insert multiple test records
        jdbcTemplate.update(
            "INSERT INTO application_error (timestamp, application_id, transaction_id, program_name, error_code) VALUES (?, ?, ?, ?, ?)",
            "2023-12-25T10:30:00", "APP1", "TRAN1", "PROG1", "ERR1"
        );
        jdbcTemplate.update(
            "INSERT INTO application_error (timestamp, application_id, transaction_id, program_name, error_code) VALUES (?, ?, ?, ?, ?)",
            "2023-12-25T10:31:00", "APP1", "TRAN1", "PROG1", "ERR2"
        );
        jdbcTemplate.update(
            "INSERT INTO application_error (timestamp, application_id, transaction_id, program_name, error_code) VALUES (?, ?, ?, ?, ?)",
            "2023-12-25T10:32:00", "APP1", "TRAN2", "PROG1", "ERR3"
        );

        // When
        List<ApplicationError> result = errorRepository.findByApplicationAndTransaction("APP1", "TRAN1");

        // Then
        assertThat(result).hasSize(2);
        assertThat(result).allMatch(error -> "APP1".equals(error.getApplicationId()) && "TRAN1".equals(error.getTransactionId()));
        assertThat(result).extracting(ApplicationError::getErrorCode)
            .containsExactlyInAnyOrder("ERR1", "ERR2");
    }
}
