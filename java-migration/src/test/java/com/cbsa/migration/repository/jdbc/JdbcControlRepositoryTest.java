package com.cbsa.migration.repository.jdbc;

import com.cbsa.migration.model.Control;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.jdbc.JdbcTest;
import org.springframework.context.annotation.Import;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.test.context.jdbc.Sql;

import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * Integration tests for JdbcControlRepository
 * Tests database interactions using H2 in-memory database
 */
@JdbcTest
@Import(JdbcControlRepository.class)
@Sql("/db/test-schema.sql")
public class JdbcControlRepositoryTest {

    @Autowired
    private JdbcTemplate jdbcTemplate;

    @Autowired
    private JdbcControlRepository controlRepository;

    @BeforeEach
    void setUp() {
        // Clean up any existing control records
        jdbcTemplate.update("DELETE FROM control WHERE id = ?", Control.CONTROL_ID);
    }

    @Test
    void testGetControl_WhenRecordExists_ReturnsControl() {
        // Given - Insert control record directly
        jdbcTemplate.update(
            "INSERT INTO control (id, customer_count, last_customer_number, account_count, last_account_number) " +
            "VALUES (?, ?, ?, ?, ?)",
            Control.CONTROL_ID, 5L, 100005L, 10, 10000010
        );

        // When
        Optional<Control> result = controlRepository.getControl();

        // Then
        assertThat(result).isPresent();
        Control control = result.get();
        assertThat(control.getCustomerCount()).isEqualTo(5L);
        assertThat(control.getLastCustomerNumber()).isEqualTo(100005L);
        assertThat(control.getAccountCount()).isEqualTo(10);
        assertThat(control.getLastAccountNumber()).isEqualTo(10000010);
        assertThat(control.getId()).isEqualTo("CONTROL");
    }

    @Test
    void testGetControl_WhenRecordDoesNotExist_ReturnsEmpty() {
        // When
        Optional<Control> result = controlRepository.getControl();

        // Then
        assertThat(result).isEmpty();
    }

    @Test
    void testSave_WhenRecordDoesNotExist_InsertsNewRecord() {
        // Given
        Control control = Control.builder()
            .customerCount(3L)
            .lastCustomerNumber(100003L)
            .accountCount(7)
            .lastAccountNumber(10000007)
            .build();

        // When
        Control saved = controlRepository.save(control);

        // Then
        assertThat(saved).isEqualTo(control);
        
        // Verify in database
        Optional<Control> retrieved = controlRepository.getControl();
        assertThat(retrieved).isPresent();
        assertThat(retrieved.get().getCustomerCount()).isEqualTo(3L);
        assertThat(retrieved.get().getLastCustomerNumber()).isEqualTo(100003L);
        assertThat(retrieved.get().getAccountCount()).isEqualTo(7);
        assertThat(retrieved.get().getLastAccountNumber()).isEqualTo(10000007);
    }

    @Test
    void testSave_WhenRecordExists_UpdatesExistingRecord() {
        // Given - Insert initial record
        jdbcTemplate.update(
            "INSERT INTO control (id, customer_count, last_customer_number, account_count, last_account_number) " +
            "VALUES (?, ?, ?, ?, ?)",
            Control.CONTROL_ID, 1L, 100001L, 2, 10000002
        );

        Control updatedControl = Control.builder()
            .customerCount(10L)
            .lastCustomerNumber(100010L)
            .accountCount(20)
            .lastAccountNumber(10000020)
            .build();

        // When
        Control saved = controlRepository.save(updatedControl);

        // Then
        assertThat(saved).isEqualTo(updatedControl);
        
        // Verify in database
        Optional<Control> retrieved = controlRepository.getControl();
        assertThat(retrieved).isPresent();
        assertThat(retrieved.get().getCustomerCount()).isEqualTo(10L);
        assertThat(retrieved.get().getLastCustomerNumber()).isEqualTo(100010L);
        assertThat(retrieved.get().getAccountCount()).isEqualTo(20);
        assertThat(retrieved.get().getLastAccountNumber()).isEqualTo(10000020);
    }

    @Test
    void testGetNextCustomerNumber_WhenRecordDoesNotExist_InitializesAndReturnsNext() {
        // When
        Long nextNumber = controlRepository.getNextCustomerNumber();

        // Then
        assertThat(nextNumber).isEqualTo(100001L);
        
        // Verify control record was initialized
        Optional<Control> control = controlRepository.getControl();
        assertThat(control).isPresent();
        assertThat(control.get().getLastCustomerNumber()).isEqualTo(100001L);
        assertThat(control.get().getCustomerCount()).isEqualTo(1L);
    }

    @Test
    void testGetNextCustomerNumber_WhenRecordExists_IncrementsAndReturns() {
        // Given - Insert initial record
        jdbcTemplate.update(
            "INSERT INTO control (id, customer_count, last_customer_number, account_count, last_account_number) " +
            "VALUES (?, ?, ?, ?, ?)",
            Control.CONTROL_ID, 5L, 100005L, 10, 10000010
        );

        // When
        Long nextNumber = controlRepository.getNextCustomerNumber();

        // Then
        assertThat(nextNumber).isEqualTo(100006L);
        
        // Verify updated values
        Optional<Control> control = controlRepository.getControl();
        assertThat(control).isPresent();
        assertThat(control.get().getLastCustomerNumber()).isEqualTo(100006L);
        assertThat(control.get().getCustomerCount()).isEqualTo(6L);
    }

    @Test
    void testGetNextAccountNumber_WhenRecordDoesNotExist_InitializesAndReturnsNext() {
        // When
        Integer nextNumber = controlRepository.getNextAccountNumber();

        // Then
        assertThat(nextNumber).isEqualTo(10000001);
        
        // Verify control record was initialized
        Optional<Control> control = controlRepository.getControl();
        assertThat(control).isPresent();
        assertThat(control.get().getLastAccountNumber()).isEqualTo(10000001);
        assertThat(control.get().getAccountCount()).isEqualTo(1);
    }

    @Test
    void testGetNextAccountNumber_WhenRecordExists_IncrementsAndReturns() {
        // Given - Insert initial record
        jdbcTemplate.update(
            "INSERT INTO control (id, customer_count, last_customer_number, account_count, last_account_number) " +
            "VALUES (?, ?, ?, ?, ?)",
            Control.CONTROL_ID, 5L, 100005L, 10, 10000010
        );

        // When
        Integer nextNumber = controlRepository.getNextAccountNumber();

        // Then
        assertThat(nextNumber).isEqualTo(10000011);
        
        // Verify updated values
        Optional<Control> control = controlRepository.getControl();
        assertThat(control).isPresent();
        assertThat(control.get().getLastAccountNumber()).isEqualTo(10000011);
        assertThat(control.get().getAccountCount()).isEqualTo(11);
    }

    @Test
    void testInitializeControlRecord_WhenRecordDoesNotExist_CreatesDefaultRecord() {
        // When
        Control initialized = controlRepository.initializeControlRecord();

        // Then
        assertThat(initialized).isNotNull();
        assertThat(initialized.getCustomerCount()).isEqualTo(0L);
        assertThat(initialized.getLastCustomerNumber()).isEqualTo(100000L);
        assertThat(initialized.getAccountCount()).isEqualTo(0);
        assertThat(initialized.getLastAccountNumber()).isEqualTo(10000000);
        
        // Verify in database
        Optional<Control> retrieved = controlRepository.getControl();
        assertThat(retrieved).isPresent();
        assertThat(retrieved.get()).usingRecursiveComparison().isEqualTo(initialized);
    }

    @Test
    void testInitializeControlRecord_WhenRecordExists_ReturnsExistingRecord() {
        // Given - Insert existing record
        jdbcTemplate.update(
            "INSERT INTO control (id, customer_count, last_customer_number, account_count, last_account_number) " +
            "VALUES (?, ?, ?, ?, ?)",
            Control.CONTROL_ID, 100L, 100100L, 200, 10000200
        );

        // When
        Control initialized = controlRepository.initializeControlRecord();

        // Then
        assertThat(initialized).isNotNull();
        assertThat(initialized.getCustomerCount()).isEqualTo(100L);
        assertThat(initialized.getLastCustomerNumber()).isEqualTo(100100L);
        assertThat(initialized.getAccountCount()).isEqualTo(200);
        assertThat(initialized.getLastAccountNumber()).isEqualTo(10000200);
    }

    @Test
    void testSequentialCustomerNumbers_GeneratesConsecutiveNumbers() {
        // When - Get multiple customer numbers
        Long first = controlRepository.getNextCustomerNumber();
        Long second = controlRepository.getNextCustomerNumber();
        Long third = controlRepository.getNextCustomerNumber();

        // Then
        assertThat(first).isEqualTo(100001L);
        assertThat(second).isEqualTo(100002L);
        assertThat(third).isEqualTo(100003L);
        
        // Verify final state
        Optional<Control> control = controlRepository.getControl();
        assertThat(control).isPresent();
        assertThat(control.get().getLastCustomerNumber()).isEqualTo(100003L);
        assertThat(control.get().getCustomerCount()).isEqualTo(3L);
    }

    @Test
    void testSequentialAccountNumbers_GeneratesConsecutiveNumbers() {
        // When - Get multiple account numbers
        Integer first = controlRepository.getNextAccountNumber();
        Integer second = controlRepository.getNextAccountNumber();
        Integer third = controlRepository.getNextAccountNumber();

        // Then
        assertThat(first).isEqualTo(10000001);
        assertThat(second).isEqualTo(10000002);
        assertThat(third).isEqualTo(10000003);
        
        // Verify final state
        Optional<Control> control = controlRepository.getControl();
        assertThat(control).isPresent();
        assertThat(control.get().getLastAccountNumber()).isEqualTo(10000003);
        assertThat(control.get().getAccountCount()).isEqualTo(3);
    }
}
