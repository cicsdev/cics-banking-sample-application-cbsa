package com.cbsa.migration.repository.jdbc;

import com.cbsa.migration.model.Transaction;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.jdbc.JdbcTest;
import org.springframework.context.annotation.Import;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.test.context.TestPropertySource;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.test.annotation.Rollback;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalTime;
import java.util.List;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * Integration tests for JdbcTransactionRepository
 * Tests database interactions using H2 in-memory database
 */
@JdbcTest
@Import(JdbcTransactionRepository.class)
@TestPropertySource(locations = "classpath:application-test.properties")
@Transactional
@Rollback
class JdbcTransactionRepositoryTest {

    @Autowired
    private JdbcTemplate jdbcTemplate;

    @Autowired
    private JdbcTransactionRepository transactionRepository;

    private Transaction sampleTransaction;
    private String compositeId;

    @BeforeEach
    void setUp() {
        // Clean up any existing records (in reverse dependency order)
        jdbcTemplate.update("DELETE FROM bank_transaction");
        jdbcTemplate.update("DELETE FROM account");
        jdbcTemplate.update("DELETE FROM customer");
        
        // Create required customer record (for foreign key constraint)
        jdbcTemplate.update(
            "INSERT INTO customer (eye_catcher, sort_code, customer_number, name, address, date_of_birth, credit_score) " +
            "VALUES (?, ?, ?, ?, ?, ?, ?)",
            "CUST", "987654", 100001, "Test Customer", "123 Test St", "1990-01-01", 750
        );
        
        // Create required account record (for foreign key constraint)
        jdbcTemplate.update(
            "INSERT INTO account (eye_catcher, customer_number, sort_code, account_number, account_type, " +
            "interest_rate, opened_date, overdraft_limit, available_balance, actual_balance) " +
            "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
            "ACCT", 100001, "987654", "12345678", "CURRENT", 0.01, "2024-01-01", 1000, 500.00, 500.00
        );
        
        // Create another account for multi-account tests
        jdbcTemplate.update(
            "INSERT INTO account (eye_catcher, customer_number, sort_code, account_number, account_type, " +
            "interest_rate, opened_date, overdraft_limit, available_balance, actual_balance) " +
            "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
            "ACCT", 100001, "987654", "87654321", "SAVINGS", 0.02, "2024-01-01", 0, 1000.00, 1000.00
        );
        
        // Create sample transaction for testing
        sampleTransaction = Transaction.builder()
            .eyeCatcher("PRTR")
            .logicallyDeleted(false)
            .sortCode("987654")
            .accountNumber("12345678")
            .transactionDate(LocalDate.of(2024, 1, 15))
            .transactionTime(LocalTime.of(12, 30, 45))
            .referenceNumber(123456L)
            .transactionType("CRE")
            .description("Test Credit Transaction")
            .amount(new BigDecimal("100.50"))
            .build();
            
        // Create second transaction for different account (use existing account 87654321)
        Transaction secondTransaction = Transaction.builder()
            .eyeCatcher("PRTR")
            .logicallyDeleted(false)
            .sortCode("987654")
            .accountNumber("87654321")
            .transactionDate(LocalDate.of(2024, 1, 16))
            .transactionTime(LocalTime.of(14, 15, 30))
            .referenceNumber(789012L)
            .transactionType("DEB") 
            .description("Test Debit Transaction")
            .amount(new BigDecimal("50.75"))
            .build();
            
        compositeId = sampleTransaction.getCompositeId();
    }

    // ======================
    // Save Operations Tests
    // ======================

    @Test
    void testSave_NewTransaction_InsertsRecord() {
        // When - Save new transaction
        Transaction saved = transactionRepository.save(sampleTransaction);

        // Then - Verify saved transaction
        assertThat(saved).isEqualTo(sampleTransaction);
        
        // Verify in database
        Optional<Transaction> retrieved = transactionRepository.findById(compositeId);
        assertThat(retrieved).isPresent();
        assertThat(retrieved.get().getEyeCatcher()).isEqualTo("PRTR");
        assertThat(retrieved.get().getSortCode()).isEqualTo("987654");
        assertThat(retrieved.get().getAccountNumber()).isEqualTo("12345678");
        assertThat(retrieved.get().getAmount().compareTo(new BigDecimal("100.50"))).isEqualTo(0);
    }

    @Test
    void testSave_ExistingTransaction_UpdatesRecord() {
        // Given - Insert initial transaction
        transactionRepository.save(sampleTransaction);
        
        // When - Update transaction
        Transaction updatedTransaction = Transaction.builder()
            .eyeCatcher("PRTR")
            .logicallyDeleted(false)
            .sortCode("987654")
            .accountNumber("12345678")
            .transactionDate(LocalDate.of(2024, 1, 15))
            .transactionTime(LocalTime.of(12, 30, 45))
            .referenceNumber(123456L)
            .transactionType("DEB")
            .description("Updated Debit Transaction")
            .amount(new BigDecimal("200.75"))
            .build();
            
        Transaction saved = transactionRepository.save(updatedTransaction);

        // Then - Verify update occurred
        assertThat(saved).isEqualTo(updatedTransaction);
        
        // Verify in database
        Optional<Transaction> retrieved = transactionRepository.findById(compositeId);
        assertThat(retrieved).isPresent();
        assertThat(retrieved.get().getTransactionType()).isEqualTo("DEB");
        assertThat(retrieved.get().getDescription()).isEqualTo("Updated Debit Transaction");
        assertThat(retrieved.get().getAmount().compareTo(new BigDecimal("200.75"))).isEqualTo(0);
    }

    @Test
    void testSave_WithValidData_ReturnsTransaction() {
        // When - Save transaction with all valid data
        Transaction saved = transactionRepository.save(sampleTransaction);

        // Then - Verify data integrity
        assertThat(saved).isNotNull();
        assertThat(saved.getEyeCatcher()).isEqualTo("PRTR");
        assertThat(saved.isLogicallyDeleted()).isFalse();
        assertThat(saved.getTransactionDate()).isEqualTo(LocalDate.of(2024, 1, 15));
        assertThat(saved.getTransactionTime()).isEqualTo(LocalTime.of(12, 30, 45));
        assertThat(saved.getReferenceNumber()).isEqualTo(123456L);
    }

    // ======================
    // Find Operations Tests
    // ======================

    @Test
    void testFindById_WithValidCompositeId_ReturnsTransaction() {
        // Given - Insert transaction
        transactionRepository.save(sampleTransaction);

        // When - Find by composite ID
        Optional<Transaction> result = transactionRepository.findById(compositeId);

        // Then - Verify found transaction
        assertThat(result).isPresent();
        Transaction found = result.get();
        assertThat(found.getSortCode()).isEqualTo("987654");
        assertThat(found.getAccountNumber()).isEqualTo("12345678");
        assertThat(found.getTransactionDate()).isEqualTo(LocalDate.of(2024, 1, 15));
        assertThat(found.getReferenceNumber()).isEqualTo(123456L);
    }

    @Test
    void testFindById_WithInvalidCompositeId_ReturnsEmpty() {
        // When - Find with invalid composite ID
        Optional<Transaction> result = transactionRepository.findById("invalid-composite-id");

        // Then - Should return empty
        assertThat(result).isEmpty();
    }

    @Test
    void testFindByAccount_WithTransactions_ReturnsOrderedList() {
        // Given - Insert multiple transactions for same account
        Transaction transaction1 = sampleTransaction;
        
        Transaction transaction2 = Transaction.builder()
            .eyeCatcher("PRTR")
            .logicallyDeleted(false)
            .sortCode("987654")
            .accountNumber("12345678")
            .transactionDate(LocalDate.of(2024, 1, 16))  // Later date
            .transactionTime(LocalTime.of(14, 15, 30))
            .referenceNumber(123457L)
            .transactionType("DEB")
            .description("Test Debit Transaction")
            .amount(new BigDecimal("50.25"))
            .build();

        transactionRepository.save(transaction1);
        transactionRepository.save(transaction2);

        // When - Find by account
        List<Transaction> transactions = transactionRepository.findByAccount("987654", "12345678");

        // Then - Verify ordered results (most recent first)
        assertThat(transactions).hasSize(2);
        assertThat(transactions.get(0).getTransactionDate()).isEqualTo(LocalDate.of(2024, 1, 16));
        assertThat(transactions.get(1).getTransactionDate()).isEqualTo(LocalDate.of(2024, 1, 15));
    }

    // ======================
    // Delete Operations Tests
    // ======================

    @Test
    void testDeleteById_WithValidId_ReturnsTrue() {
        // Given - Insert transaction
        transactionRepository.save(sampleTransaction);
        
        // Verify transaction exists
        assertThat(transactionRepository.findById(compositeId)).isPresent();

        // When - Delete by ID
        boolean deleted = transactionRepository.deleteById(compositeId);

        // Then - Verify deletion
        assertThat(deleted).isTrue();
        assertThat(transactionRepository.findById(compositeId)).isEmpty();
    }

    @Test
    void testMarkAsDeleted_WithValidId_SetsLogicalFlag() {
        // Given - Insert transaction
        transactionRepository.save(sampleTransaction);

        // When - Mark as deleted
        boolean marked = transactionRepository.markAsDeleted(compositeId);

        // Then - Verify logical deletion
        assertThat(marked).isTrue();
        
        Optional<Transaction> retrieved = transactionRepository.findById(compositeId);
        assertThat(retrieved).isPresent();
        assertThat(retrieved.get().isLogicallyDeleted()).isTrue();
    }

    @Test
    void testMarkAsDeleted_PreservesTransactionData() {
        // Given - Insert transaction
        transactionRepository.save(sampleTransaction);

        // When - Mark as deleted
        transactionRepository.markAsDeleted(compositeId);

        // Then - Verify data preservation
        Optional<Transaction> retrieved = transactionRepository.findById(compositeId);
        assertThat(retrieved).isPresent();
        Transaction marked = retrieved.get();
        
        // All original data should be preserved
        assertThat(marked.getEyeCatcher()).isEqualTo("PRTR");
        assertThat(marked.getSortCode()).isEqualTo("987654");
        assertThat(marked.getAccountNumber()).isEqualTo("12345678");
        assertThat(marked.getAmount().compareTo(new BigDecimal("100.50"))).isEqualTo(0);
        assertThat(marked.getDescription()).isEqualTo("Test Credit Transaction");
        
        // Only logical delete flag should change
        assertThat(marked.isLogicallyDeleted()).isTrue();
    }

// ... (rest of the code remains the same)
    // ======================
    // Repository Utility Tests
    // ======================

    @Test
    void testFindAll_ReturnsAllTransactions() {
        // Given - Insert multiple transactions
        transactionRepository.save(sampleTransaction);
        
        Transaction transaction2 = Transaction.builder()
            .eyeCatcher("PRTR")
            .logicallyDeleted(false)
            .sortCode("987654")
            .accountNumber("87654321")
            .transactionDate(LocalDate.of(2024, 1, 16))
            .transactionTime(LocalTime.of(10, 15, 20))
            .referenceNumber(654321L)
            .transactionType("TFR")
            .description("Test Transfer")
            .amount(new BigDecimal("75.00"))
            .build();
            
        transactionRepository.save(transaction2);

        // When - Find all transactions
        List<Transaction> allTransactions = transactionRepository.findAll();

        // Then - Verify all returned
        assertThat(allTransactions).hasSize(2);
        assertThat(allTransactions.get(0).getTransactionDate()).isEqualTo(LocalDate.of(2024, 1, 16)); // Most recent first
        assertThat(allTransactions.get(1).getTransactionDate()).isEqualTo(LocalDate.of(2024, 1, 15));
    }

    @Test
    void testCount_ReturnsCorrectCount() {
        // Given - No transactions initially
        assertThat(transactionRepository.count()).isEqualTo(0);

        // When - Add transactions
        transactionRepository.save(sampleTransaction);
        
        Transaction transaction2 = Transaction.builder()
            .eyeCatcher("PRTR")
            .logicallyDeleted(false)
            .sortCode("987654")
            .accountNumber("87654321")
            .transactionDate(LocalDate.of(2024, 1, 16))
            .transactionTime(LocalTime.of(9, 0, 0))
            .referenceNumber(999999L)
            .transactionType("CHI")
            .description("Cheque Paid In")
            .amount(new BigDecimal("250.00"))
            .build();
            
        transactionRepository.save(transaction2);

        // Then - Verify count
        assertThat(transactionRepository.count()).isEqualTo(2);
    }

    // ======================
    // Integration Test
    // ======================

    @Test
    void testCompleteTransactionLifecycle_CreateUpdateDelete() {
        // STEP 1: Create transaction
        Transaction created = transactionRepository.save(sampleTransaction);
        assertThat(created).isNotNull();
        assertThat(transactionRepository.count()).isEqualTo(1);

        // STEP 2: Update transaction
        Transaction updated = Transaction.builder()
            .eyeCatcher("PRTR")
            .logicallyDeleted(false)
            .sortCode("987654")
            .accountNumber("12345678")
            .transactionDate(LocalDate.of(2024, 1, 15))
            .transactionTime(LocalTime.of(12, 30, 45))
            .referenceNumber(123456L)
            .transactionType("TFR")
            .description("Updated Transfer Transaction")
            .targetSortCode("123456")
            .targetAccountNumber("87654321")
            .amount(new BigDecimal("500.00"))
            .build();

        Transaction savedUpdate = transactionRepository.save(updated);
        assertThat(savedUpdate.getTransactionType()).isEqualTo("TFR");
        assertThat(savedUpdate.getAmount().compareTo(new BigDecimal("500.00"))).isEqualTo(0);
        assertThat(transactionRepository.count()).isEqualTo(1); // Still one record

        // STEP 3: Mark as logically deleted (audit trail)
        boolean logicalDelete = transactionRepository.markAsDeleted(compositeId);
        assertThat(logicalDelete).isTrue();
        
        Optional<Transaction> logicallyDeleted = transactionRepository.findById(compositeId);
        assertThat(logicallyDeleted).isPresent();
        assertThat(logicallyDeleted.get().isLogicallyDeleted()).isTrue();
        assertThat(transactionRepository.count()).isEqualTo(1); // Still exists for audit

        // STEP 4: Physical delete (if needed)
        boolean physicalDelete = transactionRepository.deleteById(compositeId);
        assertThat(physicalDelete).isTrue();
        assertThat(transactionRepository.findById(compositeId)).isEmpty();
        assertThat(transactionRepository.count()).isEqualTo(0);
    }
}
