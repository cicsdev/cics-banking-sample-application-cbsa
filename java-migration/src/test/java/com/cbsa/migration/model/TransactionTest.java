package com.cbsa.migration.model;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.DisplayName;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalTime;
import static org.assertj.core.api.Assertions.assertThat;

class TransactionTest {

    @Test
    @DisplayName("Should create Transaction with builder")
    void shouldCreateTransactionWithBuilder() {
        // Given/When
        Transaction transaction = Transaction.builder()
                .eyeCatcher("PRTR")
                .logicallyDeleted(false)
                .sortCode("123456")
                .accountNumber("87654321")
                .transactionDate(LocalDate.of(2023, 12, 15))
                .transactionTime(LocalTime.of(14, 30, 45))
                .referenceNumber(123456789012L)
                .transactionType("CRE")
                .description("Credit payment")
                .amount(new BigDecimal("250.75"))
                .build();

        // Then
        assertThat(transaction.getEyeCatcher()).isEqualTo("PRTR");
        assertThat(transaction.isLogicallyDeleted()).isFalse();
        assertThat(transaction.getSortCode()).isEqualTo("123456");
        assertThat(transaction.getAccountNumber()).isEqualTo("87654321");
        assertThat(transaction.getTransactionDate()).isEqualTo(LocalDate.of(2023, 12, 15));
        assertThat(transaction.getTransactionTime()).isEqualTo(LocalTime.of(14, 30, 45));
        assertThat(transaction.getReferenceNumber()).isEqualTo(123456789012L);
        assertThat(transaction.getTransactionType()).isEqualTo("CRE");
        assertThat(transaction.getDescription()).isEqualTo("Credit payment");
        assertThat(transaction.getAmount()).isEqualTo(new BigDecimal("250.75"));
    }

    @Test
    @DisplayName("Should create composite ID from key fields")
    void shouldCreateCompositeId() {
        // Given
        Transaction transaction = Transaction.builder()
                .sortCode("123456")
                .accountNumber("87654321")
                .transactionDate(LocalDate.of(2023, 12, 15))
                .transactionTime(LocalTime.of(14, 30, 45))
                .referenceNumber(123456789012L)
                .build();

        // When
        String compositeId = transaction.getCompositeId();

        // Then
        assertThat(compositeId).isEqualTo("12345687654321-2023-12-15-14:30:45-123456789012");
    }

    @Test
    @DisplayName("Should identify transfer transactions correctly")
    void shouldIdentifyTransferTransactions() {
        // Given
        Transaction transferTransaction = Transaction.builder()
                .transactionType(Transaction.TYPE_TRANSFER)
                .build();

        Transaction nonTransferTransaction = Transaction.builder()
                .transactionType(Transaction.TYPE_CREDIT)
                .build();

        // When/Then
        assertThat(transferTransaction.isTransfer()).isTrue();
        assertThat(nonTransferTransaction.isTransfer()).isFalse();
    }

    @Test
    @DisplayName("Should identify credit transactions correctly")
    void shouldIdentifyCreditTransactions() {
        // Given
        Transaction creditTransaction = Transaction.builder()
                .transactionType(Transaction.TYPE_CREDIT)
                .build();

        Transaction paymentCreditTransaction = Transaction.builder()
                .transactionType(Transaction.TYPE_PAYMENT_CREDIT)
                .build();

        Transaction chequePaidInTransaction = Transaction.builder()
                .transactionType(Transaction.TYPE_CHEQUE_PAID_IN)
                .build();

        Transaction debitTransaction = Transaction.builder()
                .transactionType(Transaction.TYPE_DEBIT)
                .build();

        // When/Then
        assertThat(creditTransaction.isCredit()).isTrue();
        assertThat(paymentCreditTransaction.isCredit()).isTrue();
        assertThat(chequePaidInTransaction.isCredit()).isTrue();
        assertThat(debitTransaction.isCredit()).isFalse();
    }

    @Test
    @DisplayName("Should identify debit transactions correctly")
    void shouldIdentifyDebitTransactions() {
        // Given
        Transaction debitTransaction = Transaction.builder()
                .transactionType(Transaction.TYPE_DEBIT)
                .build();

        Transaction paymentDebitTransaction = Transaction.builder()
                .transactionType(Transaction.TYPE_PAYMENT_DEBIT)
                .build();

        Transaction chequePaidOutTransaction = Transaction.builder()
                .transactionType(Transaction.TYPE_CHEQUE_PAID_OUT)
                .build();

        Transaction creditTransaction = Transaction.builder()
                .transactionType(Transaction.TYPE_CREDIT)
                .build();

        // When/Then
        assertThat(debitTransaction.isDebit()).isTrue();
        assertThat(paymentDebitTransaction.isDebit()).isTrue();
        assertThat(chequePaidOutTransaction.isDebit()).isTrue();
        assertThat(creditTransaction.isDebit()).isFalse();
    }

    @Test
    @DisplayName("Should support equals and hashCode (Lombok @Data)")
    void shouldSupportEqualsAndHashCode() {
        // Given
        Transaction transaction1 = Transaction.builder()
                .eyeCatcher("PRTR")
                .sortCode("123456")
                .accountNumber("87654321")
                .transactionDate(LocalDate.of(2023, 12, 15))
                .transactionTime(LocalTime.of(14, 30, 45))
                .referenceNumber(123456789012L)
                .transactionType("CRE")
                .amount(new BigDecimal("250.75"))
                .build();

        Transaction transaction2 = Transaction.builder()
                .eyeCatcher("PRTR")
                .sortCode("123456")
                .accountNumber("87654321")
                .transactionDate(LocalDate.of(2023, 12, 15))
                .transactionTime(LocalTime.of(14, 30, 45))
                .referenceNumber(123456789012L)
                .transactionType("CRE")
                .amount(new BigDecimal("250.75"))
                .build();

        // Then
        assertThat(transaction1).isEqualTo(transaction2);
        assertThat(transaction1.hashCode()).isEqualTo(transaction2.hashCode());
    }

    @Test
    @DisplayName("Should support toString (Lombok @Data)")
    void shouldSupportToString() {
        // Given
        Transaction transaction = Transaction.builder()
                .eyeCatcher("PRTR")
                .sortCode("123456")
                .accountNumber("87654321")
                .transactionType("CRE")
                .build();

        // When
        String toString = transaction.toString();

        // Then
        assertThat(toString).contains("Transaction");
        assertThat(toString).contains("eyeCatcher=PRTR");
        assertThat(toString).contains("sortCode=123456");
        assertThat(toString).contains("accountNumber=87654321");
        assertThat(toString).contains("transactionType=CRE");
    }

    @Test
    @DisplayName("Should create Transaction with no-args constructor")
    void shouldCreateTransactionWithNoArgsConstructor() {
        // Given/When
        Transaction transaction = new Transaction();
        
        // Then
        assertThat(transaction).isNotNull();
        assertThat(transaction.getEyeCatcher()).isNull();
        assertThat(transaction.getTransactionType()).isNull();
    }

    @Test
    @DisplayName("Should set and get all fields correctly")
    void shouldSetAndGetAllFieldsCorrectly() {
        // Given
        Transaction transaction = new Transaction();
        LocalDate transactionDate = LocalDate.of(2023, 12, 15);
        LocalTime transactionTime = LocalTime.of(14, 30, 45);
        BigDecimal amount = new BigDecimal("250.75");

        // When
        transaction.setEyeCatcher("PRTR");
        transaction.setLogicallyDeleted(true);
        transaction.setSortCode("123456");
        transaction.setAccountNumber("87654321");
        transaction.setTransactionDate(transactionDate);
        transaction.setTransactionTime(transactionTime);
        transaction.setReferenceNumber(123456789012L);
        transaction.setTransactionType("CRE");
        transaction.setDescription("Credit payment");
        transaction.setAmount(amount);
        transaction.setTargetSortCode("654321");
        transaction.setTargetAccountNumber("12345678");

        // Then
        assertThat(transaction.getEyeCatcher()).isEqualTo("PRTR");
        assertThat(transaction.isLogicallyDeleted()).isTrue();
        assertThat(transaction.getSortCode()).isEqualTo("123456");
        assertThat(transaction.getAccountNumber()).isEqualTo("87654321");
        assertThat(transaction.getTransactionDate()).isEqualTo(transactionDate);
        assertThat(transaction.getTransactionTime()).isEqualTo(transactionTime);
        assertThat(transaction.getReferenceNumber()).isEqualTo(123456789012L);
        assertThat(transaction.getTransactionType()).isEqualTo("CRE");
        assertThat(transaction.getDescription()).isEqualTo("Credit payment");
        assertThat(transaction.getAmount()).isEqualTo(amount);
        assertThat(transaction.getTargetSortCode()).isEqualTo("654321");
        assertThat(transaction.getTargetAccountNumber()).isEqualTo("12345678");
    }

    @Test
    @DisplayName("Should handle transfer transaction with target fields")
    void shouldHandleTransferTransactionWithTargetFields() {
        // Given
        Transaction transferTransaction = Transaction.builder()
                .transactionType(Transaction.TYPE_TRANSFER)
                .sortCode("123456")
                .accountNumber("87654321")
                .targetSortCode("654321")
                .targetAccountNumber("12345678")
                .amount(new BigDecimal("500.00"))
                .build();

        // Then
        assertThat(transferTransaction.isTransfer()).isTrue();
        assertThat(transferTransaction.getTargetSortCode()).isEqualTo("654321");
        assertThat(transferTransaction.getTargetAccountNumber()).isEqualTo("12345678");
        assertThat(transferTransaction.getAmount()).isEqualTo(new BigDecimal("500.00"));
    }

    @Test
    @DisplayName("Should test transaction type constants")
    void shouldTestTransactionTypeConstants() {
        // Then
        assertThat(Transaction.VALID_EYECATCHER).isEqualTo("PRTR");
        assertThat(Transaction.TYPE_CREDIT).isEqualTo("CRE");
        assertThat(Transaction.TYPE_DEBIT).isEqualTo("DEB");
        assertThat(Transaction.TYPE_TRANSFER).isEqualTo("TFR");
        assertThat(Transaction.TYPE_PAYMENT_CREDIT).isEqualTo("PCR");
        assertThat(Transaction.TYPE_PAYMENT_DEBIT).isEqualTo("PDR");
        assertThat(Transaction.TYPE_CHEQUE_PAID_IN).isEqualTo("CHI");
        assertThat(Transaction.TYPE_CHEQUE_PAID_OUT).isEqualTo("CHO");
    }
}
