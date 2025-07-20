package com.cbsa.migration.model;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.DisplayName;
import java.math.BigDecimal;
import java.time.LocalDate;
import static org.assertj.core.api.Assertions.assertThat;

class AccountTest {

    @Test
    @DisplayName("Should create Account with builder")
    void shouldCreateAccountWithBuilder() {
        // Given/When
        Account account = Account.builder()
                .eyeCatcher("ACCT")
                .customerNumber(1234567890L)
                .sortCode("123456")
                .accountNumber("87654321")
                .accountType("CURRENT")
                .interestRate(new BigDecimal("2.50"))
                .openedDate(LocalDate.of(2023, 1, 15))
                .overdraftLimit(1000)
                .lastStatementDate(LocalDate.of(2023, 12, 1))
                .nextStatementDate(LocalDate.of(2024, 1, 1))
                .availableBalance(new BigDecimal("2500.75"))
                .actualBalance(new BigDecimal("2750.00"))
                .build();

        // Then
        assertThat(account.getEyeCatcher()).isEqualTo("ACCT");
        assertThat(account.getCustomerNumber()).isEqualTo(1234567890L);
        assertThat(account.getSortCode()).isEqualTo("123456");
        assertThat(account.getAccountNumber()).isEqualTo("87654321");
        assertThat(account.getAccountType()).isEqualTo("CURRENT");
        assertThat(account.getInterestRate()).isEqualTo(new BigDecimal("2.50"));
        assertThat(account.getOpenedDate()).isEqualTo(LocalDate.of(2023, 1, 15));
        assertThat(account.getOverdraftLimit()).isEqualTo(1000);
        assertThat(account.getLastStatementDate()).isEqualTo(LocalDate.of(2023, 12, 1));
        assertThat(account.getNextStatementDate()).isEqualTo(LocalDate.of(2024, 1, 1));
        assertThat(account.getAvailableBalance()).isEqualTo(new BigDecimal("2500.75"));
        assertThat(account.getActualBalance()).isEqualTo(new BigDecimal("2750.00"));
    }

    @Test
    @DisplayName("Should create composite ID from sort code and account number")
    void shouldCreateCompositeId() {
        // Given
        Account account = Account.builder()
                .sortCode("123456")
                .accountNumber("87654321")
                .build();

        // When
        String compositeId = account.getCompositeId();

        // Then
        assertThat(compositeId).isEqualTo("12345687654321");
    }

    @Test
    @DisplayName("Should support equals and hashCode (Lombok @Data)")
    void shouldSupportEqualsAndHashCode() {
        // Given
        Account account1 = Account.builder()
                .eyeCatcher("ACCT")
                .customerNumber(1234567890L)
                .sortCode("123456")
                .accountNumber("87654321")
                .accountType("CURRENT")
                .interestRate(new BigDecimal("2.50"))
                .openedDate(LocalDate.of(2023, 1, 15))
                .overdraftLimit(1000)
                .availableBalance(new BigDecimal("2500.75"))
                .actualBalance(new BigDecimal("2750.00"))
                .build();

        Account account2 = Account.builder()
                .eyeCatcher("ACCT")
                .customerNumber(1234567890L)
                .sortCode("123456")
                .accountNumber("87654321")
                .accountType("CURRENT")
                .interestRate(new BigDecimal("2.50"))
                .openedDate(LocalDate.of(2023, 1, 15))
                .overdraftLimit(1000)
                .availableBalance(new BigDecimal("2500.75"))
                .actualBalance(new BigDecimal("2750.00"))
                .build();

        // Then
        assertThat(account1).isEqualTo(account2);
        assertThat(account1.hashCode()).isEqualTo(account2.hashCode());
    }

    @Test
    @DisplayName("Should support toString (Lombok @Data)")
    void shouldSupportToString() {
        // Given
        Account account = Account.builder()
                .eyeCatcher("ACCT")
                .customerNumber(1234567890L)
                .sortCode("123456")
                .accountNumber("87654321")
                .build();

        // When
        String toString = account.toString();

        // Then
        assertThat(toString).contains("Account");
        assertThat(toString).contains("eyeCatcher=ACCT");
        assertThat(toString).contains("customerNumber=1234567890");
        assertThat(toString).contains("sortCode=123456");
        assertThat(toString).contains("accountNumber=87654321");
    }

    @Test
    @DisplayName("Should create Account with no-args constructor")
    void shouldCreateAccountWithNoArgsConstructor() {
        // Given/When
        Account account = new Account();
        
        // Then
        assertThat(account).isNotNull();
        assertThat(account.getEyeCatcher()).isNull();
        assertThat(account.getCustomerNumber()).isNull();
    }

    @Test
    @DisplayName("Should set and get all fields correctly")
    void shouldSetAndGetAllFieldsCorrectly() {
        // Given
        Account account = new Account();
        LocalDate openedDate = LocalDate.of(2023, 1, 15);
        LocalDate lastStmtDate = LocalDate.of(2023, 12, 1);
        LocalDate nextStmtDate = LocalDate.of(2024, 1, 1);
        BigDecimal interestRate = new BigDecimal("2.50");
        BigDecimal availableBalance = new BigDecimal("2500.75");
        BigDecimal actualBalance = new BigDecimal("2750.00");

        // When
        account.setEyeCatcher("ACCT");
        account.setCustomerNumber(1234567890L);
        account.setSortCode("123456");
        account.setAccountNumber("87654321");
        account.setAccountType("CURRENT");
        account.setInterestRate(interestRate);
        account.setOpenedDate(openedDate);
        account.setOverdraftLimit(1000);
        account.setLastStatementDate(lastStmtDate);
        account.setNextStatementDate(nextStmtDate);
        account.setAvailableBalance(availableBalance);
        account.setActualBalance(actualBalance);

        // Then
        assertThat(account.getEyeCatcher()).isEqualTo("ACCT");
        assertThat(account.getCustomerNumber()).isEqualTo(1234567890L);
        assertThat(account.getSortCode()).isEqualTo("123456");
        assertThat(account.getAccountNumber()).isEqualTo("87654321");
        assertThat(account.getAccountType()).isEqualTo("CURRENT");
        assertThat(account.getInterestRate()).isEqualTo(interestRate);
        assertThat(account.getOpenedDate()).isEqualTo(openedDate);
        assertThat(account.getOverdraftLimit()).isEqualTo(1000);
        assertThat(account.getLastStatementDate()).isEqualTo(lastStmtDate);
        assertThat(account.getNextStatementDate()).isEqualTo(nextStmtDate);
        assertThat(account.getAvailableBalance()).isEqualTo(availableBalance);
        assertThat(account.getActualBalance()).isEqualTo(actualBalance);
    }
}
