package com.cbsa.migration.repository;

import com.cbsa.migration.model.Account;
import com.cbsa.migration.model.Customer;
import com.cbsa.migration.repository.jdbc.JdbcAccountRepository;
import com.cbsa.migration.repository.jdbc.JdbcCustomerRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.context.annotation.Import;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Integration test for AccountRepository
 */
@SpringBootTest
@ActiveProfiles("test")
@Import(RepositoryTestConfig.class)
@Transactional
public class AccountRepositoryIntegrationTest {

    @Autowired
    private JdbcTemplate jdbcTemplate;

    @Autowired
    private JdbcAccountRepository accountRepository;

    @Autowired
    private JdbcCustomerRepository customerRepository;

    private Customer testCustomer;
    private Account testAccount;

    @BeforeEach
    void setUp() {
        // Clear tables before each test
        jdbcTemplate.update("DELETE FROM bank_transaction");
        jdbcTemplate.update("DELETE FROM account");
        jdbcTemplate.update("DELETE FROM customer");

        // Create a test customer
        testCustomer = Customer.builder()
                .eyeCatcher("CUST")
                .sortCode("123456")
                .customerNumber(1000L)
                .name("John Doe")
                .address("123 Main St, London, UK")
                .dateOfBirth(LocalDate.of(1980, 1, 1))
                .creditScore(750)
                .creditScoreReviewDate(LocalDate.now())
                .build();

        // Save the test customer
        customerRepository.save(testCustomer);

        // Create a test account
        testAccount = Account.builder()
                .eyeCatcher("ACCT")
                .customerNumber(1000L)
                .sortCode("123456")
                .accountNumber("12345678")
                .accountType("CURRENT")
                .interestRate(new BigDecimal("0.5"))
                .openedDate(LocalDate.now().minusYears(1))
                .overdraftLimit(1000)
                .lastStatementDate(LocalDate.now().minusMonths(1))
                .nextStatementDate(LocalDate.now().plusMonths(1))
                .availableBalance(new BigDecimal("1500.00"))
                .actualBalance(new BigDecimal("1500.00"))
                .build();

        // Save the test account
        accountRepository.save(testAccount);
    }

    @Test
    void testFindById() {
        // Test finding an account by ID
        Optional<Account> found = accountRepository.findById("123456", "12345678");
        
        assertTrue(found.isPresent());
        assertEquals("CURRENT", found.get().getAccountType());
        assertEquals(0, new BigDecimal("1500.00").compareTo(found.get().getActualBalance()), "Actual balance should be 1500.00");
    }

    @Test
    void testFindByIdNotFound() {
        // Test finding a non-existent account
        Optional<Account> notFound = accountRepository.findById("123456", "99999999");
        
        assertFalse(notFound.isPresent());
    }

    @Test
    void testFindByCustomerNumber() {
        // Add another account for the same customer
        Account savingsAccount = Account.builder()
                .eyeCatcher("ACCT")
                .customerNumber(1000L)
                .sortCode("123456")
                .accountNumber("87654321")
                .accountType("SAVINGS")
                .interestRate(new BigDecimal("1.25"))
                .openedDate(LocalDate.now().minusMonths(6))
                .overdraftLimit(0)
                .availableBalance(new BigDecimal("5000.00"))
                .actualBalance(new BigDecimal("5000.00"))
                .build();
        accountRepository.save(savingsAccount);

        // Test finding accounts by customer number
        List<Account> accounts = accountRepository.findByCustomerNumber(1000L);
        
        assertEquals(2, accounts.size());
        assertTrue(accounts.stream().anyMatch(a -> a.getAccountType().equals("CURRENT")));
        assertTrue(accounts.stream().anyMatch(a -> a.getAccountType().equals("SAVINGS")));
    }

    @Test
    void testSave() {
        // Test updating an existing account
        testAccount.setActualBalance(new BigDecimal("2000.00"));
        testAccount.setAvailableBalance(new BigDecimal("2000.00"));
        Account updated = accountRepository.save(testAccount);
        
        assertEquals(0, new BigDecimal("2000.00").compareTo(updated.getActualBalance()), "Actual balance should be 2000.00");
        
        // Verify the update was persisted
        Optional<Account> found = accountRepository.findById("123456", "12345678");
        assertTrue(found.isPresent());
        assertEquals(0, new BigDecimal("2000.00").compareTo(found.get().getActualBalance()), "Actual balance should be 2000.00");
    }

    @Test
    void testDeleteById() {
        // Test deleting an account
        boolean deleted = accountRepository.deleteById("123456", "12345678");
        
        assertTrue(deleted);
        
        // Verify the account was deleted
        Optional<Account> found = accountRepository.findById("123456", "12345678");
        assertFalse(found.isPresent());
    }

    @Test
    void testCount() {
        // Test counting accounts
        int count = accountRepository.count();
        
        assertEquals(1, count);
        
        // Add another account
        Account savingsAccount = Account.builder()
                .eyeCatcher("ACCT")
                .customerNumber(1000L)
                .sortCode("123456")
                .accountNumber("87654321")
                .accountType("SAVINGS")
                .interestRate(new BigDecimal("1.25"))
                .openedDate(LocalDate.now().minusMonths(6))
                .overdraftLimit(0)
                .availableBalance(new BigDecimal("5000.00"))
                .actualBalance(new BigDecimal("5000.00"))
                .build();
        accountRepository.save(savingsAccount);
        
        // Verify the count was updated
        count = accountRepository.count();
        assertEquals(2, count);
    }

    @Test
    void testFindAll() {
        // Add another account
        Account savingsAccount = Account.builder()
                .eyeCatcher("ACCT")
                .customerNumber(1000L)
                .sortCode("123456")
                .accountNumber("87654321")
                .accountType("SAVINGS")
                .interestRate(new BigDecimal("1.25"))
                .openedDate(LocalDate.now().minusMonths(6))
                .overdraftLimit(0)
                .availableBalance(new BigDecimal("5000.00"))
                .actualBalance(new BigDecimal("5000.00"))
                .build();
        accountRepository.save(savingsAccount);

        // Test finding all accounts
        List<Account> accounts = accountRepository.findAll();
        
        assertEquals(2, accounts.size());
        assertTrue(accounts.stream().anyMatch(a -> a.getAccountNumber().equals("12345678")));
        assertTrue(accounts.stream().anyMatch(a -> a.getAccountNumber().equals("87654321")));
    }
}
