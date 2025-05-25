package com.cbsa.migration.util;

import com.cbsa.migration.model.Account;
import com.cbsa.migration.model.Control;
import com.cbsa.migration.model.Customer;
import com.cbsa.migration.model.Transaction;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalTime;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.concurrent.ThreadLocalRandom;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

/**
 * Utility for generating test data for the banking application
 * Can create realistic customer, account, and transaction data
 */
@Component
public class TestDataGenerator {

    private static final Logger logger = LoggerFactory.getLogger(TestDataGenerator.class);
    
    private final JdbcTemplate jdbcTemplate;
    
    // Constants for data generation
    private static final String[] SORT_CODES = {"123456", "234567", "345678", "456789", "567890"};
    private static final String[] ACCOUNT_TYPES = {"CURRENT", "SAVINGS", "FIXED", "LOAN"};
    private static final String[] TRANSACTION_TYPES = {
            Transaction.TYPE_CREDIT, Transaction.TYPE_DEBIT, 
            Transaction.TYPE_PAYMENT_CREDIT, Transaction.TYPE_PAYMENT_DEBIT,
            Transaction.TYPE_TRANSFER, Transaction.TYPE_CHEQUE_PAID_IN, 
            Transaction.TYPE_CHEQUE_PAID_OUT
    };
    
    // Name components for realistic data generation
    private static final String[] TITLES = {"Mr", "Mrs", "Ms", "Dr", "Prof"};
    private static final String[] FIRST_NAMES = {
            "James", "John", "Robert", "Michael", "William", "David", "Richard", "Joseph", "Thomas", "Charles",
            "Mary", "Patricia", "Jennifer", "Linda", "Elizabeth", "Barbara", "Susan", "Jessica", "Sarah", "Karen"
    };
    private static final String[] LAST_NAMES = {
            "Smith", "Johnson", "Williams", "Jones", "Brown", "Davis", "Miller", "Wilson", "Moore", "Taylor",
            "Anderson", "Thomas", "Jackson", "White", "Harris", "Martin", "Thompson", "Garcia", "Martinez", "Robinson"
    };
    
    // Address components
    private static final String[] STREET_NAMES = {
            "High Street", "Main Street", "Park Road", "London Road", "Church Street", 
            "Victoria Road", "Green Lane", "Manor Road", "Church Lane", "Mill Lane"
    };
    private static final String[] CITIES = {
            "London", "Manchester", "Birmingham", "Liverpool", "Leeds",
            "Glasgow", "Edinburgh", "Bristol", "Sheffield", "Cardiff"
    };
    private static final String[] POSTCODES = {
            "AB12 3CD", "EF45 6GH", "IJ78 9KL", "MN01 2OP", "QR34 5ST",
            "UV67 8WX", "YZ90 1AB", "CD23 4EF", "GH56 7IJ", "KL89 0MN"
    };

    public TestDataGenerator(JdbcTemplate jdbcTemplate) {
        this.jdbcTemplate = jdbcTemplate;
    }

    /**
     * Reset the database by dropping and recreating all tables
     */
    @Transactional
    public void resetDatabase() {
        logger.info("Resetting database...");
        
        // Drop tables if they exist (in reverse order of dependencies)
        jdbcTemplate.execute("DROP TABLE IF EXISTS TRANSACTIONS");
        jdbcTemplate.execute("DROP TABLE IF EXISTS ACCOUNTS");
        jdbcTemplate.execute("DROP TABLE IF EXISTS CUSTOMERS");
        jdbcTemplate.execute("DROP TABLE IF EXISTS CONTROL");
        
        // Create tables
        createCustomerTable();
        createAccountTable();
        createTransactionTable();
        createControlTable();
        
        // Initialize Control record
        Control initialControl = Control.builder()
                .customerCount(0L)
                .lastCustomerNumber(1000000000L) // Start at 1000000000
                .accountCount(0)
                .lastAccountNumber(10000000) // Start at 10000000
                .build();
        
        jdbcTemplate.update(
                "INSERT INTO CONTROL (ID, CUSTOMER_COUNT, LAST_CUSTOMER_NUMBER, ACCOUNT_COUNT, LAST_ACCOUNT_NUMBER) VALUES (?, ?, ?, ?, ?)",
                Control.CONTROL_ID, initialControl.getCustomerCount(), initialControl.getLastCustomerNumber(),
                initialControl.getAccountCount(), initialControl.getLastAccountNumber());
        
        logger.info("Database reset complete");
    }

    private void createCustomerTable() {
        jdbcTemplate.execute(
            "CREATE TABLE IF NOT EXISTS CUSTOMERS (" +
            "    COMPOSITE_ID VARCHAR(20) PRIMARY KEY," +
            "    EYE_CATCHER VARCHAR(4) NOT NULL," +
            "    SORT_CODE VARCHAR(6) NOT NULL," +
            "    CUSTOMER_NUMBER BIGINT NOT NULL," +
            "    NAME VARCHAR(60) NOT NULL," +
            "    ADDRESS VARCHAR(160) NOT NULL," +
            "    DATE_OF_BIRTH DATE NOT NULL," +
            "    CREDIT_SCORE INTEGER NOT NULL," +
            "    CREDIT_SCORE_REVIEW_DATE DATE" +
            ")");
    }
    
    private void createAccountTable() {
        jdbcTemplate.execute(
            "CREATE TABLE IF NOT EXISTS ACCOUNTS (" +
            "    COMPOSITE_ID VARCHAR(14) PRIMARY KEY," +
            "    EYE_CATCHER VARCHAR(4) NOT NULL," +
            "    CUSTOMER_NUMBER BIGINT NOT NULL," +
            "    SORT_CODE VARCHAR(6) NOT NULL," +
            "    ACCOUNT_NUMBER VARCHAR(8) NOT NULL," +
            "    ACCOUNT_TYPE VARCHAR(8) NOT NULL," +
            "    INTEREST_RATE DECIMAL(6,2) NOT NULL," +
            "    OPENED_DATE DATE NOT NULL," +
            "    OVERDRAFT_LIMIT INTEGER NOT NULL," +
            "    LAST_STATEMENT_DATE DATE," +
            "    NEXT_STATEMENT_DATE DATE," +
            "    AVAILABLE_BALANCE DECIMAL(12,2) NOT NULL," +
            "    ACTUAL_BALANCE DECIMAL(12,2) NOT NULL," +
            "    FOREIGN KEY (SORT_CODE, CUSTOMER_NUMBER) REFERENCES CUSTOMERS(SORT_CODE, CUSTOMER_NUMBER)" +
            ")");
    }
    
    private void createTransactionTable() {
        jdbcTemplate.execute(
            "CREATE TABLE IF NOT EXISTS TRANSACTIONS (" +
            "    COMPOSITE_ID VARCHAR(50) PRIMARY KEY," +
            "    EYE_CATCHER VARCHAR(4) NOT NULL," +
            "    LOGICALLY_DELETED BOOLEAN NOT NULL DEFAULT FALSE," +
            "    SORT_CODE VARCHAR(6) NOT NULL," +
            "    ACCOUNT_NUMBER VARCHAR(8) NOT NULL," +
            "    TRANSACTION_DATE DATE NOT NULL," +
            "    TRANSACTION_TIME TIME NOT NULL," +
            "    REFERENCE_NUMBER BIGINT NOT NULL," +
            "    TRANSACTION_TYPE VARCHAR(3) NOT NULL," +
            "    DESCRIPTION VARCHAR(40)," +
            "    AMOUNT DECIMAL(12,2) NOT NULL," +
            "    TARGET_SORT_CODE VARCHAR(6)," +
            "    TARGET_ACCOUNT_NUMBER VARCHAR(8)," +
            "    FOREIGN KEY (SORT_CODE, ACCOUNT_NUMBER) REFERENCES ACCOUNTS(SORT_CODE, ACCOUNT_NUMBER)" +
            ")");
    }
    
    private void createControlTable() {
        jdbcTemplate.execute(
            "CREATE TABLE IF NOT EXISTS CONTROL (" +
            "    ID VARCHAR(10) PRIMARY KEY," +
            "    CUSTOMER_COUNT BIGINT NOT NULL," +
            "    LAST_CUSTOMER_NUMBER BIGINT NOT NULL," +
            "    ACCOUNT_COUNT INTEGER NOT NULL," +
            "    LAST_ACCOUNT_NUMBER INTEGER NOT NULL" +
            ")");
    }

    /**
     * Generate a specified number of customers with accounts and transactions
     */
    @Transactional
    public void generateTestData(int customerCount, int accountsPerCustomer, int transactionsPerAccount) {
        logger.info("Generating test data: {} customers, {} accounts per customer, {} transactions per account", 
                customerCount, accountsPerCustomer, transactionsPerAccount);
        
        // Get current control record
        Control control = fetchControlRecord();
        
        // Generate customers
        List<Customer> customers = generateCustomers(customerCount, control);
        
        // Generate accounts for each customer
        List<Account> allAccounts = new ArrayList<>();
        for (Customer customer : customers) {
            List<Account> customerAccounts = generateAccounts(customer, accountsPerCustomer, control);
            allAccounts.addAll(customerAccounts);
        }
        
        // Generate transactions for each account
        List<Transaction> allTransactions = new ArrayList<>();
        for (Account account : allAccounts) {
            List<Transaction> accountTransactions = generateTransactions(account, transactionsPerAccount);
            allTransactions.addAll(accountTransactions);
            
            // For some accounts, generate transfer transactions between accounts
            if (allAccounts.size() > 1 && random.nextBoolean()) {
                Account targetAccount = getRandomAccountExcept(allAccounts, account);
                Transaction transferTransaction = generateTransferTransaction(account, targetAccount);
                allTransactions.add(transferTransaction);
            }
        }
        
        // Update control record
        updateControlRecord(control);
        
        logger.info("Test data generation complete: {} customers, {} accounts, {} transactions", 
                customers.size(), allAccounts.size(), allTransactions.size());
    }
    
    private Control fetchControlRecord() {
        return jdbcTemplate.queryForObject(
                "SELECT * FROM CONTROL WHERE ID = ?",
                (rs, rowNum) -> Control.builder()
                        .customerCount(rs.getLong("CUSTOMER_COUNT"))
                        .lastCustomerNumber(rs.getLong("LAST_CUSTOMER_NUMBER"))
                        .accountCount(rs.getInt("ACCOUNT_COUNT"))
                        .lastAccountNumber(rs.getInt("LAST_ACCOUNT_NUMBER"))
                        .build(),
                Control.CONTROL_ID);
    }
    
    private void updateControlRecord(Control control) {
        jdbcTemplate.update(
                "UPDATE CONTROL SET CUSTOMER_COUNT = ?, LAST_CUSTOMER_NUMBER = ?, ACCOUNT_COUNT = ?, LAST_ACCOUNT_NUMBER = ? WHERE ID = ?",
                control.getCustomerCount(), control.getLastCustomerNumber(),
                control.getAccountCount(), control.getLastAccountNumber(),
                Control.CONTROL_ID);
    }
    
    private final Random random = new Random();
    
    /**
     * Generate a list of customers
     */
    private List<Customer> generateCustomers(int count, Control control) {
        List<Customer> customers = new ArrayList<>();
        
        for (int i = 0; i < count; i++) {
            String sortCode = getRandomSortCode();
            long customerNumber = control.getLastCustomerNumber() + 1;
            control.setLastCustomerNumber(customerNumber);
            control.setCustomerCount(control.getCustomerCount() + 1);
            
            Customer customer = Customer.builder()
                    .eyeCatcher(Customer.VALID_EYECATCHER)
                    .sortCode(sortCode)
                    .customerNumber(customerNumber)
                    .name(generateRandomName())
                    .address(generateRandomAddress())
                    .dateOfBirth(generateRandomBirthDate())
                    .creditScore(generateRandomCreditScore())
                    .creditScoreReviewDate(LocalDate.now().minusMonths(random.nextInt(12)))
                    .build();
            
            // Save to database
            jdbcTemplate.update(
                    "INSERT INTO CUSTOMERS (COMPOSITE_ID, EYE_CATCHER, SORT_CODE, CUSTOMER_NUMBER, NAME, ADDRESS, DATE_OF_BIRTH, CREDIT_SCORE, CREDIT_SCORE_REVIEW_DATE) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)",
                    customer.getCompositeId(), customer.getEyeCatcher(), customer.getSortCode(), customer.getCustomerNumber(),
                    customer.getName(), customer.getAddress(), customer.getDateOfBirth(),
                    customer.getCreditScore(), customer.getCreditScoreReviewDate());
            
            customers.add(customer);
        }
        
        return customers;
    }
    
    /**
     * Generate accounts for a customer
     */
    private List<Account> generateAccounts(Customer customer, int count, Control control) {
        List<Account> accounts = new ArrayList<>();
        
        for (int i = 0; i < count; i++) {
            int accountNumber = control.getLastAccountNumber() + 1;
            control.setLastAccountNumber(accountNumber);
            control.setAccountCount(control.getAccountCount() + 1);
            
            // Format account number to 8 digits with leading zeros
            String formattedAccountNumber = String.format("%08d", accountNumber);
            
            Account account = Account.builder()
                    .eyeCatcher(Account.VALID_EYECATCHER)
                    .customerNumber(customer.getCustomerNumber())
                    .sortCode(customer.getSortCode())
                    .accountNumber(formattedAccountNumber)
                    .accountType(getRandomAccountType())
                    .interestRate(generateRandomInterestRate())
                    .openedDate(generateRandomOpenedDate())
                    .overdraftLimit(generateRandomOverdraftLimit())
                    .lastStatementDate(LocalDate.now().minusMonths(1))
                    .nextStatementDate(LocalDate.now().plusMonths(1))
                    .availableBalance(generateRandomBalance())
                    .actualBalance(generateRandomBalance())
                    .build();
            
            // Save to database
            jdbcTemplate.update(
                    "INSERT INTO ACCOUNTS (COMPOSITE_ID, EYE_CATCHER, CUSTOMER_NUMBER, SORT_CODE, ACCOUNT_NUMBER, ACCOUNT_TYPE, INTEREST_RATE, OPENED_DATE, OVERDRAFT_LIMIT, LAST_STATEMENT_DATE, NEXT_STATEMENT_DATE, AVAILABLE_BALANCE, ACTUAL_BALANCE) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
                    account.getCompositeId(), account.getEyeCatcher(), account.getCustomerNumber(), account.getSortCode(),
                    account.getAccountNumber(), account.getAccountType(), account.getInterestRate(),
                    account.getOpenedDate(), account.getOverdraftLimit(), account.getLastStatementDate(),
                    account.getNextStatementDate(), account.getAvailableBalance(), account.getActualBalance());
            
            accounts.add(account);
        }
        
        return accounts;
    }
    
    /**
     * Generate transactions for an account
     */
    private List<Transaction> generateTransactions(Account account, int count) {
        List<Transaction> transactions = new ArrayList<>();
        
        // Generate random past dates for transactions, sorted from oldest to newest
        List<LocalDate> transactionDates = IntStream.range(0, count)
                .mapToObj(i -> LocalDate.now().minusDays(random.nextInt(365)))
                .sorted()
                .collect(Collectors.toList());
        
        for (int i = 0; i < count; i++) {
            LocalDate transactionDate = transactionDates.get(i);
            LocalTime transactionTime = generateRandomTime();
            long referenceNumber = generateUniqueReferenceNumber();
            String transactionType = getRandomTransactionType();
            
            Transaction transaction = Transaction.builder()
                    .eyeCatcher(Transaction.VALID_EYECATCHER)
                    .logicallyDeleted(false)
                    .sortCode(account.getSortCode())
                    .accountNumber(account.getAccountNumber())
                    .transactionDate(transactionDate)
                    .transactionTime(transactionTime)
                    .referenceNumber(referenceNumber)
                    .transactionType(transactionType)
                    .description(generateTransactionDescription(transactionType))
                    .amount(generateTransactionAmount(transactionType))
                    .build();
            
            // Save to database
            jdbcTemplate.update(
                    "INSERT INTO TRANSACTIONS (COMPOSITE_ID, EYE_CATCHER, LOGICALLY_DELETED, SORT_CODE, ACCOUNT_NUMBER, TRANSACTION_DATE, TRANSACTION_TIME, REFERENCE_NUMBER, TRANSACTION_TYPE, DESCRIPTION, AMOUNT, TARGET_SORT_CODE, TARGET_ACCOUNT_NUMBER) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
                    transaction.getCompositeId(), transaction.getEyeCatcher(), transaction.isLogicallyDeleted(), 
                    transaction.getSortCode(), transaction.getAccountNumber(), transaction.getTransactionDate(),
                    transaction.getTransactionTime(), transaction.getReferenceNumber(), transaction.getTransactionType(),
                    transaction.getDescription(), transaction.getAmount(), transaction.getTargetSortCode(),
                    transaction.getTargetAccountNumber());
            
            transactions.add(transaction);
        }
        
        return transactions;
    }
    
    /**
     * Generate a transfer transaction between two accounts
     */
    private Transaction generateTransferTransaction(Account fromAccount, Account toAccount) {
        LocalDate transactionDate = LocalDate.now().minusDays(random.nextInt(30));
        LocalTime transactionTime = generateRandomTime();
        long referenceNumber = generateUniqueReferenceNumber();
        BigDecimal amount = new BigDecimal(random.nextInt(5000) + 10);
        
        Transaction transaction = Transaction.builder()
                .eyeCatcher(Transaction.VALID_EYECATCHER)
                .logicallyDeleted(false)
                .sortCode(fromAccount.getSortCode())
                .accountNumber(fromAccount.getAccountNumber())
                .transactionDate(transactionDate)
                .transactionTime(transactionTime)
                .referenceNumber(referenceNumber)
                .transactionType(Transaction.TYPE_TRANSFER)
                .description("Transfer to account " + toAccount.getAccountNumber())
                .amount(amount)
                .targetSortCode(toAccount.getSortCode())
                .targetAccountNumber(toAccount.getAccountNumber())
                .build();
        
        // Save to database
        jdbcTemplate.update(
                "INSERT INTO TRANSACTIONS (COMPOSITE_ID, EYE_CATCHER, LOGICALLY_DELETED, SORT_CODE, ACCOUNT_NUMBER, TRANSACTION_DATE, TRANSACTION_TIME, REFERENCE_NUMBER, TRANSACTION_TYPE, DESCRIPTION, AMOUNT, TARGET_SORT_CODE, TARGET_ACCOUNT_NUMBER) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
                transaction.getCompositeId(), transaction.getEyeCatcher(), transaction.isLogicallyDeleted(), 
                transaction.getSortCode(), transaction.getAccountNumber(), transaction.getTransactionDate(),
                transaction.getTransactionTime(), transaction.getReferenceNumber(), transaction.getTransactionType(),
                transaction.getDescription(), transaction.getAmount(), transaction.getTargetSortCode(),
                transaction.getTargetAccountNumber());
        
        return transaction;
    }
    
    // Helper methods for generating random data
    
    private String getRandomSortCode() {
        return SORT_CODES[random.nextInt(SORT_CODES.length)];
    }
    
    private String getRandomAccountType() {
        return ACCOUNT_TYPES[random.nextInt(ACCOUNT_TYPES.length)];
    }
    
    private String getRandomTransactionType() {
        return TRANSACTION_TYPES[random.nextInt(TRANSACTION_TYPES.length)];
    }
    
    private String generateRandomName() {
        String title = TITLES[random.nextInt(TITLES.length)];
        String firstName = FIRST_NAMES[random.nextInt(FIRST_NAMES.length)];
        String lastName = LAST_NAMES[random.nextInt(LAST_NAMES.length)];
        return title + " " + firstName + " " + lastName;
    }
    
    private String generateRandomAddress() {
        int houseNumber = random.nextInt(100) + 1;
        String street = STREET_NAMES[random.nextInt(STREET_NAMES.length)];
        String city = CITIES[random.nextInt(CITIES.length)];
        String postcode = POSTCODES[random.nextInt(POSTCODES.length)];
        return houseNumber + " " + street + ", " + city + ", " + postcode;
    }
    
    private LocalDate generateRandomBirthDate() {
        int year = LocalDate.now().getYear() - 18 - random.nextInt(60); // Ages 18-78
        int month = random.nextInt(12) + 1;
        int day = random.nextInt(28) + 1; // Simplification to avoid month length issues
        return LocalDate.of(year, month, day);
    }
    
    private int generateRandomCreditScore() {
        return random.nextInt(700) + 300; // 300-999 range
    }
    
    private LocalDate generateRandomOpenedDate() {
        return LocalDate.now().minusYears(random.nextInt(10))
                .minusMonths(random.nextInt(12))
                .minusDays(random.nextInt(28));
    }
    
    private BigDecimal generateRandomInterestRate() {
        return new BigDecimal(random.nextInt(500) / 100.0).setScale(2, BigDecimal.ROUND_HALF_UP); // 0.00-4.99%
    }
    
    private int generateRandomOverdraftLimit() {
        return random.nextInt(5) * 1000; // 0, 1000, 2000, 3000, 4000
    }
    
    private BigDecimal generateRandomBalance() {
        int pounds = random.nextInt(20000) - 2000; // Allow some negative balances
        int pence = random.nextInt(100);
        return new BigDecimal(pounds + (pence / 100.0)).setScale(2, BigDecimal.ROUND_HALF_UP);
    }
    
    private LocalTime generateRandomTime() {
        return LocalTime.of(
                random.nextInt(24), // Hours
                random.nextInt(60), // Minutes
                random.nextInt(60)  // Seconds
        );
    }
    
    private long generateUniqueReferenceNumber() {
        return System.currentTimeMillis() + random.nextInt(1000);
    }
    
    private String generateTransactionDescription(String transactionType) {
        switch (transactionType) {
            case Transaction.TYPE_CREDIT:
                return "Deposit at branch";
            case Transaction.TYPE_DEBIT:
                return "Withdrawal at branch";
            case Transaction.TYPE_PAYMENT_CREDIT:
                return "Payment received";
            case Transaction.TYPE_PAYMENT_DEBIT:
                return "Payment sent";
            case Transaction.TYPE_CHEQUE_PAID_IN:
                return "Cheque deposit";
            case Transaction.TYPE_CHEQUE_PAID_OUT:
                return "Cheque payment";
            case Transaction.TYPE_TRANSFER:
                return "Transfer to another account";
            default:
                return "Transaction";
        }
    }
    
    private BigDecimal generateTransactionAmount(String transactionType) {
        boolean isCredit = transactionType.equals(Transaction.TYPE_CREDIT) || 
                transactionType.equals(Transaction.TYPE_PAYMENT_CREDIT) || 
                transactionType.equals(Transaction.TYPE_CHEQUE_PAID_IN);
        
        int amount = random.nextInt(5000) + 1;
        return new BigDecimal(amount).setScale(2, BigDecimal.ROUND_HALF_UP);
    }
    
    private Account getRandomAccountExcept(List<Account> accounts, Account exceptAccount) {
        List<Account> filteredAccounts = accounts.stream()
                .filter(a -> !a.getCompositeId().equals(exceptAccount.getCompositeId()))
                .collect(Collectors.toList());
        return filteredAccounts.get(random.nextInt(filteredAccounts.size()));
    }
}
