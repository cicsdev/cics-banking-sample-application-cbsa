package com.cbsa.migration.util;

import com.cbsa.migration.model.Account;
import com.cbsa.migration.model.Control;
import com.cbsa.migration.model.Customer;
import com.cbsa.migration.model.Transaction;
import com.cbsa.migration.repository.AccountRepository;
import com.cbsa.migration.repository.ControlRepository;
import com.cbsa.migration.repository.CustomerRepository;
import com.cbsa.migration.repository.TransactionRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Random;

/**
 * Utility to generate test data for the banking application.
 * Provides methods to reset the database and populate it with sample data.
 */
@Component
public class DataGenerator {
    private static final Logger logger = LoggerFactory.getLogger(DataGenerator.class);
    
    private static final String DEFAULT_SORT_CODE = "536892"; // Without dashes to match database requirements - represents sort code 53-68-92
    private static final List<String> ACCOUNT_TYPES = Arrays.asList("CURRENT", "SAVINGS", "CHECKING", "LOAN");
    private static final List<String> TRANSACTION_TYPES = Arrays.asList("DEPOSIT", "WITHDRAWAL", "TRANSFER", "INTEREST", "FEE");
    private static final List<String> FIRST_NAMES = Arrays.asList("John", "Jane", "Bob", "Alice", "Michael", "Sarah", "David", "Lisa", "Robert", "Emily");
    private static final List<String> LAST_NAMES = Arrays.asList("Smith", "Johnson", "Williams", "Jones", "Brown", "Davis", "Miller", "Wilson", "Moore", "Taylor");
    private static final List<String> STREETS = Arrays.asList("Main St", "Oak Ave", "Park Rd", "Maple Ln", "Cedar Dr");
    private static final List<String> CITIES = Arrays.asList("New York", "Los Angeles", "Chicago", "Houston", "Phoenix");
    private static final List<String> STATES = Arrays.asList("NY", "CA", "IL", "TX", "AZ");

    private final JdbcTemplate jdbcTemplate;
    private final ControlRepository controlRepository;
    private final CustomerRepository customerRepository;
    private final AccountRepository accountRepository;
    private final TransactionRepository transactionRepository;
    private final Random random = new Random();
    
    @Autowired
    public DataGenerator(JdbcTemplate jdbcTemplate, 
                         ControlRepository controlRepository,
                         CustomerRepository customerRepository, 
                         AccountRepository accountRepository,
                         TransactionRepository transactionRepository) {
        this.jdbcTemplate = jdbcTemplate;
        this.controlRepository = controlRepository;
        this.customerRepository = customerRepository;
        this.accountRepository = accountRepository;
        this.transactionRepository = transactionRepository;
    }

    /**
     * Reset the database by truncating all tables and reinitializing the control table.
     */
    @Transactional
    public void resetDatabase() {
        logger.info("Resetting database...");
        
        // Delete all data in reverse order of dependencies
        jdbcTemplate.update("DELETE FROM bank_transaction");
        jdbcTemplate.update("DELETE FROM account");
        jdbcTemplate.update("DELETE FROM customer");
        jdbcTemplate.update("DELETE FROM control");
        
        // Initialize control table
        Control control = new Control();
        control.setCustomerCount(0L);
        control.setLastCustomerNumber(0L);
        control.setAccountCount(0);
        control.setLastAccountNumber(0);
        
        controlRepository.save(control);
        
        logger.info("Database reset complete");
    }
    
    /**
     * Generate sample data including customers, accounts, and transactions.
     * 
     * @param customerCount Number of customers to generate
     * @param minAccountsPerCustomer Minimum number of accounts per customer
     * @param maxAccountsPerCustomer Maximum number of accounts per customer
     * @param minTransactionsPerAccount Minimum number of transactions per account
     * @param maxTransactionsPerAccount Maximum number of transactions per account
     */
    @Transactional
    public void generateSampleData(int customerCount, int minAccountsPerCustomer, int maxAccountsPerCustomer,
                                 int minTransactionsPerAccount, int maxTransactionsPerAccount) {
        logger.info("Generating sample data...");
        
        Control control = controlRepository.getControl()
                .orElseThrow(() -> new IllegalStateException("Control record not found. Reset database first."));
        
        // Generate customers
        List<Customer> customers = generateCustomers(customerCount, control);
        
        // Generate accounts for each customer
        List<Account> allAccounts = new ArrayList<>();
        for (Customer customer : customers) {
            int accountCount = randomBetween(minAccountsPerCustomer, maxAccountsPerCustomer);
            List<Account> accounts = generateAccountsForCustomer(customer, accountCount, control);
            allAccounts.addAll(accounts);
        }
        
        // Generate transactions for each account
        for (Account account : allAccounts) {
            int transactionCount = randomBetween(minTransactionsPerAccount, maxTransactionsPerAccount);
            generateTransactionsForAccount(account, transactionCount, allAccounts);
        }
        
        // Update control table with new counts
        controlRepository.save(control);
        
        logger.info("Sample data generation complete.");
        logger.info("Generated {} customers, {} accounts, and approximately {} transactions", 
                customerCount, allAccounts.size(), 
                allAccounts.size() * (minTransactionsPerAccount + maxTransactionsPerAccount) / 2);
    }
    
    /**
     * Generate a specified number of customers.
     */
    private List<Customer> generateCustomers(int count, Control control) {
        List<Customer> customers = new ArrayList<>();
        
        for (int i = 0; i < count; i++) {
            long customerNumber = control.getLastCustomerNumber() + 1;
            
            Customer customer = new Customer();
            customer.setEyeCatcher("CUST");
            customer.setSortCode(DEFAULT_SORT_CODE);
            customer.setCustomerNumber(customerNumber);
            customer.setName(randomName());
            customer.setAddress(randomAddress());
            customer.setDateOfBirth(randomDateOfBirth());
            customer.setCreditScore(randomBetween(300, 850));
            
            // Credit score review date is in the future for some customers
            if (random.nextBoolean()) {
                customer.setCreditScoreReviewDate(LocalDate.now().plusMonths(randomBetween(1, 12)));
            }
            
            customerRepository.save(customer);
            customers.add(customer);
            
            // Update control
            control.setLastCustomerNumber(customerNumber);
            control.setCustomerCount(control.getCustomerCount() + 1L);
        }
        
        return customers;
    }
    
    /**
     * Generate accounts for a specific customer.
     */
    private List<Account> generateAccountsForCustomer(Customer customer, int count, Control control) {
        List<Account> accounts = new ArrayList<>();
        
        for (int i = 0; i < count; i++) {
            int accountSeq = control.getLastAccountNumber() + 1;
            String accountNumber = String.format("%08d", accountSeq);
            
            Account account = new Account();
            account.setEyeCatcher("ACCT");
            account.setCustomerNumber(customer.getCustomerNumber());
            account.setSortCode(customer.getSortCode());
            account.setAccountNumber(accountNumber);
            account.setAccountType(getRandomElement(ACCOUNT_TYPES));
            account.setInterestRate(BigDecimal.valueOf(randomInterestRate(account.getAccountType())));
            account.setOpenedDate(randomDateInPast(1, 1825)); // 1 day to 5 years in the past
            account.setOverdraftLimit(randomOverdraftLimit(account.getAccountType()));
            
            // Set statement dates for some accounts
            if (random.nextBoolean()) {
                account.setLastStatementDate(randomDateInPast(1, 30));
                account.setNextStatementDate(LocalDate.now().plusDays(randomBetween(1, 30)));
            }
            
            // Set initial balances
            BigDecimal balance = randomAmount(account.getAccountType());
            account.setActualBalance(balance);
            account.setAvailableBalance(calculateAvailableBalance(account));
            
            accountRepository.save(account);
            accounts.add(account);
            
            // Update control
            control.setLastAccountNumber(accountSeq);
            control.setAccountCount(control.getAccountCount() + 1);
        }
        
        return accounts;
    }
    
    /**
     * Generate transactions for a specific account.
     */
    private void generateTransactionsForAccount(Account account, int count, List<Account> allAccounts) {
        BigDecimal runningBalance = account.getActualBalance();
        LocalDate startDate = account.getOpenedDate();
        LocalDate endDate = LocalDate.now();
        
        for (int i = 0; i < count; i++) {
            String transactionType = getRandomElement(TRANSACTION_TYPES);
            LocalDate transactionDate = randomDateBetween(startDate, endDate);
            LocalTime transactionTime = randomTime();
            BigDecimal amount = randomTransactionAmount(transactionType, account.getAccountType());
            
            Transaction transaction = new Transaction();
            transaction.setEyeCatcher("TRAN");
            transaction.setLogicallyDeleted(false);
            transaction.setSortCode(account.getSortCode());
            transaction.setAccountNumber(account.getAccountNumber());
            transaction.setTransactionDate(transactionDate);
            transaction.setTransactionTime(transactionTime);
            transaction.setReferenceNumber((long)(i + 1)); // Simple sequential reference
            transaction.setTransactionType(transactionType);
            transaction.setDescription(generateTransactionDescription(transactionType));
            transaction.setAmount(amount);
            
            // For transfers, set target account
            if ("TRANSFER".equals(transactionType) && allAccounts.size() > 1) {
                // Select a random account that is not the current one
                Account targetAccount;
                do {
                    targetAccount = getRandomElement(allAccounts);
                } while (targetAccount.getAccountNumber().equals(account.getAccountNumber()));
                
                transaction.setTargetSortCode(targetAccount.getSortCode());
                transaction.setTargetAccountNumber(targetAccount.getAccountNumber());
            }
            
            transactionRepository.save(transaction);
            
            // Update running balance based on transaction type
            if ("DEPOSIT".equals(transactionType) || "INTEREST".equals(transactionType)) {
                runningBalance = runningBalance.add(amount);
            } else if ("WITHDRAWAL".equals(transactionType) || "FEE".equals(transactionType)) {
                runningBalance = runningBalance.subtract(amount);
            }
            // For transfers, the balance effect would be handled in a real system by separate transactions
        }
        
        // Final balance may not match exactly due to random transactions
        // In a real system, you'd have proper double-entry bookkeeping
    }
    
    // Utility methods for random data generation
    
    private String randomName() {
        return getRandomElement(FIRST_NAMES) + " " + getRandomElement(LAST_NAMES);
    }
    
    private String randomAddress() {
        int houseNumber = randomBetween(1, 999);
        String street = getRandomElement(STREETS);
        String city = getRandomElement(CITIES);
        String state = getRandomElement(STATES);
        String zip = String.format("%05d", randomBetween(10000, 99999));
        
        return houseNumber + " " + street + ", " + city + ", " + state + " " + zip;
    }
    
    private LocalDate randomDateOfBirth() {
        // Adults between 18 and 80 years old
        int years = randomBetween(18, 80);
        return LocalDate.now().minusYears(years).minusDays(randomBetween(0, 364));
    }
    
    private LocalDate randomDateInPast(int minDays, int maxDays) {
        return LocalDate.now().minusDays(randomBetween(minDays, maxDays));
    }
    
    private LocalDate randomDateBetween(LocalDate start, LocalDate end) {
        long startEpochDay = start.toEpochDay();
        long endEpochDay = end.toEpochDay();
        long dayDiff = endEpochDay - startEpochDay;
        // Handle case where the difference might be larger than Integer.MAX_VALUE
        long randomDay = startEpochDay + (dayDiff > Integer.MAX_VALUE ? 
                random.nextInt(Integer.MAX_VALUE) : random.nextInt((int) dayDiff));
        return LocalDate.ofEpochDay(randomDay);
    }
    
    private LocalTime randomTime() {
        return LocalTime.of(randomBetween(8, 17), randomBetween(0, 59), randomBetween(0, 59));
    }
    
    private double randomInterestRate(String accountType) {
        if ("SAVINGS".equals(accountType)) {
            return 0.5 + (random.nextDouble() * 2.5); // 0.5% to 3.0%
        } else if ("LOAN".equals(accountType)) {
            return 3.0 + (random.nextDouble() * 9.0); // 3.0% to 12.0%
        } else {
            return 0.01 + (random.nextDouble() * 0.49); // 0.01% to 0.5%
        }
    }
    
    private int randomOverdraftLimit(String accountType) {
        if ("CURRENT".equals(accountType) || "CHECKING".equals(accountType)) {
            return random.nextInt(10) * 100; // 0 to 900 in steps of 100
        } else {
            return 0; // No overdraft for savings or loans
        }
    }
    
    private BigDecimal randomAmount(String accountType) {
        if ("SAVINGS".equals(accountType)) {
            return new BigDecimal(random.nextInt(10000) + 500); // 500 to 10,500
        } else if ("CURRENT".equals(accountType) || "CHECKING".equals(accountType)) {
            return new BigDecimal(random.nextInt(5000) + 100); // 100 to 5,100
        } else if ("LOAN".equals(accountType)) {
            return new BigDecimal(-1 * (random.nextInt(50000) + 1000)); // -1,000 to -51,000 (negative for a loan)
        } else {
            return new BigDecimal(random.nextInt(1000) + 100); // 100 to 1,100 default
        }
    }
    
    private BigDecimal randomTransactionAmount(String transactionType, String accountType) {
        if ("DEPOSIT".equals(transactionType)) {
            return new BigDecimal(random.nextInt(1000) + 50); // 50 to 1,050
        } else if ("WITHDRAWAL".equals(transactionType)) {
            return new BigDecimal(random.nextInt(500) + 20); // 20 to 520
        } else if ("TRANSFER".equals(transactionType)) {
            return new BigDecimal(random.nextInt(300) + 10); // 10 to 310
        } else if ("INTEREST".equals(transactionType)) {
            if ("SAVINGS".equals(accountType)) {
                return new BigDecimal(random.nextInt(100) + 1); // 1 to 101
            } else {
                return new BigDecimal(random.nextInt(10) + 1); // 1 to 11
            }
        } else if ("FEE".equals(transactionType)) {
            return new BigDecimal(random.nextInt(30) + 5); // 5 to 35
        } else {
            return new BigDecimal(random.nextInt(100) + 1); // 1 to 101 default
        }
    }
    
    private String generateTransactionDescription(String transactionType) {
        switch (transactionType) {
            case "DEPOSIT":
                return getRandomElement(Arrays.asList(
                        "Salary payment", "Cash deposit", "Check deposit", "Transfer in", "Refund"));
            case "WITHDRAWAL":
                return getRandomElement(Arrays.asList(
                        "ATM withdrawal", "Cash withdrawal", "POS payment", "Bill payment", "Debit card purchase"));
            case "TRANSFER":
                return getRandomElement(Arrays.asList(
                        "Transfer to another account", "Online payment", "Standing order", "Direct debit", "Mobile payment"));
            case "INTEREST":
                return "Interest payment";
            case "FEE":
                return getRandomElement(Arrays.asList(
                        "Monthly fee", "Overdraft fee", "Service charge", "Late payment fee", "Transaction fee"));
            default:
                return "Transaction";
        }
    }
    
    private BigDecimal calculateAvailableBalance(Account account) {
        BigDecimal actualBalance = account.getActualBalance();
        int overdraftLimit = account.getOverdraftLimit();
        
        // For loan accounts, available balance is zero (can't withdraw from a loan)
        if ("LOAN".equals(account.getAccountType())) {
            return BigDecimal.ZERO;
        }
        
        // For other accounts, available = actual + overdraft limit
        return actualBalance.add(new BigDecimal(overdraftLimit));
    }
    
    private <T> T getRandomElement(List<T> list) {
        return list.get(random.nextInt(list.size()));
    }
    
    private int randomBetween(int min, int max) {
        return min + random.nextInt(max - min + 1);
    }
    
    private long randomBetweenLong(long min, long max) {
        return min + (long)(random.nextDouble() * (max - min + 1));
    }
}
