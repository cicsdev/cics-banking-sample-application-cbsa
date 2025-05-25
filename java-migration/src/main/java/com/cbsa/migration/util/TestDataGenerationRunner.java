package com.cbsa.migration.util;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.CommandLineRunner;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.stereotype.Component;

/**
 * Command-line runner for generating test data
 * Activated with command-line arguments:
 *   --generate-test-data=true
 *   --customer-count=10
 *   --accounts-per-customer=2
 *   --transactions-per-account=5
 *   --reset-database=true
 */
@Component
@ConditionalOnProperty(name = "generate-test-data", havingValue = "true")
public class TestDataGenerationRunner implements CommandLineRunner {

    private static final Logger logger = LoggerFactory.getLogger(TestDataGenerationRunner.class);
    
    private final TestDataGenerator testDataGenerator;
    
    // Default values
    private int customerCount = 10;
    private int accountsPerCustomer = 2;
    private int transactionsPerAccount = 5;
    private boolean resetDatabase = false;
    
    public TestDataGenerationRunner(TestDataGenerator testDataGenerator) {
        this.testDataGenerator = testDataGenerator;
    }

    @Override
    public void run(String... args) {
        logger.info("Starting test data generation process");
        
        // Parse command-line arguments
        parseArguments(args);
        
        // Reset database if requested
        if (resetDatabase) {
            testDataGenerator.resetDatabase();
        }
        
        // Generate test data
        testDataGenerator.generateTestData(customerCount, accountsPerCustomer, transactionsPerAccount);
        
        logger.info("Test data generation complete");
    }
    
    private void parseArguments(String[] args) {
        for (String arg : args) {
            if (arg.startsWith("--customer-count=")) {
                customerCount = Integer.parseInt(arg.substring("--customer-count=".length()));
            } else if (arg.startsWith("--accounts-per-customer=")) {
                accountsPerCustomer = Integer.parseInt(arg.substring("--accounts-per-customer=".length()));
            } else if (arg.startsWith("--transactions-per-account=")) {
                transactionsPerAccount = Integer.parseInt(arg.substring("--transactions-per-account=".length()));
            } else if (arg.startsWith("--reset-database=")) {
                resetDatabase = Boolean.parseBoolean(arg.substring("--reset-database=".length()));
            }
        }
        
        logger.info("Configuration: customerCount={}, accountsPerCustomer={}, transactionsPerAccount={}, resetDatabase={}",
                customerCount, accountsPerCustomer, transactionsPerAccount, resetDatabase);
    }
}
