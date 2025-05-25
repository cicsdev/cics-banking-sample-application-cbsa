package com.cbsa.migration.controller;

import com.cbsa.migration.util.DataGenerator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

/**
 * Controller for generating and resetting test data.
 * For development and testing purposes only.
 */
@RestController
@RequestMapping("/api/data")
public class DataController {

    private static final Logger logger = LoggerFactory.getLogger(DataController.class);
    
    private final DataGenerator dataGenerator;
    
    @Autowired
    public DataController(DataGenerator dataGenerator) {
        this.dataGenerator = dataGenerator;
    }
    
    /**
     * Reset the database (delete all data).
     * 
     * @return Response indicating success
     */
    @PostMapping("/reset")
    public ResponseEntity<Map<String, String>> resetDatabase() {
        logger.info("Resetting database via API call");
        dataGenerator.resetDatabase();
        return ResponseEntity.ok(Map.of("status", "success", "message", "Database reset complete"));
    }
    
    /**
     * Generate sample data.
     * 
     * @param customerCount Number of customers to generate (default: 10)
     * @param minAccountsPerCustomer Minimum accounts per customer (default: 1)
     * @param maxAccountsPerCustomer Maximum accounts per customer (default: 3)
     * @param minTransactionsPerAccount Minimum transactions per account (default: 5)
     * @param maxTransactionsPerAccount Maximum transactions per account (default: 20)
     * @return Response indicating success
     */
    @PostMapping("/generate")
    public ResponseEntity<Map<String, String>> generateData(
            @RequestParam(defaultValue = "10") int customerCount,
            @RequestParam(defaultValue = "1") int minAccountsPerCustomer,
            @RequestParam(defaultValue = "3") int maxAccountsPerCustomer,
            @RequestParam(defaultValue = "5") int minTransactionsPerAccount,
            @RequestParam(defaultValue = "20") int maxTransactionsPerAccount) {
        
        logger.info("Generating sample data via API call: {} customers, {}-{} accounts per customer, {}-{} transactions per account",
                customerCount, minAccountsPerCustomer, maxAccountsPerCustomer, 
                minTransactionsPerAccount, maxTransactionsPerAccount);
        
        dataGenerator.generateSampleData(
                customerCount,
                minAccountsPerCustomer,
                maxAccountsPerCustomer,
                minTransactionsPerAccount,
                maxTransactionsPerAccount);
        
        return ResponseEntity.ok(Map.of(
                "status", "success", 
                "message", "Sample data generation complete"));
    }
    
    /**
     * Combined operation to reset database and generate new data in one call.
     * 
     * @param customerCount Number of customers to generate (default: 10)
     * @param minAccountsPerCustomer Minimum accounts per customer (default: 1)
     * @param maxAccountsPerCustomer Maximum accounts per customer (default: 3)
     * @param minTransactionsPerAccount Minimum transactions per account (default: 5)
     * @param maxTransactionsPerAccount Maximum transactions per account (default: 20)
     * @return Response indicating success
     */
    @PostMapping("/reset-and-generate")
    public ResponseEntity<Map<String, String>> resetAndGenerate(
            @RequestParam(defaultValue = "10") int customerCount,
            @RequestParam(defaultValue = "1") int minAccountsPerCustomer,
            @RequestParam(defaultValue = "3") int maxAccountsPerCustomer,
            @RequestParam(defaultValue = "5") int minTransactionsPerAccount,
            @RequestParam(defaultValue = "20") int maxTransactionsPerAccount) {
        
        logger.info("Resetting database and generating new sample data via API call");
        
        dataGenerator.resetDatabase();
        dataGenerator.generateSampleData(
                customerCount,
                minAccountsPerCustomer,
                maxAccountsPerCustomer,
                minTransactionsPerAccount,
                maxTransactionsPerAccount);
        
        return ResponseEntity.ok(Map.of(
                "status", "success", 
                "message", "Database reset and sample data generation complete"));
    }
}
