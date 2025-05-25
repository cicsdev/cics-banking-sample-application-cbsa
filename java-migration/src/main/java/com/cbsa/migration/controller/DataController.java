package com.cbsa.migration.controller;

import com.cbsa.migration.util.DataGenerator;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;
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
@Tag(name = "Test Data", description = "API endpoints for managing test data - for development use only")
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
    @Operation(summary = "Reset database", description = "Delete all data from the database, resetting it to an empty state")
    @ApiResponses(value = {
        @ApiResponse(responseCode = "200", description = "Database reset successfully", 
                  content = {@Content(mediaType = "application/json", schema = @Schema(implementation = Map.class))})
    })
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
    @Operation(summary = "Generate sample data", description = "Generate sample customers, accounts, and transactions for testing")
    @ApiResponses(value = {
        @ApiResponse(responseCode = "200", description = "Sample data generated successfully", 
                  content = {@Content(mediaType = "application/json", schema = @Schema(implementation = Map.class))})
    })
    @PostMapping("/generate")
    public ResponseEntity<Map<String, String>> generateData(
            @Parameter(description = "Number of customers to generate") @RequestParam(defaultValue = "10") int customerCount,
            @Parameter(description = "Minimum accounts per customer") @RequestParam(defaultValue = "1") int minAccountsPerCustomer,
            @Parameter(description = "Maximum accounts per customer") @RequestParam(defaultValue = "3") int maxAccountsPerCustomer,
            @Parameter(description = "Minimum transactions per account") @RequestParam(defaultValue = "5") int minTransactionsPerAccount,
            @Parameter(description = "Maximum transactions per account") @RequestParam(defaultValue = "20") int maxTransactionsPerAccount) {
        
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
    @Operation(summary = "Reset database and generate sample data", description = "Combined operation to reset the database and generate new sample data in one call")
    @ApiResponses(value = {
        @ApiResponse(responseCode = "200", description = "Database reset and sample data generated successfully", 
                  content = {@Content(mediaType = "application/json", schema = @Schema(implementation = Map.class))})
    })
    @PostMapping("/reset-and-generate")
    public ResponseEntity<Map<String, String>> resetAndGenerate(
            @Parameter(description = "Number of customers to generate") @RequestParam(defaultValue = "10") int customerCount,
            @Parameter(description = "Minimum accounts per customer") @RequestParam(defaultValue = "1") int minAccountsPerCustomer,
            @Parameter(description = "Maximum accounts per customer") @RequestParam(defaultValue = "3") int maxAccountsPerCustomer,
            @Parameter(description = "Minimum transactions per account") @RequestParam(defaultValue = "5") int minTransactionsPerAccount,
            @Parameter(description = "Maximum transactions per account") @RequestParam(defaultValue = "20") int maxTransactionsPerAccount) {
        
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
