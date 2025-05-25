package com.cbsa.migration.controller;

import com.cbsa.migration.model.Control;
import com.cbsa.migration.repository.AccountRepository;
import com.cbsa.migration.repository.ControlRepository;
import com.cbsa.migration.repository.CustomerRepository;
import com.cbsa.migration.repository.TransactionRepository;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

/**
 * Simple controller to display application status and verify setup
 */
@RestController
@RequestMapping("/api/status")
public class StatusController {

    private final AccountRepository accountRepository;
    private final CustomerRepository customerRepository;
    private final TransactionRepository transactionRepository;
    private final ControlRepository controlRepository;
    
    @Value("${spring.datasource.url}")
    private String databaseUrl;

    public StatusController(AccountRepository accountRepository,
                           CustomerRepository customerRepository,
                           TransactionRepository transactionRepository,
                           ControlRepository controlRepository) {
        this.accountRepository = accountRepository;
        this.customerRepository = customerRepository;
        this.transactionRepository = transactionRepository;
        this.controlRepository = controlRepository;
    }

    /**
     * Get application status and database connection info
     */
    @GetMapping
    public ResponseEntity<Map<String, Object>> getStatus() {
        Map<String, Object> status = new HashMap<>();
        
        // Application info
        status.put("application", "CBSA Java Migration");
        status.put("version", "0.0.1");
        status.put("database", databaseUrl);
        
        // Initialize control record if needed
        Control control = controlRepository.initializeControlRecord();
        
        // Entity counts
        status.put("customers", customerRepository.count());
        status.put("accounts", accountRepository.count());
        status.put("transactions", transactionRepository.count());
        
        // Control record info
        Map<String, Object> controlInfo = new HashMap<>();
        controlInfo.put("lastCustomerNumber", control.getLastCustomerNumber());
        controlInfo.put("lastAccountNumber", control.getLastAccountNumber());
        status.put("control", controlInfo);
        
        return ResponseEntity.ok(status);
    }
}
