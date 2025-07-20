package com.cbsa.migration.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalTime;

/**
 * Response DTO for Transaction information.
 * Provides a clean API response without exposing internal database fields.
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class TransactionResponseDto {
    
    /**
     * Transaction reference number
     */
    private Long referenceNumber;
    
    /**
     * Account sort code
     */
    private String sortCode;
    
    /**
     * Account number
     */
    private String accountNumber;
    
    /**
     * Transaction date
     */
    private LocalDate transactionDate;
    
    /**
     * Transaction time
     */
    private LocalTime transactionTime;
    
    /**
     * Transaction type (e.g., CRE, DEB, TFR)
     */
    private String transactionType;
    
    /**
     * Transaction description
     */
    private String description;
    
    /**
     * Transaction amount
     */
    private BigDecimal amount;
    
    /**
     * Target sort code (for transfers)
     */
    private String targetSortCode;
    
    /**
     * Target account number (for transfers)
     */
    private String targetAccountNumber;
    
    /**
     * Whether this is a debit transaction
     */
    private Boolean isDebit;
    
    /**
     * Transaction status (derived field)
     */
    private String status;
    
    /**
     * Running balance after this transaction (optional)
     */
    private BigDecimal runningBalance;
}
