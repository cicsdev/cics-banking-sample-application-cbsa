package com.cbsa.migration.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;
import java.time.LocalDate;

/**
 * Response DTO for Account information.
 * Provides a clean API response without exposing internal database fields.
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class AccountResponseDto {
    
    /**
     * Account number (business identifier)
     */
    private String accountNumber;
    
    /**
     * Branch sort code
     */
    private String sortCode;
    
    /**
     * Account type (e.g., CURRENT, SAVINGS)
     */
    private String accountType;
    
    /**
     * Customer number who owns this account
     */
    private Long customerNumber;
    
    /**
     * Customer name (for convenience)
     */
    private String customerName;
    
    /**
     * Current available balance
     */
    private BigDecimal availableBalance;
    
    /**
     * Actual balance (settled transactions)
     */
    private BigDecimal actualBalance;
    
    /**
     * Interest rate percentage
     */
    private BigDecimal interestRate;
    
    /**
     * Date account was opened
     */
    private LocalDate openedDate;
    
    /**
     * Overdraft limit
     */
    private Integer overdraftLimit;
    
    /**
     * Date of last statement
     */
    private LocalDate lastStatementDate;
    
    /**
     * Date of next statement
     */
    private LocalDate nextStatementDate;
    
    /**
     * Account status (derived field)
     */
    private String status;
}
