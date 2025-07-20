package com.cbsa.migration.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;

/**
 * Summary DTO for Account information.
 * Provides a lightweight account summary for customer responses.
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class AccountSummaryDto {
    
    /**
     * Account number (business identifier)
     */
    private String accountNumber;
    
    /**
     * Account type (e.g., CURRENT, SAVINGS)
     */
    private String accountType;
    
    /**
     * Current available balance
     */
    private BigDecimal availableBalance;
    
    /**
     * Actual balance (settled transactions)
     */
    private BigDecimal actualBalance;
    
    /**
     * Account status (derived field)
     */
    private String status;
}
