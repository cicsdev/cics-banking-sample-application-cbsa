package com.cbsa.migration.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.DecimalMin;
import javax.validation.constraints.Size;
import java.math.BigDecimal;

/**
 * Request DTO for Transaction creation operations.
 * Validates input data before processing.
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class TransactionRequestDto {
    
    /**
     * Account sort code
     */
    @NotBlank(message = "Sort code is required")
    @Size(min = 6, max = 6, message = "Sort code must be exactly 6 characters")
    private String sortCode;
    
    /**
     * Account number
     */
    @NotBlank(message = "Account number is required")
    @Size(max = 8, message = "Account number must not exceed 8 characters")
    private String accountNumber;
    
    /**
     * Transaction type (e.g., CRE, DEB, TFR)
     */
    @NotBlank(message = "Transaction type is required")
    @Size(max = 3, message = "Transaction type must not exceed 3 characters")
    private String transactionType;
    
    /**
     * Transaction description
     */
    @NotBlank(message = "Transaction description is required")
    @Size(max = 35, message = "Transaction description must not exceed 35 characters")
    private String description;
    
    /**
     * Transaction amount
     */
    @NotNull(message = "Transaction amount is required")
    @DecimalMin(value = "0.01", message = "Transaction amount must be greater than 0")
    private BigDecimal amount;
    
    /**
     * Target sort code (required for transfers)
     */
    @Size(min = 6, max = 6, message = "Target sort code must be exactly 6 characters")
    private String targetSortCode;
    
    /**
     * Target account number (required for transfers)
     */
    @Size(max = 8, message = "Target account number must not exceed 8 characters")
    private String targetAccountNumber;
}
