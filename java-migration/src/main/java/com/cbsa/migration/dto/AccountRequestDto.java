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
 * Request DTO for Account creation/update operations.
 * Validates input data before processing.
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class AccountRequestDto {
    
    /**
     * Account type (e.g., CURRENT, SAVINGS)
     */
    @NotBlank(message = "Account type is required")
    @Size(max = 8, message = "Account type must not exceed 8 characters")
    private String accountType;
    
    /**
     * Customer number who will own this account
     */
    @NotNull(message = "Customer number is required")
    private Long customerNumber;
    
    /**
     * Branch sort code
     */
    @NotBlank(message = "Sort code is required")
    @Size(min = 6, max = 6, message = "Sort code must be exactly 6 characters")
    private String sortCode;
    
    /**
     * Interest rate percentage (optional, defaults based on account type)
     */
    @DecimalMin(value = "0.0", message = "Interest rate must be non-negative")
    private BigDecimal interestRate;
    
    /**
     * Overdraft limit (optional, defaults to 0)
     */
    private Integer overdraftLimit;
    
    /**
     * Initial deposit amount (optional)
     */
    @DecimalMin(value = "0.0", message = "Initial deposit must be non-negative")
    private BigDecimal initialDeposit;
}
