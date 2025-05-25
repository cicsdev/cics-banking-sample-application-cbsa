package com.cbsa.migration.model;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;
import java.math.BigDecimal;
import java.time.LocalDate;

/**
 * Account entity representing the ACCOUNT copybook structure from COBOL
 * Based on ACCOUNT.cpy
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class Account {
    
    // Constants for eyecatcher validation
    public static final String VALID_EYECATCHER = "ACCT";

    /**
     * Eye catcher for the record - always "ACCT"
     * ACCOUNT-EYE-CATCHER PIC X(4)
     */
    @NotBlank
    @Size(min = 4, max = 4)
    @Pattern(regexp = VALID_EYECATCHER)
    private String eyeCatcher;

    /**
     * Customer number that owns this account
     * ACCOUNT-CUST-NO PIC 9(10)
     */
    @NotNull
    private Long customerNumber;

    /**
     * Sort code of the account
     * ACCOUNT-SORT-CODE PIC 9(6)
     */
    @NotNull
    @Pattern(regexp = "\\d{6}")
    private String sortCode;

    /**
     * Account number
     * ACCOUNT-NUMBER PIC 9(8)
     */
    @NotNull
    @Pattern(regexp = "\\d{8}")
    private String accountNumber;

    /**
     * Type of account (e.g. CURRENT, SAVINGS)
     * ACCOUNT-TYPE PIC X(8)
     */
    @NotBlank
    @Size(max = 8)
    private String accountType;

    /**
     * Interest rate for the account
     * ACCOUNT-INTEREST-RATE PIC 9(4)V99
     */
    @NotNull
    private BigDecimal interestRate;

    /**
     * Date when the account was opened
     * ACCOUNT-OPENED PIC 9(8)
     * In COBOL format: DDMMYYYY
     */
    @NotNull
    private LocalDate openedDate;

    /**
     * Overdraft limit for the account
     * ACCOUNT-OVERDRAFT-LIMIT PIC 9(8)
     */
    @NotNull
    private Integer overdraftLimit;

    /**
     * Date of the last statement
     * ACCOUNT-LAST-STMT-DATE PIC 9(8)
     */
    private LocalDate lastStatementDate;

    /**
     * Date of the next statement
     * ACCOUNT-NEXT-STMT-DATE PIC 9(8)
     */
    private LocalDate nextStatementDate;

    /**
     * Available balance including pending transactions
     * ACCOUNT-AVAILABLE-BALANCE PIC S9(10)V99
     */
    @NotNull
    private BigDecimal availableBalance;

    /**
     * Actual balance (settled transactions only)
     * ACCOUNT-ACTUAL-BALANCE PIC S9(10)V99
     */
    @NotNull
    private BigDecimal actualBalance;

    /**
     * Utility method to create a composite ID from sort code and account number
     * This will be used as the primary key in the database
     */
    public String getCompositeId() {
        return sortCode + accountNumber;
    }
}
