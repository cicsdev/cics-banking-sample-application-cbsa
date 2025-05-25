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
import java.time.LocalTime;

/**
 * Transaction entity representing the PROCTRAN copybook structure from COBOL
 * Based on PROCTRAN.cpy
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class Transaction {
    
    // Constants for validation
    public static final String VALID_EYECATCHER = "PRTR";
    public static final String LOGICAL_DELETE_FLAG = "\\xFF";
    
    // Transaction types
    public static final String TYPE_CHEQUE_ACKNOWLEDGED = "CHA";
    public static final String TYPE_CHEQUE_FAILURE = "CHF";
    public static final String TYPE_CHEQUE_PAID_IN = "CHI";
    public static final String TYPE_CHEQUE_PAID_OUT = "CHO";
    public static final String TYPE_CREDIT = "CRE";
    public static final String TYPE_DEBIT = "DEB";
    public static final String TYPE_WEB_CREATE_ACCOUNT = "ICA";
    public static final String TYPE_WEB_CREATE_CUSTOMER = "ICC";
    public static final String TYPE_WEB_DELETE_ACCOUNT = "IDA";
    public static final String TYPE_WEB_DELETE_CUSTOMER = "IDC";
    public static final String TYPE_BRANCH_CREATE_ACCOUNT = "OCA";
    public static final String TYPE_BRANCH_CREATE_CUSTOMER = "OCC";
    public static final String TYPE_BRANCH_DELETE_ACCOUNT = "ODA";
    public static final String TYPE_BRANCH_DELETE_CUSTOMER = "ODC";
    public static final String TYPE_CREATE_SODD = "OCS";
    public static final String TYPE_PAYMENT_CREDIT = "PCR";
    public static final String TYPE_PAYMENT_DEBIT = "PDR";
    public static final String TYPE_TRANSFER = "TFR";

    /**
     * Eye catcher for the record - always "PRTR"
     * PROC-TRAN-EYE-CATCHER PIC X(4)
     */
    @NotBlank
    @Size(min = 4, max = 4)
    private String eyeCatcher;

    /**
     * Flag indicating if this transaction is logically deleted
     * This is derived from PROC-TRAN-LOGICAL-DELETE-FLAG which is a redefine of eyeCatcher
     */
    private boolean logicallyDeleted;

    /**
     * Sort code for the account
     * PROC-TRAN-SORT-CODE PIC 9(6)
     */
    @NotNull
    @Pattern(regexp = "\\d{6}")
    private String sortCode;

    /**
     * Account number
     * PROC-TRAN-NUMBER PIC 9(8)
     */
    @NotNull
    @Pattern(regexp = "\\d{8}")
    private String accountNumber;

    /**
     * Date of the transaction
     * PROC-TRAN-DATE PIC 9(8)
     * In COBOL format: YYYYMMDD
     */
    @NotNull
    private LocalDate transactionDate;

    /**
     * Time of the transaction
     * PROC-TRAN-TIME PIC 9(6)
     * In COBOL format: HHMMSS
     */
    @NotNull
    private LocalTime transactionTime;

    /**
     * Reference number for the transaction
     * PROC-TRAN-REF PIC 9(12)
     */
    @NotNull
    private Long referenceNumber;

    /**
     * Type of transaction
     * PROC-TRAN-TYPE PIC X(3)
     */
    @NotBlank
    @Size(min = 3, max = 3)
    private String transactionType;

    /**
     * Description of the transaction
     * PROC-TRAN-DESC PIC X(40)
     * This has multiple redefines in COBOL for different transaction types
     */
    @Size(max = 40)
    private String description;

    /**
     * The amount of the transaction
     * PROC-TRAN-AMOUNT PIC S9(10)V99
     */
    @NotNull
    private BigDecimal amount;

    /**
     * For transfer transactions, this holds the target sort code
     */
    private String targetSortCode;

    /**
     * For transfer transactions, this holds the target account number
     */
    private String targetAccountNumber;

    /**
     * Utility method to create a composite ID from all key fields
     * This will be used as the primary key in the database
     */
    public String getCompositeId() {
        return sortCode + accountNumber + "-" + transactionDate + "-" + transactionTime + "-" + referenceNumber;
    }

    /**
     * Helper method to determine if this transaction is a transfer
     */
    public boolean isTransfer() {
        return TYPE_TRANSFER.equals(transactionType);
    }

    /**
     * Helper method to determine if this transaction is a credit
     */
    public boolean isCredit() {
        return TYPE_CREDIT.equals(transactionType) || 
               TYPE_PAYMENT_CREDIT.equals(transactionType) || 
               TYPE_CHEQUE_PAID_IN.equals(transactionType);
    }

    /**
     * Helper method to determine if this transaction is a debit
     */
    public boolean isDebit() {
        return TYPE_DEBIT.equals(transactionType) || 
               TYPE_PAYMENT_DEBIT.equals(transactionType) || 
               TYPE_CHEQUE_PAID_OUT.equals(transactionType);
    }
}
