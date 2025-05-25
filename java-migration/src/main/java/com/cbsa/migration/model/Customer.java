package com.cbsa.migration.model;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;
import java.time.LocalDate;

/**
 * Customer entity representing the CUSTOMER copybook structure from COBOL
 * Based on CUSTOMER.cpy
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class Customer {
    
    // Constants for eyecatcher validation
    public static final String VALID_EYECATCHER = "CUST";

    /**
     * Eye catcher for the record - always "CUST"
     * CUSTOMER-EYECATCHER PIC X(4)
     */
    @NotBlank
    @Size(min = 4, max = 4)
    @Pattern(regexp = VALID_EYECATCHER)
    private String eyeCatcher;

    /**
     * Sort code for the customer's branch
     * CUSTOMER-SORTCODE PIC 9(6)
     */
    @NotNull
    @Pattern(regexp = "\\d{6}")
    private String sortCode;

    /**
     * Customer's unique identifier
     * CUSTOMER-NUMBER PIC 9(10)
     */
    @NotNull
    private Long customerNumber;

    /**
     * Customer's full name
     * CUSTOMER-NAME PIC X(60)
     * In COBOL, this is a single field but comments suggest it could be broken down further:
     * - CUSTOMER-TITLE
     * - CUSTOMER-GIVEN-NAME
     * - CUSTOMER-INITIALS
     * - CUSTOMER-FAMILY-NAME
     */
    @NotBlank
    @Size(max = 60)
    private String name;

    /**
     * Customer's full address
     * CUSTOMER-ADDRESS PIC X(160)
     * In COBOL, this is a single field but comments suggest it could be broken down further:
     * - CUSTOMER-STREET-ADDRESS
     * - CUSTOMER-ADDRESS-DISTRICT
     * - CUSTOMER-ADDRESS-TOWN
     * - CUSTOMER-POSTCODE-OR-ZIP
     */
    @NotBlank
    @Size(max = 160)
    private String address;

    /**
     * Customer's date of birth
     * CUSTOMER-DATE-OF-BIRTH PIC 9(8)
     * In COBOL format: DDMMYYYY
     */
    @NotNull
    private LocalDate dateOfBirth;

    /**
     * Customer's credit score
     * CUSTOMER-CREDIT-SCORE PIC 999
     */
    @NotNull
    private Integer creditScore;

    /**
     * Date when the credit score was last reviewed
     * CUSTOMER-CS-REVIEW-DATE PIC 9(8)
     * In COBOL format: DDMMYYYY
     */
    private LocalDate creditScoreReviewDate;

    /**
     * Utility method to create a composite ID from sort code and customer number
     * This will be used as the primary key in the database
     */
    public String getCompositeId() {
        return sortCode + "-" + customerNumber;
    }
}
