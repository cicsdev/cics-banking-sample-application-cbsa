package com.cbsa.migration.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;
import javax.validation.constraints.Min;
import javax.validation.constraints.Max;
import java.time.LocalDate;

/**
 * Request DTO for credit score processing
 * Maps to COBOL WS-CONT-IN structure from CRDTAGY1
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class CreditScoreRequestDto {

    /**
     * Sort code for the customer's branch
     * WS-CONT-IN-SORTCODE PIC 9(6) DISPLAY
     */
    @NotBlank(message = "Sort code is required")
    @Pattern(regexp = "\\d{6}", message = "Sort code must be 6 digits")
    private String sortCode;

    /**
     * Customer's unique identifier
     * WS-CONT-IN-NUMBER PIC 9(10) DISPLAY
     */
    @NotNull(message = "Customer number is required")
    private Long customerNumber;

    /**
     * Customer's full name
     * WS-CONT-IN-NAME PIC X(60)
     */
    @NotBlank(message = "Customer name is required")
    @Size(max = 60, message = "Customer name cannot exceed 60 characters")
    private String name;

    /**
     * Customer's full address
     * WS-CONT-IN-ADDRESS PIC X(160)
     */
    @NotBlank(message = "Customer address is required")
    @Size(max = 160, message = "Customer address cannot exceed 160 characters")
    private String address;

    /**
     * Customer's date of birth
     * WS-CONT-IN-DATE-OF-BIRTH PIC 9(8)
     */
    @NotNull(message = "Date of birth is required")
    private LocalDate dateOfBirth;

    /**
     * Current credit score (optional - may be null for new customers)
     * WS-CONT-IN-CREDIT-SCORE PIC 999
     */
    @Min(value = 1, message = "Credit score must be between 1 and 999")
    @Max(value = 999, message = "Credit score must be between 1 and 999")
    private Integer currentCreditScore;
}
