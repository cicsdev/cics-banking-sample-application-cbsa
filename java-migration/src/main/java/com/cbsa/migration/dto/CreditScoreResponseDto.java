package com.cbsa.migration.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDate;

/**
 * Response DTO for credit score processing
 * Maps to COBOL WS-CONT-IN structure after processing in CRDTAGY1
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class CreditScoreResponseDto {

    /**
     * Sort code for the customer's branch
     */
    private String sortCode;

    /**
     * Customer's unique identifier
     */
    private Long customerNumber;

    /**
     * Updated credit score (1-999)
     */
    private Integer updatedCreditScore;

    /**
     * Date when the credit score was reviewed/updated
     */
    private LocalDate scoreReviewDate;

    /**
     * Processing time in milliseconds (for simulation tracking)
     */
    private Long processingTimeMs;

    /**
     * Indicates if the credit scoring was successful
     */
    private Boolean success;

    /**
     * Error message if processing failed
     */
    private String errorMessage;
}
