package com.cbsa.migration.controller;

import com.cbsa.migration.dto.CreditScoreRequestDto;
import com.cbsa.migration.dto.CreditScoreResponseDto;
import com.cbsa.migration.service.CreditAgencyService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;

/**
 * Credit Agency Controller - REST API for credit score processing
 * Migrated from COBOL program CRDTAGY1
 * 
 * Provides REST endpoint equivalent to CICS container-based credit agency service
 */
@RestController
@RequestMapping("/api/credit-agency")
@Validated
@Tag(name = "Credit Agency", description = "Credit scoring and agency services")
public class CreditAgencyController {

    private static final Logger logger = LoggerFactory.getLogger(CreditAgencyController.class);

    private final CreditAgencyService creditAgencyService;

    public CreditAgencyController(CreditAgencyService creditAgencyService) {
        this.creditAgencyService = creditAgencyService;
    }

    /**
     * Process credit score request
     * Equivalent to COBOL CRDTAGY1 program execution
     * 
     * @param request Credit score request with customer data
     * @return Credit score response with updated score and processing metadata
     */
    @PostMapping("/score")
    @Operation(
        summary = "Process credit score request",
        description = "Simulates external credit agency processing with random delay and score generation. " +
                     "Equivalent to COBOL CRDTAGY1 program functionality."
    )
    @ApiResponses(value = {
        @ApiResponse(responseCode = "200", description = "Credit score processed successfully"),
        @ApiResponse(responseCode = "400", description = "Invalid request data"),
        @ApiResponse(responseCode = "404", description = "Customer not found"),
        @ApiResponse(responseCode = "500", description = "Internal processing error")
    })
    public ResponseEntity<CreditScoreResponseDto> processCredit(
            @Parameter(description = "Credit score request with customer data", required = true)
            @Valid @RequestBody CreditScoreRequestDto request) {
        
        logger.info("Received credit score request for customer: {}-{}", 
                   request.getSortCode(), request.getCustomerNumber());

        try {
            CreditScoreResponseDto response = creditAgencyService.processCredit(request);
            
            if (response.getSuccess()) {
                logger.info("Credit score processing successful for customer {}-{}: score={}, time={}ms", 
                           request.getSortCode(), request.getCustomerNumber(), 
                           response.getUpdatedCreditScore(), response.getProcessingTimeMs());
                return ResponseEntity.ok(response);
            } else {
                // Determine appropriate HTTP status based on error
                HttpStatus status = determineErrorStatus(response.getErrorMessage());
                
                logger.warn("Credit score processing failed for customer {}-{}: {} (HTTP {})", 
                           request.getSortCode(), request.getCustomerNumber(), 
                           response.getErrorMessage(), status.value());
                
                return ResponseEntity.status(status).body(response);
            }
            
        } catch (Exception e) {
            logger.error("Unexpected error processing credit score for customer {}-{}: {}", 
                        request.getSortCode(), request.getCustomerNumber(), e.getMessage(), e);
            
            CreditScoreResponseDto errorResponse = CreditScoreResponseDto.builder()
                .sortCode(request.getSortCode())
                .customerNumber(request.getCustomerNumber())
                .success(false)
                .errorMessage("Internal server error")
                .processingTimeMs(0L)
                .build();
            
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(errorResponse);
        }
    }

    /**
     * Determine appropriate HTTP status code based on error message
     */
    private HttpStatus determineErrorStatus(String errorMessage) {
        if (errorMessage == null) {
            return HttpStatus.INTERNAL_SERVER_ERROR;
        }
        
        String lowerError = errorMessage.toLowerCase();
        
        if (lowerError.contains("not found")) {
            return HttpStatus.NOT_FOUND;
        } else if (lowerError.contains("mismatch") || lowerError.contains("invalid")) {
            return HttpStatus.BAD_REQUEST;
        } else if (lowerError.contains("timeout")) {
            return HttpStatus.REQUEST_TIMEOUT;
        } else {
            return HttpStatus.INTERNAL_SERVER_ERROR;
        }
    }
}
