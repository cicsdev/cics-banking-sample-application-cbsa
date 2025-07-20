package com.cbsa.migration.dto;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Response DTO for error logging API.
 * Returns confirmation of error record creation.
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
public class ErrorResponseDto {
    
    private Long errorId;
    private String status;
    private String message;
    private String timestamp;
    
    /**
     * Create success response
     */
    public static ErrorResponseDto success(Long errorId, String timestamp) {
        return new ErrorResponseDto(
            errorId, 
            "SUCCESS", 
            "Error record successfully logged", 
            timestamp
        );
    }
    
    /**
     * Create error response when logging fails
     */
    public static ErrorResponseDto failure(String message, String timestamp) {
        return new ErrorResponseDto(
            null, 
            "FAILURE", 
            message, 
            timestamp
        );
    }
    
    /**
     * Create validation error response
     */
    public static ErrorResponseDto validationError(String message, String timestamp) {
        return new ErrorResponseDto(
            null, 
            "VALIDATION_ERROR", 
            message, 
            timestamp
        );
    }
}
