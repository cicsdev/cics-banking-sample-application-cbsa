package com.cbsa.migration.dto;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.Size;

/**
 * Request DTO for error logging API.
 * Corresponds to COBOL ABNDPROC DFHCOMMAREA structure.
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
public class ErrorRequestDto {
    
    @NotBlank(message = "Program name is required")
    @Size(max = 8, message = "Program name must not exceed 8 characters")
    private String programName;
    
    @Size(max = 8, message = "Application ID must not exceed 8 characters")
    private String applicationId;
    
    @Size(max = 4, message = "Transaction ID must not exceed 4 characters")
    private String transactionId;
    
    @Size(max = 10, message = "Date must not exceed 10 characters")
    private String date;
    
    @Size(max = 8, message = "Time must not exceed 8 characters")
    private String time;
    
    @Size(max = 4, message = "Error code must not exceed 4 characters")
    private String errorCode;
    
    @Size(max = 500, message = "Error message must not exceed 500 characters")
    private String errorMessage;
    
    @Size(max = 2000, message = "Stack trace must not exceed 2000 characters")
    private String stackTrace;
    
    // COBOL response codes
    private String responseCode;
    private String response2Code;
    private String sqlCode;
    
    @Size(max = 600, message = "Freeform text must not exceed 600 characters")
    private String freeformText;
    
    /**
     * Create ErrorRequestDto from exception
     */
    public static ErrorRequestDto fromException(String programName, Exception exception) {
        ErrorRequestDto dto = new ErrorRequestDto();
        dto.setProgramName(programName);
        dto.setErrorCode("JAVA");
        dto.setErrorMessage(exception.getMessage());
        dto.setStackTrace(getStackTraceAsString(exception));
        return dto;
    }
    
    /**
     * Create ErrorRequestDto from exception with transaction context
     */
    public static ErrorRequestDto fromException(String programName, String applicationId, 
                                               String transactionId, Exception exception) {
        ErrorRequestDto dto = fromException(programName, exception);
        dto.setApplicationId(applicationId);
        dto.setTransactionId(transactionId);
        return dto;
    }
    
    private static String getStackTraceAsString(Exception exception) {
        if (exception == null) return null;
        
        StringBuilder sb = new StringBuilder();
        sb.append(exception.getClass().getSimpleName()).append(": ").append(exception.getMessage()).append("\n");
        
        StackTraceElement[] elements = exception.getStackTrace();
        int maxElements = Math.min(10, elements.length); // Limit to first 10 stack trace elements
        
        for (int i = 0; i < maxElements; i++) {
            sb.append("  at ").append(elements[i].toString()).append("\n");
        }
        
        if (elements.length > maxElements) {
            sb.append("  ... ").append(elements.length - maxElements).append(" more\n");
        }
        
        return sb.toString();
    }
}
