package com.cbsa.migration.model;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;

/**
 * Domain model representing an application error record.
 * Migrated from COBOL ABNDPROC program's ABNDINFO copybook structure.
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
public class ApplicationError {
    
    private Long id;
    private String timestamp;
    private String applicationId;
    private String transactionId;
    private String errorCode;
    private String programName;
    private String errorMessage;
    private String stackTrace;
    private String createdAt;
    
    // Response codes from COBOL DFHCOMMAREA
    private String responseCode;
    private String response2Code;
    private String sqlCode;
    private String freeformText;
    
    /**
     * Constructor for creating new error records (without ID)
     */
    public ApplicationError(String timestamp, String applicationId, String transactionId,
                          String errorCode, String programName, String errorMessage,
                          String stackTrace, String responseCode, String response2Code,
                          String sqlCode, String freeformText) {
        this.timestamp = timestamp;
        this.applicationId = applicationId;
        this.transactionId = transactionId;
        this.errorCode = errorCode;
        this.programName = programName;
        this.errorMessage = errorMessage;
        this.stackTrace = stackTrace;
        this.responseCode = responseCode;
        this.response2Code = response2Code;
        this.sqlCode = sqlCode;
        this.freeformText = freeformText;
    }
    
    /**
     * Check if this error has critical severity based on COBOL logic
     */
    public boolean isCritical() {
        return errorCode != null && (
            errorCode.startsWith("ABN") || 
            errorCode.equals("ASRA") || 
            errorCode.equals("ASRB")
        );
    }
    
    /**
     * Get short error summary for logging
     */
    public String getErrorSummary() {
        return String.format("[%s] %s in %s: %s", 
            errorCode != null ? errorCode : "UNKNOWN",
            programName != null ? programName : "UNKNOWN_PROGRAM",
            transactionId != null ? transactionId : "UNKNOWN_TRANS",
            errorMessage != null ? errorMessage.substring(0, Math.min(100, errorMessage.length())) : "No message"
        );
    }
}
