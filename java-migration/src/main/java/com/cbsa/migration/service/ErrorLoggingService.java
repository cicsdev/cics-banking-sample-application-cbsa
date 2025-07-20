package com.cbsa.migration.service;

import com.cbsa.migration.dto.ErrorRequestDto;
import com.cbsa.migration.dto.ErrorResponseDto;
import com.cbsa.migration.model.ApplicationError;
import com.cbsa.migration.repository.ApplicationErrorRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.List;

/**
 * Service for error logging functionality.
 * Migrated from COBOL ABNDPROC program business logic.
 */
@Service
public class ErrorLoggingService {
    
    private final ApplicationErrorRepository applicationErrorRepository;
    private static final DateTimeFormatter TIMESTAMP_FORMATTER = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");
    
    @Autowired
    public ErrorLoggingService(ApplicationErrorRepository applicationErrorRepository) {
        this.applicationErrorRepository = applicationErrorRepository;
    }
    
    /**
     * Log an error record to the database.
     * Equivalent to COBOL CICS WRITE FILE('ABNDFILE') operation.
     * 
     * @param errorRequest the error information to log
     * @return response indicating success or failure
     */
    @Transactional
    public ErrorResponseDto logError(ErrorRequestDto errorRequest) {
        String currentTimestamp = LocalDateTime.now().format(TIMESTAMP_FORMATTER);
        
        try {
            // Validate required fields
            validateErrorRequest(errorRequest);
            
            // Create ApplicationError from request
            ApplicationError applicationError = createApplicationError(errorRequest, currentTimestamp);
            
            // Save to database
            Long errorId = applicationErrorRepository.save(applicationError);
            
            // Return success response
            return ErrorResponseDto.success(errorId, currentTimestamp);
            
        } catch (IllegalArgumentException e) {
            // Validation error
            return ErrorResponseDto.validationError(e.getMessage(), currentTimestamp);
        } catch (Exception e) {
            // Handle "error-while-logging-error" scenario gracefully
            String errorMessage = String.format("Failed to log error for program %s: %s", 
                errorRequest.getProgramName(), e.getMessage());
            return ErrorResponseDto.failure(errorMessage, currentTimestamp);
        }
    }
    
    /**
     * Log an error from a Java exception.
     * Convenience method for internal service calls.
     * 
     * @param programName the program where the error occurred
     * @param exception the exception to log
     * @return response indicating success or failure
     */
    @Transactional
    public ErrorResponseDto logError(String programName, Exception exception) {
        ErrorRequestDto errorRequest = ErrorRequestDto.fromException(programName, exception);
        return logError(errorRequest);
    }
    
    /**
     * Log an error from a Java exception with transaction context.
     * 
     * @param programName the program where the error occurred
     * @param applicationId the application ID
     * @param transactionId the transaction ID
     * @param exception the exception to log
     * @return response indicating success or failure
     */
    @Transactional
    public ErrorResponseDto logError(String programName, String applicationId, 
                                   String transactionId, Exception exception) {
        ErrorRequestDto errorRequest = ErrorRequestDto.fromException(
            programName, applicationId, transactionId, exception);
        return logError(errorRequest);
    }
    
    /**
     * Get error statistics for a program.
     * 
     * @param programName the program name
     * @return count of errors for the program
     */
    public long getErrorCount(String programName) {
        return applicationErrorRepository.countByProgramName(programName);
    }
    
    /**
     * Get recent errors for monitoring purposes.
     * 
     * @param limit maximum number of errors to return
     * @return list of recent error records
     */
    public List<ApplicationError> getRecentErrors(int limit) {
        return applicationErrorRepository.findRecentErrors(limit);
    }
    
    /**
     * Get all errors for a specific program.
     * 
     * @param programName the program name
     * @return list of error records for the program
     */
    public List<ApplicationError> getErrorsByProgram(String programName) {
        return applicationErrorRepository.findByProgramName(programName);
    }
    
    private void validateErrorRequest(ErrorRequestDto errorRequest) {
        if (errorRequest == null) {
            throw new IllegalArgumentException("Error request cannot be null");
        }
        
        if (errorRequest.getProgramName() == null || errorRequest.getProgramName().trim().isEmpty()) {
            throw new IllegalArgumentException("Program name is required");
        }
        
        if (errorRequest.getProgramName().length() > 8) {
            throw new IllegalArgumentException("Program name must not exceed 8 characters");
        }
    }
    
    private ApplicationError createApplicationError(ErrorRequestDto errorRequest, String currentTimestamp) {
        // Use provided timestamp or current timestamp
        String errorTimestamp = (errorRequest.getDate() != null && errorRequest.getTime() != null) 
            ? errorRequest.getDate() + " " + errorRequest.getTime()
            : currentTimestamp;
        
        return new ApplicationError(
            errorTimestamp,
            errorRequest.getApplicationId(),
            errorRequest.getTransactionId(),
            errorRequest.getErrorCode(),
            errorRequest.getProgramName(),
            errorRequest.getErrorMessage(),
            errorRequest.getStackTrace(),
            errorRequest.getResponseCode(),
            errorRequest.getResponse2Code(),
            errorRequest.getSqlCode(),
            errorRequest.getFreeformText()
        );
    }
}
