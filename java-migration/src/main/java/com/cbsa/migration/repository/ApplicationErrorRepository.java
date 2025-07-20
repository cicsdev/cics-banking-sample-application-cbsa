package com.cbsa.migration.repository;

import com.cbsa.migration.model.ApplicationError;

import java.util.List;
import java.util.Optional;

/**
 * Repository interface for ApplicationError persistence operations.
 * Provides abstraction for error logging functionality migrated from COBOL ABNDPROC.
 */
public interface ApplicationErrorRepository {
    
    /**
     * Save a new application error record.
     * Equivalent to COBOL CICS WRITE FILE('ABNDFILE') operation.
     * 
     * @param applicationError the error record to save
     * @return the generated ID of the saved error record
     */
    Long save(ApplicationError applicationError);
    
    /**
     * Find an error record by its ID
     * 
     * @param id the error record ID
     * @return Optional containing the error record if found
     */
    Optional<ApplicationError> findById(Long id);
    
    /**
     * Find all error records for a specific program
     * 
     * @param programName the program name to search for
     * @return list of error records for the program
     */
    List<ApplicationError> findByProgramName(String programName);
    
    /**
     * Find all error records for a specific application/transaction combination
     * 
     * @param applicationId the application ID
     * @param transactionId the transaction ID
     * @return list of error records for the app/transaction
     */
    List<ApplicationError> findByApplicationAndTransaction(String applicationId, String transactionId);
    
    /**
     * Count total number of error records in the system
     * 
     * @return total count of error records
     */
    long count();
    
    /**
     * Count error records for a specific program
     * 
     * @param programName the program name
     * @return count of error records for the program
     */
    long countByProgramName(String programName);
    
    /**
     * Find recent error records (useful for monitoring)
     * 
     * @param limit maximum number of records to return
     * @return list of most recent error records
     */
    List<ApplicationError> findRecentErrors(int limit);
}
