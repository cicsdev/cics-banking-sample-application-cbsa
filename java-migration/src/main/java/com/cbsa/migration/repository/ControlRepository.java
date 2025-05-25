package com.cbsa.migration.repository;

import com.cbsa.migration.model.Control;

import java.util.Optional;

/**
 * Repository interface for Control operations
 */
public interface ControlRepository {

    /**
     * Get the control record (only one exists in the system)
     * 
     * @return the control record if found
     */
    Optional<Control> getControl();
    
    /**
     * Save the control record
     * 
     * @param control the control record to save
     * @return the saved control record
     */
    Control save(Control control);
    
    /**
     * Increment and get the next customer number
     * 
     * @return the next customer number
     */
    Long getNextCustomerNumber();
    
    /**
     * Increment and get the next account number
     * 
     * @return the next account number
     */
    Integer getNextAccountNumber();
    
    /**
     * Initialize the control record with default values if it doesn't exist
     * 
     * @return the initialized control record
     */
    Control initializeControlRecord();
}
