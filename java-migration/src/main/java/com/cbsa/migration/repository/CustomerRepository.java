package com.cbsa.migration.repository;

import com.cbsa.migration.model.Customer;

import java.util.List;
import java.util.Optional;

/**
 * Repository interface for Customer operations
 */
public interface CustomerRepository {

    /**
     * Find a customer by sort code and customer number
     * 
     * @param sortCode the sort code
     * @param customerNumber the customer number
     * @return the customer if found
     */
    Optional<Customer> findById(String sortCode, Long customerNumber);
    
    /**
     * Find customers by name (partial match)
     * 
     * @param name the name to search for
     * @return list of matching customers
     */
    List<Customer> findByNameContaining(String name);
    
    /**
     * Save a new customer or update an existing one
     * 
     * @param customer the customer to save
     * @return the saved customer
     */
    Customer save(Customer customer);
    
    /**
     * Delete a customer by sort code and customer number
     * 
     * @param sortCode the sort code
     * @param customerNumber the customer number
     * @return true if deleted, false if not found
     */
    boolean deleteById(String sortCode, Long customerNumber);
    
    /**
     * Find all customers
     * 
     * @return list of all customers
     */
    List<Customer> findAll();
    
    /**
     * Count all customers
     * 
     * @return the number of customers
     */
    int count();
}
