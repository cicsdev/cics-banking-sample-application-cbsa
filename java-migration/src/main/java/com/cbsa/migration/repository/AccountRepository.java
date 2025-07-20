package com.cbsa.migration.repository;

import com.cbsa.migration.model.Account;

import java.util.List;
import java.util.Optional;

/**
 * Repository interface for Account operations
 */
public interface AccountRepository {

    /**
     * Find an account by its sort code and account number
     * 
     * @param sortCode the sort code
     * @param accountNumber the account number
     * @return the account if found
     */
    Optional<Account> findById(String sortCode, String accountNumber);
    
    /**
     * Find all accounts for a specific customer
     * 
     * @param customerNumber the customer number
     * @return list of accounts owned by the customer
     */
    List<Account> findByCustomerNumber(Long customerNumber);
    
    /**
     * Save a new account or update an existing one
     * 
     * @param account the account to save
     * @return the saved account
     */
    Account save(Account account);
    
    /**
     * Delete an account by its sort code and account number
     * 
     * @param sortCode the sort code
     * @param accountNumber the account number
     * @return true if deleted, false if not found
     */
    boolean deleteById(String sortCode, String accountNumber);
    
    /**
     * Find all accounts
     * 
     * @return list of all accounts
     */
    List<Account> findAll();
    
    /**
     * Count all accounts
     * 
     * @return the number of accounts
     */
    int count();

    /**
     * Find all accounts for the specified sort code.
     *
     * @param sortCode the branch sort code
     * @return list of matching accounts (may be empty)
     */
    List<Account> findBySortCode(String sortCode);

    /**
     * Fetch the account with the highest account number for the sort code.
     * Mirrors COBOL last-record logic.
     *
     * @param sortCode branch sort code
     * @return highest-numbered account if present
     */
    Optional<Account> findTopBySortCodeOrderByAccountNumberDesc(String sortCode);
}
