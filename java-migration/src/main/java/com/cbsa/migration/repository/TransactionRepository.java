package com.cbsa.migration.repository;

import com.cbsa.migration.model.Transaction;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

/**
 * Repository interface for Transaction operations
 */
public interface TransactionRepository {

    /**
     * Find a transaction by its composite ID (sortCode, accountNumber, date, time, reference)
     * 
     * @param compositeId the composite ID
     * @return the transaction if found
     */
    Optional<Transaction> findById(String compositeId);
    
    /**
     * Find all transactions for a specific account
     * 
     * @param sortCode the sort code
     * @param accountNumber the account number
     * @return list of transactions for the account
     */
    List<Transaction> findByAccount(String sortCode, String accountNumber);
    
    /**
     * Find transactions for an account within a date range
     * 
     * @param sortCode the sort code
     * @param accountNumber the account number
     * @param fromDate the start date (inclusive)
     * @param toDate the end date (inclusive)
     * @return list of transactions within the date range
     */
    List<Transaction> findByAccountAndDateRange(String sortCode, String accountNumber, 
                                                LocalDate fromDate, LocalDate toDate);
    
    /**
     * Find transactions by type
     * 
     * @param transactionType the transaction type
     * @return list of transactions of the specified type
     */
    List<Transaction> findByTransactionType(String transactionType);
    
    /**
     * Save a new transaction or update an existing one
     * 
     * @param transaction the transaction to save
     * @return the saved transaction
     */
    Transaction save(Transaction transaction);
    
    /**
     * Delete a transaction by its composite ID
     * 
     * @param compositeId the composite ID
     * @return true if deleted, false if not found
     */
    boolean deleteById(String compositeId);
    
    /**
     * Mark a transaction as logically deleted
     * 
     * @param compositeId the composite ID
     * @return true if updated, false if not found
     */
    boolean markAsDeleted(String compositeId);
    
    /**
     * Find all transactions
     * 
     * @return list of all transactions
     */
    List<Transaction> findAll();
    
    /**
     * Count all transactions
     * 
     * @return the number of transactions
     */
    int count();
}
