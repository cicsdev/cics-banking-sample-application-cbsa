package com.cbsa.migration.repository.jdbc;

import com.cbsa.migration.model.Transaction;
import com.cbsa.migration.repository.TransactionRepository;
import org.springframework.dao.EmptyResultDataAccessException;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.core.RowMapper;
import org.springframework.stereotype.Repository;

import java.math.BigDecimal;
import java.sql.ResultSet;
import java.time.LocalDate;
import java.time.LocalTime;
import java.util.List;
import java.util.Optional;

/**
 * JDBC implementation of TransactionRepository using Spring's JdbcTemplate
 */
@Repository
public class JdbcTransactionRepository implements TransactionRepository {

    private final JdbcTemplate jdbcTemplate;
    
    /**
     * Row mapper for Transaction entities
     */
    private final RowMapper<Transaction> rowMapper = (ResultSet rs, int rowNum) -> {
        Transaction transaction = new Transaction();
        transaction.setEyeCatcher(rs.getString("eye_catcher"));
        transaction.setLogicallyDeleted(rs.getBoolean("logically_deleted"));
        transaction.setSortCode(rs.getString("sort_code"));
        transaction.setAccountNumber(rs.getString("account_number"));
        transaction.setTransactionDate(LocalDate.parse(rs.getString("transaction_date")));
        transaction.setTransactionTime(LocalTime.parse(rs.getString("transaction_time")));
        transaction.setReferenceNumber(rs.getLong("reference_number"));
        transaction.setTransactionType(rs.getString("transaction_type"));
        transaction.setDescription(rs.getString("description"));
        transaction.setTargetSortCode(rs.getString("target_sort_code"));
        transaction.setTargetAccountNumber(rs.getString("target_account_number"));
        transaction.setAmount(new BigDecimal(rs.getString("amount")));
        
        return transaction;
    };

    public JdbcTransactionRepository(JdbcTemplate jdbcTemplate) {
        this.jdbcTemplate = jdbcTemplate;
    }

    @Override
    public Optional<Transaction> findById(String compositeId) {
        // Parse the composite ID to extract components
        String[] parts = compositeId.split("-");
        if (parts.length < 5) {
            return Optional.empty();
        }
        
        String sortCode = parts[0].substring(0, 6);
        String accountNumber = parts[0].substring(6);
        String transactionDate = parts[1];
        String transactionTime = parts[2];
        String referenceNumber = parts[3];
        
        try {
            Transaction transaction = jdbcTemplate.queryForObject(
                "SELECT * FROM bank_transaction WHERE sort_code = ? AND account_number = ? " +
                "AND transaction_date = ? AND transaction_time = ? AND reference_number = ?",
                rowMapper,
                sortCode,
                accountNumber,
                transactionDate,
                transactionTime,
                Long.parseLong(referenceNumber)
            );
            return Optional.ofNullable(transaction);
        } catch (EmptyResultDataAccessException e) {
            return Optional.empty();
        }
    }

    @Override
    public List<Transaction> findByAccount(String sortCode, String accountNumber) {
        return jdbcTemplate.query(
            "SELECT * FROM bank_transaction WHERE sort_code = ? AND account_number = ? " +
            "ORDER BY transaction_date DESC, transaction_time DESC",
            rowMapper,
            sortCode,
            accountNumber
        );
    }

    @Override
    public List<Transaction> findByAccountAndDateRange(String sortCode, String accountNumber, 
                                                     LocalDate fromDate, LocalDate toDate) {
        return jdbcTemplate.query(
            "SELECT * FROM bank_transaction WHERE sort_code = ? AND account_number = ? " +
            "AND transaction_date >= ? AND transaction_date <= ? " +
            "ORDER BY transaction_date DESC, transaction_time DESC",
            rowMapper,
            sortCode,
            accountNumber,
            fromDate.toString(),
            toDate.toString()
        );
    }

    @Override
    public List<Transaction> findByTransactionType(String transactionType) {
        return jdbcTemplate.query(
            "SELECT * FROM bank_transaction WHERE transaction_type = ? " +
            "ORDER BY transaction_date DESC, transaction_time DESC",
            rowMapper,
            transactionType
        );
    }

    @Override
    public Transaction save(Transaction transaction) {
        // Build the composite ID
        String compositeId = transaction.getCompositeId();
        
        // Check if transaction exists
        Optional<Transaction> existingTransaction = findById(compositeId);
        
        if (existingTransaction.isPresent()) {
            // Update
            jdbcTemplate.update(
                "UPDATE bank_transaction SET " +
                "eye_catcher = ?, " +
                "logically_deleted = ?, " +
                "transaction_type = ?, " +
                "description = ?, " +
                "target_sort_code = ?, " +
                "target_account_number = ?, " +
                "amount = ? " +
                "WHERE sort_code = ? AND account_number = ? " +
                "AND transaction_date = ? AND transaction_time = ? AND reference_number = ?",
                transaction.getEyeCatcher(),
                transaction.isLogicallyDeleted() ? 1 : 0,
                transaction.getTransactionType(),
                transaction.getDescription(),
                transaction.getTargetSortCode(),
                transaction.getTargetAccountNumber(),
                transaction.getAmount().toString(),
                transaction.getSortCode(),
                transaction.getAccountNumber(),
                transaction.getTransactionDate().toString(),
                transaction.getTransactionTime().toString(),
                transaction.getReferenceNumber()
            );
        } else {
            // Insert
            jdbcTemplate.update(
                "INSERT INTO bank_transaction (" +
                "eye_catcher, logically_deleted, sort_code, account_number, " +
                "transaction_date, transaction_time, reference_number, transaction_type, " +
                "description, target_sort_code, target_account_number, amount) " +
                "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
                transaction.getEyeCatcher(),
                transaction.isLogicallyDeleted() ? 1 : 0,
                transaction.getSortCode(),
                transaction.getAccountNumber(),
                transaction.getTransactionDate().toString(),
                transaction.getTransactionTime().toString(),
                transaction.getReferenceNumber(),
                transaction.getTransactionType(),
                transaction.getDescription(),
                transaction.getTargetSortCode(),
                transaction.getTargetAccountNumber(),
                transaction.getAmount().toString()
            );
        }
        
        return transaction;
    }

    @Override
    public boolean deleteById(String compositeId) {
        // Parse the composite ID to extract components
        String[] parts = compositeId.split("-");
        if (parts.length < 5) {
            return false;
        }
        
        String sortCode = parts[0].substring(0, 6);
        String accountNumber = parts[0].substring(6);
        String transactionDate = parts[1];
        String transactionTime = parts[2];
        String referenceNumber = parts[3];
        
        int rowsAffected = jdbcTemplate.update(
            "DELETE FROM bank_transaction WHERE sort_code = ? AND account_number = ? " +
            "AND transaction_date = ? AND transaction_time = ? AND reference_number = ?",
            sortCode,
            accountNumber,
            transactionDate,
            transactionTime,
            Long.parseLong(referenceNumber)
        );
        
        return rowsAffected > 0;
    }

    @Override
    public boolean markAsDeleted(String compositeId) {
        // Parse the composite ID to extract components
        String[] parts = compositeId.split("-");
        if (parts.length < 5) {
            return false;
        }
        
        String sortCode = parts[0].substring(0, 6);
        String accountNumber = parts[0].substring(6);
        String transactionDate = parts[1];
        String transactionTime = parts[2];
        String referenceNumber = parts[3];
        
        int rowsAffected = jdbcTemplate.update(
            "UPDATE bank_transaction SET logically_deleted = 1 " +
            "WHERE sort_code = ? AND account_number = ? " +
            "AND transaction_date = ? AND transaction_time = ? AND reference_number = ?",
            sortCode,
            accountNumber,
            transactionDate,
            transactionTime,
            Long.parseLong(referenceNumber)
        );
        
        return rowsAffected > 0;
    }

    @Override
    public List<Transaction> findAll() {
        return jdbcTemplate.query(
            "SELECT * FROM bank_transaction ORDER BY transaction_date DESC, transaction_time DESC", 
            rowMapper
        );
    }

    @Override
    public int count() {
        return jdbcTemplate.queryForObject("SELECT COUNT(*) FROM bank_transaction", Integer.class);
    }
}
