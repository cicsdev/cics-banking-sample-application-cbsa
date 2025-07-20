package com.cbsa.migration.repository.jdbc;

import com.cbsa.migration.model.Account;
import com.cbsa.migration.repository.AccountRepository;
import org.springframework.dao.EmptyResultDataAccessException;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.core.RowMapper;
import org.springframework.stereotype.Repository;

import java.math.BigDecimal;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

/**
 * JDBC implementation of AccountRepository using Spring's JdbcTemplate
 */
@Repository
public class JdbcAccountRepository implements AccountRepository {

    private final JdbcTemplate jdbcTemplate;
    
    /**
     * Row mapper for Account entities
     */
    private final RowMapper<Account> rowMapper = (ResultSet rs, int rowNum) -> {
        Account account = new Account();
        account.setEyeCatcher(rs.getString("eye_catcher"));
        account.setCustomerNumber(rs.getLong("customer_number"));
        account.setSortCode(rs.getString("sort_code"));
        account.setAccountNumber(rs.getString("account_number"));
        account.setAccountType(rs.getString("account_type"));
        account.setInterestRate(new BigDecimal(rs.getString("interest_rate")));
        account.setOpenedDate(LocalDate.parse(rs.getString("opened_date")));
        account.setOverdraftLimit(rs.getInt("overdraft_limit"));
        
        String lastStmtDate = rs.getString("last_statement_date");
        if (lastStmtDate != null && !lastStmtDate.isEmpty()) {
            account.setLastStatementDate(LocalDate.parse(lastStmtDate));
        }
        
        String nextStmtDate = rs.getString("next_statement_date");
        if (nextStmtDate != null && !nextStmtDate.isEmpty()) {
            account.setNextStatementDate(LocalDate.parse(nextStmtDate));
        }
        
        account.setAvailableBalance(new BigDecimal(rs.getString("available_balance")));
        account.setActualBalance(new BigDecimal(rs.getString("actual_balance")));
        
        return account;
    };

    public JdbcAccountRepository(JdbcTemplate jdbcTemplate) {
        this.jdbcTemplate = jdbcTemplate;
    }

    @Override
    public Optional<Account> findById(String sortCode, String accountNumber) {
        try {
            Account account = jdbcTemplate.queryForObject(
                "SELECT * FROM account WHERE sort_code = ? AND account_number = ?",
                rowMapper,
                sortCode,
                accountNumber
            );
            return Optional.ofNullable(account);
        } catch (EmptyResultDataAccessException e) {
            return Optional.empty();
        }
    }

    @Override
    public List<Account> findByCustomerNumber(Long customerNumber) {
        return jdbcTemplate.query(
            "SELECT * FROM account WHERE customer_number = ?",
            rowMapper,
            customerNumber
        );
    }

    @Override
    public Account save(Account account) {
        // Check if account exists
        Optional<Account> existingAccount = findById(account.getSortCode(), account.getAccountNumber());
        
        if (existingAccount.isPresent()) {
            // Update
            jdbcTemplate.update(
                "UPDATE account SET " +
                "eye_catcher = ?, " +
                "customer_number = ?, " +
                "account_type = ?, " +
                "interest_rate = ?, " +
                "opened_date = ?, " +
                "overdraft_limit = ?, " +
                "last_statement_date = ?, " +
                "next_statement_date = ?, " +
                "available_balance = ?, " +
                "actual_balance = ? " +
                "WHERE sort_code = ? AND account_number = ?",
                account.getEyeCatcher(),
                account.getCustomerNumber(),
                account.getAccountType(),
                account.getInterestRate().toString(),
                account.getOpenedDate().toString(),
                account.getOverdraftLimit(),
                account.getLastStatementDate() != null ? account.getLastStatementDate().toString() : null,
                account.getNextStatementDate() != null ? account.getNextStatementDate().toString() : null,
                account.getAvailableBalance().toString(),
                account.getActualBalance().toString(),
                account.getSortCode(),
                account.getAccountNumber()
            );
        } else {
            // Insert
            jdbcTemplate.update(
                "INSERT INTO account (" +
                "eye_catcher, customer_number, sort_code, account_number, account_type, " +
                "interest_rate, opened_date, overdraft_limit, last_statement_date, " +
                "next_statement_date, available_balance, actual_balance) " +
                "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
                account.getEyeCatcher(),
                account.getCustomerNumber(),
                account.getSortCode(),
                account.getAccountNumber(),
                account.getAccountType(),
                account.getInterestRate().toString(),
                account.getOpenedDate().toString(),
                account.getOverdraftLimit(),
                account.getLastStatementDate() != null ? account.getLastStatementDate().toString() : null,
                account.getNextStatementDate() != null ? account.getNextStatementDate().toString() : null,
                account.getAvailableBalance().toString(),
                account.getActualBalance().toString()
            );
        }
        
        return account;
    }

    @Override
    public boolean deleteById(String sortCode, String accountNumber) {
        int rowsAffected = jdbcTemplate.update(
            "DELETE FROM account WHERE sort_code = ? AND account_number = ?",
            sortCode,
            accountNumber
        );
        
        return rowsAffected > 0;
    }

    @Override
    public List<Account> findAll() {
        return jdbcTemplate.query("SELECT * FROM account", rowMapper);
    }

    @Override
    public int count() {
        return jdbcTemplate.queryForObject("SELECT COUNT(*) FROM account", Integer.class);
    }

    @Override
    public List<Account> findBySortCode(String sortCode) {
        return jdbcTemplate.query(
                "SELECT * FROM account WHERE sort_code = ?",
                rowMapper,
                sortCode
        );
    }

    @Override
    public Optional<Account> findTopBySortCodeOrderByAccountNumberDesc(String sortCode) {
        try {
            Account account = jdbcTemplate.queryForObject(
                    "SELECT * FROM account WHERE sort_code = ? ORDER BY account_number DESC LIMIT 1",
                    rowMapper,
                    sortCode
            );
            return Optional.ofNullable(account);
        } catch (EmptyResultDataAccessException e) {
            return Optional.empty();
        }
    }
}
