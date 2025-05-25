package com.cbsa.migration.repository.jdbc;

import com.cbsa.migration.model.Control;
import com.cbsa.migration.repository.ControlRepository;
import org.springframework.dao.EmptyResultDataAccessException;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.core.RowMapper;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import java.sql.ResultSet;
import java.util.Optional;

/**
 * JDBC implementation of ControlRepository using Spring's JdbcTemplate
 */
@Repository
public class JdbcControlRepository implements ControlRepository {

    private final JdbcTemplate jdbcTemplate;
    
    /**
     * Row mapper for Control entities
     */
    private final RowMapper<Control> rowMapper = (ResultSet rs, int rowNum) -> {
        Control control = new Control();
        control.setCustomerCount(rs.getLong("customer_count"));
        control.setLastCustomerNumber(rs.getLong("last_customer_number"));
        control.setAccountCount(rs.getInt("account_count"));
        control.setLastAccountNumber(rs.getInt("last_account_number"));
        return control;
    };

    public JdbcControlRepository(JdbcTemplate jdbcTemplate) {
        this.jdbcTemplate = jdbcTemplate;
    }

    @Override
    public Optional<Control> getControl() {
        try {
            Control control = jdbcTemplate.queryForObject(
                "SELECT * FROM control WHERE id = ?",
                rowMapper,
                Control.CONTROL_ID
            );
            return Optional.ofNullable(control);
        } catch (EmptyResultDataAccessException e) {
            return Optional.empty();
        }
    }

    @Override
    public Control save(Control control) {
        // Check if control record exists
        if (recordExists()) {
            // Update
            jdbcTemplate.update(
                "UPDATE control SET " +
                "customer_count = ?, " +
                "last_customer_number = ?, " +
                "account_count = ?, " +
                "last_account_number = ? " +
                "WHERE id = ?",
                control.getCustomerCount(),
                control.getLastCustomerNumber(),
                control.getAccountCount(),
                control.getLastAccountNumber(),
                Control.CONTROL_ID
            );
        } else {
            // Insert
            jdbcTemplate.update(
                "INSERT INTO control (" +
                "id, customer_count, last_customer_number, account_count, last_account_number) " +
                "VALUES (?, ?, ?, ?, ?)",
                Control.CONTROL_ID,
                control.getCustomerCount(),
                control.getLastCustomerNumber(),
                control.getAccountCount(),
                control.getLastAccountNumber()
            );
        }
        
        return control;
    }

    @Override
    @Transactional
    public Long getNextCustomerNumber() {
        // Ensure control record exists
        if (!recordExists()) {
            initializeControlRecord();
        }
        
        // Get current value
        Long currentValue = jdbcTemplate.queryForObject(
            "SELECT last_customer_number FROM control WHERE id = ?",
            Long.class,
            Control.CONTROL_ID
        );
        
        // Increment value
        Long nextValue = currentValue + 1;
        
        // Update in database
        jdbcTemplate.update(
            "UPDATE control SET last_customer_number = ?, customer_count = customer_count + 1 WHERE id = ?",
            nextValue,
            Control.CONTROL_ID
        );
        
        return nextValue;
    }

    @Override
    @Transactional
    public Integer getNextAccountNumber() {
        // Ensure control record exists
        if (!recordExists()) {
            initializeControlRecord();
        }
        
        // Get current value
        Integer currentValue = jdbcTemplate.queryForObject(
            "SELECT last_account_number FROM control WHERE id = ?",
            Integer.class,
            Control.CONTROL_ID
        );
        
        // Increment value
        Integer nextValue = currentValue + 1;
        
        // Update in database
        jdbcTemplate.update(
            "UPDATE control SET last_account_number = ?, account_count = account_count + 1 WHERE id = ?",
            nextValue,
            Control.CONTROL_ID
        );
        
        return nextValue;
    }

    @Override
    @Transactional
    public Control initializeControlRecord() {
        if (!recordExists()) {
            Control control = new Control();
            control.setCustomerCount(0L);
            control.setLastCustomerNumber(100000L);
            control.setAccountCount(0);
            control.setLastAccountNumber(10000000);
            save(control);
            return control;
        } else {
            return getControl().get();
        }
    }
    
    /**
     * Check if control record exists
     * 
     * @return true if exists, false otherwise
     */
    private boolean recordExists() {
        Integer count = jdbcTemplate.queryForObject(
            "SELECT COUNT(*) FROM control WHERE id = ?", 
            Integer.class, 
            Control.CONTROL_ID
        );
        return count != null && count > 0;
    }
}
