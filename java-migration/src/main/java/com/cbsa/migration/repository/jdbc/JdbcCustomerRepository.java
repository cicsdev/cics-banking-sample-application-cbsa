package com.cbsa.migration.repository.jdbc;

import com.cbsa.migration.model.Customer;
import com.cbsa.migration.repository.CustomerRepository;
import org.springframework.dao.EmptyResultDataAccessException;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.core.RowMapper;
import org.springframework.stereotype.Repository;

import java.sql.ResultSet;
import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

/**
 * JDBC implementation of CustomerRepository using Spring's JdbcTemplate
 */
@Repository
public class JdbcCustomerRepository implements CustomerRepository {

    private final JdbcTemplate jdbcTemplate;
    
    /**
     * Row mapper for Customer entities
     */
    private final RowMapper<Customer> rowMapper = (ResultSet rs, int rowNum) -> {
        Customer customer = new Customer();
        customer.setEyeCatcher(rs.getString("eye_catcher"));
        customer.setSortCode(rs.getString("sort_code"));
        customer.setCustomerNumber(rs.getLong("customer_number"));
        customer.setName(rs.getString("name"));
        customer.setAddress(rs.getString("address"));
        customer.setDateOfBirth(LocalDate.parse(rs.getString("date_of_birth")));
        customer.setCreditScore(rs.getInt("credit_score"));
        
        String reviewDate = rs.getString("credit_score_review_date");
        if (reviewDate != null && !reviewDate.isEmpty()) {
            customer.setCreditScoreReviewDate(LocalDate.parse(reviewDate));
        }
        
        return customer;
    };

    public JdbcCustomerRepository(JdbcTemplate jdbcTemplate) {
        this.jdbcTemplate = jdbcTemplate;
    }

    @Override
    public Optional<Customer> findById(String sortCode, Long customerNumber) {
        try {
            Customer customer = jdbcTemplate.queryForObject(
                "SELECT * FROM customer WHERE sort_code = ? AND customer_number = ?",
                rowMapper,
                sortCode,
                customerNumber
            );
            return Optional.ofNullable(customer);
        } catch (EmptyResultDataAccessException e) {
            return Optional.empty();
        }
    }

    @Override
    public List<Customer> findByNameContaining(String name) {
        return jdbcTemplate.query(
            "SELECT * FROM customer WHERE name LIKE ?",
            rowMapper,
            "%" + name + "%"
        );
    }

    @Override
    public Customer save(Customer customer) {
        // Check if customer exists
        Optional<Customer> existingCustomer = findById(customer.getSortCode(), customer.getCustomerNumber());
        
        if (existingCustomer.isPresent()) {
            // Update
            jdbcTemplate.update(
                "UPDATE customer SET " +
                "eye_catcher = ?, " +
                "name = ?, " +
                "address = ?, " +
                "date_of_birth = ?, " +
                "credit_score = ?, " +
                "credit_score_review_date = ? " +
                "WHERE sort_code = ? AND customer_number = ?",
                customer.getEyeCatcher(),
                customer.getName(),
                customer.getAddress(),
                customer.getDateOfBirth().toString(),
                customer.getCreditScore(),
                customer.getCreditScoreReviewDate() != null ? customer.getCreditScoreReviewDate().toString() : null,
                customer.getSortCode(),
                customer.getCustomerNumber()
            );
        } else {
            // Insert
            jdbcTemplate.update(
                "INSERT INTO customer (" +
                "eye_catcher, sort_code, customer_number, name, address, " +
                "date_of_birth, credit_score, credit_score_review_date) " +
                "VALUES (?, ?, ?, ?, ?, ?, ?, ?)",
                customer.getEyeCatcher(),
                customer.getSortCode(),
                customer.getCustomerNumber(),
                customer.getName(),
                customer.getAddress(),
                customer.getDateOfBirth().toString(),
                customer.getCreditScore(),
                customer.getCreditScoreReviewDate() != null ? customer.getCreditScoreReviewDate().toString() : null
            );
        }
        
        return customer;
    }

    @Override
    public boolean deleteById(String sortCode, Long customerNumber) {
        int rowsAffected = jdbcTemplate.update(
            "DELETE FROM customer WHERE sort_code = ? AND customer_number = ?",
            sortCode,
            customerNumber
        );
        
        return rowsAffected > 0;
    }

    @Override
    public List<Customer> findAll() {
        return jdbcTemplate.query("SELECT * FROM customer", rowMapper);
    }

    @Override
    public int count() {
        return jdbcTemplate.queryForObject("SELECT COUNT(*) FROM customer", Integer.class);
    }
}
