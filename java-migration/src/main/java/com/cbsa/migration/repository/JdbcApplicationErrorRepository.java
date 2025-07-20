package com.cbsa.migration.repository;

import com.cbsa.migration.model.ApplicationError;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.EmptyResultDataAccessException;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.core.RowMapper;
import org.springframework.jdbc.support.GeneratedKeyHolder;
import org.springframework.jdbc.support.KeyHolder;
import org.springframework.stereotype.Repository;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.List;
import java.util.Optional;

/**
 * JDBC implementation of ApplicationErrorRepository.
 * Migrated from COBOL ABNDPROC program's CICS FILE operations.
 */
@Repository
public class JdbcApplicationErrorRepository implements ApplicationErrorRepository {
    
    private final JdbcTemplate jdbcTemplate;
    
    @Autowired
    public JdbcApplicationErrorRepository(JdbcTemplate jdbcTemplate) {
        this.jdbcTemplate = jdbcTemplate;
    }
    
    @Override
    public Long save(ApplicationError applicationError) {
        String sql = "INSERT INTO application_error (" +
                "timestamp, application_id, transaction_id, error_code, " +
                "program_name, error_message, stack_trace, response_code, " +
                "response2_code, sql_code, freeform_text, created_at" +
                ") VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, CURRENT_TIMESTAMP)";
        
        KeyHolder keyHolder = new GeneratedKeyHolder();
        
        jdbcTemplate.update(connection -> {
            PreparedStatement ps = connection.prepareStatement(sql, Statement.RETURN_GENERATED_KEYS);
            ps.setString(1, applicationError.getTimestamp());
            ps.setString(2, applicationError.getApplicationId());
            ps.setString(3, applicationError.getTransactionId());
            ps.setString(4, applicationError.getErrorCode());
            ps.setString(5, applicationError.getProgramName());
            ps.setString(6, applicationError.getErrorMessage());
            ps.setString(7, applicationError.getStackTrace());
            ps.setString(8, applicationError.getResponseCode());
            ps.setString(9, applicationError.getResponse2Code());
            ps.setString(10, applicationError.getSqlCode());
            ps.setString(11, applicationError.getFreeformText());
            return ps;
        }, keyHolder);
        
        return ((Number) keyHolder.getKeys().get("ID")).longValue();
    }
    
    @Override
    public Optional<ApplicationError> findById(Long id) {
        String sql = "SELECT id, timestamp, application_id, transaction_id, error_code, " +
                "program_name, error_message, stack_trace, response_code, " +
                "response2_code, sql_code, freeform_text, created_at " +
                "FROM application_error WHERE id = ?";
        
        try {
            ApplicationError error = jdbcTemplate.queryForObject(sql, applicationErrorRowMapper(), id);
            return Optional.ofNullable(error);
        } catch (EmptyResultDataAccessException e) {
            return Optional.empty();
        }
    }
    
    @Override
    public List<ApplicationError> findByProgramName(String programName) {
        String sql = "SELECT id, timestamp, application_id, transaction_id, error_code, " +
                "program_name, error_message, stack_trace, response_code, " +
                "response2_code, sql_code, freeform_text, created_at " +
                "FROM application_error WHERE program_name = ? " +
                "ORDER BY created_at DESC";
        
        return jdbcTemplate.query(sql, applicationErrorRowMapper(), programName);
    }
    
    @Override
    public List<ApplicationError> findByApplicationAndTransaction(String applicationId, String transactionId) {
        String sql = "SELECT id, timestamp, application_id, transaction_id, error_code, " +
                "program_name, error_message, stack_trace, response_code, " +
                "response2_code, sql_code, freeform_text, created_at " +
                "FROM application_error " +
                "WHERE application_id = ? AND transaction_id = ? " +
                "ORDER BY created_at DESC";
        
        return jdbcTemplate.query(sql, applicationErrorRowMapper(), applicationId, transactionId);
    }
    
    @Override
    public long count() {
        String sql = "SELECT COUNT(*) FROM application_error";
        return jdbcTemplate.queryForObject(sql, Long.class);
    }
    
    @Override
    public long countByProgramName(String programName) {
        String sql = "SELECT COUNT(*) FROM application_error WHERE program_name = ?";
        return jdbcTemplate.queryForObject(sql, Long.class, programName);
    }
    
    @Override
    public List<ApplicationError> findRecentErrors(int limit) {
        String sql = "SELECT id, timestamp, application_id, transaction_id, error_code, " +
                "program_name, error_message, stack_trace, response_code, " +
                "response2_code, sql_code, freeform_text, created_at " +
                "FROM application_error " +
                "ORDER BY created_at DESC " +
                "LIMIT ?";
        
        return jdbcTemplate.query(sql, applicationErrorRowMapper(), limit);
    }
    
    private RowMapper<ApplicationError> applicationErrorRowMapper() {
        return (ResultSet rs, int rowNum) -> {
            ApplicationError error = new ApplicationError();
            error.setId(rs.getLong("id"));
            error.setTimestamp(rs.getString("timestamp"));
            error.setApplicationId(rs.getString("application_id"));
            error.setTransactionId(rs.getString("transaction_id"));
            error.setErrorCode(rs.getString("error_code"));
            error.setProgramName(rs.getString("program_name"));
            error.setErrorMessage(rs.getString("error_message"));
            error.setStackTrace(rs.getString("stack_trace"));
            error.setResponseCode(rs.getString("response_code"));
            error.setResponse2Code(rs.getString("response2_code"));
            error.setSqlCode(rs.getString("sql_code"));
            error.setFreeformText(rs.getString("freeform_text"));
            error.setCreatedAt(rs.getString("created_at"));
            return error;
        };
    }
}
