package com.cbsa.migration.repository;

import com.cbsa.migration.model.Customer;
import com.cbsa.migration.repository.jdbc.JdbcCustomerRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.context.annotation.Import;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Integration test for CustomerRepository
 */
@SpringBootTest
@ActiveProfiles("test")
@Import(RepositoryTestConfig.class)
@Transactional
public class CustomerRepositoryIntegrationTest {

    @Autowired
    private JdbcTemplate jdbcTemplate;

    @Autowired
    private JdbcCustomerRepository customerRepository;

    private Customer testCustomer;

    @BeforeEach
    void setUp() {
        // Clear the customer table before each test
        jdbcTemplate.update("DELETE FROM customer");

        // Create a test customer
        testCustomer = Customer.builder()
                .eyeCatcher("CUST")
                .sortCode("123456")
                .customerNumber(1000L)
                .name("John Doe")
                .address("123 Main St, London, UK")
                .dateOfBirth(LocalDate.of(1980, 1, 1))
                .creditScore(750)
                .creditScoreReviewDate(LocalDate.now())
                .build();

        // Save the test customer
        customerRepository.save(testCustomer);
    }

    @Test
    void testFindById() {
        // Test finding a customer by ID
        Optional<Customer> found = customerRepository.findById("123456", 1000L);
        
        assertTrue(found.isPresent());
        assertEquals("John Doe", found.get().getName());
        assertEquals("123 Main St, London, UK", found.get().getAddress());
    }

    @Test
    void testFindByIdNotFound() {
        // Test finding a non-existent customer
        Optional<Customer> notFound = customerRepository.findById("123456", 9999L);
        
        assertFalse(notFound.isPresent());
    }

    @Test
    void testFindByNameContaining() {
        // Add another customer
        Customer anotherCustomer = Customer.builder()
                .eyeCatcher("CUST")
                .sortCode("123456")
                .customerNumber(1001L)
                .name("Jane Doe")
                .address("456 Oak St, London, UK")
                .dateOfBirth(LocalDate.of(1985, 5, 5))
                .creditScore(800)
                .build();
        customerRepository.save(anotherCustomer);

        // Test finding customers by name
        List<Customer> customers = customerRepository.findByNameContaining("Doe");
        
        assertEquals(2, customers.size());
        assertTrue(customers.stream().anyMatch(c -> c.getName().equals("John Doe")));
        assertTrue(customers.stream().anyMatch(c -> c.getName().equals("Jane Doe")));
    }

    @Test
    void testSave() {
        // Test updating an existing customer
        testCustomer.setName("John Smith");
        Customer updated = customerRepository.save(testCustomer);
        
        assertEquals("John Smith", updated.getName());
        
        // Verify the update was persisted
        Optional<Customer> found = customerRepository.findById("123456", 1000L);
        assertTrue(found.isPresent());
        assertEquals("John Smith", found.get().getName());
    }

    @Test
    void testDeleteById() {
        // Test deleting a customer
        boolean deleted = customerRepository.deleteById("123456", 1000L);
        
        assertTrue(deleted);
        
        // Verify the customer was deleted
        Optional<Customer> found = customerRepository.findById("123456", 1000L);
        assertFalse(found.isPresent());
    }

    @Test
    void testCount() {
        // Test counting customers
        int count = customerRepository.count();
        
        assertEquals(1, count);
        
        // Add another customer
        Customer anotherCustomer = Customer.builder()
                .eyeCatcher("CUST")
                .sortCode("123456")
                .customerNumber(1001L)
                .name("Jane Doe")
                .address("456 Oak St, London, UK")
                .dateOfBirth(LocalDate.of(1985, 5, 5))
                .creditScore(800)
                .build();
        customerRepository.save(anotherCustomer);
        
        // Verify the count was updated
        count = customerRepository.count();
        assertEquals(2, count);
    }

    @Test
    void testFindAll() {
        // Add another customer
        Customer anotherCustomer = Customer.builder()
                .eyeCatcher("CUST")
                .sortCode("123456")
                .customerNumber(1001L)
                .name("Jane Doe")
                .address("456 Oak St, London, UK")
                .dateOfBirth(LocalDate.of(1985, 5, 5))
                .creditScore(800)
                .build();
        customerRepository.save(anotherCustomer);

        // Test finding all customers
        List<Customer> customers = customerRepository.findAll();
        
        assertEquals(2, customers.size());
        assertTrue(customers.stream().anyMatch(c -> c.getCustomerNumber().equals(1000L)));
        assertTrue(customers.stream().anyMatch(c -> c.getCustomerNumber().equals(1001L)));
    }
}
