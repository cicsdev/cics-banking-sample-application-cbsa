package com.cbsa.migration.model;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.DisplayName;
import java.time.LocalDate;
import static org.assertj.core.api.Assertions.assertThat;

class CustomerTest {

    @Test
    @DisplayName("Should create Customer with builder")
    void shouldCreateCustomerWithBuilder() {
        // Given/When
        Customer customer = Customer.builder()
                .eyeCatcher("CUST")
                .sortCode("123456")
                .customerNumber(1234567890L)
                .name("John Smith")
                .address("123 Main Street, Anytown, AT1 2BC")
                .dateOfBirth(LocalDate.of(1985, 6, 15))
                .creditScore(750)
                .creditScoreReviewDate(LocalDate.of(2023, 12, 1))
                .build();

        // Then
        assertThat(customer.getEyeCatcher()).isEqualTo("CUST");
        assertThat(customer.getSortCode()).isEqualTo("123456");
        assertThat(customer.getCustomerNumber()).isEqualTo(1234567890L);
        assertThat(customer.getName()).isEqualTo("John Smith");
        assertThat(customer.getAddress()).isEqualTo("123 Main Street, Anytown, AT1 2BC");
        assertThat(customer.getDateOfBirth()).isEqualTo(LocalDate.of(1985, 6, 15));
        assertThat(customer.getCreditScore()).isEqualTo(750);
        assertThat(customer.getCreditScoreReviewDate()).isEqualTo(LocalDate.of(2023, 12, 1));
    }

    @Test
    @DisplayName("Should create composite ID from sort code and customer number")
    void shouldCreateCompositeId() {
        // Given
        Customer customer = Customer.builder()
                .sortCode("123456")
                .customerNumber(1234567890L)
                .build();

        // When
        String compositeId = customer.getCompositeId();

        // Then
        assertThat(compositeId).isEqualTo("123456-1234567890");
    }

    @Test
    @DisplayName("Should support equals and hashCode (Lombok @Data)")
    void shouldSupportEqualsAndHashCode() {
        // Given
        Customer customer1 = Customer.builder()
                .eyeCatcher("CUST")
                .sortCode("123456")
                .customerNumber(1234567890L)
                .name("John Smith")
                .address("123 Main Street, Anytown, AT1 2BC")
                .dateOfBirth(LocalDate.of(1985, 6, 15))
                .creditScore(750)
                .build();

        Customer customer2 = Customer.builder()
                .eyeCatcher("CUST")
                .sortCode("123456")
                .customerNumber(1234567890L)
                .name("John Smith")
                .address("123 Main Street, Anytown, AT1 2BC")
                .dateOfBirth(LocalDate.of(1985, 6, 15))
                .creditScore(750)
                .build();

        // Then
        assertThat(customer1).isEqualTo(customer2);
        assertThat(customer1.hashCode()).isEqualTo(customer2.hashCode());
    }

    @Test
    @DisplayName("Should support toString (Lombok @Data)")
    void shouldSupportToString() {
        // Given
        Customer customer = Customer.builder()
                .eyeCatcher("CUST")
                .sortCode("123456")
                .customerNumber(1234567890L)
                .name("John Smith")
                .build();

        // When
        String toString = customer.toString();

        // Then
        assertThat(toString).contains("Customer");
        assertThat(toString).contains("eyeCatcher=CUST");
        assertThat(toString).contains("sortCode=123456");
        assertThat(toString).contains("customerNumber=1234567890");
        assertThat(toString).contains("name=John Smith");
    }

    @Test
    @DisplayName("Should create Customer with no-args constructor")
    void shouldCreateCustomerWithNoArgsConstructor() {
        // Given/When
        Customer customer = new Customer();
        
        // Then
        assertThat(customer).isNotNull();
        assertThat(customer.getEyeCatcher()).isNull();
        assertThat(customer.getCustomerNumber()).isNull();
    }

    @Test
    @DisplayName("Should set and get all fields correctly")
    void shouldSetAndGetAllFieldsCorrectly() {
        // Given
        Customer customer = new Customer();
        LocalDate dateOfBirth = LocalDate.of(1985, 6, 15);
        LocalDate reviewDate = LocalDate.of(2023, 12, 1);

        // When
        customer.setEyeCatcher("CUST");
        customer.setSortCode("123456");
        customer.setCustomerNumber(1234567890L);
        customer.setName("John Smith");
        customer.setAddress("123 Main Street, Anytown, AT1 2BC");
        customer.setDateOfBirth(dateOfBirth);
        customer.setCreditScore(750);
        customer.setCreditScoreReviewDate(reviewDate);

        // Then
        assertThat(customer.getEyeCatcher()).isEqualTo("CUST");
        assertThat(customer.getSortCode()).isEqualTo("123456");
        assertThat(customer.getCustomerNumber()).isEqualTo(1234567890L);
        assertThat(customer.getName()).isEqualTo("John Smith");
        assertThat(customer.getAddress()).isEqualTo("123 Main Street, Anytown, AT1 2BC");
        assertThat(customer.getDateOfBirth()).isEqualTo(dateOfBirth);
        assertThat(customer.getCreditScore()).isEqualTo(750);
        assertThat(customer.getCreditScoreReviewDate()).isEqualTo(reviewDate);
    }

    @Test
    @DisplayName("Should handle null credit score review date")
    void shouldHandleNullCreditScoreReviewDate() {
        // Given
        Customer customer = Customer.builder()
                .eyeCatcher("CUST")
                .sortCode("123456")
                .customerNumber(1234567890L)
                .name("John Smith")
                .address("123 Main Street, Anytown, AT1 2BC")
                .dateOfBirth(LocalDate.of(1985, 6, 15))
                .creditScore(750)
                .creditScoreReviewDate(null)
                .build();

        // Then
        assertThat(customer.getCreditScoreReviewDate()).isNull();
    }
}
