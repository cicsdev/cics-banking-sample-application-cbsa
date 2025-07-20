package com.cbsa.migration.dto.mapper;

import com.cbsa.migration.dto.*;
import com.cbsa.migration.model.Account;
import com.cbsa.migration.model.Customer;
import com.cbsa.migration.model.Transaction;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.stream.Collectors;

/**
 * Utility class for mapping between domain models and DTOs.
 * Centralizes conversion logic and provides clean separation between layers.
 */
@Component
public class DtoMapper {
    
    // Customer mappings
    
    public CustomerResponseDto toCustomerResponseDto(Customer customer) {
        return CustomerResponseDto.builder()
                .name(customer.getName())
                .address(customer.getAddress())
                .dateOfBirth(customer.getDateOfBirth())
                .customerNumber(customer.getCustomerNumber())
                .sortCode(customer.getSortCode())
                .status(deriveCustomerStatus(customer))
                .build();
    }
    
    public CustomerResponseDto toCustomerResponseDto(Customer customer, List<Account> accounts) {
        CustomerResponseDto dto = toCustomerResponseDto(customer);
        dto.setAccounts(accounts.stream()
                .map(this::toAccountSummaryDto)
                .collect(Collectors.toList()));
        dto.setAccountCount(accounts.size());
        return dto;
    }
    
    public Customer toCustomer(CustomerRequestDto requestDto) {
        Customer customer = new Customer();
        customer.setName(requestDto.getName());
        customer.setAddress(requestDto.getAddress());
        customer.setDateOfBirth(requestDto.getDateOfBirth());
        customer.setSortCode(requestDto.getSortCode());
        if (requestDto.getCreditScore() != null) {
            customer.setCreditScore(requestDto.getCreditScore());
        }
        return customer;
    }
    
    // Account mappings
    
    public AccountResponseDto toAccountResponseDto(Account account) {
        return AccountResponseDto.builder()
                .accountNumber(account.getAccountNumber())
                .sortCode(account.getSortCode())
                .accountType(account.getAccountType())
                .customerNumber(account.getCustomerNumber())
                .availableBalance(account.getAvailableBalance())
                .actualBalance(account.getActualBalance())
                .interestRate(account.getInterestRate())
                .openedDate(account.getOpenedDate())
                .overdraftLimit(account.getOverdraftLimit())
                .lastStatementDate(account.getLastStatementDate())
                .nextStatementDate(account.getNextStatementDate())
                .status(deriveAccountStatus(account))
                .build();
    }
    
    public AccountResponseDto toAccountResponseDto(Account account, String customerName) {
        AccountResponseDto dto = toAccountResponseDto(account);
        dto.setCustomerName(customerName);
        return dto;
    }
    
    public AccountSummaryDto toAccountSummaryDto(Account account) {
        return AccountSummaryDto.builder()
                .accountNumber(account.getAccountNumber())
                .accountType(account.getAccountType())
                .availableBalance(account.getAvailableBalance())
                .actualBalance(account.getActualBalance())
                .status(deriveAccountStatus(account))
                .build();
    }
    
    public Account toAccount(AccountRequestDto requestDto) {
        Account account = new Account();
        account.setAccountType(requestDto.getAccountType());
        account.setCustomerNumber(requestDto.getCustomerNumber());
        account.setSortCode(requestDto.getSortCode());
        if (requestDto.getInterestRate() != null) {
            account.setInterestRate(requestDto.getInterestRate());
        }
        if (requestDto.getOverdraftLimit() != null) {
            account.setOverdraftLimit(requestDto.getOverdraftLimit());
        }
        return account;
    }
    
    // Transaction mappings
    
    public TransactionResponseDto toTransactionResponseDto(Transaction transaction) {
        return TransactionResponseDto.builder()
                .referenceNumber(transaction.getReferenceNumber())
                .sortCode(transaction.getSortCode())
                .accountNumber(transaction.getAccountNumber())
                .transactionDate(transaction.getTransactionDate())
                .transactionTime(transaction.getTransactionTime())
                .transactionType(transaction.getTransactionType())
                .description(transaction.getDescription())
                .amount(transaction.getAmount())
                .targetSortCode(transaction.getTargetSortCode())
                .targetAccountNumber(transaction.getTargetAccountNumber())
                .isDebit(transaction.isDebit())
                .status(deriveTransactionStatus(transaction))
                .build();
    }
    
    public Transaction toTransaction(TransactionRequestDto requestDto) {
        Transaction transaction = new Transaction();
        transaction.setSortCode(requestDto.getSortCode());
        transaction.setAccountNumber(requestDto.getAccountNumber());
        transaction.setTransactionType(requestDto.getTransactionType());
        transaction.setDescription(requestDto.getDescription());
        transaction.setAmount(requestDto.getAmount());
        transaction.setTargetSortCode(requestDto.getTargetSortCode());
        transaction.setTargetAccountNumber(requestDto.getTargetAccountNumber());
        return transaction;
    }
    
    // Helper methods for deriving status
    
    private String deriveCustomerStatus(Customer customer) {
        // Business logic to determine customer status
        if (customer.getCreditScore() != null && customer.getCreditScore() >= 700) {
            return "GOOD_STANDING";
        } else if (customer.getCreditScore() != null && customer.getCreditScore() >= 500) {
            return "FAIR";
        } else {
            return "REVIEW_REQUIRED";
        }
    }
    
    private String deriveAccountStatus(Account account) {
        // Business logic to determine account status
        if (account.getAvailableBalance().compareTo(account.getActualBalance()) < 0) {
            return "OVERDRAWN";
        } else if (account.getAvailableBalance().signum() < 0) {
            return "OVERDRAFT";
        } else {
            return "ACTIVE";
        }
    }
    
    private String deriveTransactionStatus(Transaction transaction) {
        // Business logic to determine transaction status
        return "COMPLETED"; // Simplified - in real system would check processing status
    }
    
    // Credit Score mappings
    
    public CreditScoreRequestDto toCreditScoreRequestDto(Customer customer) {
        return CreditScoreRequestDto.builder()
                .sortCode(customer.getSortCode())
                .customerNumber(customer.getCustomerNumber())
                .name(customer.getName())
                .address(customer.getAddress())
                .dateOfBirth(customer.getDateOfBirth())
                .currentCreditScore(customer.getCreditScore())
                .build();
    }
    
    public Customer updateCustomerFromCreditResponse(Customer customer, CreditScoreResponseDto response) {
        if (response.getSuccess() && response.getUpdatedCreditScore() != null) {
            customer.setCreditScore(response.getUpdatedCreditScore());
            customer.setCreditScoreReviewDate(response.getScoreReviewDate());
        }
        return customer;
    }
}
