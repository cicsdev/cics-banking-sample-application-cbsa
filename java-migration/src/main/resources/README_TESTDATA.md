# Test Data Generator

This document explains how to use the test data generator for the CBSA Java Migration project.

## Overview

The test data generator creates realistic banking data for testing the application, including:

- Customers with realistic personal details
- Accounts of various types (Current, Savings, Checking, Loan)
- Transactions with appropriate amounts and descriptions

## REST Endpoints

The following REST endpoints are available for managing test data:

### Reset Database

```
POST /api/data/reset
```

Deletes all data from the database and reinitializes the control record.

**Example Response:**
```json
{
  "status": "success",
  "message": "Database reset complete"
}
```

### Generate Sample Data

```
POST /api/data/generate
```

**Parameters:**

| Parameter | Description | Default |
|-----------|-------------|--------|
| customerCount | Number of customers to generate | 10 |
| minAccountsPerCustomer | Minimum accounts per customer | 1 |
| maxAccountsPerCustomer | Maximum accounts per customer | 3 |
| minTransactionsPerAccount | Minimum transactions per account | 5 |
| maxTransactionsPerAccount | Maximum transactions per account | 20 |

**Example Request:**
```
POST /api/data/generate?customerCount=5&minAccountsPerCustomer=1&maxAccountsPerCustomer=2&minTransactionsPerAccount=3&maxTransactionsPerAccount=10
```

**Example Response:**
```json
{
  "status": "success",
  "message": "Sample data generation complete"
}
```

### Reset and Generate in One Call

```
POST /api/data/reset-and-generate
```

Combines both operations: resets the database and generates new sample data. Accepts the same parameters as the generate endpoint.

## Generated Data Characteristics

### Customers
- Randomly generated names, addresses, and credit scores
- Date of birth set to create customers between 18-80 years old
- Some customers have credit score review dates in the future

### Accounts
- Account types: CURRENT, SAVINGS, CHECKING, LOAN
- Interest rates appropriate to account type (higher for savings, lower for checking)
- Overdraft limits only for current/checking accounts
- Loan accounts have negative balances
- Opening dates within the past 5 years

### Transactions
- Transaction types: DEPOSIT, WITHDRAWAL, TRANSFER, INTEREST, FEE
- Appropriate descriptions for each transaction type
- Amounts vary by transaction type and account type
- For transfers, target accounts are randomly selected from existing accounts

## Example Usage

The easiest way to populate the database with test data is to use the reset-and-generate endpoint:

```bash
curl -X POST "http://localhost:8080/api/data/reset-and-generate?customerCount=20&minAccountsPerCustomer=1&maxAccountsPerCustomer=3&minTransactionsPerAccount=5&maxTransactionsPerAccount=20"
```

This will create a dataset with:
- 20 customers
- 1-3 accounts per customer (average 40 accounts total)
- 5-20 transactions per account (approximately 500 transactions total)
