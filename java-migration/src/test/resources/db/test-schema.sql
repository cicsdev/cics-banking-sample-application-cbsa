-- H2-compatible test schema for the banking application
-- Based on production schema but adapted for H2 in-memory database
-- Maintains same table structure as production SQLite schema

-- Control table - stores system-wide counters and settings
CREATE TABLE IF NOT EXISTS control (
    id VARCHAR(255) PRIMARY KEY CHECK(id = 'CONTROL'),
    customer_count INTEGER NOT NULL DEFAULT 0,
    last_customer_number INTEGER NOT NULL DEFAULT 0,
    account_count INTEGER NOT NULL DEFAULT 0,
    last_account_number INTEGER NOT NULL DEFAULT 0
);

-- Customer table - migrated from CUSTOMER.cpy VSAM file
CREATE TABLE IF NOT EXISTS customer (
    eye_catcher VARCHAR(255) NOT NULL CHECK(eye_catcher = 'CUST'),
    sort_code VARCHAR(255) NOT NULL,
    customer_number INTEGER NOT NULL,
    name VARCHAR(255) NOT NULL,
    address VARCHAR(255) NOT NULL,
    date_of_birth VARCHAR(255) NOT NULL, -- Stored as TEXT in ISO format
    credit_score INTEGER NOT NULL,
    credit_score_review_date VARCHAR(255), -- Nullable, stored as TEXT in ISO format
    PRIMARY KEY (sort_code, customer_number)
);

-- Account table - migrated from ACCOUNT.cpy DB2 table
CREATE TABLE IF NOT EXISTS account (
    eye_catcher VARCHAR(255) NOT NULL CHECK(eye_catcher = 'ACCT'),
    customer_number INTEGER NOT NULL,
    sort_code VARCHAR(255) NOT NULL,
    account_number VARCHAR(255) NOT NULL,
    account_type VARCHAR(255) NOT NULL,
    interest_rate REAL NOT NULL,
    opened_date VARCHAR(255) NOT NULL, -- Stored as TEXT in ISO format
    overdraft_limit INTEGER NOT NULL,
    last_statement_date VARCHAR(255), -- Nullable, stored as TEXT in ISO format
    next_statement_date VARCHAR(255), -- Nullable, stored as TEXT in ISO format
    available_balance REAL NOT NULL,
    actual_balance REAL NOT NULL,
    PRIMARY KEY (sort_code, account_number),
    FOREIGN KEY (sort_code, customer_number) REFERENCES customer(sort_code, customer_number)
);

-- Transaction table - migrated from PROCTRAN.cpy DB2 table
CREATE TABLE IF NOT EXISTS bank_transaction (
    eye_catcher VARCHAR(255) NOT NULL,
    logically_deleted INTEGER NOT NULL DEFAULT 0, -- Boolean (0/1) for logical delete flag
    sort_code VARCHAR(255) NOT NULL,
    account_number VARCHAR(255) NOT NULL,
    transaction_date VARCHAR(255) NOT NULL, -- Stored as TEXT in ISO format
    transaction_time VARCHAR(255) NOT NULL, -- Stored as TEXT in ISO format
    reference_number INTEGER NOT NULL,
    transaction_type VARCHAR(255) NOT NULL,
    description VARCHAR(255),
    target_sort_code VARCHAR(255), -- Only for transfer transactions
    target_account_number VARCHAR(255), -- Only for transfer transactions
    amount REAL NOT NULL,
    PRIMARY KEY (sort_code, account_number, transaction_date, transaction_time, reference_number),
    FOREIGN KEY (sort_code, account_number) REFERENCES account(sort_code, account_number)
);

-- H2-compatible indexes (no WHERE clauses)
-- Basic indexes
CREATE INDEX IF NOT EXISTS idx_customer_name ON customer(name);
CREATE INDEX IF NOT EXISTS idx_account_customer ON account(customer_number);
CREATE INDEX IF NOT EXISTS idx_transaction_type ON bank_transaction(transaction_type);
CREATE INDEX IF NOT EXISTS idx_transaction_date ON bank_transaction(transaction_date);

-- Additional optimized indexes for better performance
-- Compound index for faster customer searches by name and sort_code
CREATE INDEX IF NOT EXISTS idx_customer_name_sortcode ON customer(name, sort_code);

-- Compound index for accounts by customer number and account type
CREATE INDEX IF NOT EXISTS idx_account_customer_type ON account(customer_number, account_type);

-- Index for account balance queries
CREATE INDEX IF NOT EXISTS idx_account_balance ON account(actual_balance);

-- Compound index for faster transaction lookups by date and amount
CREATE INDEX IF NOT EXISTS idx_transaction_date_amount ON bank_transaction(transaction_date, amount);

-- Index to speed up logically_deleted flag searches
CREATE INDEX IF NOT EXISTS idx_transaction_deleted ON bank_transaction(logically_deleted);

-- Index for transfer transactions (H2 doesn't support WHERE in CREATE INDEX)
CREATE INDEX IF NOT EXISTS idx_transaction_transfer ON bank_transaction(target_sort_code, target_account_number);

-- Application error table - migrated from COBOL ABNDPROC program
-- Stores application errors and exceptions for centralized monitoring
CREATE TABLE IF NOT EXISTS application_error (
    id BIGINT AUTO_INCREMENT PRIMARY KEY,
    timestamp VARCHAR(255) NOT NULL,
    application_id VARCHAR(255),
    transaction_id VARCHAR(255),
    error_code VARCHAR(255),
    program_name VARCHAR(255) NOT NULL,
    error_message TEXT,
    stack_trace TEXT,
    response_code VARCHAR(255),      -- COBOL RESP code
    response2_code VARCHAR(255),     -- COBOL RESP2 code
    sql_code VARCHAR(255),           -- COBOL SQLCODE
    freeform_text TEXT,              -- COBOL freeform area (up to 600 chars)
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Indexes for application_error table
CREATE INDEX IF NOT EXISTS idx_error_program ON application_error(program_name);
CREATE INDEX IF NOT EXISTS idx_error_created ON application_error(created_at);
CREATE INDEX IF NOT EXISTS idx_error_app_trans ON application_error(application_id, transaction_id);
CREATE INDEX IF NOT EXISTS idx_error_code ON application_error(error_code);
