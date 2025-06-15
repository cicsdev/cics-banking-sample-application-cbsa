package com.bank.cbsa.inqacc.service;

import com.bank.cbsa.inqacc.dto.AccountDto;

/**
 * Service interface derived from INQACC COBOL paragraphs.
 */
public interface InquiryAccountService {

    /**
     * Retrieves account details by composite key.
     *
     * @param sortCode bank sort code
     * @param accountNumber account number
     * @return AccountDto filled with account data
     */
    AccountDto inquireAccount(String sortCode, String accountNumber);
}
