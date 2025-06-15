package com.bank.cbsa.inqacc.service.impl;

import com.bank.cbsa.inqacc.dto.AccountDto;
import com.bank.cbsa.inqacc.mapper.InquiryAccountMapper;
import com.bank.cbsa.inqacc.model.AccountEntity;
import com.bank.cbsa.inqacc.repository.AccountRepository;
import com.bank.cbsa.inqacc.service.InquiryAccountService;
import org.springframework.stereotype.Service;

/**
 * Service implementation translating INQACC COBOL business logic.
 */
@Service
public class InquiryAccountServiceImpl implements InquiryAccountService {

    private final AccountRepository accountRepository;
    private final InquiryAccountMapper mapper;

    public InquiryAccountServiceImpl(AccountRepository accountRepository, InquiryAccountMapper mapper) {
        this.accountRepository = accountRepository;
        this.mapper = mapper;
    }

    @Override
    public AccountDto inquireAccount(String sortCode, String accountNumber) {
        // TODO: Implement business logic once repository & mapper are implemented.
        return null;
    }
}
