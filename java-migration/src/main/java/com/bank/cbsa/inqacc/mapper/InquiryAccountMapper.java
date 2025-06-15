package com.bank.cbsa.inqacc.mapper;

import com.bank.cbsa.inqacc.dto.AccountDto;
import com.bank.cbsa.inqacc.model.AccountEntity;

/**
 * Mapper between AccountEntity and AccountDto.
 * TODO: Implement with MapStruct once dependency is added.
 */
public interface InquiryAccountMapper {
    AccountDto toDto(AccountEntity entity);
    AccountEntity toEntity(AccountDto dto);
}
