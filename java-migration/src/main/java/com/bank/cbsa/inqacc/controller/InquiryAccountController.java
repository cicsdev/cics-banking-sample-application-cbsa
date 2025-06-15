package com.bank.cbsa.inqacc.controller;

import com.bank.cbsa.inqacc.dto.AccountDto;
import com.bank.cbsa.inqacc.service.InquiryAccountService;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.*;

/**
 * REST controller exposing INQACC inquiry endpoint.
 */
@RestController
@RequestMapping("/api/inqacc")
public class InquiryAccountController {

    private final InquiryAccountService service;

    public InquiryAccountController(InquiryAccountService service) {
        this.service = service;
    }

    @GetMapping
    @ResponseStatus(HttpStatus.NOT_IMPLEMENTED)
    public AccountDto getAccount(@RequestParam String sortCode,
                                 @RequestParam String accountNumber) {
        // TODO: integrate with service when implemented
        return null;
    }
}
