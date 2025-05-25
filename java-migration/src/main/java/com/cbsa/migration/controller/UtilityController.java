package com.cbsa.migration.controller;

import com.cbsa.migration.model.CompanyInfoResponse;
import com.cbsa.migration.model.SortCodeResponse;
import com.cbsa.migration.service.CompanyInfoService;
import com.cbsa.migration.service.SortCodeService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * REST Controller for utility functions migrated from COBOL.
 * This controller provides endpoints for functionality previously
 * implemented in GETSCODE.cbl and GETCOMPY.cbl COBOL programs.
 */
@RestController
@RequestMapping("/api/utility")
public class UtilityController {

    private final SortCodeService sortCodeService;
    private final CompanyInfoService companyInfoService;

    @Autowired
    public UtilityController(SortCodeService sortCodeService, CompanyInfoService companyInfoService) {
        this.sortCodeService = sortCodeService;
        this.companyInfoService = companyInfoService;
    }

    /**
     * Endpoint to get the bank sort code.
     * Corresponds to GETSCODE.cbl COBOL program.
     *
     * @return ResponseEntity with the sort code
     */
    @GetMapping("/sortcode")
    public ResponseEntity<SortCodeResponse> getSortCode() {
        String sortCode = sortCodeService.getSortCode();
        SortCodeResponse response = new SortCodeResponse(sortCode);
        return ResponseEntity.ok(response);
    }

    /**
     * Endpoint to get the company name.
     * Corresponds to GETCOMPY.cbl COBOL program.
     *
     * @return ResponseEntity with the company name
     */
    @GetMapping("/company-name")
    public ResponseEntity<CompanyInfoResponse> getCompanyName() {
        String companyName = companyInfoService.getCompanyName();
        CompanyInfoResponse response = new CompanyInfoResponse(companyName);
        return ResponseEntity.ok(response);
    }
}
