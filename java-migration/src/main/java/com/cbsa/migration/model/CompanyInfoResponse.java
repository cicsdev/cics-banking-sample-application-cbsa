package com.cbsa.migration.model;

/**
 * Response model for the company information.
 * This corresponds to the data structure in GETCOMPY.cpy COBOL copybook.
 */
public class CompanyInfoResponse {
    private String companyName;

    public CompanyInfoResponse() {
    }

    public CompanyInfoResponse(String companyName) {
        this.companyName = companyName;
    }

    public String getCompanyName() {
        return companyName;
    }

    public void setCompanyName(String companyName) {
        this.companyName = companyName;
    }
}
