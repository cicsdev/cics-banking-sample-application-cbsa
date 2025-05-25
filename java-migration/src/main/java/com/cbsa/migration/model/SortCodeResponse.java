package com.cbsa.migration.model;

/**
 * Response model for the sort code information.
 * This corresponds to the data structure in GETSCODE.cpy COBOL copybook.
 */
public class SortCodeResponse {
    private String sortCode;

    public SortCodeResponse() {
    }

    public SortCodeResponse(String sortCode) {
        this.sortCode = sortCode;
    }

    public String getSortCode() {
        return sortCode;
    }

    public void setSortCode(String sortCode) {
        this.sortCode = sortCode;
    }
}
