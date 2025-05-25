package com.cbsa.migration.service;

import org.springframework.stereotype.Service;

/**
 * This service replicates the functionality of the GETSCODE COBOL program.
 * It provides the bank sort code, which in the original COBOL application
 * was a fixed value of 987654.
 */
@Service
public class SortCodeService {

    private static final String SORT_CODE = "987654";

    /**
     * Gets the bank sort code.
     *
     * @return The 6-digit sort code
     */
    public String getSortCode() {
        return SORT_CODE;
    }
}
