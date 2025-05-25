package com.cbsa.migration.model;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.NotNull;

/**
 * Control entity representing the CONTROL copybook structure from COBOL
 * Based on CONTROLI.cpy
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class Control {

    /**
     * Total number of customers in the system
     * CONTROL-CUSTOMER-COUNT PIC 9(10) PACKED-DECIMAL
     */
    @NotNull
    private Long customerCount;

    /**
     * The last customer number assigned
     * CONTROL-CUSTOMER-LAST PIC 9(10) PACKED-DECIMAL
     */
    @NotNull
    private Long lastCustomerNumber;

    /**
     * Total number of accounts in the system
     * CONTROL-ACCOUNT-COUNT PIC 9(8) PACKED-DECIMAL
     */
    @NotNull
    private Integer accountCount;

    /**
     * The last account number assigned
     * CONTROL-ACCOUNT-LAST PIC 9(8) PACKED-DECIMAL
     */
    @NotNull
    private Integer lastAccountNumber;

    /**
     * Static ID for the Control record (only one exists in the system)
     */
    public static final String CONTROL_ID = "CONTROL";

    /**
     * Get the unique ID for this control record
     */
    public String getId() {
        return CONTROL_ID;
    }
}
