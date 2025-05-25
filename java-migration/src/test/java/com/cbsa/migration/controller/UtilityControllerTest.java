package com.cbsa.migration.controller;

import com.cbsa.migration.service.CompanyInfoService;
import com.cbsa.migration.service.SortCodeService;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.web.servlet.MockMvc;

import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * Integration tests for the UtilityController class.
 */
@WebMvcTest(UtilityController.class)
public class UtilityControllerTest {

    @Autowired
    private MockMvc mockMvc;

    @MockBean
    private SortCodeService sortCodeService;

    @MockBean
    private CompanyInfoService companyInfoService;

    @Test
    public void testGetSortCode() throws Exception {
        // 1. Arrange
        when(sortCodeService.getSortCode()).thenReturn("987654");

        // 2. Act & Assert
        mockMvc.perform(get("/api/utility/sortcode"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.sortCode").value("987654"));
    }

    @Test
    public void testGetCompanyName() throws Exception {
        // 1. Arrange
        when(companyInfoService.getCompanyName()).thenReturn("CICS Bank Sample Application");

        // 2. Act & Assert
        mockMvc.perform(get("/api/utility/company-name"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.companyName").value("CICS Bank Sample Application"));
    }
}
