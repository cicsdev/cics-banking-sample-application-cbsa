package com.cbsa.migration.controller;

import com.cbsa.migration.dto.ErrorRequestDto;
import com.cbsa.migration.dto.ErrorResponseDto;
import com.cbsa.migration.service.ErrorLoggingService;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

import static org.hamcrest.Matchers.is;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;

/**
 * Integration tests for ErrorController using MockMvc
 * Tests REST API endpoints with mocked service layer
 */
@WebMvcTest(ErrorController.class)
class ErrorControllerTest {

    @Autowired
    private MockMvc mockMvc;

    @Autowired
    private ObjectMapper objectMapper;

    @MockBean
    private ErrorLoggingService errorLoggingService;

    @Test
    void testLogError_validRequest_returnsSuccessResponse() throws Exception {
        // Given
        ErrorRequestDto request = new ErrorRequestDto();
        request.setApplicationId("TESTAPP");
        request.setTransactionId("T001");
        request.setErrorCode("ABND");
        request.setProgramName("TESTPROG");
        request.setResponseCode("12");
        request.setResponse2Code("34");
        request.setSqlCode("-803");
        request.setFreeformText("Test error message");

        ErrorResponseDto mockResponse = ErrorResponseDto.success(1L, "2023-12-25T10:30:00");

        when(errorLoggingService.logError(any(ErrorRequestDto.class)))
            .thenReturn(mockResponse);

        // When & Then
        mockMvc.perform(post("/api/errors")
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(request)))
            .andExpect(status().isCreated())
            .andExpect(content().contentType(MediaType.APPLICATION_JSON))
            .andExpect(jsonPath("$.errorId", is(1)))
            .andExpect(jsonPath("$.status", is("SUCCESS")))
            .andExpect(jsonPath("$.message", is("Error record successfully logged")))
            .andExpect(jsonPath("$.timestamp", is("2023-12-25T10:30:00")));
    }

    @Test
    void testLogError_missingApplicationId_returnsBadRequest() throws Exception {
        // Given
        ErrorRequestDto request = new ErrorRequestDto();
        request.setErrorCode("ABND");
        request.setFreeformText("Test error");

        // When & Then
        mockMvc.perform(post("/api/errors")
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(request)))
            .andExpect(status().isBadRequest());
    }

    @Test
    void testLogError_missingErrorCode_returnsBadRequest() throws Exception {
        // Given
        ErrorRequestDto request = new ErrorRequestDto();
        request.setApplicationId("TESTAPP");
        request.setFreeformText("Test error");

        // When & Then
        mockMvc.perform(post("/api/errors")
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(request)))
            .andExpect(status().isBadRequest());
    }

    @Test
    void testLogError_invalidJson_returnsBadRequest() throws Exception {
        // Given
        String invalidJson = "{ invalid json }";

        // When & Then
        mockMvc.perform(post("/api/errors")
                .contentType(MediaType.APPLICATION_JSON)
                .content(invalidJson))
            .andExpect(status().isBadRequest());
    }

    @Test
    void testLogError_serviceThrowsException_returnsInternalServerError() throws Exception {
        // Given
        ErrorRequestDto request = new ErrorRequestDto();
        request.setApplicationId("TESTAPP");
        request.setProgramName("TESTPROG");
        request.setErrorCode("ABND");
        request.setFreeformText("Test error");

        when(errorLoggingService.logError(any(ErrorRequestDto.class)))
            .thenThrow(new RuntimeException("Database connection failed"));

        // When & Then
        mockMvc.perform(post("/api/errors")
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(request)))
            .andExpect(status().isInternalServerError());
    }

    @Test
    void testLogError_emptyBody_returnsBadRequest() throws Exception {
        // When & Then
        mockMvc.perform(post("/api/errors")
                .contentType(MediaType.APPLICATION_JSON)
                .content(""))
            .andExpect(status().isBadRequest());
    }
}
