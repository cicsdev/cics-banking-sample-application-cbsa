package com.cbsa.migration.controller;

import com.cbsa.migration.dto.ErrorRequestDto;
import com.cbsa.migration.dto.ErrorResponseDto;
import com.cbsa.migration.model.ApplicationError;
import com.cbsa.migration.service.ErrorLoggingService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import javax.validation.constraints.Max;
import javax.validation.constraints.Min;
import java.util.List;

/**
 * REST Controller for error logging functionality.
 * Migrated from COBOL ABNDPROC program.
 */
@RestController
@RequestMapping("/api/errors")
@Tag(name = "Error Logging", description = "Application error logging and monitoring API")
@Validated
public class ErrorController {
    
    private final ErrorLoggingService errorLoggingService;
    
    @Autowired
    public ErrorController(ErrorLoggingService errorLoggingService) {
        this.errorLoggingService = errorLoggingService;
    }
    
    /**
     * Log an application error.
     * Equivalent to COBOL ABNDPROC program functionality.
     * 
     * @param errorRequest the error details to log
     * @return response indicating success or failure
     */
    @PostMapping
    @Operation(summary = "Log application error", 
               description = "Log an application error to the centralized error datastore")
    @ApiResponses(value = {
        @ApiResponse(responseCode = "201", description = "Error successfully logged"),
        @ApiResponse(responseCode = "400", description = "Invalid error data"),
        @ApiResponse(responseCode = "500", description = "Error logging failed")
    })
    public ResponseEntity<ErrorResponseDto> logError(@Valid @RequestBody ErrorRequestDto errorRequest) {
        try {
            ErrorResponseDto response = errorLoggingService.logError(errorRequest);
            
            switch (response.getStatus()) {
                case "SUCCESS":
                    return ResponseEntity.status(HttpStatus.CREATED).body(response);
                case "VALIDATION_ERROR":
                    return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(response);
                case "FAILURE":
                default:
                    return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(response);
            }
        } catch (Exception e) {
            ErrorResponseDto errorResponse = ErrorResponseDto.failure("Service error: " + e.getMessage(), 
                java.time.LocalDateTime.now().toString());
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(errorResponse);
        }
    }
    
    /**
     * Get error count for a specific program.
     * 
     * @param programName the program name
     * @return count of errors for the program
     */
    @GetMapping("/count/{programName}")
    @Operation(summary = "Get error count by program", 
               description = "Get the number of errors logged for a specific program")
    @ApiResponse(responseCode = "200", description = "Error count retrieved successfully")
    public ResponseEntity<Long> getErrorCount(
            @Parameter(description = "Program name to get error count for")
            @PathVariable String programName) {
        long count = errorLoggingService.getErrorCount(programName);
        return ResponseEntity.ok(count);
    }
    
    /**
     * Get recent errors for monitoring.
     * 
     * @param limit maximum number of errors to return (default 10, max 100)
     * @return list of recent error records
     */
    @GetMapping("/recent")
    @Operation(summary = "Get recent errors", 
               description = "Get the most recent application errors for monitoring purposes")
    @ApiResponse(responseCode = "200", description = "Recent errors retrieved successfully")
    public ResponseEntity<List<ApplicationError>> getRecentErrors(
            @Parameter(description = "Maximum number of errors to return")
            @RequestParam(defaultValue = "10") 
            @Min(1) @Max(100) int limit) {
        List<ApplicationError> errors = errorLoggingService.getRecentErrors(limit);
        return ResponseEntity.ok(errors);
    }
    
    /**
     * Get all errors for a specific program.
     * 
     * @param programName the program name
     * @return list of error records for the program
     */
    @GetMapping("/program/{programName}")
    @Operation(summary = "Get errors by program", 
               description = "Get all error records for a specific program")
    @ApiResponse(responseCode = "200", description = "Program errors retrieved successfully")
    public ResponseEntity<List<ApplicationError>> getErrorsByProgram(
            @Parameter(description = "Program name to get errors for")
            @PathVariable String programName) {
        List<ApplicationError> errors = errorLoggingService.getErrorsByProgram(programName);
        return ResponseEntity.ok(errors);
    }
    
    /**
     * Health check endpoint for error logging service.
     * 
     * @return simple health status
     */
    @GetMapping("/health")
    @Operation(summary = "Error service health check", 
               description = "Check if the error logging service is operational")
    @ApiResponse(responseCode = "200", description = "Service is healthy")
    public ResponseEntity<String> healthCheck() {
        return ResponseEntity.ok("Error logging service is operational");
    }
}
