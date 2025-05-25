package com.cbsa.migration.util;

import org.junit.jupiter.api.Test;

import java.time.LocalDate;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Unit tests for the CobolConverter utility class.
 */
public class CobolConverterTest {

    @Test
    public void testConvertCobolDateToLocalDate() {
        // 1. Test a valid date conversion
        LocalDate date = CobolConverter.convertCobolDateToLocalDate("20230515");
        assertEquals(LocalDate.of(2023, 5, 15), date);
        
        // 2. Test null handling
        assertNull(CobolConverter.convertCobolDateToLocalDate(null));
        
        // 3. Test empty string handling
        assertNull(CobolConverter.convertCobolDateToLocalDate(""));
    }

    @Test
    public void testConvertLocalDateToCobolDate() {
        // 1. Test a valid date conversion
        String cobolDate = CobolConverter.convertLocalDateToCobolDate(LocalDate.of(2023, 5, 15));
        assertEquals("20230515", cobolDate);
        
        // 2. Test null handling
        assertEquals("", CobolConverter.convertLocalDateToCobolDate(null));
    }

    @Test
    public void testPadRight() {
        // 1. Test padding a string shorter than the target length
        String padded = CobolConverter.padRight("ABC", 5);
        assertEquals(5, padded.length());
        assertEquals("ABC  ", padded);
        
        // 2. Test a string already at the target length
        padded = CobolConverter.padRight("ABCDE", 5);
        assertEquals(5, padded.length());
        assertEquals("ABCDE", padded);
        
        // 3. Test a string longer than the target length (should truncate)
        padded = CobolConverter.padRight("ABCDEFG", 5);
        assertEquals(5, padded.length());
        assertEquals("ABCDE", padded);
        
        // 4. Test null handling
        padded = CobolConverter.padRight(null, 5);
        assertEquals(5, padded.length());
        assertEquals("     ", padded);
    }

    @Test
    public void testPadZeros() {
        // 1. Test padding a number with leading zeros
        String padded = CobolConverter.padZeros(123, 5);
        assertEquals("00123", padded);
        
        // 2. Test a number with the same number of digits
        padded = CobolConverter.padZeros(12345, 5);
        assertEquals("12345", padded);
        
        // 3. Test zero
        padded = CobolConverter.padZeros(0, 5);
        assertEquals("00000", padded);
    }
}
