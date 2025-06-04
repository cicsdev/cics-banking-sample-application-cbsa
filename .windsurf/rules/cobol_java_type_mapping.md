---
trigger: model_decision
description: Standard rule defining COBOL→Java data-type mappings applied automatically during migration workflows.
---

# COBOL → Java Data-Type Mapping Rule

## Mapping Guidelines

### 1. Alphanumeric

1. `PIC X(n)` & `PIC A(n)`  
   1. Map to `String`.  
   2. Trim trailing blanks on inbound values.  
   3. Pad with blanks on outbound values to preserve fixed length.

### 2. Unsigned Numerics

1. `PIC 9(n)` (no implied decimal)  
   1. If *n* ≤ 9 ⇒ `int`.  
   2. If 10 ≤ *n* ≤ 18 ⇒ `long`.  
   3. If *n* > 18 ⇒ `BigInteger` (rare; verify necessity).

2. `PIC 9(n)V9(m)` (implied decimal)  
   1. Map to `BigDecimal` with scale =`m`.  
   2. Use `CobolConverter.parseDecimal()` / `formatDecimal()` helpers.

### 3. Signed Numerics

1. `PIC S9(n)` & `PIC S9(n)V9(m)`  
   1. Follow the same rules as their unsigned counterparts but maintain sign.

### 4. Packed Decimal & COMP-3

1. Any `COMP-3` (packed decimal) field  
   1. Map to `BigDecimal`.  
   2. Use `CobolConverter.unpackComp3()` & `packComp3()` helpers.  
   3. Preserve scale based on the number of implied decimal digits.

### 5. Computational Binary (COMP, COMP-4, BINARY)

1. 1 – 4 bytes ⇒ `int`.  
2. 5 – 8 bytes ⇒ `long`.  
3. Larger   ⇒ `BigInteger`.

### 6. Dates & Times

1. `YYYYMMDD` numeric or alphanumeric ⇒ `java.time.LocalDate`.  
2. `YYYYMMDDHHMMSS` or `YYYYDDDHHMMSS` ⇒ `java.time.LocalDateTime`.  
3. Use `CobolConverter.parseDate()` / `formatDate()` helpers.

## Padding & Formatting Rules

1. Strings returned to COBOL clients must be padded to their original length.  
2. Numeric strings shorter than their PIC length must be left-padded with zeros on outbound messages.  
3. For BigDecimal values, always set scale explicitly to avoid scientific notation in JSON serialization.

## Examples

1. `PIC X(10)` ⇢ `String` ("CUSTOMER  ")  
2. `PIC 9(5)` ⇢ `int` (12345)  
3. `PIC 9(9)V9(2)` ⇢ `BigDecimal` (123456789.01)  
4. `PIC S9(7) COMP-3` ⇢ `BigDecimal` (-1234567)  
5. `PIC 9(8)` date (20250604) ⇢ `LocalDate` (2025-06-04)

## Rationale

1. Maintaining precision for monetary and packed-decimal values mandates `BigDecimal`.  
2. Simpler integer fields remain primitive for performance and readability.  
3. Using `LocalDate/LocalDateTime` avoids timezone ambiguity inherent in COBOL date representations.

## References

1. Phase-2 documentation: `plan/historical_learnings/learnings_from_phase_2.md` §4.1 COBOL → Java Data Type Mapping.  
2. `CobolConverter` utility class in source tree (handles padding & packed decimal).
