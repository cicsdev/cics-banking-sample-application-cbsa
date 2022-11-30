# Payments interface and customer services panel utilising Zos Connect EE

## Johnny Morrison-Howe

Accessed from the services panel(*/services*).
The project is written in Java with Spring Boot, with Thymeleaf templates powering the web pages.

The controllers folder contains a class with all page mappings, and a small class providing a utility to initiate a payments transfer using an GET request.

*(Max sizes of values (i.e. Account number) can be quickly changed by using find and replace on `%8s` , `leadingZeroes(8` and `max = 8` )*

## To Do

- [X] Customer services text at top
- [X] Round money to 2dp
- [X] Overdrafts are ints
- [X] Sort codes to 6 digits
- [X] Remove EYE
- [X] Remove success/error codes from responses
- [X] Format all dates nicely
  - [X] Leading zeroes on dates
- [X] Add account opened date
- [X] Leading zeroes when showing customer number (and probably account numbers) for consistency
- [X] On account creation, Interest rate and balances should be floats. Overdraft is still an int
  - [X] Also needs overdraft limit and interest rate
- [X] Listacc needs an error message when customer doesn't exist
- [X] Add debit/credit radio buttons to paydbcr
- [X] Few more comments probably
- [X] Find out how to package up to WAR bundle
- [X] Domain should also be a command line arg
- [X] Split payment interface and customer services into separate projects
- [X] Note where account number length is defined so it can be changed easily
