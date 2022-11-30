# Payments interface and customer services panel utilising Zos Connect EE

Accessed from the services panel(*/services*).
The project is written in Java with Spring Boot, with Thymeleaf templates powering the web pages.

The controllers folder contains a class with all page mappings, and a small class providing a utility to initiate a payments transfer using an GET request.

*(Max sizes of values (i.e. Account number) can be quickly changed by using find and replace on `%8s` , `leadingZeroes(8` and `max = 8` )*
