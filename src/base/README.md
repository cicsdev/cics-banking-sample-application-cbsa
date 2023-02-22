# CBSA base

The base application is a working COBOL application that runs in CICS Transaction Server for z/OS. There are some programs that run in a batch environment.

The CICS banking sample application simulates a bank. Tellers at the bank use IBM 3270 terminals, "green screens", to deal with customers on a face to face basis. This includes registering new customers, opening and closing accounts, depositing and withdrawing money over the counter and moving funds from one account to another.

You can use the CICS bank sample application to explore the following:

* Separation of presentation and business logic
* Use of CICS web services to provide new interfaces to existing code
* Running workloads using IBM Workload Simulator

## Prerequisites

* CICS Transaction Server V5.4 or later
* IBM Db2 V12 or later

## Application architecture

* As supplied, the application runs is a 3270 application that runs in a single CICS region. It reads and writes to VSAM files and Db2 tables. This configuration can be changed to embrace CICS Multi-Region Operation (MRO).

### bms_src

The Basic Mapping Support (BMS) maps for the 3270 interface.

### cobol_src

This contains the programs that are part of the CBSA base application. These are both online CICS programs and batch programs.

### cobol_copy

These contain "copylib" files, which are included in programs as required. These are used to share data structures between programs, such as COMMAREA or CONTAINER layouts, file data structures or Db2 record structures.