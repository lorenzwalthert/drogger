
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Travis build status](https://travis-ci.org/lorenzwalthert/drogger.svg?branch=master)](https://travis-ci.org/lorenzwalthert/drogger)

drogger
=======

The goal of drogger is twofold:

Simple wrappers around futil loggers
------------------------------------

These wrappers

-   use snake case (i.e. `flog_info` instead of `flog.info`).
-   Use `...` for message creation, i.e. you can `flog_info("Hi ", "I am Lorenz")` instead of `flog.info(paste("Hi", "I am Lorenz"))` to create a single log line.

Providing utilities for storing and accessing logs
--------------------------------------------------

All logs are stored under `./logs`

-   `flog_start()` initializes a logger that writes to a new file with a time stamp.
-   `flog_stop()` ends writing to the open connection, adding all content from the current run to a log that contains all logged lines.
-   `open_log()` opens a log, by default the last log, which is found via `find_last_log()`

Installation
------------

You can install the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("lorenzwalthert/drogger")
```
