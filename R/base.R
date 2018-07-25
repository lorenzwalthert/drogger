#' A logging scheme for drake
#'
#' Utility functions to log drake build processes. They work as follows:
#'
#' * put flog_start() before drake::make()
#' * put flog_stop() after drake::make()
#'
#' You will get two types of log stored into ./logs:
#'
#' * A log with named with a time stamp for each run you executed.
#' * A log called `all.txt` that contains the whole log up to now, in reversed
#'   order, i.e. the top line corresponds to the last logging entry, the last
#'   row to the first logging entry.
#'
#' You can open the last log with `open_log()`.
#' You can find the last log file with `find_last_log()`.
#'
#' @docType package
NULL


#' Returns the name of the background logger
background_file_logger <- function() {
  "background_logger"
}


#' Loggers
#'
#' These are wrappers around [futile.logger::flog_error()] and friends. They
#' write to the background logger by default and use `...` for message creation.
flog_template <- function(logger, ..., name = background_file_logger()) {
  logger(paste0(c(...), collapse = ""), name = name)
}

#' @rdname flog_template
#' @export
flog_info <- purrr::partial(flog_template, logger = futile.logger::flog.info)

#' @rdname flog_template
#' @export
flog_debug <- purrr::partial(flog_template, logger = futile.logger::flog.debug)

#' @rdname flog_template
#' @export
flog_warn <- purrr::partial(flog_template, logger = futile.logger::flog.warn)
flog_warn <- purrr::partial(flog_template, logger = futile.logger::flog.warn)

#' @rdname flog_template
#' @export
flog_error <- purrr::partial(flog_template, logger = futile.logger::flog.error)

#' @rdname flog_template
#' @export
flog_fatal <- purrr::partial(flog_template, logger = futile.logger::flog.fatal)


timestamp_format <- function() {
  "%Y-%m-%d--%H-%M-%S"
}

#' Initialize a logger
#'
#' @param file_name The name fo the file where the log should be saved to
#' @param name The name of the logger.
flog_init <- function(file_name = file.path("logs", "all.txt"),
                      name = background_file_logger()) {
  futile.logger::flog.appender(
    futile.logger::appender.file(file_name),
    name = name
  )
}

#' Start logging
#'
#'
flog_start <- function(threshold = futile.logger::INFO,
                       name = background_file_logger(),
                       msg = "Started drake::make() ") {
  timestamp <- paste0(format(Sys.time(), timestamp_format()), ".txt")
  flog_init(file.path("logs", timestamp), name)
  futile.logger::flog.threshold(threshold)
  flog_info(msg, paste(rep("-", 20), collapse = ""))

  writeLines(timestamp, "logs/.current")
}

#' Stop Logging
flog_stop <- function(name = background_file_logger(),
                      msg = "Completed drake::make() ") {
  flog_info(msg, paste(rep("-", 20), collapse = ""))
  location_log_current_meta <- "logs/.current"
  location_log_current <- "logs/current.txt"
  location_log_all <- "logs/all.txt"

  if (file.exists(location_log_current_meta)) {
    log_current <- location_log_current_meta %>%
      readLines() %>%
      file.path("logs", .) %>%
      readLines()
    fs::file_create(location_log_all)
    full_log <- readLines(location_log_all)
    writeLines(c(full_log, rev(log_current)), location_log_all)
    unlink(location_log_current_meta)
  }
}

#' Open a log
#'
#' @param file The path to a file with the log to open.
#' @export
open_log <- function(file = find_last_log()) {
  rstudioapi::navigateToFile(file)
}

#' Find the last log from file names
#'
#' @export
find_last_log <- function() {
  dates_dec <- fs::dir_info("logs/", regexp = "--")$path %>%
    basename() %>%
    substr(1, nchar(.) - 4) %>%
    lubridate::parse_date_time(timestamp_format()) %>%
    sort(decreasing = TRUE)
  file <- format(lubridate::as_datetime(dates_dec[1]),
                 format = timestamp_format()
  )
  paste0("logs/", file, ".txt", collapse = "")
}
