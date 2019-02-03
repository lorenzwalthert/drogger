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
#' @importFrom magrittr %>%
"_PACKAGE"
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("."))
}

#' Returns the name of the background logger
#' @export
background_file_logger <- function() {
  "background_logger"
}


#' Loggers
#'
#' These are wrappers around [futile.logger::flog_error()] and friends. They
#' write to the background logger by default and use `...` for message creation.
#' @param logger The loging function to use.
#' @param name The name of the logger to use.
#' @param ... Message passed to the logger via `c(...)`.
flog_template <- function(logger, ..., name = background_file_logger()) {
  logger(paste0(c(...), collapse = ""), name = name)
}

#' @rdname flog_template
#' @export
flog_trace <- purrr::partial(flog_template, logger = futile.logger::flog.trace)

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
#' @export
flog_init <- function(file_name = file.path("logs", "all.txt"),
                      name = background_file_logger()) {
  futile.logger::flog.appender(
    futile.logger::appender.file(file_name),
    name = name
  )
}

#' Define the threshold for logging
#'
#' Wrapper around [flog.threshold()].
#' @inheritParams futile.logger::flog.threshold
#' @export
flog_threshold <- function(treshold, name) {
  futile.logger::flog.threshold(treshold, name)
}

#' Start logging
#'
#' @param threshold  The new threshold for the given logger.
#' @param name The name of the logger.
#' @param msg An initial message to pass to the logger.
#' @return
#' Invisibly returns the path to the file that contains the log.
#' @export
flog_start <- function(threshold = futile.logger::INFO,
                       name = background_file_logger(),
                       msg = "Started logging run") {
  timestamp <- paste0(generate_time_stamp(), ".txt")
  fs::dir_create("logs")
  flog_init(file.path("logs", timestamp), name)
  futile.logger::flog.threshold(threshold, name)
  flog_info(msg, paste(rep("-", 20), collapse = ""))

  writeLines(timestamp, "logs/.current")
  invisible(timestamp)
}

generate_time_stamp <- function(time = Sys.time()) {
  format(time, timestamp_format())
}

#' Stop Logging
#' @inheritParams flog_start
#' @param append Whether or not to append the data from the most recent log
#'   to the file logs/all.txt.
#' @return
#' Invisibly returns the path to the file that contains the log.
#' @export
flog_stop <- function(name = background_file_logger(),
                      msg = "Completed logging run",
                      append = FALSE) {
  flog_info(msg, paste(rep("-", 20), collapse = ""))
  location_log_current_meta <- "logs/.current"
  location_log_current <- "logs/current.txt"
  location_log_all <- "logs/all.txt"

  if (file.exists(location_log_current_meta)) {
    log_path_current <- location_log_current_meta %>%
      readLines() %>%
      file.path("logs", .)
    log_current <- log_path_current %>%
      readLines()
    fs::file_create(location_log_all)
    full_log <- readLines(location_log_all)
    if (append) writeLines(c(full_log, rev(log_current)), location_log_all)
    unlink(location_log_current_meta)
  }
}
