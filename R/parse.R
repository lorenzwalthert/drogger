#' Parse a log file
#'
#' Parses a log that follows the structure used by `futile.loggeer`, i.e.
#' `INFO [2018-09-11 23:59:29] Here is a message.`
#' @param path The path to a file that is a log.
#' @export
parse_log <- function(path) {
  lines <- readLines(path)
  strsplit(lines, " ", fixed = TRUE) %>%
    purrr::map(parse_log_line) %>%
    do.call(rbind, .)
}

parse_log_line <- function(split_line) {
  tibble::tibble(
    log_level = split_line[1],
    timestamp = paste(split_line[2:3], collapse = " ") %>%
      substring(2) %>%
      substr(1, nchar(.) - 1) %>%
      strptime(format = "%Y-%m-%d %H:%M:%S") %>%
      as.POSIXct(),
    message = split_line[-c(1:3)] %>% paste(collapse = " ")
  )
}
