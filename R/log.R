
#' Open a log
#'
#' @param file The path to a file with the log to open.
#' @export
open_log <- function(file = find_last_log()) {
  rstudioapi::navigateToFile(file)
}

#' Find the last log from file names
#'
#' @param root The root logging directory.
#' @export
find_last_log <- function(root = ".") {
  logs_dir <- fs::path(root, "logs")
  dates_dec <- fs::dir_info(logs_dir, regexp = "--")$path %>%
    basename() %>%
    substr(1, nchar(.) - 4) %>%
    lubridate::parse_date_time(timestamp_format()) %>%
    sort(decreasing = TRUE)
  file <- format(lubridate::as_datetime(dates_dec[1]),
                 format = timestamp_format()
  )
  paste0(fs::path(logs_dir, file), ".txt", collapse = "")
}


#' Archive logs
#'
#' @param regex Regular expression a log must match to be archived
#' @param log_dir The logging directory.
#' @param new_dir The directory where the logs should be moved to. `NULL`
#'   indicates a time stamped directory under "logs".
#' @param create_empty Whether or not to create an empty directory.
#' @export
archive_logs <- function(regex = ".*",
                        log_dir = "logs",
                        new_dir = NULL,
                        create_empty = FALSE) {

  files <- fs::dir_ls(log_dir, regexp = regex,type = "file")
  if (length(files) < 1 && !create_empty) {
    warning("Did not create empty archive.", call. = FALSE)
    return(invisible())
  }
  if (is.null(new_dir)) {
    new_dir <- fs::path(log_dir, glue::glue("archived-", generate_timestamp()))
  }

  fs::dir_create(new_dir)
  fs::file_move(files, new_dir)
}

