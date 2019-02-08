#' Delete logs with a checkpoint older than a reference date
#'
#' @param relieve date The date that marks the cut-off for discarding logs.
#'   All logs older than that will be deleted.
#' @param communicator The bare name of a function that communicates the
#'   how many logs are deleted.
#' @export
delete_logs <- function(relieve_date = Sys.Date() - months(6),
                        communicator = flog_info) {
  paths <- fs::dir_ls("logs", type = "file")
  dates <- paths %>%
    fs::path_ext_remove() %>%
    fs::path_file() %>%
    as.Date(format = "%Y-%m-%d--%H-%M-%S", optional = TRUE)
  to_delete <- na.omit(paths[dates < relieve_date])
  communicator(
    "Deleting ", length(to_delete), " logs older than ", as.character(relieve_date), "."
  )
  fs::file_delete(to_delete)
}
