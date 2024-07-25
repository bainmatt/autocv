# This script contains reusable cli alert messages.


# Writing to/reading from ------------------------------------------------------


#' Notify the user of the directory being read from or written to.
#' 
#' @description
#' `alert_writing_to` gives the base output folder to which files are written.
#' 
#' `alert_reading_from` gives the base input folder from which files are read.
#' 
#' @param folder an absolute path to the directory in question.
#'
#' @family cli-dev
#' @export
alert_writing_to <- function(folder) {
  cli::cli_alert_info(
    paste0("Writing to: ", cli::col_green(fs::path_rel(folder)))
  )
}
  

#' @rdname alert_writing_to
#'
#' @export
alert_reading_from <- function(folder) {
  cli::cli_alert_info(
    paste0("Reading from: ", cli::col_cyan(fs::path_rel(folder)))
  )
}


# Success ----------------------------------------------------------------------


#' Issue an alert that a file or folder has been created at the provided path.
#' 
#' @description
#' `alert_file_created` issues an alert that a file has been created.
#' 
#' `alert_folder_created` issues an alert that a folder has been created.
#' 
#' @param file (string) an absolute path to the file in question.
#' @param base_dir (string) a path (abs or relative) to root output directory
#' (defaults to ".", the project root).
#' @param action (string) text stating how the function reacts to the message
#' (default to an empty string).
#'
#' @family cli-dev
#' @export
alert_file_created <- function(file, base_dir = ".", action = "") {
  cli::cli_alert_success(
    paste0(
      "Writing file: ", 
      cli::col_green(fs::path_rel(file, start = base_dir)), 
      format_action(action)
    )
  )
}


#' @rdname alert_file_created
#'
#' @param folder an absolute path to the directory in question.
#'
#' @export
alert_folder_created <- function(folder, base_dir = ".", action = "") {
  cli::cli_alert_success(
    paste0(
      "Creating folder: ", 
      cli::col_green(fs::path_rel(folder, start = base_dir)),
      format_action(action)
    )
  )
}


#' Issue an alert that a file or folder is opening from the provided path.
#'
#' @param path an absolute path to the file or folder in question.
#'
#' @family cli-dev
#' @export
alert_opening <- function(path, base_dir = ".", action = "") {
  cli::cli_alert_success(
    paste0(
      "Opening: ", 
      cli::col_green(fs::path_rel(path, start = base_dir)),
      format_action(action)
    )
  )
}


# Warning ----------------------------------------------------------------------


#' Issue a warning that a file or folder already exists at the provided path.
#' 
#' @description
#' `warn_file_exists` issues a warning that a file already exists.
#' 
#' `warn_folder_exists` issues a warning that a folder already exists.
#'
#' @family cli-dev
#' @export
warn_file_exists <- function(file, base_dir = ".", action = "skipping") {
  cli::cli_alert_warning(
    paste0(
      "File already exists: ", 
      cli::col_cyan(fs::path_rel(file, start = base_dir)), 
      format_action(action)
    )
  )
}


#' @rdname warn_file_exists
#'
#' @export
warn_folder_exists <- function(folder, base_dir = ".", action = "skipping") {
  cli::cli_alert_warning(
    paste0(
      "Folder already exists: ", 
      cli::col_cyan(fs::path_rel(folder, start = base_dir)), 
      format_action(action)
    )
  )
}


# Danger -----------------------------------------------------------------------


#' Issue a warning that a file or folder does not exist at the provided path.
#' 
#' @description
#' `warn_file_missing` issues a warning that a file is missing.
#' 
#' `warn_folder_missing` issues a warning that a folder is missing.
#'
#' @family cli-dev
#' @export
warn_file_missing <- function(file, base_dir = ".", action = "aborting") {
  cli::cli_alert_danger(
    paste0(
      "File doesn't exist: ", 
      cli::col_red(fs::path_rel(file, start = base_dir)),
      format_action(action)
    )
  )
}


#' @rdname warn_file_missing
#'
#' @export
warn_folder_missing <- function(folder, base_dir = ".", action = "aborting") {
  cli::cli_alert_danger(
    paste0(
      "Folder doesn't exist: ", 
      cli::col_red(fs::path_rel(folder, start = base_dir)),
      format_action(action)
    )
  )
}


# Helpers ----------------------------------------------------------------------


#' Format any 'action' text provided to a cli alert before printing.
#' 
#' @family cli-dev
#' @export
format_action <- function(action) {
  if (!(action == "")) {
    action = paste0(" (", action, ") ")
  }
  return(action)
}
