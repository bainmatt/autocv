# This script contains tools for working with datasets in the autocv package.

# library(fs)
# library(here)
# library(stringr)


#' Find the location of template data files on your system.
#' 
#' @examples
#' # Obtain a complete list of top-level resources included with autocv:
#' autocv_resources()
#' 
#' # Obtain a complete list of resources included in the css/ directory:
#' autocv_resources("css")
#' 
#' # Obtain a complete path to the custom_resume.css resource:
#' autocv_resources("css/custom_resume.css")
#'
#' # Obtain a complete list of template files included with autocv:
#' autocv_resources(inst_dir = "templates")
#' 
#' @family data
#' @export
autocv_resources <- function(path = NULL, inst_dir = "extdata") {
  test_path <- system.file(file.path(inst_dir, path), package = "autocv")
  if (is.null(path)) {
    dir(system.file(inst_dir, package = "autocv"))
  } else if (fs::is_dir(test_path)) {
    dir(system.file(paste0(inst_dir, "/", path), package = "autocv"))
  } else {
    system.file(inst_dir, path, package = "autocv", mustWork = TRUE)
  }
}


# TODO: ?accept additional directory names to append to path + check they exist

#' Construct an absolute path to a project directory for saving and loading.
#' 
#' This function constructs an absolute path to a specified project directory.
#' Directory paths, relative to the project root, are defined in the 
#' user's .Rprofile environment variables.
#'
#' @param dir (string) the directory name.
#'
#' @examples
#' # get_path_to("src")
#'
#' @family data 
#' @export
get_path_to <- function(
    dir = c(
      "src", "input", "output", 
      "applications", "notebooks", "templates",
      "extdata", "css", "latex", "resources"
    )
) {
  # Validate argument using partial matching
  # TODO consider changing this to be strict -- could be dangerous
  dir <- match.arg(dir)
  env_variable <- paste0(stringr::str_to_upper(dir), "_DIR")
  relpath <- Sys.getenv(env_variable, NA)
  
  if (is.na(relpath)) {
    stop("Invalid directory name")
  } 
  path <- here::here(relpath)
  return(path)
}
