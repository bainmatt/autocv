# This module contains tools to build, access, update, and check applications.

library(dplyr)

# TODO: remove extraneous imports
# library(fs)
# library(cli)
# library(yaml)
# library(stats)
# library(tidyr)
# library(purrr)
# library(utils)
# library(readxl)
# library(stringr)


# Construct --------------------------------------------------------------------


#' Read user-defined metadata for the present job from a config file.
#' 
#' @return A nested named list of strings with user-specified job details
#' 
#' @family cli-utils
#' @export
load_job_info <- function(
    field = "all",
    filename = "job_metadata.yml",
    dir = "input"
) {
  dir_path = get_path_to(dir)
  filepath <- file.path(dir_path, filename)
  job_info <- yaml::read_yaml(filepath)
  
  if (!all(field == "all")) {
    # Retrieve value of all valid fields
    job_info <- job_info[names(job_info) %in% field]
  }
  return(job_info)
}


#' Take an application config object and produce the target dir.
#'
#' @family cli-utils
construct_app_filepath <- function(app_info = load_job_info()) {
  # Check log to see if log exists already
  log_path <- get_path_to("applications")
  app_period <- app_info$period
  log_filepath <- file.path(log_path, app_period, "log.rds")
  
  if (fs::is_file(log_filepath)) {
    log <- readRDS(log_filepath) 
  } else {
    log = NA
  }
  
  # Retrieve dated path from log if app exists already
  if (!(all(is.na(log))) && any(app_info$id %in% log$id)) {
    date_created <- log[log$id == app_info$id,]$date_created
    
  } else if (!(all(is.na(log)))) {
    return(log$app_path)
  }
  
  dirname <- paste0(
    date_created, "-", 
    str_to_filename(app_info$company), "-",
    str_to_filename(app_info$position)
  )
  app_path <- file.path(log_path, app_info$period, dirname)
  return(app_path)
}


#' Create the metadata for a new application.
#'
#' @description
#' This information is also used to construct the application log entry.
#' 
#' @family cli-utils
#' @export
construct_app_metadata <- function(
    metadata_filename = "job_metadata.yml",
    metadata_dir = "input"
) {
  # TODO: halt if metadata file already exists
  
  app_info  <- load_job_info(filename = metadata_filename, dir = metadata_dir)
  base_path <- get_path_to(metadata_dir)
  
  app_path    <- construct_app_filepath(app_info)
  log_path    <- dirname(app_path)
  input_path  <- file.path(app_path, "input")
  output_path <- file.path(app_path, "output")
  
  app_id       <- app_info$id
  name         <- str_to_filename(app_info$name, separator = "")
  current_date <- as.character(Sys.Date())
  out_suffix   <- paste0("_", name, "_", app_id)
  
  app_df <- data.frame(
    id           = app_id,
    period       = app_info$period,
    company      = app_info$company,
    position     = app_info$position,
    posting_url  = app_info$posting_url,
    portal_url   = app_info$portal_url,
    
    status       = {"ipr"},
    date_created = current_date,
    last_updated = current_date,
    app_path     = app_path,
    
    # TODO: ?store following two fields when resume built
    base_resume_data_path = file.path(base_path, "resume_data.xlsx"),
    base_cover_data_path  = file.path(base_path, "cover_data.xlsx"),
    resume_data_path      = file.path(input_path, "resume_data.xlsx"),
    cover_data_path       = file.path(input_path, "cover_data.xlsx"),
    posting_path          = file.path(input_path, "posting.txt"),
    log_path              = file.path(log_path, "log.rds"),
    metadata_path         = file.path(app_path, "metadata.yml"),
    
    resume_path = file.path(output_path, paste0("resume", out_suffix, ".pdf")),
    cover_path  = file.path(output_path, paste0("cover", out_suffix, ".pdf")),
    
    resume_plain_path          = file.path(
      output_path, paste0("resume", out_suffix, ".txt")
    ),
    skill_counts_posting_path   = file.path(
      output_path, "skill_counts_posting.csv"
    ),
    skill_counts_resume_path    = file.path(
      output_path, "skill_counts_resume.csv"
    ),
    keyword_counts_posting_path = file.path(output_path, "keyword_counts.csv"),
    skill_report_path           = file.path(output_path, "skill_report.csv"),
    report_path                 = file.path(output_path, "report.txt")
    
    # TODO also get application and interview qs
  )
  return(app_df)
}


# Access -----------------------------------------------------------------------


#' Load the log for a given application period.
#' 
#' @family cli-utils
#' @export
load_log <- function(
    app_period = "latest",
    app_dir = "applications"
) {
  app_path <- get_path_to(app_dir)
  
  # Get available periods
  app_dir_ls <- list(fs::dir_ls(app_path))
  app_periods <- sapply(app_dir_ls, function(path) basename(path))
  
  if (app_period == "latest") {
    app_period <- max(app_periods)
  
  } else if (!(app_period %in% app_periods)) {
    app_period <- match.arg(app_period, app_periods)
  }
  
  log_filepath <- file.path(app_path, app_period, "log.rds")
  log <- readRDS(log_filepath)
  
  return(log)
}


#' Access and open application files.
#'
#' @description
#' `get_app_info` retrieves requested info for a given application from its log.
#' 
#' `get_app_path_to` returns an abs path to a doc from a given application.
#' 
#' `open_app` opens the requested doc for a given application.
#'
#' @family cli
#' @export
get_app_info <- function(
    id = c("all", "latest"),
    field = "all",
    status = c(
      "all", "ipr", "applied", "interviewed", 
      "rejected", "interviewed_then_rejected"
    ),
    company = NA,
    position = NA,
    app_period = "latest",
    app_dir = "applications"
) {
  # Validate args
  if (any(id %in% c("all", "latest"))) { id <- match.arg(id) }
  status <- match.arg(status)
  
  # Load log
  app_df <- load_log(app_period = app_period, app_dir = app_dir)
  
  # Filter id
  if (all(id == "latest")) {
    app_df = app_df[app_df$date_created == max(app_df$date_created),]
    
    # Handle instances where multiple apps were created on the same day
    if (nrow(app_df) > 1) {
      app_df = app_df[-1,]
    }
    
  } else if (!all((id == "all"))) {
    app_df = app_df[app_df$id %in% id,]
    
    # TODO: make this approach work for clarity
    # app_df <- app_df %>%
    #   filter(
    #     .data$id %in% id |
    #       (.data$company == company & .data$position == position)
    #   )
  }
  
  # Filter status
  if (!(all(status == "all"))) {
    app_df = app_df[app_df$status %in% status,]
  }
  
  # Select
  if (!all(field == "all")) {
    app_df <- app_df %>% 
      # TODO: mention that this function is vectorized over fields/ids
      select(., dplyr::any_of(field))
    
  # return only non-path and non-url fields by default
  } else {
    app_df <- app_df %>%
      select(., !tidyselect::ends_with(c("path", "url")))
  }
  
  return(app_df)
}


#' @rdname get_app_info
#' 
#' @export
get_app_path_to <- function(
    id = "latest",
    doc = "resume",
    company = NA,
    position = NA,
    app_period = "latest",
    app_dir = "applications"
) {
  field <- paste0(doc, "_path")
  
  path <- get_app_info(
    id = id, 
    field = field,
    company = company,
    position = position,
    app_period = app_period,
    app_dir = app_dir
  )
  return(as.character(path))
}


#' @rdname get_app_info
#' 
#' @export
open_app <- function(
    doc = "resume",
    id = "latest",
    company = NA,
    position = NA,
    app_period = "latest",
    app_dir = "applications"
) {
  # Get valid field names
  fields <- names(load_log(app_period = app_period, app_dir = app_dir))
  opts <- fields[stringr::str_ends(fields, stringr::regex("path|url"))] %>% 
    stringr::str_replace_all("_path", "")
  opts_str <- opts %>% 
    stringr::str_c(collapse = ", ")
  
  # Error on invalid 'doc' values
  doc <- match.arg(doc, opts)
  
  # TODO: remove this in favour of the above line?
  # if (!(doc %in% opts)) {
  #   cli::cli_alert_warning(
  #     paste0("invalid value provided for 'doc'. Must be one of:\n", opts_str)
  #   )
  #   return(invisible(FALSE))
  # }
  
  # Open url
  if (stringr::str_ends(doc, "url")) {
    url <- get_app_info(
      id = id,
      field = doc,
      company = company,
      position = position,
      app_dir = app_dir,
      app_period = app_period
    )
    url <- as.character(url)
    fs::file_show(url)
    
    cli::cli_alert_success(
      paste("opening", url)
    )
    return(invisible(FALSE))
    
  # If "url" suffix not given, "path" suffix is implied
  } else {
    path <- get_app_path_to(
      id = id,
      doc = doc,
      company = company,
      position = position,
      app_period = app_period,
      app_dir = app_dir
    )
  }
  
  # Error if file does not exist at path
  if (!(fs::file_exists(path))) {
    cli::cli_alert_warning(
      paste0("the file '", fs::path_rel(path), "' does not exist.")
    )
    return(invisible(FALSE))
  
  # Open csv files in R if they exist
  } else if (fs::path_ext(path) == "csv") {
    doc <- utils::read.csv(path)
    View(doc)
    
    cli::cli_alert_success(
      paste("opening", fs::path_rel(path))
    )
    return(doc)

  # Open all other file types (pdf, xlsx, txt, yaml) in their native app
  } else {
    fs::file_show(path)
    
    cli::cli_alert_success(
      paste("opening", fs::path_rel(path))
    )
    return(invisible(FALSE))
  }
}


# Build ------------------------------------------------------------------------


#' Save the metadata for a new application.
#'
#' @family cli-utils
#' @export
write_app_metadata <- function(app_df = construct_app_metadata()) {
  metadata_filepath <- file.path(app_df$app_path, "metadata.yml")
  
  # Sanitize metadata of path info, retaining only user-relevant fields
  app_df <- app_df %>%
    dplyr::select(., -dplyr::ends_with("path"))
  
  if (fs::is_file(metadata_filepath)) {
    cli::cli_alert_danger(
      paste(fs::path_rel(metadata_filepath), "already exists, skipping")
    )
    return(invisible(FALSE))  
    
  } else {
    fs::file_create(metadata_filepath)
    
    con <- file(metadata_filepath, "w")
    yaml::write_yaml(app_df, con)
    close(con)
    
    cli::cli_alert_success(
      paste("metadata file created at", fs::path_rel(metadata_filepath))
    ) 
  }
}


#' Record the current application in the log for the present period.
#'
#' @family cli-utils
#' @export
write_log_entry <- function(app_df = construct_app_metadata()) {
  log_filepath <- app_df$log_path
  new_app_id <- app_df$id
  
  if (fs::is_file(log_filepath)) {
    log_data <- readRDS(log_filepath) %>% arrange(., .data$date_created)
    id_exists <- new_app_id %in% log_data$id
    
    if (id_exists & !validate_id(log_data, app_df)) {
      cli::cli_alert_warning(
        paste0("the identifier '", new_app_id, "' is not unique, aborting")
      )
      return(invisible(FALSE))
      
    } else if (id_exists) {
      cli::cli_alert_warning(
        paste0(
          new_app_id, " already exists in '", 
          fs::path_rel(log_filepath), "', updating the row"
        )
      )
      log_data[log_data$id == new_app_id,] <- app_df
      saveRDS(log_data, file = log_filepath)
      
      return(invisible(FALSE))
      
    } else {
      log_data <- log_data %>% add_row(., app_df)
      saveRDS(log_data, file = log_filepath)
      
      cli::cli_alert_success(
        paste0("writing entry to '", fs::path_rel(log_filepath), "'")
      )
    }
    
  } else {
    log_data <- app_df
    saveRDS(log_data, file = log_filepath)
  }
}


#' Create the file tree and data files for the present app.
#' 
#' @family cli
#' @export
build_app_directory <- function(
    app_df = construct_app_metadata(),
    base_app_id = NA
) {
  df <- app_df
  
  # TODO: ?store apps_path, input_path, output_path in log
  # apps_path   <- fs::path_dir(df$app_path)
  # app_path    <- df$app_path
  
  # Create folders
  input_path  <- fs::path_dir(df$posting_path)
  output_path <- fs::path_dir(df$resume_path)
  
  dir_paths <- c(input_path, output_path)
  for (dir_path in dir_paths) {
    if (!fs::dir_exists(dir_path)) {
      fs::dir_create(dir_path)
      cli::cli_alert_success(
        paste0("creating folder '", fs::path_rel(dir_path), "'")
      )
    }
  }
  
  # Write files
  write_app_metadata(df)
  write_log_entry(df)
  download_webpage_txt(df$posting_url, output_filepath = df$posting_path)
  # (posting skill report)
  
  
  # (resume data copy)
  
  
}


# Helpers ----------------------------------------------------------------------


#' Convert a string to a valid file name.
#'
#' @family cli-utils
#' @export
str_to_filename <- function(string, separator = "-", lower = TRUE) {
  stringr::str_to_lower(stringr::str_replace_all(string, " ", separator))
}


#' Verify that matching job identifiers belong to the same job.
#'
#' @family cli-utils
validate_id <- function(log_data, app_df) {
  # TODO: wrap this in try except to be sure
  new_app_id <- app_df$id
  matching_row <- log_data[log_data$id == new_app_id,]
  
  if (all(matching_row$company == app_df$company) & 
      all(matching_row$position == app_df$position)) {
    return(TRUE)
    
  } else {
    return(FALSE)
  }
}


# One-off tests ----------------------------------------------------------------

# app_df <- construct_app_metadata()
# write_app_metadata(app_df)
# write_log_entry(app_df)
# log <- load_log()
