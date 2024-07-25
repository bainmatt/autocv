# This module contains tools to build, access, update, and check applications.

library(dplyr)


# Construct --------------------------------------------------------------------


# TODO: func to handle (call in load_job_info) validation of: 
# id/base_id (existing or "/")/period, co/pos (str), links (urls or "/").

#' Read user-defined metadata for the present job.
#' 
#' @return A nested named list of strings with user-specified job details.
#' 
#' @examples
#' load_job_info(filename = "template_job_metadata.yml", dir = "templates")
#' 
#' @family data
#' @export
load_job_info <- function(
    field = "all",
    filename = "job_metadata.yml",
    dir = "input",
    open = FALSE
) {
  dir_path = get_path_to(dir)
  filepath <- file.path(dir_path, filename)
  
  if (open) { open_doc_and_wait(path = filepath) }
  
  job_info <- yaml::read_yaml(filepath)
  
  # Retrieve value of all valid fields
  if (!all(field == "all")) {
    job_info <- job_info[names(job_info) %in% field]
  }
  return(job_info)
}


# TODO: deprecate this!! redundant

#' Take an application config object and produce the target dir.
#'
#' @examples
#' data(example_job_metadata)
#' construct_app_path(app_info = example_job_metadata)
#'
#' @noRd
construct_app_path <- function(app_info = load_job_info(), log = NA) {
  # Check if log exists
  log_path <- get_path_to("applications")
  log_exists <- TRUE
  
  if (all(is.na(log))) {
    app_period <- app_info$period
    log_filepath <- file.path(log_path, app_period, "log.rds")
    log_exists <- file.exists(log_filepath) 
  }
  
  # If it exists and was not passed to the function, load it
  if (log_exists & all(is.na(log))) {
    log <- readRDS(log_filepath)
  }
  
  # If the log exists, check if it contains an entry for the app
  id_exists <- FALSE
  if (log_exists) {
    id_exists <- any(app_info$id %in% log$id)
  }
  
  # Load the path if the entry exists or construct an id based on the 
  # number of entries created today.
  current_date <- as.character(Sys.Date())
  
  if (id_exists) {
    app_path <- log[log$id == app_info$id,]$app_path
    return(app_path)
    
  } else if (log_exists) {
    date_count <- sum(stringr::str_detect(log$date_created, current_date)) + 1
    date_seq <- sprintf("%02d", date_count)
  
  } else {
    date_seq <- "01"
  }
  
  # Construct from scratch if the application entry does not already exist
  dirname <- paste0(
    current_date, "-", 
    date_seq, "-",
    str_to_filename(app_info$company), "-",
    str_to_filename(app_info$position), "-",
    app_info$id
  )
  app_path <- file.path(log_path, app_info$period, dirname)
  return(app_path)
}


#' Create the metadata for a new application.
#'
#' @description
#' This information is also used to construct the application log entry.
#' 
#' @examples
#' data(example_job_metadata)
#' app_df <- construct_app_metadata(app_info = example_job_metadata)
#' app_df_concise <- dplyr::select(app_df, -dplyr::ends_with(c("path", "url")))
#' app_df_concise
#' 
#' @family build-dev
#' @export
construct_app_metadata <- function(app_info = load_job_info(open = TRUE)) {
  # Check to see if log exists
  log_path <- get_path_to("applications")
  app_period <- app_info$period
  log_filepath <- file.path(log_path, app_period, "log.rds")
  log_exists <- file.exists(log_filepath)
  
  # If the log exists, load it and check if the app exists and id is unique
  id_exists <- FALSE
  id_is_valid <- TRUE
  if (log_exists) {
    log <- readRDS(log_filepath)
    id_exists <- any(app_info$id %in% log$id)
    
    # TODO: run this AFTER metadata constructed? to avoid circ dependency
    id_is_valid <- validate_id(log = log, app_df = app_info)
    if (!id_is_valid) { return(invisible(FALSE)) }
  }
  
  # Return the app if it exists
  if (id_exists & id_is_valid) {
    app_df <- log[log$id == app_info$id,]
    return(app_df)
  }

  # If app does not exist, construct an id based on entries created today
  current_date <- as.character(Sys.Date())
  if (log_exists) {
    today_mask <- stringr::str_detect(log$date_created, current_date)
    today_seqs <- as.numeric(log[today_mask,]$date_seq)
    
    date_seq <- sprintf(
      "%02d",
      ifelse(rlang::is_empty(today_seqs), 1, max(today_seqs) + 1)
    )
    
    # date_count <- sum(stringr::str_detect(log$date_created, current_date)) + 1
    # date_seq <- sprintf("%02d", date_count)

  } else {
    date_seq <- "01"
  }
  
  # Construct a path to the app
  company_formatted <- stringr::str_replace_all(
    app_info$company, "[(),]", ""
  )
  position_formatted <- stringr::str_replace_all(
    app_info$position, "[(),]", ""
  )
  
  dirname <- paste0(
    current_date, "-",
    date_seq, "-",
    str_to_filename(company_formatted), "-",
    # str_to_filename(app_info$company), "-",
    str_to_filename(position_formatted), "-",
    # str_to_filename(app_info$position), "-",
    app_info$id
  )
  app_path <- file.path(log_path, app_info$period, dirname)
  
  # Construct paths to important directories
  period_path <- dirname(app_path)
  input_path  <- file.path(app_path, "input")
  output_path <- file.path(app_path, "output")
  base_path   <- get_path_to("input")
  
  # Load log and obtain path to base data files if base_id provided
  base_id <- app_info$base_id
  
  if (!all(base_id == "/")) {
    log <- load_log(app_period = app_info$period)
    
    ids <- get_valid_opts(log = log, arg = "id")
    base_id <- match.arg(base_id, ids)

    base_resume_data_path = log[log$id == base_id,]$resume_data_path
    base_cover_data_path = log[log$id == base_id,]$cover_data_path
    
  } else {
    base_resume_data_path = file.path(base_path, "resume_data.xlsx")
    base_cover_data_path = file.path(base_path, "cover_data.xlsx")
  }

  # Store important identifiers
  app_id       <- app_info$id
  name         <- str_to_filename(app_info$name, sep = "")
  current_date <- as.character(Sys.Date())
  out_id       <- paste0(name, "_", app_id)
  
  # Build job metadata
  app_df <- data.frame(
    id              = app_id,
    base_id         = base_id,
    period          = app_info$period,
    company         = app_info$company,
    position        = app_info$position,
    portal_url      = app_info$portal_url,
    posting_url     = app_info$posting_url,
    linkedin_url    = app_info$linkedin_url,
    recruiter_email = app_info$recruiter_email,
    
    status          = {"ipr"},
    date_created    = current_date,
    last_updated    = current_date,
    date_applied    = "/",
    date_seq        = date_seq,
    notes           = app_info$notes,
    
    period_path     = period_path,
    app_path        = app_path,
    input_path      = input_path,
    output_path     = output_path,
    
    log_path         = as_filename(period_path, "log",         "rds",        ),
    metadata_path    = as_filename(app_path,    "metadata",    "yml",  app_id),
    qa_path          = as_filename(app_path,    "qa",          "yml",  app_id),
    posting_path     = as_filename(input_path,  "posting",     "txt",  app_id),
    resume_data_path = as_filename(input_path,  "resume_data", "xlsx", app_id),
    cover_data_path  = as_filename(input_path,  "cover_data",  "xlsx", app_id),
    
    base_resume_data_path = base_resume_data_path,
    base_cover_data_path  = base_cover_data_path,
    resume_path           = as_filename(output_path, "resume", "pdf", out_id),
    cover_path            = as_filename(output_path, "cover",  "pdf", out_id),
    resume_plain_path     = as_filename(output_path, "resume", "txt", out_id),
    cover_plain_path      = as_filename(output_path, "cover",  "txt", out_id),
    email_path            = as_filename(output_path, "email",  "txt", out_id),
    
    keyword_counts_posting_path = as_filename(
      output_path, "keyword_counts",       "csv", app_id
    ),
    skill_counts_posting_path   = as_filename(
      output_path, "skill_counts_posting", "csv", app_id
    ),
    skill_counts_resume_path    = as_filename(
      output_path, "skill_counts_resume",  "csv", app_id
    ),
    skill_report_path           = as_filename(
      output_path, "skill_report",         "csv", app_id
    ),
    report_path                 = as_filename(
      output_path, "report",               "txt", app_id
    )
  )
  return(app_df)
}


# Access -----------------------------------------------------------------------


#' Load the log for a given application period.
#' 
#' @examples
#' #
#' 
#' @family data
#' @export
load_log <- function(
    app_period = "latest",
    app_dir = "applications"
) {
  apps_path <- get_path_to(app_dir)
  
  # Get valid periods
  period_dirs <- list(fs::dir_ls(apps_path))
  app_periods <- sapply(period_dirs, function(path) basename(path))
  
  # Locate/validate period
  if (app_period == "latest") {
    app_period <- max(app_periods)
  
  } else if (!(app_period %in% app_periods)) {
    app_period <- match.arg(app_period, app_periods)
  }
  
  # Load
  log_filepath <- file.path(apps_path, app_period, "log.rds")
  # alert_reading_from(log_filepath)
  if (file.exists(log_filepath)) {
    log <- readRDS(log_filepath)
  } else {
    warn_file_missing(log_filepath, apps_path)
  }
  
  return(log)
}


# TODO: if returning status maybe compute days_since_app col for all "applied"
# (and hide "last_updated").

#' Access application metadata.
#'
#' @description
#' `get_app_info` retrieves requested info for a given application from its log.
#' 
#' `get_status_report` returns a table of proportions for each status.
#' 
#' `get_app_path_to` returns an abs path to a doc from a given application.
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
    app_period = "latest",
    app_dir = "applications"
) {
  # Load log
  log <- load_log(app_period = app_period, app_dir = app_dir)
  
  # Get valid args
  ids <- get_valid_opts(log = log, arg = "id")
  fields <- get_valid_opts(log = log, arg = "field")
  
  # Validate args
  if (any(id %in% c("all", "latest"))) { 
    id <- match.arg(id)
  } else {
    id <- match.arg(id, ids, several.ok = TRUE)
  }
  if (!(any(field == "all"))) {
    field <- match.arg(field, fields, several.ok = TRUE)
  }
  status <- match.arg(status)
  
  # Filter id
  if (all((id == "all"))) {
    app_df = log
  } else if (all(id == "latest")) {
    app_df <- get_latest_entry(log = log)
  } else {
    app_df = log[log$id %in% id,]
  }
  
  # Filter status
  if (!(all(status == "all"))) {
    app_df = app_df[app_df$status %in% status,]
  }
  
  # Select fields
  if (all(field == "all")) {
    # Return only non-path and non-url fields by default
    app_df <- app_df %>%
      select(., !tidyselect::ends_with(c(
        "path", "url", "email", "date_seq", "period", "base_id"
      )))
  } else {
    app_df <- app_df %>% 
      select(., dplyr::any_of(c("id", field))) 
  }
  
  # Truncate long strings in the data frame for at-a-glance viewing
  if (all(field == "all")) {
    columns_to_truncate <- c("company", "position")
    # columns_to_truncate <- c("company", "position", "notes")
    
    app_df[columns_to_truncate] <- lapply(
      app_df[columns_to_truncate], function(col) {
        sapply(col, truncate_string, max_length = 15)
      }
    ) 
  }
  return(app_df)
}


#' @rdname get_app_info
#' 
#' @export
get_status_report <- function(app_period = "latest") {
  df <- get_app_info(field = c("company", "status"), app_period = app_period)
  # df_company <- df$company
  df_company <- df$id
  df_status <- df$status
  
  n_apps <- length(df_status)
  # status <- validate_status(several.ok = TRUE)
  status <- unique(df_status)
  
  counts <- as.numeric(lapply(status, function(x) sum(x == df_status)))
  ids <- lapply(
    status,
    function(x) paste(df_company[x == df_status], collapse = ", ")
  )
  
  fraction <- paste0(counts, "/", n_apps)
  proportion <- paste0(round(counts / n_apps * 100, 0), "%")
  
  report <- as.data.frame(cbind(status, fraction, proportion, ids))
  report$ids <- sapply(report$ids, truncate_string, max_length = 70)
  return(report)
}


# TODO: deprecate this! Redundant

#' @rdname get_app_info
#' 
#' @export
get_app_path_to <- function(
    id = "latest",
    doc = "resume",
    app_period = "latest"
) {
  # assert_that(length(doc) == 1)
  
  field <- paste0(doc, "_path")
  path <- get_app_info(id = id, field = field, app_period = app_period) %>% 
    select(-.data$id)
  path <- as.character(path)

  return(path)
}


# Modify -----------------------------------------------------------------------


#' Open, edit, and remove application documents.
#' 
#' @description
#' `open_app` opens documents (resume, by default) for a given application.
#' 
#' `edit_app` opens spreadsheet data files for a given application for editing.
#' 
#' `edit_base` opens base spreadsheet data files for editing.
#' 
#' `delete_app` deletes the application folder and log entry for a given id.
#' 
#' @family cli
#' @export
open_app <- function(
    doc = "resume",
    id = "latest",
    app_period = "latest"
) {
  log <- load_log(app_period = app_period)
  
  # Get valid args and validate
  ids <- get_valid_opts(log = log, arg = "id")
  docs <- get_valid_opts(log = log, arg = "doc")
  
  doc <- match.arg(doc, docs)
  if (any(id %in% c("all", "latest"))) { 
    id <- match.arg(id) 
  } else {
    id <- match.arg(id, ids) 
  }
  
  # Load app data
  app_df <- if (all(id == "latest")) {
    get_latest_entry(log = log)
  } else {
    log[log$id == id,]
  }
  base_dir <- app_df$app_path
  # alert_reading_from(base_dir)
  
  # Open url
  if (stringr::str_ends(doc, "url")) {
    url <- as.character(app_df[doc])
    
    fs::file_show(url)
    cli::cli_alert_success(paste("Opening:", url))
    return(invisible(FALSE))
    
  # If "url" suffix not given, "path" suffix is implied
  } else {
    path <- as.character(app_df[paste0(doc, "_path")])
  }
  
  # Error if file does not exist at path
  if (!(file.exists(path))) {
    dir <- app_df$app_path
    warn_file_missing(
      path, base_dir,
      action = "aborting, see directory tree below"
    )
    fs::dir_tree(dir)
    return(invisible(FALSE))
  
  # Open csv files in R if they exist
  } else if (fs::path_ext(path) == "csv") {
    df <- utils::read.csv(path)
    alert_opening(path, base_dir)
    # View(df)
    
    if (stringr::str_detect(doc, "report")) {
      report_skill_metrics(df)
      df <- sort_skill_report(df)
    }
    
    return(df %>% select(-.data$X))

  # Open all other file types (pdf, xlsx, txt, yaml) in the default apps
  } else {
    alert_opening(path, base_dir)

    # Display the excel formula to load the report manually if needed
    if (grepl("resume_data", path)) {
      path_dir <- fs::path_dir(path)
      report_path <- file.path(path_dir, "report.xlsx")
      report_length <- nrow(readxl::read_excel(report_path))
      
      message(paste0(
        "Excel formula to load the report into 'resume_data.xlsx': \n",
        "='", fs::path_dir(path), "/[report.xlsx]Sheet1'", 
        "!$A1:$B", report_length + 1
      ))
    }
    
    fs::file_show(path)
    return(invisible(FALSE))
  }
}


# TODO: add log as arg to open_app to prevent 2 calls to load_log

#' @rdname open_app
#' 
#' @export
edit_app <- function(id = "latest", app_period = "latest") {
  open_app(doc = "qa", id = id, app_period = app_period)
  open_app(doc = "posting", id = id, app_period = app_period)
  open_app(doc = "cover_data", id = id, app_period = app_period)
  open_app(doc = "resume_data", id = id, app_period = app_period)
}


#' @rdname open_app
#' 
#' @export
edit_base <- function() {
  path <- get_path_to(dir = "input")
  filepaths <- c(
    file.path(path, "qa.yml"),
    file.path(path, "cover_data.xlsx"),
    file.path(path, "resume_data.xlsx")
  )

  for (path in filepaths) {
    alert_opening(path)
    fs::file_show(path)
  }
}


#' @rdname open_app
#'
#' @export
delete_app <- function(app_id, log = load_log()) {
  valid_ids <- get_valid_opts(log = log, arg = "id")
  app_id <- match.arg(app_id, valid_ids)
  
  app_path <- log[log$id == app_id,]$app_path
  log_path <- log[log$id == app_id,]$log_path
  base_dir <- log[log$id == app_id,]$period_path
  
  # Filter to-be-deleted row from log
  log <- log %>% dplyr::filter(., .data$id != app_id)
  
  # Warn
  cli::cli_inform(paste0(
    "You have requested to delete application '", app_id, "'. ",
    "If you continue, the following will be ",
    cli::col_red("permanently deleted"), ":"
  ))
  cli::cli_bullets(c(
    "x" = paste0(
      "Entry '", cli::col_red(app_id),
      "' from ", fs::path_rel(log_path, start = get_path_to("applications"))
    ),
    "x" = paste0(
      "Directory ", cli::col_red(fs::path_rel(app_path, start = base_dir)), 
      "/ and all of its contents."
    )
  ))
  
  cli::cli_text("")
  prompt <- paste(
    "Do you wish to proceed?",
    "Enter 'YES' if you are certain",
    "you want to delete these files.",
    "Enter 'NO' to exit: "
  )
  
  response <- ""
  while (response != "YES") {
    response <- readline(prompt)
    
    if (response == "NO") {
      message("Exiting...")
      return(invisible(FALSE))
    }
  }
  
  # Overwrite
  cli::cli_bullets(c(
    "x" = paste0(
      "Deleting entry '", cli::col_red(app_id),
      "' from ", fs::path_rel(log_path, start = get_path_to("applications"))
    ),
    "x" = paste0(
      "Deleting directory ", 
      cli::col_red(fs::path_rel(app_path, start = base_dir)), 
      "/ and all of its contents."
    )
  ))
  
  saveRDS(log, file = log_path)
  fs::dir_delete(app_path)
}


# Build ------------------------------------------------------------------------


# TODO: probably don't overwrite this (redundant with id validation)

#' Store metadata for a new application.
#' 
#' @description
#' `write_app_metadata` saves the metadata for a new application.
#' 
#' `write_log_entry` records the current app in the log for the present period.
#'
#' @param app_df A data frame containing metadata for a new job application
#' (defaults to the value returned by `construct_app_metadata()`,
#' which constructs the new log entry from the base metadata yaml file).
#' Fields must match those in the log file for the current period.
#'
#' @examples
#' data(example_job_metadata)
#' app_df <- construct_app_metadata(app_info = example_job_metadata)
#' # write_app_metadata(app_df = app_df)  # error: the folder doesn't exist
#'
#' @family build-dev
#' @export
write_app_metadata <- function(app_df = construct_app_metadata()) {
  # If app_df constructed by construct_app_metadata, will fail on invalid id
  if (all(app_df == FALSE)) { return(invisible(TRUE)) }
  validate_status(app_df$status)
  
  # Sanitize metadata of path info, retaining only user-relevant fields
  app_path <- app_df$app_path
  base_dir <- app_path
  metadata_filepath <- app_df$metadata_path
  
  app_df <- app_df %>%
    dplyr::select(., -dplyr::ends_with("path"))
  
  # Check that folder exists
  if (!dir.exists(app_path)) {
    warn_folder_missing(app_path, action = "aborting, must build app first")
    return(invisible(FALSE))
  }
  
  # Write
  metadata_exists <- file.exists(metadata_filepath)
  if (metadata_exists) {
    warn_file_exists(metadata_filepath, base_dir, action = "overwriting")
  }
  
  fs::file_create(metadata_filepath)
  con <- file(metadata_filepath, "w")
  yaml::write_yaml(app_df, con)
  close(con)
  
  if (!metadata_exists) {
    alert_file_created(metadata_filepath, base_dir) 
  }
  return(invisible(FALSE))
}


#' @rdname write_app_metadata
#' 
#' @examples
#' data(example_job_metadata)
#' app_df <- construct_app_metadata(app_info = example_job_metadata)
#' # write_log_entry(app_df = app_df)  # error: the folder doesn't exist
#' 
#' @export
write_log_entry <- function(app_df = construct_app_metadata()) {
  # If app_df constructed by construct_app_metadata, will fail on invalid id
  if (all(app_df == FALSE)) { return(invisible(TRUE)) }
  validate_status(app_df$status)
  
  log_filepath <- app_df$log_path
  new_app_id <- app_df$id
  base_dir <- app_df$app_path
  
  # Create the log if it doesn't exist
  if (!file.exists(log_filepath)) {
    log <- app_df
    saveRDS(log, file = log_filepath)
    alert_file_created(log_filepath, base_dir)
    return(invisible(FALSE))
  }

  log <- readRDS(log_filepath) %>%
    arrange(., .data$date_created, .data$date_seq)
  
  # Verify uniqueness of id
  id_exists <- new_app_id %in% log$id
  id_is_valid <- validate_id(log, app_df)
  if (!id_is_valid) {
    return(invisible(FALSE)) 
  }
    
  # Write entry
  if (id_exists) {
    log[log$id == new_app_id,] <- app_df
    
    warn_file_exists(
      log_filepath, base_dir,
      action = paste0("updating entry for '", new_app_id, "'")
    )
    saveRDS(log, file = log_filepath)
    return(invisible(FALSE))
  }
  log <- log %>% add_row(., app_df)
  
  
  saveRDS(log, file = log_filepath)
  alert_file_created(
    log_filepath, base_dir,
    action = paste0("writing entry for '", new_app_id, "'")
  )
  return(invisible(FALSE))
}


# TODO: allow app_df to be constructed from metadata (pass id; i.e., rebuild)

#' Create the file tree and data files for the present application.
#' 
#' @examples
#' # Complete end-to-end example (build -> (edit) -> render -> check) ----------
#' in_tmp_env({
#'   message("1. Building project directory...")
#'   build_base_directory()
#'   
#'   data(example_job_metadata)
#'   app_df <- construct_app_metadata(app_info = example_job_metadata)
#'   
#'   message("")
#'   message("2. Building application...")
#'   build_app_directory(app_df = app_df, open = FALSE)
#'   
#'   message("")
#'   message("3. Rendering application...")
#'   render_app(cover = FALSE, email = FALSE)
#'   
#'   message("")
#'   message("4. Checking keywords...")
#'   report_df <- check_skills()
#'   print(report_df)
#' })
#' 
#' @family cli
#' @export
build_app_directory <- function(
    app_df = construct_app_metadata(), 
    open = TRUE
) {
  # If app_df constructed by construct_app_metadata, will fail on invalid id
  if (all(app_df == FALSE)) { return(invisible(TRUE)) }
  
  df <- app_df
  base_dir <- df$app_path
  
  # TODO: just handle id validation in construct_app_meta/validate_id
  # Verify uniqueness of id (in case non-default app_df value given)
  log_exists <- file.exists(df$log_path)
  if (log_exists) {
    if (!validate_id(log = load_log(), df)) { return(invisible(FALSE)) }
  }
  cli::cli_text("")
  alert_writing_to(base_dir)
  
  # Create input/output folders
  cli::cli_text("")
  cli::cli_rule("Creating directory tree")
  
  dir_paths <- c(df$input_path, df$output_path)
  create_folders(
    target_paths = dir_paths,
    base_dir = base_dir
  )  
  # Copy base data files
  cli::cli_text("")
  cli::cli_rule("Copying base data files into directory")
  
  source_paths <- c(df$base_resume_data_path, df$base_cover_data_path)
  target_paths <- c(df$resume_data_path, df$cover_data_path)
  copy_files(
    source_paths = source_paths,
    target_paths = target_paths,
    base_dir = base_dir
  )
  
  # Write metadata
  cli::cli_text("")
  cli::cli_rule("Writing application metadata")
  
  write_app_metadata(df)
  write_log_entry(df)
  
  source_path <- file.path(get_path_to("input"), "qa.yml")
  target_path <- df$qa_path
  if (file.exists(source_path)) {
    copy_files(
      source_paths = source_path,
      target_paths = target_path,
      base_dir = base_dir
    ) 
  }
  
  # Download posting and create skill report
  cli::cli_text("")
  cli::cli_rule("Downloading job posting and building a skill report")
  
  download_webpage_txt(
    df$posting_url, output_filepath = df$posting_path, base_dir = base_dir
  )
  if (open) { open_doc_and_wait(path = df$posting_path, base_dir = base_dir) }
  
  check_skills(app_id = df$id, log = df, orderby = "doc", check_resume = FALSE)
  
  cli::cli_text("")
  open_app(doc = "app", id = app_df$id, app_period = app_df$period)
}


#' Create the file tree and necessary data files to begin building a resume.
#' 
#' @examples
#' in_tmp_env({
#'   build_base_directory()
#' })
#' 
#' @family cli
#' @export
build_base_directory <- function() {
  # Set environment path variables (ask, then set "ROOT" and set as 'here'),
  # as well as Rproject and Rprofile files (sourced to complete path config).
  cli::cli_text("")
  cli::cli_rule("Setting project paths")
  
  set_project_paths()
  root_path <- Sys.getenv("ROOT")
  create_rproject(root_path = root_path, project_name = "mycv")
  
  cli::cli_text("")
  alert_writing_to(root_path)
  
  # Create directory tree
  cli::cli_text("")
  cli::cli_rule("Creating directory tree")
  
  dir_paths <- lapply(
    c("notebooks", "src", "input", "output", "applications"), 
    function(dir) get_path_to(dir)
  )
  create_folders(
    target_paths = dir_paths,
    base_dir = root_path
  )
  
  # Copy base data files
  cli::cli_text("")
  cli::cli_rule("Writing skeleton data files")
  
  temp_path <- get_path_to("templates")
  targ_path <- get_path_to("input")
  
  source_paths <- c(
    file.path(temp_path, "template_resume_data.xlsx"),
    file.path(temp_path, "template_cover_data.xlsx"),
    file.path(temp_path, "template_job_metadata.yml")
  )
  target_paths <- file.path(
    targ_path, stringr::str_split_i(basename(source_paths), "template_", 2)
  )
  copy_files(
    source_paths = source_paths, target_paths = target_paths,
    base_dir = root_path
  )
  
  # Copy resume-building notebooks
  cli::cli_text("")
  cli::cli_rule("Writing resume-building notebooks")
  
  targ_path <- Sys.getenv("ROOT")

  source_paths <- c(
    file.path(temp_path, "template_resume.Rmd"),
    file.path(temp_path, "template_cv.Rmd")
    # file.path(temp_path, "template_cover.Rmd")
  )
  target_paths <- file.path(
    targ_path, stringr::str_split_i(basename(source_paths), "template_", 2)
  )
  copy_files(
    source_paths = source_paths, target_paths = target_paths,
    base_dir = root_path
  )
  
  # Notify user which files to edit first (resume, cover, job)
  fs::file_show(root_path)
}


# Update -----------------------------------------------------------------------


#' Update application metadata.
#'
#' @description
#' Update the log entry for a particular application as well its metadata file.
#'
#' `update_app_info` updates the log and metadata from user-supplied data.
#' 
#' `update_datestamp` auto-updates the application datestamp upon rendering.
#' 
#' `apply_to` sets the `date_applied` datestamp and `status` to applied.
#' 
#' @param id The unique identifier associated with the application of interest.
#' @param ... Key-value pairs of the fields to update and their new values.
#' If none are provided, the log is updated directly from the metadata file.
#'
#' @family cli
#' @export
update_app_info <- function(app_id = "latest", ..., log = load_log()) {
  updates <- list(...)

  valid_ids <- get_valid_opts(log = log, arg = "id")
  app_id <- match.arg(app_id, valid_ids)
  
  # Load metadata file
  old_entry <- if (all(app_id == "latest")) {
    get_latest_entry(log = log)
  } else {
    log[log$id == app_id,]
  }
  # old_entry <- log[log$id == app_id,]
  metadata_path <- old_entry$metadata_path
  metadata <- yaml::read_yaml(metadata_path)

  # TODO: possibly add "mutable" option to get_valid_opts
  # Get available fields to modify and filter out immutable ones
  fields <- names(metadata)
  immutable_fields <- c(
    "id", "base_id", "period", "company", "position", 
    "date_created", "date_seq"
  )
  
  # Retrieve updates from file if no key-value pairs provided
  update_from_yaml <- length(updates) == 0
  if (update_from_yaml) {
    valid_fields <- setdiff(names(metadata), immutable_fields)
    updates = metadata[valid_fields]
  }
  requested_fields_to_update <- names(updates)
  
  # Don't allow updating of date_applied if it has already been set
  if ("date_applied" %in% requested_fields_to_update & 
      old_entry$date_applied != "/") {
    immutable_fields <- c(immutable_fields, "date_applied")
  }
  valid_fields <- setdiff(fields, immutable_fields)
  
  # Validate fields
  fields_to_update <- match.arg(
    requested_fields_to_update, valid_fields, several.ok = TRUE
  )
  
  # Report skipped fields
  skipped_fields <- setdiff(requested_fields_to_update, fields_to_update)
  skip_reason <- ifelse(
    skipped_fields %in% fields, "immutable", "does not exist"
  )
  if (length(skipped_fields) > 0) {
    cli::cli_li(paste0(
      "Skipping field: '", skipped_fields, "' (", skip_reason, ")"
    ))
  }
  
  # Validate status if provided
  if ("status" %in% fields_to_update) {
    updates[["status"]] <- validate_status(updates[["status"]])
  }
  
  # Filter out fields that have not changed
  old_elements <- old_entry[fields_to_update]
  new_elements <- updates[fields_to_update]
  
  modified_fields_mask <- sapply(old_elements != new_elements, any)
  modified_fields <- names(old_elements[modified_fields_mask])
  
  unmodified_elements <- old_elements[modified_fields]
  modified_elements <- new_elements[modified_fields]
    
  # Construct updated entry
  new_entry <- old_entry
  new_entry[modified_fields] = modified_elements
  
  if (length(modified_elements) > 0) {
    cli::cli_li(paste0(
      "Modifying field: ", cli::col_blue(names(modified_elements)),
      " (", unname(unmodified_elements), " -> ", unname(modified_elements), ")"
    ))
  }
  
  # Return if no elements modified
  if (length(modified_elements) == 0) {
    cli::cli_alert_success(
      paste0("All fields are up-to-date. Synchronizing metadata and log...")
    )
  }
  cli::cli_text("")
  
  # Update log entry and metadata file. Do this even if updating from file 
  # to correct modification by users of immutable fields.
  write_app_metadata(app_df = new_entry)
  write_log_entry(app_df = new_entry)
}


#' @rdname update_app_info
#'
#' @export
update_datestamp <- function(app_id = "latest", log = load_log()) {
  current_date <- as.character(Sys.Date())
  update_app_info(app_id = app_id, last_updated = current_date, log = log)
}


#' @rdname update_app_info
#'
#' @export
apply_to <- function(app_id = "latest", log = load_log()) {
  current_date <- as.character(Sys.Date())
  update_app_info(
    app_id = app_id, 
    status = "applied", 
    date_applied = current_date, 
    log = log
  )
}


# Helpers ----------------------------------------------------------------------


#' Convert a string to a valid file name.
#'
#' @family build-dev
#' @export
str_to_filename <- function(string, sep = "-", lower = TRUE) {
  stringr::str_to_lower(stringr::str_replace_all(string, " ", sep))
}


# TODO: rename this parts_to_path or make_path

#' Construct a file name given a folder path, base name, id, and extension.
#'
#' @family build-dev
#' @export
as_filename <- function(path, basename, ext, id = NA, sep = "_") {
  suffix <- ifelse(is.na(id), ".", paste0(sep, id, "."))
  file.path(path, paste0(basename, suffix, ext))
}


#' Shorten long strings in a data frame for neater printing.
#'
#' @family build-dev
#' @export
truncate_string <- function(str, max_length = 20) {
  if (nchar(str) > max_length) {
    return(paste0(substr(str, 1, max_length - 3), "..."))
  } else {
    return(str)
  }
}


#' Verify that matching job identifiers belong to the same job.
#'
#' @family build-dev
#' @export
validate_id <- function(log = load_log(), app_df = construct_app_metadata()) {
  # If app_df constructed by construct_app_metadata, will fail on invalid id
  # TODO: resolve circular dependency (workaround for construct_app_meta call
  # OR run validation AFTER meta constructed).
  # if (all(app_df == FALSE)) { return(invisible(TRUE)) }
  new_app_id <- app_df$id
  
  id_exists <- new_app_id %in% log$id
  if (!id_exists) {
    return(TRUE)
  }

  matching_row <- log[log$id == new_app_id,]
  if (all(matching_row$company == app_df$company) & 
      all(matching_row$position == app_df$position)) {
    return(TRUE)
  }
  
  alert <- paste0(
    "Identifier '", app_df$id,
    "' is not unique (aborting, see existing identifiers below)"
  )
  cli::cli_alert_danger(alert)
  cli::cli_li(cli::col_blue(log$id))
  return(FALSE)
}


#' Verify that the supplied status is valid.
#'
#' @family build-dev
#' @export
validate_status <- function(
    status = c(
      "all", "ipr", "applied", "interviewed", 
      "rejected", "interviewed_then_rejected", "closed"
    ),
    several.ok = FALSE
) {
  status <- match.arg(status, several.ok = several.ok)
  return(status)
}


#' Obtain a set of valid options for indexing a job log by a given argument.
#' 
#' @family build-dev
#' @export
get_valid_opts <- function(log = load_log(), arg = c("id", "field", "doc")) {
  arg <- match.arg(arg)
  
  opts <- switch(
    arg,
    id = c("latest", log$id),
    field = names(log),
    doc = names(log)
  )
  if (arg == "doc") {
    opts <- opts[stringr::str_ends(opts, stringr::regex("path|url"))] %>% 
      stringr::str_replace_all("_path", "")
  }
  return(opts)
}


#' Obtain the latest entry in an application log.
#' 
#' @family build-dev
#' @export
get_latest_entry <- function(log = load_log()) {
  latest_date <- max(log$date_created)
  latest_entries <- log[log$date_created == latest_date,]
  app_df <- latest_entries[which.max(latest_entries$date_seq),]
  
  return(app_df)
}


#' Copy files and create folders from lists of source and target paths.
#' 
#' @description
#' `copy_files` copies files from source to corresponding target paths.
#' 
#' `create_folders` creates folders at the given target paths.
#'
#' @param source_paths a char vector of corresponding absolute source paths.
#' @param target_paths a char vector of target paths.
#' @param base_dir the root folder from which to report command line messages.
#'
#' @family build-dev
#' @export
copy_files <- function(source_paths, target_paths, base_dir = ".") {
  assert_that(length(source_paths) == length(target_paths))
  
  for (i in seq_along(source_paths)) {
    if (file.exists(target_paths[i])) {
      warn_file_exists(target_paths[i], base_dir)
    } else {
      fs::file_copy(source_paths[i], target_paths[i])
      alert_file_created(target_paths[i], base_dir)
    }
  }
}


#' @rdname copy_files
#'
#' @export
create_folders <- function(target_paths, base_dir = ".") {
  for (target_path in target_paths) {
    if (dir.exists(target_path)) {
      warn_folder_exists(target_path, base_dir)
    } else {
      fs::dir_create(target_path)
      alert_folder_created(target_path, base_dir)
    }
  }
}


# Testing helpers --------------------------------------------------------------


#' Establish a temporary working tree for testing app-building functionality.
#' 
#' This function creates a temporary `autocv` project directory with a
#' working tree mirroring that of the user's, which is defined in their
#' .Rprofile file. This temporary directory serves as an environment 
#' for testing the following application-building functionality:
#' * building applications
#' * rendering applications
#' * accessing application logs
#' * running application keyword checks
#' 
#' @examples
#' # Set temporary path environment variables ----------------------------------
#' in_tmp_env(message("Temporary root directory: ", Sys.getenv("ROOT")))
#' message("Reset root directory: ", Sys.getenv("ROOT"))
#' 
#' in_tmp_env(message("Temporary templates loc: ", get_path_to("templates")))
#' message("Reset templates loc: ", get_path_to("templates"))
#'
#' # Wrap multi-line expressions -----------------------------------------------
#' in_tmp_env({
#'   x <- c(1, 2, 3)
#'   for (i in x) {
#'     print(i)
#'   }
#' })
#' 
#' # Handle input/output operations in the temporary environment ---------------
#' in_tmp_env({
#'   tmpdir <- get_path_to("input")
#'   if (!dir.exists(tmpdir)) { 
#'     dir.create(tmpdir, recursive = TRUE) 
#'   }
#'   
#'   fil <- file.path(tmpdir, "test.txt")
#'   con <- file(fil, open = "w")
#'   writeLines("this is a test", con = con)
#'   close(con)
#'   
#'   cat(paste0("In ", cli::col_blue(fil), ":\n", readLines(fil)))
#' })
#'
#' @family build-dev
#' @export
in_tmp_env <- function(expr) {
  tmp_root <- tempdir()

  # Run the provided code (expr) with the temporary paths set
  result <- withr::with_envvar(new = c(ROOT = tmp_root), {
    eval(substitute(expr), envir = parent.frame())
  })
  unlink(tmp_root)
}


#' Open an application document and pause execution until a key is entered.
#'
#' @family build-dev 
#' @export
open_doc_and_wait <- function(
    path,
    base_dir = ".",
    prompt = "Enter y to continue: "
) {
  action <- "edit as desired"
  alert_opening(path = path, base_dir = base_dir, action = action)
  fs::file_show(path)
  
  response <- ""
  while (tolower(response) != "y") {
    response <- readline(prompt)
  }
  message("Continuing...")
}


#' Establish the root directory when building a new project.
#'
#' @family build-dev 
#' @export
set_project_paths <- function() {
  prompt <- paste0(
    "Set your desired project path relative to your current path: "
  )
  current_path <- Sys.getenv("ROOT", here::here())
  root_exists <- FALSE
  
  # Request new path
  while (!root_exists) {
    message("\nYour current path is: ", current_path, "/")
    
    response <- readline(prompt)
    root_entered <- file.path(current_path, as.character(response))
    root_exists <- dir.exists(root_entered)
    message("\nYou entered the path: ", root_entered)
    
    if (!root_exists) { 
      message("The path entered does not exist. Try again.")
    }
  }
  
  # Set root if valid path provided
  message("\nSetting your project root to: ", root_entered)
  Sys.setenv(ROOT = root_entered)
  here::set_here(path = Sys.getenv("ROOT"))
  
  # Establish and set i/o paths accordingly
  root_path <- Sys.getenv("ROOT")
  
  create_rprofile(root_path = root_path)
  rprofile_path <- file.path(Sys.getenv("ROOT"), ".Rprofile")
  source(rprofile_path)
}


#' Write a default .Rprofile to a new project directory.
#'
#' @family build-dev 
#' @export
create_rprofile <- function(root_path = here::here()) {
  rprofile_content <- glue::glue(
    "# Set project root",
    "Sys.setenv(ROOT = '{root_path}')",
    "",
    "# Set input/output paths relative to the project root directory",
    "Sys.setenv(SRC_DIR = './R')",
    "Sys.setenv(INPUT_DIR = './R/input')",
    "Sys.setenv(OUTPUT_DIR = './R/output')",
    "Sys.setenv(APPLICATIONS_DIR = './R/applications')",
    "Sys.setenv(NOTEBOOKS_DIR = '.')",
    .sep = "\n"
  )
  
  rprofile_path <- file.path(root_path, ".Rprofile")
  
  if (file.exists(rprofile_path)) {
    warn_file_exists(rprofile_path, root_path)
    return(invisible(FALSE))
  }
  writeLines(rprofile_content, con = rprofile_path)
  alert_file_created(rprofile_path, root_path)
}


#' Write a default .Rproject file to a new project directory and open.
#'
#' @examples
#' create_rproject(root_path = tempdir(), project_name = "test_project")
#' 
#' @family build-dev
#' @export
create_rproject <- function(root_path, project_name) {
  rproject_content <- glue::glue(
    "Version: 1.0",
    "",
    "RestoreWorkspace: Default",
    "SaveWorkspace: Default",
    "AlwaysSaveHistory: Default",
    "",
    "EnableCodeIndexing: Yes",
    "UseSpacesForTab: Yes",
    "NumSpacesForTab: 2",
    "Encoding: UTF-8",
    "",
    "RnwWeave: knitr",
    "LaTeX: pdfLaTeX",
    .sep = "\n"
  )
  
  rproject_filename <- paste0(project_name, ".Rproj")
  rproject_path <- file.path(root_path, rproject_filename)
  
  if (file.exists(rproject_path)) {
    warn_file_exists(rproject_path, root_path)
    return(invisible(FALSE))
  }
  writeLines(rproject_content, con = rproject_path)
  alert_file_created(rproject_path, root_path)
  
  # rstudioapi::openProject(rproject_path, newSession = TRUE)
}


# One-off tests ----------------------------------------------------------------

# log <- load_log()
# app_df <- construct_app_metadata()
# write_app_metadata(app_df)
# write_log_entry(app_df)
# build_app_directory()
