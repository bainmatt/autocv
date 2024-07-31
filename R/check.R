# Produce a report containing frequencies of key terms found in a file.

library(dplyr)
library(assertthat)


# Functions --------------------------------------------------------------------


#' Escape special characters in a string for use in regex.
#'
#' @family report-dev
#' @noRd
escape_special_chars <- function(string) {
  gsub("([\\.|\\^|\\$|\\*|\\+|\\?|\\(|\\)|\\[|\\{|\\|])", "\\\\\\1", string)
}


#' Find the number and position of terms matching a given term in a given text.
#'
#' @param term A string representing the term to be matched.
#' @param text A string representing the text to check for matches.
#' @return A named list with the following elements:
#' \describe{
#'   \item{count}{Integer, the number of matches.}
#'   \item{first_ix}{Integer, the position of the first match.}
#' }
#' 
#' @examples
#' text <- "This is a sample text with several sample words."
#' term <- "sample"
#' find_matches_and_positions(term = term, text = text)
#' 
#' @family report-dev
#' @export
find_matches_and_positions <- function(term, text, ignore.case = TRUE) {
  term <- escape_special_chars(term)
  
  # Seek whole-word matches in the flattened text
  patterns <- c(paste0("\\b", term, "\\b"))
  text <- paste(text, collapse = " ")
  
  # Find positions of any matches, ignoring -1 (NULL) values
  positions <- as.numeric(
    gregexpr(patterns, text, ignore.case = ignore.case)[[1]]
  )
  count <- sum(positions > 0)
  positions <- positions[positions > 0]
  
  return(list(count = count, positions = positions))
}


#' Prepare raw strings and entries in a keyword list for analysis.
#'
#' Strip markdown characters, trailing whitespace, blank lines,
#' and all but the unique lines from the supplied term list.
#' 
#' @param terms A character vector containing keywords.
#' @return A character vector containing only the unique, processed keywords.
#'
#' @examples
#' terms <- c("### ML", "", "#### Supervised Learning", "SVMs", "ML")
#' print(terms)
#' 
#' processed_terms <- prep_term_list(terms)
#' print(processed_terms)
#'
#' @family report-dev
#' @export
prep_term_list <- function(terms) {
  terms <- terms %>%
    gsub("#", "", .) %>%
    trimws(.) %>%
    .[. != ""] %>%
    unique(.)

  return(terms)
}


#' Count keyword occurrences in a file based on a supplied list.
#' 
#' @examples
#' # Load example data
#' data("example_posting", package = "autocv")
#' terms <- readLines(autocv_resources("resources/skill_list.txt"))
#' 
#' # Generate some random counts
#' counts <- sample(0:10, length(prep_term_list(terms)), replace = TRUE)
#' 
#' posting_counts <- count_terms(
#'   terms = terms,
#'   counts = counts,
#'   doc = example_posting,
#'   orderby = "doc",
#'   filterby = "both"
#' )
#' posting_counts
#' 
#' @family report
#' @export
count_terms <- function(
    terms,
    counts = NULL,
    doc,
    orderby = c("counts", "doc", "source"),
    filterby = c("both", "count", "matches")
) {
  orderby <- match.arg(orderby)
  filterby <- match.arg(filterby)
  terms <- prep_term_list(terms)
  if (!is.null(counts)) { assert_that(length(counts) == length(terms)) }
  
  # Collect and count all matches in the doc
  matches_df <- data.frame(
    
    # FIXME: cleaner solution for matches/position unlisting?
    
    term = terms,
    t(sapply(terms, function(term) {
      matches_and_positions <- find_matches_and_positions(term, doc)
      list(
        matches = unlist(matches_and_positions$count),
        position = unlist(dplyr::first(matches_and_positions$positions))
      )
    })),
    row.names = NULL
  )
  
  # FIXME: put this in function
  
  # For each skill with n occurrences: if contained in any other 
  # term with m occurrences (as a whole word match), subtract m from n.
  matches_df <- matches_df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      
      # FIXME: resolve contained_in list/duplicate_count num conversion/unlist
      
      contained_in = list(matches_df$term[!is.na(
        stringr::str_match(
          stringr::str_to_lower(matches_df$term),
          paste0("\\b", stringr::str_to_lower(.data$term), "\\b")
        )
      ) & matches_df$term != .data$term]),

      duplicate_count = sum(as.numeric(
        matches_df$matches[matches_df$term %in% unlist(.data$contained_in)]
      )),
      matches = .data$matches - .data$duplicate_count
    ) %>%
    dplyr::select(
      -.data$contained_in, -.data$duplicate_count
    ) %>%
    dplyr::ungroup()

  # Filter
  if (is.null(counts)) {
    matches_df <- matches_df %>% 
      dplyr::filter(.data$matches > 0)

  # Add counts column and filter
  } else {
    matches_df <- matches_df %>%
      dplyr::mutate(count = counts) %>%
      dplyr::relocate(.data$count, .after = .data$term)
    
    matches_df <- switch(
      filterby,
      count = matches_df %>% dplyr::filter(.data$count > 0),
      matches = matches_df %>% dplyr::filter(.data$matches > 0),
      both = matches_df %>% dplyr::filter(.data$count > 0 & .data$matches > 0)
    )
  }
  
  # Sort
  matches_df <- switch(
    orderby,
    source = matches_df,
    doc = matches_df %>% dplyr::arrange(as.numeric(.data$position)),
    counts = if (is.null(counts)) {
      matches_df %>% 
        dplyr::arrange(dplyr::desc(.data$matches))
    } else {
      matches_df %>% 
        dplyr::arrange(dplyr::desc(.data$count), dplyr::desc(.data$matches)) 
    }
  )
  matches_df <- matches_df %>% select(-.data$position)
  return(matches_df)
}


#' Display skill report info for at-a-glance viewing.
#'
#' @description
#' `report_skill_metrics` computes basic coverage statistics.
#'  
#' `sort_skill_report` arranges rows in a convenient viewing order.
#'
#' @family report
#' @export
report_skill_metrics <- function(skill_report_df) {
  coverage <- sum(skill_report_df$matches > 0) / nrow(skill_report_df) * 100
  
  cli::cli_text("")
  
  cli::cli_li(paste0(
    cli::col_cyan(paste0(round(coverage, 2), "%")),
    " of the posting keywords are in your resume."
  ))
  
  cli::cli_text("")
}


#' @rdname report_skill_metrics
#'
#' @export
sort_skill_report <- function(skill_report_df) {
  df_sorted <- skill_report_df %>% dplyr::arrange(
    dplyr::desc(.data$matches > 0)
    # dplyr::desc(.data$in_my_skill_set)
    # .data$term,
    # dplyr::desc(.data$count)
  )
  return(df_sorted)
}


# Main -------------------------------------------------------------------------


#' Obtain keyword counts for a given doc/term list and generate reports.
#' 
#' @description
#' `run_skill_count` returns keyword counts for a given document/term list.
#' 
#' `check_skills` runs `run_skill_count` on a posting and compares to a resume.
#' 
#' `count_terms_base` returns keyword counts for a base resume.
#' 
#' @family report
#' @export
run_skill_count <- function(
    app_id = "latest",
    doc = c("posting", "resume"),
    term_list_filename = c("skill_list.txt", "keyword_list.txt"),
    term_list_dir = "resources",
    log = load_log(),
    
    orderby = c("counts", "doc", "source"),
    filterby = c("both", "count", "matches"),
    overwrite = FALSE,
    is_posting_count = FALSE
) {
  doc <- match.arg(doc)
  term_list_filename <- match.arg(term_list_filename)
  
  # Get valid args and validate
  ids <- get_valid_opts(log = log, arg = "id")
  if (app_id != "latest") { app_id <- match.arg(app_id, ids) }
  
  # Get app data
  app_df <- if (app_id == "latest") {
    get_latest_entry(log = log)
  } else {
    log[log$id == app_id,]
  }
  
  # Get paths
  output_prefix <- stringr::str_split_i(term_list_filename, "_", 1)
  output_filename <- paste0(output_prefix, "_counts_", doc)
  
  if (doc == "resume") { 
    input_file = "resume_plain" 
  } else {
    input_file = doc
  }

  base_dir <- app_df$app_path
  # alert_writing_to(base_dir)
  
  output_filepath <- as.character(app_df[paste0(output_filename, "_path")])
  term_list_filepath <- file.path(
    get_path_to(term_list_dir), term_list_filename
  )
  input_filepath <- as.character(app_df[paste0(input_file, "_path")])
  
  # Check if the necessary files and folders exist
  output_path <- fs::path_dir(output_filepath)
  
  if (!dir.exists(output_path)) {
    warn_folder_missing(output_path, base_dir)
    return(invisible(FALSE))
  }
  
  if (file.exists(output_filepath)) {
    if (overwrite) {
      warn_file_exists(output_filepath, base_dir, action = "overwriting")
    } else {
      warn_file_exists(output_filepath, base_dir, action = "skipping")
      
      skill_counts <- utils::read.csv(output_filepath)
      print(skill_counts %>% select(-.data$X))
      return(invisible(FALSE))
    }
    
  } else {
    alert_file_created(output_filepath, base_dir)
  }

  # Load
  files <- c(term_list_filepath, input_filepath)
  for (file in files) {
    if (!file.exists(file)) {
      warn_file_missing(file, base_dir)
      return(invisible(FALSE))
    } 
  }
  term_list <- readLines(term_list_filepath)
  input <- readLines(as.character(input_filepath))
  
  # Compute counts
  output_df <- count_terms(
    terms = term_list, 
    doc = input,
    orderby = orderby,
    filterby = filterby
  )
  output_df$matches <- unlist(output_df$matches)
  
  # Save
  con <- file(output_filepath, "w")
  utils::write.csv(output_df, con)
  close(con)
  
  # Save as generic xlsx for dynamic conditional formatting
  if (is_posting_count) {
    xl_path <- file.path(app_df$input_path, "report.xlsx")
    
    con <- file(xl_path, "w")
    writexl::write_xlsx(x = output_df, path = xl_path, col_names = TRUE)
    close(con)
  }

  print(output_df, n = max(nrow(output_df), 50))
  return(output_df)
}


#' Generate a skill report for a given job application.
#' 
#' @family report
#' @export
run_skill_report <- function(
    app_id = "latest",
    log = load_log(),
    # TODO: clarify whether orderby left or right in join (doc implies source)
    orderby = c("source", "counts", "doc")
) {
  orderby <- match.arg(orderby)
  
  # Get valid args and validate
  ids <- get_valid_opts(log = log, arg = "id")
  if (app_id != "latest") { app_id <- match.arg(app_id, ids) }
  
  # Get app data
  app_df <- if (app_id == "latest") {
    get_latest_entry(log = log)
  } else {
    log[log$id == app_id,]
  }
  
  # Get paths
  output_filepath               <- app_df$skill_report_path
  skill_counts_posting_filepath <- app_df$skill_counts_posting_path
  skill_counts_resume_filepath  <- app_df$skill_counts_resume_path
  base_dir                      <- app_df$app_path
  
  # Load
  if (file.exists(skill_counts_posting_filepath)) {
    skill_counts_posting <- utils::read.csv(skill_counts_posting_filepath)
    
  } else {
    warn_file_missing(skill_counts_posting, base_dir)
    return(invisible(FALSE))
  }
  
  if (file.exists(skill_counts_resume_filepath)) {
    skill_counts_resume <- utils::read.csv(skill_counts_resume_filepath)
    
  } else {
    warn_file_missing(skill_counts_resume_filepath, base_dir)
    return(invisible(FALSE))
  }
  
  # Join counts on posting terms
  output_df <- dplyr::left_join(
    skill_counts_posting, skill_counts_resume, by = "term"
  ) %>%
    dplyr::mutate(
      dplyr::across(tidyselect::everything(), ~ ifelse(is.na(.), 0, .))
    ) %>%
    dplyr::select(c("term", "matches.x", "matches.y")) %>% 
    dplyr::mutate(
      matches.y = replace(.data$matches.y, all(is.na(.data$matches.y)), 0)
    )
  colnames(output_df) <- c("term", "count", "matches")
  
  # Add column for if the term is in your skill set and/or skill section
  skill_data <- load_application_data(
    filename = "resume_data.xlsx",
    sheet = "skills",
    app_id = app_id
    # skip = 2
  )
  my_skills <- c(
    skill_data$skill, unique(skill_data$category), unique(skill_data$alias)
  )
  # Obtain skills in resume and remove parenthetical acronyms
  skill_list <- skill_data %>%
    dplyr::filter(.data$include == "x") %>%
    dplyr::select(.data$skill) %>%
    dplyr::pull(.data$skill) %>%
    stringr::str_replace_all("\\s*\\([^\\)]*\\)\\s*", "")
  
  output_df <- output_df %>% 
    dplyr::mutate(
      in_my_skill_set = 
        tolower(skill_counts_posting$term) %in% tolower(my_skills),
      in_my_skill_list = 
        tolower(skill_counts_posting$term) %in% tolower(skill_list)
    )
  cli::cli_inform("Not in your skill set:")
  cli::cli_ol(sort(output_df[!output_df$in_my_skill_set,]$term))
  
  # Sort
  output_df_sorted <- sort_skill_report(output_df)
  if (orderby == "counts") {
    output_df <- output_df_sorted
  }
  
  # Save
  con <- file(output_filepath, "w")
  utils::write.csv(output_df, con)
  close(con)
  
  cli::cli_text("")
  alert_file_created(output_filepath, base_dir)
  
  report_skill_metrics(output_df)
  return(output_df_sorted)
}


# TODO: load log from app_period if log not provided

#' @rdname run_skill_count
#' 
#' @export
check_skills <- function(
    app_id = "latest",
    app_period = "latest",
    log = load_log(),
    orderby = c("doc", "counts", "source"),
    overwrite = FALSE,
    check_resume = TRUE
) {
  orderby <- match.arg(orderby)
  
  # Posting vs keyword list counts
  cli::cli_text("")
  cli::cli_rule(cli::col_blue(paste0(
    "Keyword check: Posting vs job terms list"
  )))
  run_skill_count(
    app_id = app_id, 
    doc = "posting", 
    term_list_filename = "keyword_list",
    log = log, 
    orderby = orderby, 
    overwrite = overwrite
  )
  
  # Posting vs term list counts
  cli::cli_text("")
  cli::cli_rule(cli::col_blue(paste0(
    "Keyword check: Posting vs data terms list"
  )))
  run_skill_count(
    app_id = app_id, 
    doc = "posting", 
    term_list_filename = "skill_list",
    log = log, 
    orderby = orderby, 
    overwrite = overwrite, 
    is_posting_count = TRUE
  )

  if (!check_resume) { return(invisible(FALSE)) }
  
  # Resume vs term list counts
  cli::cli_text("")
  cli::cli_rule(cli::col_blue(paste0(
    "Keyword check: Resume vs data terms list"
  )))
  run_skill_count(
    app_id = app_id, 
    doc = "resume", 
    log = log, 
    orderby = orderby, 
    overwrite = TRUE
  )
  
  # Resume vs posting report
  cli::cli_text("")
  cli::cli_rule(cli::col_blue(paste0(
    "Keyword report: Resume vs posting keywords"
  )))
  run_skill_report(
    app_id = app_id, 
    log = log, 
    orderby = orderby
  )
}


#' @rdname run_skill_count
#' 
#' @export
count_terms_base <- function(
    # target = c("base", "linkedin"),
    # input_basename = c("resume", "resume_linkedin"),
    input_dir = "output",
    term_list_filename = "skill_list.txt",
    term_list_dir = "resources",
    orderby = c("counts", "doc", "source"),
    filterby = c("both", "count", "matches"),
    use_abridged = FALSE
) {
  suffix_abridged <- ifelse(use_abridged, "_linkedin", "")
  
  term_list_filepath <- file.path(
    get_path_to(term_list_dir), term_list_filename
  )
  name <- load_job_info(field = "name")
  suffix <- paste0("_", str_to_filename(name, sep = ""))
  input_filename <- paste0("resume", suffix, suffix_abridged, ".txt")
  input_filepath <- file.path(get_path_to(input_dir), input_filename)
  
  files <- c(term_list_filepath, input_filepath)
  for (file in files) {
    if (!file.exists(file)) {
      # warn_file_missing(file, get_path_to(input_dir))
      warn_file_missing(file)
      return(invisible(FALSE))
    }
  }
  term_list <- readLines(term_list_filepath)
  input <- readLines(as.character(input_filepath))  
  
  # Compute counts
  output_df <- count_terms(
    terms = term_list, 
    doc = input,
    orderby = orderby,
    filterby = filterby
  )
  output_df$matches <- unlist(output_df$matches)
  
  # Add column for if the term is in your skill list
  skill_data <- load_application_data(
    target = "base",
    filename = "resume_data.xlsx",
    sheet = "skills"
  )
  
  # Obtain skills in resume and remove parenthetical acronyms
  skill_list <- skill_data %>%
    { if (use_abridged)
        dplyr::filter(., .data$in_profile)
      else
        dplyr::filter(., .data$in_base == "x")
    } %>%
    # dplyr::filter(.data$in_profile) %>%
    dplyr::select(.data$skill) %>%
    dplyr::pull(.data$skill) %>%
    stringr::str_replace_all("\\s*\\([^\\)]*\\)\\s*", "")
  
  # TODO: omit this
  # my_skills <- c(
  #   skill_data$skill, unique(skill_data$category), unique(skill_data$alias)
  # )

  output_df <- output_df %>% 
    dplyr::mutate(
      in_my_skill_list = 
        tolower(output_df$term) %in% tolower(skill_list)
    ) %>%
    dplyr::arrange(dplyr::desc(.data$in_my_skill_list))

  return(output_df)
}


# Helpers ----------------------------------------------------------------------


# TODO: probably discard the following 2 functions

#' Save term counts.
#' 
#' @description 
#' `save_as_txt` saves term counts to a .txt file.
#' 
#' `save_as_xlsx` saves term counts to a .csv file.
#' 
#' @family report-dev
#' @noRd
save_as_txt <- function(output_filepath, terms, counts) {
  # Write each term from file1 alongside its count in file2 (tab-delimited)
  con <- file(output_filepath, "w")
  for (i in seq_along(terms)) {
    line_with_count <- paste0(counts[i], "\t", terms[i])
    if (counts[i] > 0) {
      writeLines(line_with_count, con)
    }
  }
  close(con)
}


#' @rdname save_as_txt
#' 
#' @noRd
save_as_xlsx <- function(output_filepath, terms, counts) {  
  # Concatenate terms and counts where count is positive and save to file
  counts_df <- dplyr::bind_cols(terms[counts > 0], counts[counts > 0])
  colnames(counts_df) <- c("terms", "counts")
  
  # Save to file
  con <- file(output_filepath, "w")
  writexl::write_xlsx(counts_df, output_filepath, col_names = TRUE)
  close(con)
}


# One-off tests ----------------------------------------------------------------
