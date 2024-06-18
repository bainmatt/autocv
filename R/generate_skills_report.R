# Produce a report containing frequencies of key terms found in a file.

library(fs)
library(tidyr)
library(dplyr)
library(utils)
library(writexl)

# source("R/paths.R")


#' Escape special characters in a string for use in regex.
#'
#' @family report-utils
#' @noRd
escape_special_chars <- function(string) {
  gsub("([\\.|\\^|\\$|\\*|\\+|\\?|\\(|\\)|\\[|\\{|\\|])", "\\\\\\1", string)
}


#' Check for term matches embedded in text.
#'
#' @param term Term to match against.
#' @param term Text to check.
#' 
#' @family report-utils
#' @export
match_pattern <- function(term, text) {
  term <- escape_special_chars(term)
  patterns <- c(
    # Whole-word match
    paste0("\\b", term, "\\b")
  )
  sum(sapply(
    patterns,
    function(pattern) grepl(pattern, text, ignore.case = TRUE)
  ))
}


# TODO for each skill in output with n occurrences: if contained in any other 
# term with m occurrences (as a whole word match), subtract n from m

# TODO output skills in the order they first occur in the posting (`bysource`)
# TODO add option to sort skills output by count (`bycount`)


#' Count keyword occurrences in a file based on a supplied list.
#' 
#' @examples
#' # Get resume counts ---------------------------------------------------------
#' 
#' # Load example data
#' data("example_posting", package = "autocv")
#' terms <- readLines(autocv_resources("resources/skill_list.txt"))
#' 
#' posting_counts <- count_terms(
#'   terms = terms,
#'   document = example_posting, 
#'   order = "bysource"
#' )
#'
#' posting_counts
#' 
#' @family report
#' @export
count_terms <- function(
    terms,
    counts = NA,
    document,
    order = c("bysource", "bycount")
) {
  order <- match.arg(order)
  
  # Strip markdown characters, trailing whitespace, blank lines,
  # and all but the unique lines from term list.
  terms <- terms %>% 
    gsub("#", "", .) %>% 
    trimws(.) %>% 
    .[. != ""] %>% 
    unique(.)
  
  # Collect and count all matches in the document
  matches <- numeric(length(terms))
  for (i in seq_along(terms)) {
    matches[i] <- sum(match_pattern(terms[i], document))
  }
  
  # Concatenate terms/counts from file1 with matches in file2
  if (is.na(counts)) {
    matches_df <- dplyr::bind_cols(terms[matches > 0], matches[matches > 0])
    colnames(matches_df) <- c("terms", "counts")

  } else {
    matches_df <- dplyr::bind_cols(
      terms[counts > 0], counts[counts > 0], matches[counts > 0]
    )
    colnames(matches_df) <- c("terms", "counts", "matches")
    
    matches_df <- matches_df %>% 
      arrange(desc(.data$counts), desc(.data$matches))
  }
  
  return(matches_df)
}


# Main -------------------------------------------------------------------------

#' Obtain keyword counts for a given document/term list and generate reports.
#' 
#' @description
#' `run_skill_count` returns keyword counts for a given document/term list.
#' 
#' `check_skills` obtains skill counts for a posting and creates a resume report
#' 
#' @family report
#' @export
run_skill_count <- function(
    app_id,
    app_period = "latest",
    document = c("posting", "resume"),
    term_list_filename = c("skill_list.txt", "keyword_list.txt"),
    term_list_dir = "resources"
) {
  document <- match.arg(document)
  term_list_filename <- match.arg(term_list_filename)
  
  # Get paths
  output_prefix <- stringr::str_split_i(term_list_filename, "_", 1)
  output_filepath <- get_path_to_app(
    id = app_id, app_period = app_period, 
    document = paste0(output_prefix, "_counts_", document)
  )
  term_list_filepath <- file.path(
    get_path_to(term_list_dir), term_list_filename
  )
  if (document == "resume") { 
    input_file = "resume_plain" 
  } else {
    input_file = document
  }
  input_filepath <- get_path_to_app(
    id = app_id, app_period = app_period, document = input_file
  )

  # Load
  term_list <- readLines(term_list_filepath)
  input <- readLines(as.character(input_filepath))
  
  # Compute counts
  output_df <- count_terms(terms = term_list, document = input)
  
  # Save
  con <- file(output_filepath, "w")
  write.csv(output_df, con)
  close(con)
  
  return(output_df)
}


#' Generate a skill report for a given job application.
#' 
#' @family report
#' @export
run_skill_report <- function(
    app_id,
    app_period = "latest"
) {
  # Get paths
  output_filepath <- get_path_to_app(
    id = app_id, app_period = app_period, 
    document = "skill_report"
  )
  skill_counts_posting_filepath <- get_path_to_app(
    id = app_id, app_period = app_period, 
    document = "skill_counts_posting"
  )
  skill_counts_resume_filepath <- get_path_to_app(
    id = app_id, app_period = app_period, 
    document = "skill_counts_resume"
  )

  # Load
  skill_counts_posting <- read.csv(skill_counts_posting_filepath)
  skill_counts_resume <- read.csv(skill_counts_resume_filepath)
  
  # Join counts on posting terms
  output_df <- dplyr::left_join(
    skill_counts_posting, skill_counts_resume, by = "terms"
  ) %>% 
    select(c("terms", "counts.x", "counts.y")) %>% 
    mutate(counts.y = replace(.data$counts.y, is.na(.data$counts.y), 0))
  
  colnames(output_df) <- c("terms", "counts", "matches")
  
  # Save
  con <- file(output_filepath, "w")
  write.csv(output_df, con)
  close(con)
  
  return(output_df)
}


#' @rdname run_skill_count
#' 
#' @export
check_skills <- function(
    app_id,
    app_period = "latest",
    document = list("posting", "resume"),
    term_list_filename = c("skill_list.txt", "keyword_list.txt"),
    term_list_dir = "resources"
) {
  # TODO: report success/failures inside relevant function

  # Posting vs term list counts
  run_skill_count(app_id = app_id, document = "posting")

  # Resume vs term list counts
  run_skill_count(app_id = app_id, document = "resume")

  # Posting vs keyword list counts
  run_skill_count(
    app_id = app_id, document = "posting", term_list_filename = "keyword"
  )
  
  # Resume vs posting counts
  run_skill_report(app_id = app_id, app_period = app_period)
}



# Helpers ----------------------------------------------------------------------

# # TODO: finish or discard this
# 
# load_term_counts <- function(
#     template = "skill_list.txt",
#     term_list_dir = NA,
#     term_list_filepath = NA
# ) {
#   # Use template term list unless a specific directory is provided
#   if (is.na(term_list_dir)) {
#     term_list_filepath <- fs::path_package(
#       "autocv", "extdata/resources", term_list_filename
#     )
#   } else {
#     term_list_filepath <- file.path(
#       get_path_to(term_list_dir), term_list_filename
#     )
#   }
#   file1 <- readLines(term_list_filepath)
# }


# TODO: probably discard the following 2 functions

#' Save term counts.
#' 
#' @description 
#' `save_as_txt` saves term counts to a .txt file.
#' 
#' `save_as_xlsx` saves term counts to a .csv file.
#' 
#' @family report-utils
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
  write_xlsx(counts_df, output_filepath, col_names = TRUE)
  close(con)
}


# One-off tests ----------------------------------------------------------------

# # Posting vs term list counts
# run_skill_count(app_id = "CT", document = "posting")
# 
# # Resume vs posting counts
# run_skill_count(app_id = "CT", document = "resume")
# 
# # Posting vs keyword list counts
# run_skill_count(
#   app_id = "CT", document = "posting", term_list_filename = "keyword"
# )


# Skills aggregate vs term list counts
# count_terms(
#   input_filename = "skillsofvalue.txt",
#   output_filename = "skill_counts_skillsofvalue.txt",
#   order = "bysource",
#   make_report = FALSE
# )
