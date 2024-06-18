# Update the resume html and pdf file in one script

library(dplyr)

# source("R/paths.R")
# source('R/plain.R')
# source('R/preprocess.R')


# TODO accept period, job_id as args to construct path to input/output, suffix


#' Build an informal HTML-format resume based on spreadsheet data.
#' 
#' @family cli
#' @export
render_cv_as_html <- function(
    input_filename = "cv.Rmd",
    data_filename = "resume_data.xlsx",
    output_basename = "cv",
    input_dir = "notebooks",
    data_dir = "input",
    output_dir = "output",
    stylesheets = list("custom_resume.css", "styles_html.css")
) {
  name <- load_job_info("name")
  suffix <- paste0("_", str_to_filename(name, separator = ""))
  output_filename <- paste0(output_basename, suffix, ".html")
  
  input_filepath <- file.path(get_path_to(input_dir), input_filename)
  output_filepath <- file.path(get_path_to(output_dir), output_filename)
  
  custom_css <- sapply(
    stylesheets,
    function(sheet) fs::path_package("autocv", "extdata", "css", sheet)
  )
  css <- c(
    custom_css, 
    "resume",
    # Source styles for dynamic development (comment out before build)
    file.path(get_path_to("extdata"), "css/custom_resume.css"),
    file.path(get_path_to("extdata"), "css/styles_html.css")
  )
  
  rmarkdown::render(
    input = input_filepath,
    output_file = output_filepath,
    output_options = list(css = css, self_contained = TRUE),
    params = list(doctype = "HTML", resume_data_filename = data_filename)
  )
  fs::file_show(output_filepath)
}


#' Create a pdf export of a web-formatted resume based on spreadsheet data.
#' 
#' @family cli
#' @export
render_cv_as_pdf <- function(
    input_filename = "cv.Rmd",
    data_filename = "resume_data.xlsx",
    output_basename = "cv",
    input_dir = "notebooks",
    data_dir = "input",
    output_dir = "output",
    stylesheets = list("custom_resume.css", "styles_pdf.css")
) {
  # Prep
  name <- load_job_info("name")
  suffix <- paste0("_", str_to_filename(name, separator = ""))
  output_filename <- paste0(output_basename, suffix, ".pdf")
  
  input_filepath <- file.path(get_path_to(input_dir), input_filename)
  output_filepath <- file.path(get_path_to(output_dir), output_filename)
  
  custom_css <- sapply(
    stylesheets,
    function(sheet) fs::path_package("autocv", "extdata", "css", sheet)
  )
  css <- c(
    custom_css, 
    "resume",
    # Source styles for dynamic development (comment out before build)
    file.path(get_path_to("extdata"), "css/custom_resume.css"),
    file.path(get_path_to("extdata"), "css/styles_pdf.css")
  )
  
  # Knit the PDF version to a temporary html location
  tmp_html_cv_loc <- fs::file_temp(ext = ".html")
  rmarkdown::render(
    input = input_filepath,
    output_file = tmp_html_cv_loc,
    output_options = list(css = css, self_contained = TRUE),
    params = list(doctype = "PDF", resume_data_filename = data_filename)
  )
  
  # Convert to PDF using Pagedown
  pagedown::chrome_print(
    input = tmp_html_cv_loc,
    output = output_filepath
  )
  fs::file_show(output_filepath)
}


#' Build a LaTeX-rendered pdf of a resume based on spreadsheet data.
#' 
#' @family cli
#' @export
render_resume <- function(
    input_filename = "resume.Rmd",
    data_filename = "resume_data.xlsx",
    output_basename = "resume",
    input_dir = "notebooks",
    data_dir = "input",
    output_dir = "output",
    stylesheets = list("custom_resume.tex")
) {
  # Prep
  name <- load_job_info("name")
  suffix <- paste0("_", str_to_filename(name, separator = ""))
  output_filename <- paste0(output_basename, suffix, ".pdf")
  
  input_filepath <- file.path(get_path_to(input_dir), input_filename)
  output_filepath <- file.path(get_path_to(output_dir), output_filename)
  
  custom_tex <- sapply(
    stylesheets,
    function(sheet) fs::path_package("autocv", "extdata", "latex", sheet)
  )
  # Source styles for dynamic development (comment out before build)
  custom_tex <- file.path(get_path_to("extdata"), "latex/custom_resume.tex")
  
  rmarkdown::render(
    input = input_filepath,
    output_file = output_filepath,
    output_options = list(
      latex_engine = "xelatex",
      keep_tex = FALSE,
      includes = list(in_header = custom_tex)
    ),
    params = list(resume_data_filename = data_filename)
  )  
  fs::file_show(output_filepath)
}


#' Build an plain text resume based on spreadsheet data.
#' 
#' @family cli
#' @export
render_resume_plain <- function(
    input_filename = "plain.Rmd",
    data_filename = "resume_data.xlsx",
    output_basename = "resume",
    input_dir = "notebooks",
    data_dir = "input",
    output_dir = "output"
) {
  # Prep
  name <- load_job_info("name")
  suffix <- paste0("_", str_to_filename(name, separator = ""))
  output_filename <- paste0(output_basename, suffix, ".txt")
  
  input_filepath <- file.path(get_path_to(input_dir), input_filename)
  output_filepath <- file.path(get_path_to(output_dir), output_filename)
  # output_filepath <- file.path(get_path_to("output"), "resume_matthewbain.txt")
  
  
  
  position_data <- load_application_data(
    filename = "resume_data.xlsx",
    sheet = "entries",
    skip = 1
  ) %>% preprocess_entries(., style = "txt")
  
  output_text <- print_txt_section(position_data = position_data)
  
  writeLines(output_text, output_filepath)
  # cat(paste("Output created:", output_filepath, "\n"))
  cat(sprintf("\033[35mOutput created: %s\n\033[0m", output_filepath))
  
  fs::file_show(output_filepath)
}


#' Run each resume/CV rendering option in sequence. 
#'
#' @family cli
#' @export
render_all <- function() {
  render_cv_as_html()
  # render_cv_as_pdf()
  render_resume()
  render_resume_plain()
}
