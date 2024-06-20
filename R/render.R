# Update the resume html and pdf file in one script

library(dplyr)

# source("R/paths.R")
# source('R/plain.R')
# source('R/preprocess.R')


# TODO: accept period, job_id as args to construct path to input/output, suffix


#' @rdname render_resume
#' 
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
  name <- load_job_info(field = "name")
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
    params = list(
      doctype = "HTML", 
      resume_data_filename = data_filename,
      target = "base"
    )
  )
  fs::file_show(output_filepath)
}


#' @rdname render_resume
#' 
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
    params = list(
      doctype = "PDF", 
      resume_data_filename = data_filename,
      target = "base"
    )
  )
  
  # Convert to PDF using Pagedown
  pagedown::chrome_print(
    input = tmp_html_cv_loc,
    output = output_filepath
  )
  fs::file_show(output_filepath)
}


#' Construct a resume based on spreadsheet data.
#' 
#' @description
#' `render_cv_as_html` constructs an informal HTML-format resume.
#' 
#' `render_cv_as_pdf` converts an HTML-format resume to PDF.
#' 
#' `render_resume` constructs a formal, ATS-compatible, LaTeX-style resume.
#' 
#' `render_resume_plain` constructs a plain text resume for analysis.
#'
#' @family cli
#' @export
render_resume <- function(
    target = c("app", "base"),
    input_filename = "resume.Rmd",
    data_filename = "resume_data.xlsx",
    output_basename = "resume",
    input_dir = "notebooks",
    data_dir = "input",
    output_dir = "output",
    stylesheets = list("custom_resume.tex"),
    app_id = "latest",
    app_period = "latest",
    app_dir = "applications"
) {
  # Validate arguments
  target <- match.arg(target)
  
  # Ensure the following arguments are defined if loading tailored app
  if (target == "app") {
    assert_that(all(!is.na(c(app_id, app_period, app_dir))))
    
  } else if (target == "base") {
    cli::cli_li("note: args 'app_id', 'app_period', 'app_dir' unused")
  }
  
  # Get path to application data
  # if (target == "app") {
  #   doc = fs::path_ext_remove(filename)
  #   data_filepath <- get_app_path_to(
  #     id = app_id, 
  #     doc = doc, 
  #     app_dir = app_dir, 
  #     app_period = app_period
  #   )
  #   
  # } else if (target == "base") {
  #   data_filepath <- file.path(get_path_to(data_dir), filename)
  # }
  # 
  
  
  
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
    params = list(
      resume_data_filename = data_filename,
      target = target
    )
  )  
  fs::file_show(output_filepath)
}


#' @rdname render_resume
#' 
#' @export
render_resume_plain <- function(
    target = c("app", "base"),
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
    target = target,
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
