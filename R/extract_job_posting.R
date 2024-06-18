library(fs)
library(cli)
library(here)
library(rvest)
library(pagedown)


#' Save the text from a webpage url.
#' 
#' @description 
#' `download_webpage_txt` saves output to a .txt file.
#' 
#' `download_webpage_pdf` saves output to a .pdf file.
#' 
#' @param url The web address of the job posting.
#' @param output_file The desired output filepath.
#' 
#' @examples
#' library(rvest, warn.conflicts = FALSE)
#' url <- "https://en.wikipedia.org/wiki/R_(programming_language)"
#' fil <- tempfile("file", tempdir(), fileext = ".txt")
#' download_webpage_txt(url, fil)
#' readLines(fil, n = 1)
#' 
#' unlink(fil)
#' 
#' @family report-utils
#' @export
download_webpage_txt <- function(
    url, 
    output_filepath = NA,
    output_dir = "input",
    output_filename = "posting.txt"
) {
  if (is.na(output_filepath)) {
    output_filepath <- file.path(get_path_to(output_dir), output_filename)
  }
  
  if (fs::file_exists(output_filepath)) {
    cli::cli_alert_warning(
      paste0("'", fs::path_rel(output_filepath), "' already exists, skipping")
    )
    return(invisible(FALSE))
    
  } else {
    fs::file_create(output_filepath)
    cli::cli_alert_success(
      paste("posting file created at", fs::path_rel(output_filepath))
    )
  }
  
  html <- rvest::read_html(url) %>% 
    rvest::html_text() %>% 
    base::writeLines(., output_filepath)
  
  cli::cli_alert_success(
    paste0("writing posting to '", fs::path_rel(output_filepath), "'")
  )
}


#' @rdname download_webpage_txt
#' 
#' @examples
#' url <- "https://stat.ethz.ch/R-manual/R-devel/library/utils/html/download.file.html"
#' fil <- tempfile("file", tempdir(), fileext = ".pdf")
#' download_webpage_pdf(url, fil)
#' unlink(fil)
#' 
#' @export
download_webpage_pdf <- function(
    url, 
    output_filepath = NA,
    output_dir = "input",
    output_filename = "posting.pdf"
) {
  if (is.na(output_filepath)) {
    output_filepath = file.path(get_path_to(output_dir), output_filename)
  }

  pagedown::chrome_print(
    url, 
    output = output_filepath,
    format = "pdf",
    options = list(
      printBackground = FALSE,
      scale = 1,
      box_model = "border"
    ),
    timeout = 5
  )
}
