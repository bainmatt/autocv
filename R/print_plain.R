# Build plain text resume file using data in spreadsheets

library(dplyr)

# TODO: remove extraneous imports
# library(tibble)
# library(stringr)


#' Print sections of a plain text resume.
#' 
#' @family print
#' @export
print_txt_section <- function(position_data) {
  included_data <- position_data %>%
    filter(.data$include == "x") %>%
    # Format and sort data by start date in descending order
    mutate(
      start = convert_date_(.data$start),
      end = convert_date_(.data$end)
    ) %>%
    arrange(desc(.data$start))
  
  # Iterate over each row and construct output text
  output_text <- ""
  for (i in 1:nrow(included_data)) {
    row <- included_data[i, ]
    section <- row$section
    
    # Construct row text
    row_text <- paste(
      row$start, "-", row$end, "\n",
      row$institution, " - ", row$loc, "\n",
      row$title,
      ifelse(is.na(row$description_1), "", "\n\n+ "),
      paste(na.omit(
        c(
          row$description_1, 
          row$description_2, 
          row$description_3, 
          row$description_4, 
          row$description_5)
        ), 
        collapse = "\n+ "), "\n\n",
      sep = ""
    )
    
    # Append row text to output, adding section header if necessary
    # FIXME: streamline this
    if (i == 1 || section != included_data[i - 1, "section"]) {
      output_text <- paste(
        output_text,
        "\n",
        toupper(section), 
        "\n\n==============================\n\n", sep = ""
      )
    }
    output_text <- paste(output_text, row_text, sep = "")
  }
  return(output_text)
}


# Helper function to convert date format
convert_date_ <- function(date_str) {
  ifelse(is.na(date_str), "Present", format(as.Date(date_str), "%b %Y"))
}


# position_data <- load_application_data(
#   filename = "resume_data.xlsx",
#   sheet = "entries",
#   skip = 1
# ) %>% preprocess_entries(., style = "latex", bullet_style = "-")
# print_latex_section(position_data, "work")
