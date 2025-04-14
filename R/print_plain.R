# Build plain text resume file using data in spreadsheets

library(glue)
library(dplyr)


#' Print sections of a plain text resume.
#' 
#' @family print
#' @export
print_txt_section <- function(
    position_data, 
    section_id = c(
      "work", "education", "certifications", "projects", 
      "writing", "publications", "volunteering", "additional_info"
    ),
    target = c("app", "base"),
    use_abridged = FALSE
) {
  section_id <- match.arg(section_id)
  target     <- match.arg(target)
  
  # Filter
  position_data <- position_data %>% 
    filter(
      .data$section == section_id & (
        (target == "app" & .data$include == "x") |
        (target == "base" & .data$in_base == "x" & !use_abridged) |
        (target == "base" & .data$in_base %in% c("x", "~") & use_abridged)
      )
    )
  
  # Construct
  position_data <- position_data %>%
    # filter(.data$section == section_id & .data$include == "x") %>%
    mutate(
      # Format locations
      loc = if_else(is.na(.data$loc), "", glue(" - {loc}")),
      # loc = if_else(is.na(.data$loc), "", glue(" - {loc}", "\n\n")),

      # Remove additional newlines around entry elements lacking descriptions
      # TODO: put in prep func (prep_padding)
      no_inst_or_loc = (is.na(.data$institution) & is.na(.data$loc)),
      no_bullets = grepl("^\\s+$", .data$description_bullets),
      bullets_prepadding = if_else(.data$no_inst_or_loc, "", "\n\n"),
      entry_postpadding = ifelse(.data$no_bullets, "", "\n\n"),
      
      # Print
      txt_output = glue::glue(
        "{timeline}",
        "\n",
        "{institution}{loc}",
        "\n",
        "{title}{formatted_link}",
        "{bullets_prepadding}",
        "{description_bullets}",
        "{entry_postpadding}",
        .na = "",
        .trim = TRUE
      )
    )
  return(glue::glue_collapse(position_data$txt_output))
}


#' Print a formatted plain text resume header.
#' 
#' @examples
#' print_txt_header("this is my header")
#'
#' @family print
#' @export
print_txt_header <- function(header) {
  n_char <- stringr::str_length(header)
  
  formatted_header <- glue::glue(
    "\n\n\n\n",
    "{stringr::str_to_upper(header)}",
    "\n\n",
    "{strrep('=', n_char)}",
    "\n\n\n"
  )
  return(formatted_header)
}


# position_data <- load_application_data(
#   filename = "resume_data.xlsx",
#   sheet = "entries",
#   skip = 1
# ) %>% preprocess_entries(., style = "txt", bullet_style = "+")
# print_txt_section(position_data, "work")
