# Helper functions for printing ATS-friendly resume built with LaTeX

library(glue)
library(dplyr)


#' Build a two-column table of skills.
#'
#' @description Returns a string of text that can be rendered as LaTeX.
#'
#' @param skill_data A data frame containing skill data.
#' @returns A LaTeX string.
#'
#' @examples
#' # Define some data ----------------------------------------------------------
#' library(dplyr, warn.conflicts = FALSE)
#' skill <- c("R", "SQL", "Excel", "Pandas", "Feature Engineering")
#' alias <- c("Coding", "Coding", "Data Analysis", "Data Analysis", "ML")
#' skill_data <- dplyr::bind_cols(alias, skill)
#' colnames(skill_data) <- c("alias", "skill")
#' print(skill_data)
#'
#'
#' # Render --------------------------------------------------------------------
#' skill_table <- build_skill_table(skill_data)
#' paste(skill_table)
#'
#' # With a larger dataset -----------------------------------------------------
#' data("example_skill_data", package = "autocv")
#' skill_table <- build_skill_table(example_skill_data)
#' paste(skill_table)
#'
#' @family build
#' @export
build_skill_table <- function(skill_data) {
  skill_table <- paste0(
    "\\begin{tabular}{@{}p{0.475\\linewidth}p{0.475\\linewidth}@{}}\n"
  )
  sections <- unique(skill_data$alias)
  for (i in seq_along(sections)) {
    category <- sections[i]
    skills <- skill_data$skill[skill_data$alias == category]
    skill_table <- paste(
      skill_table, 
      "\\textbf{", category, ":} ", 
      paste(skills, collapse = ", "), 
      sep = ""
    )
    if (i %% 2 == 0) {
      skill_table <- paste(skill_table, "\\\\", sep = "")
    } else {
      skill_table <- paste(skill_table, "&", sep = "")
    }
  }
  skill_table <- paste(skill_table, "\\end{tabular}\n\n", sep = "") 
  return(skill_table)
}


#' Print a section from a position dataframe to LaTeX.
#' 
#' @family print
#' @export
print_latex_section <- function(
    position_data, 
    section_id = c(
      "work", "education", "certifications", "projects", 
      "writing", "publications", "volunteering", "additional_info"
    ),
    short_entries = FALSE,
    target = c("app", "base"),
    padding = 1,
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
      # Handle missing entry elements
      # TODO: From print_latex_section: prepare_na_fields
      dates = if_else(
        !is.na(.data$timeline), glue("\\textit{{{timeline}}}"), ""
      ),
      inst = if_else(
        !is.na(.data$institution), glue("{institution}"), ""
      ),
      loc = if_else(
        !is.na(.data$loc), glue("\\textit{{{loc}}}"), ""
      ),
      bullets = if_else(
        !is.na(.data$description_bullets), glue("{description_bullets}"), ""
      ),
      link = if_else(
        !is.na(.data$formatted_link), glue("{formatted_link}"), ""
      ),
      
      # Handle spacing around entry elements
      # TODO: From print_latex_section: prep_padding: args txt size/scale/short
      row_id = row_number(),
      no_inst = .data$inst == "",
      no_loc = .data$loc == "",
      no_inst_or_loc = (.data$no_inst & .data$no_loc),
      no_bullets = grepl("^\\s*$", .data$bullets),
      no_link = .data$link == "",
      
      short_entry_comma_1 = if_else(.data$no_inst, "", ", "),
      short_entry_comma_2 = if_else(.data$no_loc, "", ", "),
      short_entry_comma_3 = if_else(.data$no_link, "", ", "),
      
      entry_prepadding = if_else(
        .data$row_id != min(.data$row_id), 
        "\\vspace{3pt}",
        "\\vspace{-7pt}"
      ),
      bullets_prepadding = if_else(
        .data$no_inst_or_loc,
        "\\vspace{-15pt}\n\n",
        "\n\n"
      ),
      entry_postpadding = if_else(
        .data$no_bullets & .data$row_id != max(.data$row_id), 
        "\\vspace{-7pt}\n\n",
        "\n\n"
      ),
      
      # Concatenate
      latex_output = case_when(
        short_entries == TRUE ~ glue(
          "{entry_prepadding}",
          # Underline work entries
          # TODO: From print_latex_section: format_heading (args ul/ital)
          ifelse(
            section_id == "work",
            "**\\uline{{{title}}}**",
            "**{title}**"
          ),
          "{link}",
          "{short_entry_comma_1}",
          "{inst}",
          "{short_entry_comma_2}",
          "{loc}",
          # "{short_entry_comma_3}",
          # "{link}",
          "\\hfill{dates}",
          "\\newline\n",
          "\\vspace{{-15pt}}\n\n",
          "{bullets}",
          "{entry_postpadding}"
        ),
        
        short_entries == FALSE ~ glue(
          "{entry_prepadding}",
          # Underline work entries
          ifelse(
            section_id == "work",
            "**\\uline{{{title}}}**{link}",
            "**{title}**{link}"
          ),
          "\\hfill{dates}",
          "\\newline\n",
          "{inst}",
          "\\hfill{loc}",
          "{bullets_prepadding}",
          "{bullets}",
          "{entry_postpadding}"
        )
      )
    ) %>% 
    pull(.data$latex_output) %>% 
    cat(sep = "\n")
}
