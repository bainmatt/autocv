# Helper functions for printing cv built with R pagedown

library(dplyr)
library(ggplot2)

# TODO: remove extraneous imports
# library(glue)
# library(tidyr)
# library(purrr)
# library(stringr)


#' Construct a bar chart of skills.
#' 
#' @references
#' Strayer N (2020). datadrivencv. R package version 0.1.0.
#' 
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#' library(ggplot2)
#' data("example_skill_data", package = "autocv")
#' technical_skills <- filter(example_skill_data, key_competency == "x")
#' build_skill_bars(technical_skills)
#' 
#' @family build
#' @export
build_skill_bars <- function(skill_data) {
  skill_data %>%  
    ggplot2::ggplot(aes(x = reorder(.data$skill, .data$level), y = 5)) +
    geom_col(fill = "lightgrey") +
    geom_col(aes(
      x = reorder(.data$skill, .data$level), 
      y = .data$level), 
      fill = "darkgrey") +
    coord_flip() +
    geom_text(
      aes(label = .data$skill, y = 0.25),
      hjust = 0, size = 12, color = "white"
    ) +
    expand_limits(y = c(0, 5)) +
    labs(x = NULL, y = NULL) + 
    theme_void() +
    theme(
      panel.background = element_rect(fill = "transparent", colour = NA),
      plot.background  = element_rect(fill = "transparent", colour = NA)
    ) 
}


#' Print a section from a position dataframe to markdown.
#' 
#' @family print
#' @export
print_section <- function(
    position_data, 
    section_id = c(
      "work", "education", "certifications", "projects", 
      "writing", "publications", "volunteering", "additional_info"
    ),
    target = c("base", "app")
) {
  section_id <- match.arg(section_id)
  target     <- match.arg(target)
  
  # Filter
  position_data <- position_data %>% 
    filter(
      .data$section == section_id & (
        (target == "app" & .data$include == "x") |
        (target == "base" & .data$in_base %in% c("x", "~"))
        # (target == "base" & .data$in_base == "x")
      )
    )
  
  # Construct
  position_data %>%
    # filter(.data$section == section_id & .data$include == "x") %>%
    
    # Prepare formatted links. Note: Since this is an added field that is not
    # present in the base pagedown template, we will manually omit NA entries.
    # TODO: Put this & below in prep func along w/ latex (prepare NA fields).
    mutate(
      link = ifelse(is.na(.data$formatted_link), "", .data$formatted_link)
    ) %>% 
    # Replace all instances of "NA" with "N/A" so pagedown hides these items
    mutate(
      across(
        where(is.character),
        ~ ifelse(is.na(.), "N/A", .)
      )
    ) %>% 
    rowwise() %>% 
    mutate(markdown_output = glue(
      # "### {title} {link}",
      # TODO: add link formatting to prepare_links
      "### {title} <span style='font-weight: normal;'>{link}</span>",
      "\n\n",
      "{institution}",
      "\n\n",
      "{loc}",
      "\n\n",
      "{timeline}",
      "\n\n",
      "{description_bullets}",
      "\n\n\n"
    )) %>% 
    pull(.data$markdown_output) %>% 
    cat(sep = "\n")
}
