# Helper functions for preparing resume data for resume built with R pagedown

library(dplyr)
library(assertthat)

# Source helpers for dev
# source("R/paths.R")
# source("R/build.R")

# TODO: remove extraneous imports
# library(fs)
# library(yaml)
# library(glue)
# library(stats)
# library(tidyr)
# library(purrr)
# library(readxl)
# library(stringr)


#' Prepare bio. 
#' 
#' @param text_data A spreadsheet containing resume text data.
#' 
#' @family prepare
prepare_bio <- function(text_data) {
  # Build bio
  bio <- text_data %>% 
    filter(stringr::str_detect(.data$loc, "bio"), .data$include == TRUE) %>%
    arrange(.data$order) %>% 
    pull(.data$text) %>% 
    glue::glue_collapse(" ")
  
  # Initialize new row and add bio
  text_data <- text_data %>% 
    add_row(
      loc = "bio", 
      text = as.character(bio), 
      include = TRUE, 
      order = max(text_data$order) + 1
    )
  
  return(text_data)
}


#' Prepare dates. 
#' 
#' @param data A spreadsheet containing position data.
#' 
#' @family prepare
prepare_timeline <- function(
    data,
    order = c("chronological", "reversed")
) {
  order <- match.arg(order)
  
  data <- data %>%
    # Sort data by end date in descending order
    arrange(desc(.data$end)) %>%
    # Format dates: if month supplied, use mmm-YYYY format
    mutate(
      formatted_start_date = format(as.Date(.data$start), "%b %Y"),
      formatted_end_date = format(as.Date(.data$end), "%b %Y")
    ) %>%
    rowwise %>% 
    # Construct timeline for each entry based on start/end pairs provided
    mutate(
      timeline = dplyr::case_when(
        is.na(.data$start) & is.na(.data$end) ~ NA,
        is.na(.data$start) ~ formatted_end_date,
        is.na(.data$end) ~ ifelse(
          order == "chronological",
          glue('{formatted_start_date} -- Present'),
          glue('Present - {formatted_start_date}')
        ),
        # is.na(end) ~ glue('Present - {formatted_start_date}'),
        start == end ~ formatted_end_date,
        TRUE ~ ifelse(
          order == "chronological",
          glue('{formatted_start_date} -- {formatted_end_date}'),
          glue('{formatted_end_date} - {formatted_start_date}')
        )
      )
    )
  return(data)
}


#' Prepare properly formatted links with custom text.
#' 
#' @param style style of link, either "markdown", "latex", or "txt" (plain)
#' @param macro macro to use for "latex" references
#' 
#' @family prepare
prepare_links <- function(
    data,
    style = c("markdown", "latex", "txt"), 
    macro = c("myhref", "href", NA)
) {
  style <- match.arg(style)
  macro <- match.arg(macro)
  
  data <- data %>% 
    rowwise() %>% 
    mutate(
      formatted_link = dplyr::case_when(
        is.na(.data$link) ~ NA,
        style == "markdown" ~ stringr::str_c(
          "[[", .data$link_text, "](", .data$link, ")]"
        ),
        style == "latex" ~ stringr::str_c(
          " [\\", macro, "{", .data$link, "}{", .data$link_text, "}]"
        ),
        style == "txt" ~ stringr::str_c(
          " [", .data$link_text, ": ", .data$link, "]"
        )
        # .default = stop("`style` must be either 'markdown' or 'latex'")
      )
    )
  return(data)
}


#' Append skills to a description field with matching index.
#' 
#' @family prepare-utils
append_skills_to_bullets <- function(data, ix) {
  skill_prefix <- paste0("skill_", ix)
  description_col <- paste0("description_", ix)
  
  data <- data %>%
    rowwise() %>%
    mutate(
      # Concatenate all skill_ix columns, filtering out NA values
      skills_concat = stringr::str_c(
        na.omit(dplyr::c_across(dplyr::starts_with(skill_prefix))), 
        collapse = ", "
      ),
      !!rlang::sym(description_col) := if_else(
        .data$skills_concat != "",
        stringr::str_c(
          !!rlang::sym(description_col), " (", .data$skills_concat, ")"
        ),
        !!rlang::sym(description_col)
      )
    ) %>%
    ungroup() %>%
    # Remove temporary column
    select(-.data$skills_concat)
  
  return(data)
}


# TODO: simplify this...
# use tidyr::unite(tidyr::starts_with('description'), 
# col = "description_bullets", sep = "\n- ", na.rm = TRUE) as in:

# cv$entries_data %<>%
#   tidyr::unite(
#     tidyr::starts_with('description'),
#     col = "description_bullets",
#     sep = "\n- ",
#     na.rm = TRUE
#   ) %>%
#   dplyr::mutate(
#     description_bullets = paste0("- ", description_bullets)
#   )


#' Prepare entry descriptions. 
#' 
#' @family prepare
prepare_description_bullets <- function(
    data,
    bullet_style = c("-", "+")
) {
  bullet_style <- match.arg(bullet_style)
  
  data <- data %>% 
    mutate(id = dplyr::row_number()) %>%
    tidyr::pivot_longer(
      .,
      cols = dplyr::starts_with('description'),
      names_to = 'description_num',
      values_to = 'description'
    ) %>%
    filter(!is.na(
      .data$description) | .data$description_num == 'description_1'
    ) %>%
    group_by(.data$id) %>%
    mutate(
      descriptions = list(.data$description),
      no_descriptions = is.na(first(.data$description))
    ) %>% 
    ungroup() %>%
    filter(.data$description_num == 'description_1') %>%
    mutate(
      description_bullets = case_when(
        .data$no_descriptions ~ ' ',
        TRUE ~ purrr::map_chr(
          .data$descriptions, 
          ~paste(bullet_style, ., collapse = '\n')
        )
      )
    ) %>%
    select(-c(
      .data$description, .data$descriptions, 
      .data$no_descriptions, .data$description_num
    ))
  return(data)
}


# TODO: add latex/plain option here

#' Omit spreadsheet entries beginning with a preset prefix.
#' 
#' @family prepare-utils
omit_hidden_fields <- function(
    data,
    style = "markdown",
    prefix = "/"
  ) {
  pattern <- paste0("^", prefix)
  data <- data %>%
    mutate(across(
      dplyr::where(is.character),
      ~ ifelse(stringr::str_detect(., pattern), NA, .)
    ))
  return(data)
}


# Helpers ----------------------------------------------------------------------


#' Prepare individual entry in contact info card.
#' 
#' @description 
#' `make_markdown_contacts` prepares an individual markdown entry.
#' 
#' `make_latex_contacts` prepares an individual LaTeX entry.
#' 
#' `make_txt_contacts` prepares an individual plain text entry.
#' 
#' @param contact_data A data frame containing the contact data.
#' 
#' @family prepare-utils
make_markdown_contacts <- function(contact_data) {
  contact_data <- contact_data %>% 
    rowwise %>% 
    mutate(contact_text = if_else(
      is.na(.data$address),
      glue('- <i class="fa fa-{icon}"></i> {address_text}'),
      glue('- <i class="fa fa-{icon}"></i> [{address_text}]({address})')
    ))
  return(contact_data$contact_text)
}


#' @rdname make_markdown_contacts
#'
#' @param macro The desired LaTeX macro to use for rendering hyperlinks.
make_latex_contacts <- function(contact_data, macro) {
  contact_data <- contact_data %>% 
    rowwise %>% 
    mutate(contact_text = if_else(
      is.na(.data$address),
      .data$address_text,
      stringr::str_c(
        " \\", macro, "{", .data$address, "}{", .data$address_text, "}"
      )
    ))
  return(contact_data$contact_text)
}


#' @rdname make_markdown_contacts
#' 
make_txt_contacts <- function(contact_data) {
  contact_data <- contact_data %>% 
    rowwise %>% 
    mutate(contact_text = if_else(
      is.na(.data$address),
      .data$address_text,
      stringr::str_c(.data$loc, ": ", .data$address)
    ))
  return(contact_data$contact_text)
}


#' Sort skills.
#' 
#' @family prepare-utils
#' @export
sort_skills <- function(skill_data) {
  # Verify that required fields are present
  assert_that(all(c("category_id", "is_a_tool") %in% names(skill_data)))
  
  # Sort by tools/competencies (tools first) -> section id -> skill level
  skill_data <- skill_data %>% 
    filter(.data$include == "x") %>%
    arrange(.data$is_a_tool, .data$category_id, desc(.data$level))
  return(skill_data)
}


# Data validation --------------------------------------------------------------

# NOTE: probably skip this...far too tedious. 
# Just document templates + validate required fields


#' @noRd
#' 
validate_skills_data <- function() {
  
}


# Preprocessing pipeline -------------------------------------------------------


#' Load application data (resume or cover letter).
#' 
#' @family cli
#' @export
load_application_data <- function(
    target = c("app", "base"),
    filename = c("resume_data.xlsx", "cover_data.xlsx"),
    sheet = c("entries", "skills", "contact_info", "text_blocks"),
    data_dir = "input",
    skip = 1,
    app_id = "latest",
    app_period = "latest",
    app_dir = "applications"
) {
  # Validate arguments
  target <- match.arg(target)
  filename <- match.arg(filename)
  sheet <- match.arg(sheet)
  
  # Ensure the following arguments are defined if loading tailored app
  if (target == "app") {
    assert_that(all(!is.na(c(app_id, app_period, app_dir))))
    
  } else if (target == "base") {
    cli::cli_li("note: args 'app_id', 'app_period', 'app_dir' unused")
  }
    
  # Get path to application data
  if (target == "app") {
    doc = fs::path_ext_remove(filename)
    data_filepath <- get_app_path_to(
      id = app_id, 
      doc = doc, 
      app_dir = app_dir, 
      app_period = app_period
    )
    
  } else if (target == "base") {
    data_filepath <- file.path(get_path_to(data_dir), filename)
  }
  
  data <- readxl::read_excel(
    data_filepath, 
    sheet = sheet, 
    na = c("", "NA", "na"),
    skip = skip
  )
  cli::cli_alert_success(
    paste0("loading '", data_filepath, "'")
  )
  return(data)
}


# Compose preprocessing steps --------------------------------------------------


#' Run each resume data pre-processing step in sequence.
#' 
#' @family pipeline
#' @export
preprocess_entries <- function(
    entry_data, 
    style = c("markdown", "latex", "txt"),
    order = c("chronological", "reversed"),
    bullet_style = c("-", "+")
) {
  # Validate arguments
  style <- match.arg(style)
  order <- match.arg(order)
  bullet_style <- match.arg(bullet_style)
  
  data <- entry_data %>%
    prepare_timeline(., order = order) %>% 
    prepare_links(., style = style) %>% 
    # append_links_to_titles(., style = style) %>%
    purrr::reduce(1:5, function(data, i) {
      append_skills_to_bullets(data, i)
    }, .init = .) %>% 
    omit_hidden_fields() %>% 
    prepare_description_bullets(., bullet_style = bullet_style)

  return(data)
}


#' Prepare contact info card. 
#' 
#' @family pipeline
#' @export
preprocess_contacts <- function(
    contact_data, 
    style = c("markdown", "latex", "txt"), 
    macro = c("myhref", "href", NA)
) {
  # Validate arguments
  style <- match.arg(style)
  macro <- match.arg(macro)
  
  # Initialize a new column for processed contact info
  contact_data <- contact_data %>% 
    arrange(.data$order) %>%
    mutate(contact_text = NA)
  
  # Populate name/pic fields manually
  contact_data[contact_data$loc == "name",] <- contact_data %>% 
    filter(., .data$loc == "name") %>% 
    mutate(contact_text = .data$address_text)
  
  contact_data[contact_data$loc == "pic",] <- contact_data %>% 
    filter(., .data$loc == "pic") %>% 
    mutate(contact_text = glue('
      ![{address_text}]({address}){{.circular-frame}}
    '))
  
  # Populate links 
  entries <- contact_data %>% 
    filter(!.data$loc %in% c("name", "pic")) %>% 
    pull(.data$loc)
  contact_data[contact_data$loc %in% entries,] <- contact_data %>% 
    subset(contact_data$loc %in% entries) %>% 
    mutate(contact_text = dplyr::case_when(
      style == "markdown" ~ make_markdown_contacts(.),
      style == "latex" ~ make_latex_contacts(., macro = macro),
      style == "txt" ~ make_txt_contacts(.),
      TRUE ~ NA
    ))
  return(contact_data)
}


#' Prepare text blocks. 
#' 
#' @family pipeline
#' @export
preprocess_text <- function(
    text_data,
    style = c("markdown", "latex", "txt")
) {
  style <- match.arg(style)
  
  text_data <- text_data %>% 
    prepare_bio()
}


# Printing helpers -------------------------------------------------------------


#' Print contact info.
#' 
#' @family print-utils
#' @export
print_contact_info <- function(
    contact_data, 
    section = c("info", "links", "both"),
    sep = c(" | ", "\n")
) {
  section <- match.arg(section)
  sep <- match.arg(sep)
  
  info_fields <- c("location", "email", "phone")
  link_fields <- c("website", "github", "linkedin")
  all_fields <- c(info_fields, link_fields)

  entries <- switch(
    section,
    info = info_fields,
    links = link_fields,
    both = all_fields
  )
  
  contact_text <- contact_data %>% 
    filter(.data$loc %in% entries) %>%
    pull(contact_text) %>%
    glue::glue_collapse(sep)
  return(contact_text)
}


#' Build skill list.
#' 
#' @family build
#' @export
build_skill_list <- function(
    skill_data,
    sep = c(" • ", "\n"),
    separate_competencies = TRUE,
    competencies_header = "Other"
) {
  style <- match.arg(sep)
  assert_that(is.logical(separate_competencies))
  assert_that(is.na(competencies_header) | is.character(competencies_header))
  
  # Filter skill_data by tools and get competencies to append to end of list
  core_skills_data <- if (separate_competencies) {
    skill_data[!is.na(skill_data$is_a_tool), ]
  } else {
    skill_data
  }
  
  extra_skills_list <- if (separate_competencies) {
    stringr::str_c(
      skill_data[is.na(skill_data$is_a_tool),]$skill, 
      collapse = ", "
    )
  } else {
    NA
  }
  
  sections <- unique(core_skills_data$alias)
  
  # Initialize list
  skill_list <- ""
  
  for (i in seq_along(sections)) {
    # Concatenate section header and skills
    skills <- stringr::str_c(
      core_skills_data[core_skills_data$alias == sections[i],]$skill, 
      collapse = ", "
    )
    skill_list <- paste0(
      skill_list, 
      "**", sections[i], ":** ", 
      skills,
      # Add separator if not the last section; otherwise append competencies
      ifelse(i < length(sections), sep, 
        ifelse(
          is.na(extra_skills_list), "",
          paste0(sep, "**", competencies_header, ":** ", extra_skills_list)
        )
      )
    )
  }
  return(skill_list)
}


# One-off tests ----------------------------------------------------------------


# position_data <- load_application_data() %>% 
#   preprocess_entries(
#     style = "markdown",
#     order = "reversed"
#   )
# 
# skill_data <- load_application_data(
#   sheet = "skills", 
#   skip = 2
# )
# skill_list <- skill_data %>% 
#   sort_skills(.) %>% 
#   build_skill_list(
#     sep = " • ", 
#     separate_competencies = FALSE,
#     competencies_header = NA
#   )
# 
# contact_data <- load_application_data(
#   filename = "cover_data.xlsx",
#   sheet = "contact_info"
# ) %>% preprocess_contacts(style = "txt")
# 
# text_data <- load_application_data(
#   filename = "cover_data.xlsx",
#   sheet = "text_blocks"
# ) %>% preprocess_text()
