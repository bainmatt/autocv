# Helper functions for preparing resume data for resume built with R pagedown

library(dplyr)
library(assertthat)


# Basics -----------------------------------------------------------------------


#' Prepare bio. 
#' 
#' @param text_data A spreadsheet containing resume text data.
#' 
#' @family prepare
prepare_bio <- function(text_data) {
  # Build bio
  bio <- text_data %>% 
    filter(stringr::str_detect(.data$loc, "bio"), .data$include == "x") %>%
    arrange(.data$order) %>% 
    pull(.data$text) %>% 
    glue::glue_collapse(" ")
  
  # Initialize new row and add bio
  text_data <- text_data %>% 
    add_row(
      loc = "bio", 
      text = as.character(bio), 
      include = "x", 
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
    order = c("chronological", "reversed"),
    style = c("markdown", "latex", "txt")
) {
  order <- match.arg(order)
  style <- match.arg(style)
  sep <- switch(style, markdown = " - ", latex = " -- ", txt = " - ")
  
  data <- data %>%
    # Sort data by end date in descending order
    arrange(desc(.data$end)) %>%
    # Format dates: if month supplied, use mmm-YYYY format
    mutate(
      formatted_start_date = format(as.Date(.data$start), "%b %Y"),
      formatted_end_date = format(as.Date(.data$end), "%b %Y"),
    ) %>%
    # Add 'in progress' column for ongoing entries and format accordingly
    mutate(
      ipr = as.Date(.data$start) == as.Date("1900-01-01"),
      start = dplyr::if_else(.data$ipr, NA, .data$start),
      formatted_end_date = dplyr::if_else(
        .data$ipr,
        paste("Expected", .data$formatted_end_date),
        .data$formatted_end_date
      )
    ) %>% 
    rowwise %>%
    # Construct timeline for each entry based on start/end pairs provided
    mutate(
      timeline = dplyr::case_when(
        is.na(.data$start) & is.na(.data$end) ~ NA,
        is.na(.data$start) ~ formatted_end_date,
        is.na(.data$end) ~ ifelse(
          order == "chronological",
          glue('{formatted_start_date}', '{sep}', 'Present'),
          glue('Present', '{sep}',  '{formatted_start_date}')
        ),
        # is.na(end) ~ glue('Present - {formatted_start_date}'),
        start == end ~ formatted_end_date,
        TRUE ~ ifelse(
          order == "chronological",
          glue('{formatted_start_date}', '{sep}', '{formatted_end_date}'),
          glue('{formatted_end_date}', '{sep}', '{formatted_start_date}')
        )
      )
    )
  return(data)
}


# TODO: add function (render_links()) to detect links (of the style []())
# + extract text/link in raw resume/cover body text
# + format as either markdown, latex, or txt by calling prepare_links
# (call render_links() first in preprocess_text/_entries).


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
#' @family prepare-dev
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
#' @family prepare-dev
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
#' @family prepare-dev
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
    dplyr::mutate(contact_text = dplyr::case_when(
      is.na(.data$address) ~ .data$address_text,
      .data$loc %in% c("email", "phone") ~ .data$address_text,
      .default = stringr::str_c(.data$address_text, ": ", .data$address)
      # stringr::str_c(.data$loc, ": ", .data$address)
    ))
  return(contact_data$contact_text)
}


#' Sort skills.
#' 
#' @family prepare-dev
#' @export
sort_skills <- function(skill_data, target = c("app", "base")) {
  target <- match.arg(target)
  
  # Filter
  skill_data <- skill_data %>% 
    filter(
      (
        (target == "app" & .data$include == "x") |
        (target == "base" & .data$in_base == "x")
      )
    )
  
  # Verify that required fields are present
  assert_that(all(c("category_id", "is_a_tool") %in% names(skill_data)))
  
  # Sort by tools/competencies (tools first) -> section id -> skill level
  skill_data <- skill_data %>% 
    # filter(.data$include == "x") %>%
    arrange(.data$is_a_tool, .data$category_id, desc(.data$level))
  return(skill_data)
}


# Load -------------------------------------------------------------------------


# TODO: workaround constructing data path to avoid load_log call or pass log

#' Load application data.
#' 
#' @family data
#' @export
load_application_data <- function(
    target = c("app", "base"),
    filename = c("resume_data.xlsx", "cover_data.xlsx"),
    sheet = c("entries", "skills", "contact_info", "text_blocks"),
    # skip = 1,
    data_dir = "input",
    app_id = "latest",
    app_period = "latest"
) {
  target <- match.arg(target)
  sheet <- match.arg(sheet)
  
  # Set filename and number of header rows dynamically
  if (sheet %in% c("entries", "skills")) {
    # filename = "resume_data.xlsx"
    skip = 2
    
  } else if (sheet %in% c("contact_info", "text_blocks")) {
    # filename = "cover_data.xlsx"
    skip = 1
  }
  
  if (target == "app") {
    assert_that(all(!is.na(c(app_id, app_period))))
  }
    
  # TODO: !!remove get_app_path_to call here
  # Get path to application data
  if (target == "app") {
    doc <- fs::path_ext_remove(filename)
    data_filepath <- get_app_path_to(
      id = app_id,
      doc = doc,
      app_period = app_period
    )
    
  } else if (target == "base") {
    data_filepath <- file.path(get_path_to(data_dir), filename)
  }
  
  if (!all(file.exists(data_filepath))) {
    warn_file_missing(data_filepath)
    stop("Missing data file")
    # return(invisible(FALSE))
  }
    
  data <- readxl::read_excel(
    data_filepath,
    sheet = sheet,
    na = c("", "NA", "na"),
    skip = skip
  )
  return(data)
}


# Compose ----------------------------------------------------------------------


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
  style <- match.arg(style)
  order <- match.arg(order)
  bullet_style <- match.arg(bullet_style)
  
  data <- entry_data %>%
    prepare_timeline(., order = order, style = style) %>%
    prepare_links(., style = style) %>% 
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
#' @family print
#' @export
print_contact_info <- function(
    contact_data, 
    section = c("info", "links", "both", "signoff"),
    sep = c(" | ", "\n")
) {
  section <- match.arg(section)
  # sep <- match.arg(sep)
  
  info_fields    <- c("location", "email", "phone")
  link_fields    <- c("website", "github", "linkedin")
  all_fields     <- c(info_fields, link_fields)
  signoff_fields <- c("name", "email", "phone")

  entries <- switch(
    section,
    info = info_fields,
    links = link_fields,
    both = all_fields,
    signoff = signoff_fields
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
    bullet_style = c("", "-", "+"),
    bold_headers = TRUE,
    separate_competencies = TRUE,
    competencies_header = "Other"
) {
  sep <- match.arg(sep)
  bullet_style <- match.arg(bullet_style)
  
  assert_that(is.logical(bold_headers))
  assert_that(is.logical(separate_competencies))
  assert_that(is.na(competencies_header) | is.character(competencies_header))
  
  if (bullet_style != "") {
    bullet_style = paste0(bullet_style, " ")
  }
  
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
  
  # Get section headers and format
  sections <- unique(core_skills_data$alias)
  sections_combined <- c(sections, competencies_header)
  formatted_headers <- if (bold_headers) {
    paste0("**", sections_combined, ":** ")
  } else {
    paste0(sections_combined, ": ")
  }
  
  # Build list
  final_section <- if(is.na(extra_skills_list)) {
    ""
  } else {
    paste0(
      sep, bullet_style, dplyr::last(formatted_headers), extra_skills_list
    )
  }
  skill_list <- ""
  
  for (i in seq_along(sections)) {
    
    # Concatenate skills
    skills <- stringr::str_c(
      core_skills_data[core_skills_data$alias == sections[i],]$skill, 
      collapse = ", "
    )
    
    # Concatenate section header and skills
    skill_list <- paste0(
      skill_list,
      bullet_style,
      formatted_headers[i], 
      skills,
      
      # Add separator if not the last section; otherwise append competencies
      ifelse(i < length(sections), sep, final_section)
    )
  }
  return(skill_list)
}


# One-off tests ----------------------------------------------------------------
