# This script prepares and prints each section of a plain text resume.

library(dplyr)


#' Prepare and print each section of a plain text resume.
#'
#' @family print
#' @export
print_resume_plain <- function(
  target = c("app", "base"),
  app_id = "latest",
  app_period = "latest",
  use_abridged = FALSE,
  sort_appended = FALSE
  # style = c("txt", "latex")
) {
  target = match.arg(target)
  # style = match.arg(style)
  
  # Load and process the data --------------------------------------------------
  skill_data <- load_application_data(
    target = target,
    filename = "resume_data.xlsx",
    sheet = "skills",
    # skip = 2
    app_id = app_id,
    app_period = app_period
  ) %>% sort_skills(., target = target, use_abridged = use_abridged)

  position_data <- load_application_data(
    target = target,
    filename = "resume_data.xlsx",
    sheet = "entries",
    # skip = 1,
    app_id = app_id,
    app_period = app_period
  ) %>% preprocess_entries(
    .,
    style = "txt",
    bullet_style = "+",
    use_abridged = use_abridged,
    sort_appended = sort_appended,
    skill_set_sorted = skill_data
  )
  
  contact_data <- load_application_data(
    target = target,
    filename = "cover_data.xlsx",
    sheet = "contact_info",
    app_id = app_id,
    app_period = app_period
  ) %>% preprocess_contacts(., style = "txt")
  
  text_data <- load_application_data(
    target = target,
    filename = "cover_data.xlsx",
    sheet = "text_blocks",
    app_id = app_id,
    app_period = app_period
  ) %>% preprocess_text(use_abridged = use_abridged)
  
  # Extract key elements -------------------------------------------------------
  
  name <- contact_data$address_text[contact_data$loc == "name"]
  role <- text_data$text[text_data$loc == "title"]
  bio <- text_data$text[text_data$loc == "bio"]
  suffix <- contact_data$address_text[contact_data$loc == "title"]
  
  contacts_text <- print_contact_info(
    contact_data, 
    section = "info", 
    sep = "\n"
  )
  links_text <- print_contact_info(
    contact_data, 
    section = "links", 
    sep = "\n"
  )

  skills_list <- build_skill_list(
    skill_data,
    sep = "\n",
    bullet_style = "+",
    bold_headers = FALSE,
    separate_competencies = TRUE,
    competencies_header = "Expertise"
  )
  
  # Print ----------------------------------------------------------------------
  
  resume <- glue::glue(
    "{name}, {suffix}", "\n",
    "{contacts_text}",  "\n",
    "{links_text}",
    
    "{print_txt_header('professional summary')}",
    "{bio}",
    "{print_txt_header('relevant skills')}",
    "{skills_list}",
    
    "{print_txt_header('professional experience')}",
    "{print_txt_section(
      position_data, 
      section_id = 'work', 
      target = target
    )}",
    
    "{print_txt_header('education')}",
    "{print_txt_section(
      position_data, 
      section_id = 'education', 
      target = target
    )}",
    
    "{print_txt_header('certifications')}",
    "{print_txt_section(
      position_data, 
      section_id = 'certifications',
      target = target
    )}",

    "{print_txt_header('projects')}",
    "{print_txt_section(
      position_data, 
      section_id = 'projects',
      target = target
    )}"
  )
  
  # Post-process ---------------------------------------------------------------
  
  # FIXME: hacky
  
  # Write to/read from tempfile to make processing line-by-line easier
  fil <- tempfile(fileext = ".txt")
  writeLines(resume, con = fil)
  lines <- readLines(fil)
  
  # Remove leading spaces from each line
  lines <- sub("^\\s+", "", lines)
  
  # Replace instances of four consecutive blanklines with two
  lines <- paste(lines, collapse = "\n")
  lines <- gsub(
    "(\n\\s*\n\\s*\n\\s*\n\\s*\n)+", "\n\n\n", lines, perl = TRUE
  )
  
  # Replace two consecutive lines at the end of the file with one
  lines <- gsub("(\n\\s*\n)+$", "\n", lines, perl = TRUE)
  lines <- strsplit(lines, "\n")[[1]]
  
  return(lines)
}
