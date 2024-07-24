# This script prepares and prints each section of a plain text cover letter.


#' Print the achievements listed in a text dataframe as bullet points.
#' 
#' @family print
#' @export
print_achievements <- function(text_data, bullet_style = c("-", "+")) {
  bullet_style <- match.arg(bullet_style)
  
  achivements <- text_data %>%
    filter(
      stringr::str_detect(.data$loc, "achievement_"), .data$include == "x"
    ) %>%
    arrange(.data$order) %>% 
    pull(.data$text) %>% 
    {glue::glue_collapse(glue::glue(bullet_style, " { . }"), "\n")}
  
  return(achivements)
}


#' Print a cover letter from a text dataframe in a plain text or LaTeX style.
#' 
#' @family print
#' @export
print_cover_body <- function(
    text_data,
    position,
    company,
    name = NULL,
    style = c("latex", "txt"),
    use_bullets = FALSE,
    bullet_style = c("-", "+"),
    type = c("cover", "email")
) {
  style <- match.arg(style)
  type <- match.arg(type)
  sep <- switch(style, latex = "\\vspace{{5pt}}\n\n", txt = "\n\n")
  
  # Filter
  text_data <- text_data %>% dplyr::filter(.data$include == "x")
  
  salutation <- text_data[text_data$loc == "salutation",]$text
  signoff    <- text_data[text_data$loc == "signoff",]$text
  opening    <- text_data[text_data$loc == paste0(type, "_opening"),]$text
  closing    <- text_data[text_data$loc == paste0(type, "_closing"),]$text
  postscript <- text_data[text_data$loc == paste0(type, "_postscript"),]$text
  
  if (use_bullets) {
    achievements_preamble <- text_data[
      text_data$loc == "achievements_preamble",]$text
    achivements <- print_achievements(text_data, bullet_style = bullet_style)
    
    body <- glue::glue(achievements_preamble, achivements, .sep = sep)
    
  } else {
    body <- text_data[text_data$loc == paste0(type, "_body"),]$text
  }
  
  body_text <- glue::glue(
    "{salutation}",
    "{opening}",
    "{body}",
    "{closing}",
    "{postscript}",
    "{signoff}",
    .sep = sep
  )
  if (type == "email") {
    subject <- paste(
      "Subject:", text_data[text_data$loc == "email_subject",]$text
    )
    body_text <- glue::glue("{subject}", "{body_text}", .sep = sep)
  }
  
  body_text <- body_text %>%
    glue::glue_collapse(.) %>% 
    gsub("<company>", company, .) %>% 
    gsub("<position>", position, .)
  
  return(body_text)
}


#' Prepare and print each section of a plain text cover letter.
#'
#' @family print
#' @export
print_cover_plain <- function(
    position,
    company,
    # target = c("app", "base"),
    app_id = "latest",
    app_period = "latest",
    # style = c("txt", "latex"),
    use_bullets = FALSE,
    bullet_style = c("-", "+"),
    type = c("cover", "email")
) {
  type <- match.arg(type)
  # target = match.arg(target)
  # style = match.arg(style)
  
  # Load and process the data --------------------------------------------------
  
  contact_data <- load_application_data(
    target = "app",
    filename = "cover_data.xlsx",
    sheet = "contact_info",
    app_id = app_id,
    app_period = app_period
  ) %>% preprocess_contacts(., style = "txt")
  
  text_data <- autocv::load_application_data(
    target = "app",
    filename = "cover_data.xlsx",
    sheet = "text_blocks",
    app_id = app_id,
    app_period = app_period
  ) %>% autocv::preprocess_text()
  
  # Extract key elements -------------------------------------------------------

  name <- contact_data[contact_data$loc == "name",]$address_text
  
  contacts_text <- autocv::print_contact_info(
    contact_data,
    section = "signoff",
    sep = "\n"
  )

  # Build ----------------------------------------------------------------------
  
  body_text <- print_cover_body(
    text_data,
    position = position,
    company = company,
    name = name,
    style = "txt",
    use_bullets = use_bullets,
    bullet_style = bullet_style,
    type = type
  )
  
  cover <- glue::glue(
    # "Dear Hiring Manager, ",
    "{body_text}",
    # "Sincerely,",
    "{contacts_text}",
    .sep = "\n\n"
  ) %>%
    # glue::glue_collapse(.) %>% 
    gsub("<name>", name, .)
  return(cover)
}
