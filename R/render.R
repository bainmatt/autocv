# Generate and update html, pdf, and plain text cv/resume documents.

library(dplyr)


# CV ---------------------------------------------------------------------------


#' Construct a curriculum vitae based on spreadsheet data.
#'
#' @description
#' `render_cv_as_html` constructs an informal HTML-format resume.
#'
#' `render_cv_as_pdf` converts an HTML-format resume to PDF.
#'
#' @family cli
#' @export
render_cv_as_html <- function(
    input_filename = "cv.Rmd",
    # data_filename = "resume_data.xlsx",
    output_basename = "cv",
    input_dir = "notebooks",
    data_dir = "input",
    output_dir = "output",
    stylesheets = list("custom_resume.css", "styles_html.css"),
    sort_appended = FALSE,
    show = FALSE
) {
  name <- load_job_info(field = "name")
  suffix <- paste0("_", str_to_filename(name, sep = ""))
  output_filename <- paste0(output_basename, suffix, ".html")

  # Get paths
  input_filepath <- file.path(get_path_to(input_dir), input_filename)
  output_filepath <- file.path(get_path_to(output_dir), output_filename)

  custom_css <- sapply(
    stylesheets,
    function(sheet) file.path(get_path_to("css"), sheet)
  )
  css <- c(custom_css, "resume"
    # Source styles for dynamic development (comment out before build)
    # file.path(get_path_to("extdata"), "css/custom_resume.css"),
    # file.path(get_path_to("extdata"), "css/styles_html.css")
  )

  # Render
  cli::cli_text("")
  cli::cli_rule(cli::col_blue(paste0("Building ", output_filename)))
  backup_file(output_filepath)

  rmarkdown::render(
    input = input_filepath,
    output_file = output_filepath,
    output_options = list(css = css, self_contained = TRUE),
    params = list(
      doctype = "HTML",
      # resume_data_filename = data_filename,
      target = "base",
      sort_appended = sort_appended
    ),
    quiet = TRUE
  )
  if (show) { fs::file_show(output_filepath) }
}


#' @rdname render_cv_as_html
#'
#' @export
render_cv_as_pdf <- function(
    input_filename = "cv.Rmd",
    # data_filename = "resume_data.xlsx",
    output_basename = "cv",
    input_dir = "notebooks",
    data_dir = "input",
    output_dir = "output",
    stylesheets = list("custom_resume.css", "styles_pdf.css"),
    sort_appended = FALSE,
    show = FALSE
) {
  # Prep
  name <- load_job_info("name")
  suffix <- paste0("_", str_to_filename(name, sep = ""))
  output_filename <- paste0(output_basename, suffix, ".pdf")

  # Get paths
  input_filepath <- file.path(get_path_to(input_dir), input_filename)
  output_filepath <- file.path(get_path_to(output_dir), output_filename)

  custom_css <- sapply(
    stylesheets,
    function(sheet) file.path(get_path_to("css"), sheet)
  )
  css <- c(custom_css, "resume"
    # Source styles for dynamic development (comment out before build)
    # file.path(get_path_to("extdata"), "css/custom_resume.css"),
    # file.path(get_path_to("extdata"), "css/styles_pdf.css")
  )

  cli::cli_text("")
  cli::cli_rule(cli::col_blue(paste0("Building ", output_filename)))
  backup_file(output_filepath)

  # Knit the PDF version to a temporary html location
  tmp_html_cv_loc <- fs::file_temp(ext = ".html")
  rmarkdown::render(
    input = input_filepath,
    output_file = tmp_html_cv_loc,
    output_options = list(css = css, self_contained = TRUE),
    params = list(
      doctype = "PDF",
      # resume_data_filename = data_filename,
      target = "base",
      sort_appended = sort_appended
    ),
    quiet = TRUE
  )

  # Convert to PDF using Pagedown
  pagedown::chrome_print(
    input = tmp_html_cv_loc,
    output = output_filepath
  )
  if (show) { fs::file_show(output_filepath) }
  # fs::file_show(output_filepath)
}


# Resume -----------------------------------------------------------------------


#' Construct a resume based on spreadsheet data.
#'
#' `render_resume` constructs a formal, ATS-compatible, LaTeX-style resume.
#'
#' `render_resume_plain` constructs a plain text resume for analysis.
#'
#' @family cli
#' @export
render_resume <- function(
    target = c("app", "base"),
    app_id = "latest",
    app_period = "latest",
    stylesheets = list("custom_resume.tex"),

    # data_filename = "resume_data.xlsx",
    input_filename = "resume.Rmd",
    output_basename = "resume",

    app_dir = "applications",
    input_dir = "notebooks",
    data_dir = "input",
    output_dir = "output",
    use_abridged = FALSE,
    sort_appended = FALSE,
    show = FALSE
) {
  target <- match.arg(target)
  suffix_abridged <- ifelse(use_abridged, "_linkedin", "")

  if (target == "app") {
    assert_that(all(!is.na(c(app_id, app_period, app_dir))))

  }
  # } else if (target == "base") {
  #   unused_args <- c('app_id', 'app_period', 'app_dir')
  #   cli::cli_li(paste0("Unused argument: ", cli::col_blue(unused_args)))
  # }

  # Get paths to application data and output files
  if (target == "app") {
    log <- load_log(app_period = app_period, app_dir = app_dir)

    # Validate app_id
    ids <- get_valid_opts(log = log, arg = "id")
    if (app_id != "latest") { app_id <- match.arg(app_id, ids) }

    # Get app_df
    app_df <- if (app_id == "latest") {
      get_latest_entry(log = log)
    } else {
      log[log$id == app_id,]
    }

    input_filepath <- file.path(get_path_to(input_dir), input_filename)
    output_filepath <- app_df$resume_path
    base_dir <- app_df$app_path

  } else if (target == "base") {
    name <- load_job_info("name")
    suffix <- paste0("_", str_to_filename(name, sep = ""), suffix_abridged)
    output_filename <- paste0(output_basename, suffix, ".pdf")

    input_filepath <- file.path(get_path_to(input_dir), input_filename)
    output_filepath <- file.path(get_path_to(output_dir), output_filename)
    base_dir <- "."
  }

  custom_tex <- sapply(
    stylesheets,
    function(sheet) file.path(get_path_to("latex"), sheet)
  )

  # Source styles for dynamic development (comment out before build)
  custom_tex <- file.path(get_path_to("latex"), "custom_resume.tex")

  # Issue alerts
  cli::cli_text("")
  cli::cli_rule(cli::col_blue(paste0("Building ", output_basename, ".pdf")))
  # alert_writing_to(base_dir)

  if (file.exists(output_filepath)) {
    warn_file_exists(output_filepath, base_dir, action = "overwriting")
    backup_file(output_filepath)
  } else {
    alert_file_created(output_filepath, base_dir)
  }
  if (target == "app") { update_datestamp(app_id = app_id) }

  # Render
  rmarkdown::render(
    input = input_filepath,
    output_file = output_filepath,
    output_options = list(
      latex_engine = "xelatex",
      keep_tex = FALSE,
      includes = list(in_header = custom_tex)
    ),
    params = list(
      # resume_data_filename = data_filename,
      target = target,
      app_id = app_id,
      app_period = app_period,
      use_abridged = use_abridged,
      sort_appended = sort_appended
    ),
    quiet = TRUE
  )
  if (show) { fs::file_show(output_filepath) }
  # fs::file_show(output_filepath)
}


#' @rdname render_resume
#'
#' @examples
#' # TODO: render_resume_plain example: Use templates & base
#'
#' @export
render_resume_plain <- function(
    target = c("app", "base"),
    app_id = "latest",
    app_period = "latest",

    # data_filename = "resume_data.xlsx",
    output_basename = "resume",

    app_dir = "applications",
    data_dir = "input",
    output_dir = "output",
    use_abridged = FALSE,
    sort_appended = FALSE,
    show = FALSE
) {
  target <- match.arg(target)
  suffix_abridged <- ifelse(use_abridged, "_linkedin", "")

  # Get paths to application data and output files
  if (target == "app") {
    log <- load_log(app_period = app_period, app_dir = app_dir)

    app_df <- if (app_id == "latest") {
      get_latest_entry(log = log)
    } else {
      log[log$id == app_id,]
    }
    output_filepath <- app_df$resume_plain_path
    base_dir <- app_df$app_path

  } else if (target == "base") {
    name <- load_job_info("name")
    suffix <- paste0("_", str_to_filename(name, sep = ""), suffix_abridged)
    output_filename <- paste0(output_basename, suffix, ".txt")
    output_filepath <- file.path(get_path_to(output_dir), output_filename)
    base_dir <- "."
  }

  # Issue alerts
  cli::cli_text("")
  cli::cli_rule(cli::col_blue(paste0("Building ", output_basename, ".txt")))
  # alert_writing_to(base_dir)

  if (file.exists(output_filepath)) {
    warn_file_exists(output_filepath, base_dir, action = "overwriting")
    backup_file(output_filepath)
  } else {
    alert_file_created(output_filepath, base_dir)
  }

  # Render
  resume_text <- print_resume_plain(
    target = target,
    app_id = app_id,
    app_period = app_period,
    use_abridged = use_abridged,
    sort_appended = sort_appended
  )
  writeLines(resume_text, output_filepath)
  if (show) { fs::file_show(output_filepath) }
  # fs::file_show(output_filepath)
}


# Cover ------------------------------------------------------------------------


#' Construct a cover letter based on spreadsheet data.
#'
#' @description
#' `render_cover` constructs a formal, LaTeX-style resume.
#'
#' `render_cover_plain` constructs a plain text cover letter for simplicity.
#'
#' @family cli
#' @export
render_cover <- function(
    target = c("app", "base"),
    app_id = "latest",
    app_period = "latest",
    use_bullets = TRUE,
    bullet_style = c("-", "+"),

    stylesheets = list("custom_cover.tex"),
    # data_filename = "cover_data.xlsx",
    input_filename = "cover.Rmd",
    output_basename = "cover",

    app_dir = "applications",
    input_dir = "notebooks",
    data_dir = "input",
    output_dir = "output",
    show = FALSE
) {
  target <- match.arg(target)

  # Get paths to application data and output files
  if (target == "app") {
    log <- load_log(app_period = app_period, app_dir = app_dir)
    if (app_id == "latest") {
      app_df <- get_latest_entry(log = log)
      app_id <- app_df$id
    } else {
      app_df <- log[log$id == app_id,]
    }

    # Get metadata and paths
    company         <- app_df$company
    position        <- app_df$position

    input_filepath  <- file.path(get_path_to(input_dir), input_filename)
    output_filepath <- app_df$cover_path
    base_dir        <- app_df$app_path

  } else if (target == "base") {
    name <- load_job_info("name")
    suffix <- paste0("_", str_to_filename(name, sep = ""))
    output_filename <- paste0(output_basename, suffix, ".pdf")

    # Get metadata and paths
    company         <- NULL
    position        <- NULL

    input_filepath  <- file.path(get_path_to(input_dir), input_filename)
    output_filepath <- file.path(get_path_to(output_dir), output_filename)
    base_dir        <- "."
  }

  custom_tex <- sapply(
    stylesheets,
    function(sheet) file.path(get_path_to("latex"), sheet)
  )

  # Issue alerts
  cli::cli_text("")
  cli::cli_rule(cli::col_blue(paste0("Building ", output_basename, ".pdf")))
  # alert_writing_to(base_dir)

  if (file.exists(output_filepath)) {
    warn_file_exists(output_filepath, base_dir, action = "overwriting")
    backup_file(output_filepath)
    # update_datestamp(app_id = app_id)
  } else {
    alert_file_created(output_filepath, base_dir)
  }

  # Render
  rmarkdown::render(
    input = input_filepath,
    output_file = output_filepath,
    output_options = list(
      latex_engine = "xelatex",
      keep_tex = FALSE,
      includes = list(in_header = custom_tex)
    ),
    params = list(
      # cover_data_filename = data_filename,
      target = target,
      position = position,
      company = company,
      use_bullets = use_bullets,
      app_id = app_id,
      app_period = app_period
    ),
    quiet = TRUE
  )
  if (show) { fs::file_show(output_filepath) }
  # fs::file_show(output_filepath)
}


#' @rdname render_cover
#'
#' @export
render_cover_plain <- function(
    target = c("app", "base"),
    app_id = "latest",
    app_period = "latest",
    use_bullets = TRUE,
    bullet_style = c("-", "+"),
    type = c("cover", "email"),
    app_dir = "applications",
    data_dir = "input",
    output_dir = "output",
    show = FALSE
) {
  target <- match.arg(target)
  type <- match.arg(type)
  output_basename <- type

  # Load application data and get metadata and paths
  if (target == "app") {
    log <- load_log(app_period = app_period, app_dir = app_dir)
    if (app_id == "latest") {
      app_df <- get_latest_entry(log = log)
      app_id <- app_df$id
    } else {
      app_df <- log[log$id == app_id,]
    }

    company         <- app_df$company
    position        <- app_df$position
    base_dir        <- app_df$app_path

    output_filepath <- switch(
      type,
      cover = app_df$cover_plain_path,
      email = app_df$email_path
    )
  } else if (target == "base") {
    name <- load_job_info("name")
    suffix <- paste0("_", str_to_filename(name, sep = ""))
    output_filename <- paste0(output_basename, suffix, ".txt")

    company         <- NULL
    position        <- NULL

    output_filepath <- file.path(get_path_to(output_dir), output_filename)
    base_dir        <- "."
  }

  # Issue alerts
  cli::cli_text("")
  cli::cli_rule(cli::col_blue(paste0("Building ", output_basename, ".txt")))

  if (file.exists(output_filepath)) {
    warn_file_exists(output_filepath, base_dir, action = "overwriting")
    backup_file(output_filepath)
  } else {
    alert_file_created(output_filepath, base_dir)
  }

  # Render
  cover_text <- print_cover_plain(
    target = target,
    position = position,
    company = company,
    app_id = app_id,
    app_period = app_period,
    use_bullets = use_bullets,
    bullet_style = bullet_style,
    type = type
  )
  writeLines(cover_text, output_filepath)
  if (show) { fs::file_show(output_filepath) }
  # fs::file_show(output_filepath)
}


# Compose ----------------------------------------------------------------------


# TODO: In render_app: arg docs: (resume, cover, both)

#' Run each resume or CV rendering step in sequence.
#'
#' @description
#' `render_app` builds a pdf and plain text resume for a given application.
#'
#' `render_base` builds an html CV, pdf and plain text resume from base data.
#'
#' `render_cv` builds an html CV and converts it to pdf.
#'
#' `render_linkedin` builds a short resume suitable for professional profiles.
#'
#' @family cli
#' @export
render_app <- function(
    app_id = "latest",
    app_period = "latest",
    docs = c("resume", "all", "cover"),
    # cover = TRUE,
    # email = TRUE,
    use_bullets = TRUE,
    show = FALSE
) {
  docs <- match.arg(docs, several.ok = TRUE)

  app_df <- get_app_info(
    id = app_id,
    app_period = app_period,
    field = c("period_path", "output_path", "status")
  )

  # Don't permit re-rendering if app is already submitted
  stop_statuses <- c(
    "applied", "rejected", "interviewed_then_rejected", "closed"
  )
  if (app_df$status %in% stop_statuses) {
    cli::cli_alert_danger(
      "Status is {.val {app_df$status}}. Must be {.val ipr} to render. (aborting)"
    )
    return(invisible(FALSE))
    # stop(glue::glue("Status is '{app_df$status}', stopping execution."))
  }

  if ("cover" %in% docs || "all" %in% docs) {
  # if (email) {
    render_cover_plain(
      target = "app",
      app_id = app_id,
      app_period = app_period,
      use_bullets = FALSE,
      bullet_style = "+",
      type = "email",
      show = show
    )
  # }

  # if (cover) {
    render_cover_plain(
      target = "app",
      app_id = app_id,
      app_period = app_period,
      use_bullets = use_bullets,
      bullet_style = "+",
      show = show
    )
    render_cover(
      target = "app",
      app_id = app_id,
      app_period = app_period,
      use_bullets = use_bullets,
      bullet_style = "-",
      show = show
    )
  # }
  }

  if ("resume" %in% docs || "all" %in% docs) {
    render_resume_plain(
      target = "app",
      app_id = app_id,
      app_period = app_period,
      show = show
    )
    render_resume(
      target = "app",
      app_id = app_id,
      app_period = app_period,
      show = show
    )
  }
  
  if (!show) {
    # alert_opening(app_df$output_path, base_dir = app_df$period_path)
    alert_opening(app_df$output_path)
    fs::file_show(app_df$output_path)
  }
}


# TODO: temp_cover_data new loc rows: cover_base_{opening, body, closing, ps}

#' @rdname render_app
#'
#' @export
render_base <- function(
    report_counts = TRUE,
    cover = FALSE,
    show = FALSE
) {
  # NOTE: Set target to "app" for a 1-page resume;
  # enable latex stylesheet header (name) and footer (page numbers) otherwise.
  # render_cv_as_html(
  #   show = show
  # )
  # render_cv_as_pdf(
  #   show = show
  # )

  render_resume_plain(
    target = "base",
    show = show
  )
  render_resume(
    target = "base",
    show = show
  )

  if (cover) {
    render_cover(
      target = "base",
      use_bullets = FALSE,
      show = show
    )
    render_cover_plain(
      target = "base",
      type = "cover",
      use_bullets = FALSE,
      show = show
    )
  }

  if (report_counts) {
    cli::cli_text("")
    cli::cli_rule(cli::col_blue(paste0("Checking keywords")))

    output_df <- count_terms_base()
    print(output_df, n = max(nrow(output_df), 50))
  }

  if (!show) {
    output_path <- get_path_to("output")
    alert_opening(output_path)
    fs::file_show(output_path)
  }
}


#' @rdname render_app
#'
#' @export
render_cv <- function(show = FALSE) {
  render_cv_as_html(
    show = show
  )
  render_cv_as_pdf(
    show = show
  )
  
  if (!show) {
    output_path <- get_path_to("output")
    alert_opening(output_path)
    fs::file_show(output_path)
  }
}


#' @rdname render_app
#'
#' @export
render_linkedin <- function(
    report_counts = TRUE,
    show = FALSE
) {
  render_resume_plain(
    target = "base",
    use_abridged = TRUE,
    sort_appended = FALSE,
    show = show
  )
  render_resume(
    target = "base",
    use_abridged = TRUE,
    sort_appended = FALSE,
    show = show
  )

  if (report_counts) {
    cli::cli_text("")
    cli::cli_rule(cli::col_blue(paste0("Checking keywords")))

    output_df <- count_terms_base(use_abridged = TRUE)
    print(output_df, n = max(nrow(output_df), 50))
  }

  if (!show) {
    output_path <- get_path_to("output")
    alert_opening(output_path)
    fs::file_show(output_path)
  }
}


# Helpers ----------------------------------------------------------------------


#' Transfer existing files to a backup directory Before overwriting.
#'
#' @family cli-dev
#' @export
backup_file <- function(path) {
  if (!file.exists(path)) {
    return(invisible(FALSE))
  }
  
  # Ensure path is absolute and normalize
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  base_dir <- get_path_to(dir = "src")
  
  # Create backup directory
  backup_dir <- file.path(dirname(path), ".backup")
  if (!dir.exists(backup_dir)) {
    dir.create(backup_dir, recursive = TRUE)
  }
  
  # Create timestamped filename
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  file_name <- basename(path)
  backup_path <- file.path(backup_dir, paste0(timestamp, "_", file_name))
  
  # Move the file
  cli::cli_alert_info(
    paste0(
      "Moving ", cli::col_green(file_name),
      " from: ", cli::col_green("output/"),
      " to: ", cli::col_green("output/.backup/")
      # " from: ", cli::col_green(
      #   fs::path_dir(fs::path_rel(path, start = base_dir))),
      # " to: ", cli::col_green(
      #   fs::path_dir(fs::path_rel(backup_path, start = base_dir)))
    )
  )
  file.rename(from = path, to = backup_path)
  return(invisible(TRUE))
}
