# CLI handlers for core autocv functionality.


#' CLI handler for 'build'.
#'
#' @family cli-handlers
#' @export
cli_build <- function(args) {
  parser <- optparse::OptionParser(
    usage = "cv build",
    description = "[STEP 1/4] Build an application directory.",
    epilogue = "
Details:
    1.  Opens 'input/job_metadata.yml' and prompts you to enter
        metadata for a job of interest, including:
        - a unique identifier `id`
        - the current application period `period`
        - hyperlinks to the job posting

        Upon confirmation, adds a logfile entry to the application directory
        for the given period and constructs the job directory from your base
        data files:

        {applications/<period>/<job>/}

    2.  Attempts to download the job posting and waits for you to
        make edits to the retrieved text. Once confirmed, generates keyword
        reports for your reference.

See also:
    {autocv::build_base_directory}
        Creates the required stylesheets, notebooks, and base data
        files inside the 'input/' directory, including:

        {cover_data.xlsx}
        {resume_data.xlsx}
        {qa.yml}
        {job_metadata.yml}
    "
  )
  opts <- optparse::parse_args(parser, args = args)

  autocv::build_app_directory()
}


#' CLI handler for 'edit'.
#'
#' @family cli-handlers
#' @export
cli_edit <- function(args) {
  opt_list <- list(
    optparse::make_option(
      c("-p", "--period"),
      type = "character",
      default = "latest",
      help = "Application period [default %default]",
      metavar = "period"
    ),
    optparse::make_option(
      c("-d", "--doc"),
      type = "character",
      default = "resume",
      help = glue::glue(
        "Document to edit: one of all, resume, cover, qa. ",
        "Ignored with --base. [default %default]"
      ),
      metavar = "document"
    ),
    optparse::make_option(
      c("-b", "--base"),
      action = "store_true",
      default = FALSE,
      help = "Edit the base application data instead of a specific period/job"
    )
  )

  parser <- optparse::OptionParser(
    usage = glue::glue(
      "cv edit [id='latest'] [--period PERIOD='latest'] ",
      "[--doc DOCUMENT='resume'] [--base]",
    ),
    option_list = opt_list,
    description = "[STEP 2/4] Edit application data files.",
    epilogue = "
Details:
    a.  Opens the following documents inside the
        'application/<period>/<job>/input/' directory for editing purposes:

        {posting_<id>.txt}
        {resume_data_<id>.xlsx}       [--doc all|resume]
        {cover_data_<id>.xlsx}        [--doc all|cover]
        {qa_<id>.yml}                 [--doc all|qa]
    
        Modify these files to tailor résumé entries, bullet points, keywords,
        and cover letter sections for a specific job application. Modify the
        following fields to change the rendered outputs:

        include
            Toggle included résumé and cover letter entries.
    
    b.  With the --base flag, opens the following documents inside the root
        'input/' directory:

        {resume_data.xlsx}            [--doc all|resume]
        {cover_data.xlsx}             [--doc all|cover]
        {qa.yml}                      [--doc all|qa]
    
        Modify these files to maintain your base list of positions, projects,
        and skills (from which job-specific files are derived) as they grow
        over time. Modify the following fields to change the rendered outputs:

        in_base
            Toggle included base résumé entries.

        core_tools
        current_tech
        key_competency
            Modify CV-specific details.

        short_summary
        tool_*
        competency_*
            Modify LinkedIn-specific details.

Notes:
    # TODO: cli_edit: Add notes about formatting in xlsx (& future plan)

See also:
    {cv build}
        Must be run as a prerequisite to construct the application directory.
    "
  )
  args_parsed <- optparse::parse_args(
    parser,
    args = args,
    positional_arguments = TRUE
  )
  pos_args <- args_parsed$args
  opts <- args_parsed$options

  docs <- opts$doc
  if (!is.null(docs)) {
    docs <- as.character(docs)
  }

  if (opts$base) {
    autocv::edit_base(docs = docs)
  } else {
    id <- ifelse(length(pos_args) > 0, pos_args[1], "latest")
    autocv::edit_app(id = id, app_period = opts$period, docs = docs)
  }
}


#' CLI handler for 'render'.
#'
#' @family cli-handlers
#' @export
cli_render <- function(args) {
  opt_list <- list(
    optparse::make_option(
      c("-p", "--period"),
      type = "character",
      default = "latest",
      help = glue::glue(
        "Application period. Ignored with --base, --cv, or --linkedin. ",
        "[default %default]"
      ),
      metavar = "period"
    ),
    optparse::make_option(
      c("-d", "--doc"),
      type = "character",
      default = "resume",
      help = glue::glue(
        "Document to render: one of all, resume, cover. ",
        "Ignored with --base, --cv, or --linkedin. ",
        "[default %default]"
      ),
      metavar = "document"
    ),
    optparse::make_option(
      c("-s", "--show"),
      action = "store_true",
      default = FALSE,
      help = glue::glue(
        "Open the rendered documents ",
        "instead of showing them in the file explorer"
      )
    ),
    optparse::make_option(
      c("-a", "--achievements"),
      action = "store_true",
      default = FALSE,
      help = glue::glue(
        "Use a list of achievements in place of the cover letter body text. ",
        "Ignored unless --doc cover|all supplied."
      )
    ),
    optparse::make_option(
      c("-b", "--base"),
      action = "store_true",
      default = FALSE,
      help = glue::glue(
        "Render the base résumé (as both a PDF and plain text file) ",
        "instead of those for a specific period/job"
      )
    ),
    optparse::make_option(
      c("-c", "--cv"),
      action = "store_true",
      default = FALSE,
      help = glue::glue(
        "Render the base, web-optimized CV (as both an HTML and PDF file) ",
        "instead of those for a specific period/job"
      )
    ),
    optparse::make_option(
      c("-l", "--linkedin"),
      action = "store_true",
      default = FALSE,
      help = glue::glue(
        "Render the base résumé (as both a PDF and plain text file) ",
        "using the `short_summary` and `linkedin_*` fields ",
        "instead of those for a specific period/job"
      )
    )
  )

  parser <- optparse::OptionParser(
    usage = glue::glue(
      "cv render [id='latest'] [--period PERIOD='latest'] ",
      "[--doc DOCUMENT='resume'] [--base] [--cv] [--linkedin]",
    ),
    option_list = opt_list,
    description = "[STEP 3/4] Render application documents.",
    epilogue = "
Details:
    a.  Using the applicable input data files and stylesheets (see `cv edit`
        and `cv base`), generates the following documents inside the
        'application/<period>/<job>/output/' directory:

        {resume_<yourname>_<id>.pdf}    [--doc all|resume]
        {resume_<yourname>_<id>.txt}    [--doc all|resume]
        {cover_<yourname>_<id>.pdf}     [--doc all|cover]
        {cover_<yourname>_<id>.txt}     [--doc all|cover]
        {email_<yourname>_<id>.txt}     [--doc all|cover]

    b.  With the --base flag, renders the following documents and saves
        them inside the root 'output/' directory:

        {resume_<yourname>.pdf}
        {resume_<yourname>.txt}

    c.  With the --cv flag, generates the following documents inside
        the root 'output/' directory:

        {cv_<yourname>.html}
        {cv_<yourname>.pdf}

    d.  With the --linkedin flag, generates the following documents inside
        the root 'output/' directory:

        {resume_<yourname>_linkedin.pdf}
        {resume_<yourname>_linkedin.txt}

Notes:
    # TODO: cli_render: Add notes about latex vspace formatting

See also:
    {cv edit}
        Run this first to tailor data files as desired.
    "
  )
  args_parsed <- optparse::parse_args(
    parser,
    args = args,
    positional_arguments = TRUE
  )
  pos_args <- args_parsed$args
  opts <- args_parsed$options

  docs <- opts$doc
  if (!is.null(docs)) {
    docs <- as.character(docs)
  }

  # if (!opts$base & length(pos_args) == 0) {
  #   stop(glue::glue("
  #     Error: Require <id> argument unless --base is specified.
  #     Run with --help for usage.
  #   ")
  # }
  if (opts$base) {
    autocv::render_base(
      show = opts$show,
      report_counts = FALSE
    )
  } else if (opts$cv) {
    autocv::render_cv(
      show = opts$show
    )
  } else if (opts$linkedin) {
    autocv::render_linkedin(
      show = opts$show,
      report_counts = FALSE
    )
  } else {
    id <- ifelse(length(pos_args) > 0, pos_args[1], "latest")
    autocv::render_app(
      app_id = id,
      app_period = opts$period,
      use_bullets = opts$achievements,
      show = opts$show,
      docs = docs
    )
  }
}


#' CLI handler for 'check'.
#'
#' @family cli-handlers
#' @export
cli_check <- function(args) {
  opt_list <- list(
    optparse::make_option(
      c("-p", "--period"),
      type = "character",
      default = "latest",
      help = glue::glue(
        "Application period. Ignored with --base or --linkedin. ",
        "[default %default]"
      ),
      metavar = "period"
    ),
    optparse::make_option(
      c("-b", "--base"),
      action = "store_true",
      default = FALSE,
      help = glue::glue(
        "Count skills in the base résumé ",
        "instead of those for a specific period/job"
      )
    ),
    optparse::make_option(
      c("-l", "--linkedin"),
      action = "store_true",
      default = FALSE,
      help = glue::glue(
        "Count skills in the LinkedIn-appropriate base résumé ",
        "instead of those for a specific period/job"
      )
    )
  )
  
  parser <- optparse::OptionParser(
    usage = glue::glue(
      "cv check [id='latest'] [--period PERIOD='latest'] ",
      "[--base] [--linkedin]",
    ),
    option_list = opt_list,
    description = "[STEP 4/4] Check application keywords.",
    epilogue = "
Details:
    a.  Extract job-relevant terms from a résumé and cross-check them against
        the job posting. The term bank is a plain text file with each term
        (case-sensitive) defined on a new line. By default, it is located
        inside the 'inst/extdata/resources/' directory:
    
        {skill_list.txt}
        {keyword_list.txt}

        This step generates CSV files inside the
        'application/<period>/<job>/output/' directory, namely:

        {keyword_counts_<id>.csv}
        {skill_counts_posting_<id>.csv}
        {skill_counts_resume_<id>.csv}
        {skill_report_<id>.csv}

    b.  With the --base flag, only prints skill counts based on the base
        résumé With the --linkedin flag, only prints skill counts based on
        the LinkedIn-appropriate base résumé

See also:
    {cv render}
        Run this first to generate the résumé to check.
    "
  )
  args_parsed <- optparse::parse_args(
    parser,
    args = args,
    positional_arguments = TRUE
  )
  pos_args <- args_parsed$args
  opts <- args_parsed$options

  if (opts$base) {
    autocv::count_terms_base(
      use_abridged = FALSE
    )
  } else if (opts$linkedin) {
    autocv::count_terms_base(
      use_abridged = TRUE
    )
  } else {
    id <- ifelse(length(pos_args) > 0, pos_args[1], "latest")
    autocv::check_skills(
      app_id = id,
      app_period = opts$period
    )
  }
}


#' CLI handler for 'review'.
#'
#' @family cli-handlers
#' @export
cli_review <- function(args) {
  opt_list <- list(
    optparse::make_option(
      c("-p", "--period"),
      type = "character",
      default = "latest",
      help = "Application period [default %default]",
      metavar = "period"
    ),
    optparse::make_option(
      c("-r", "--report"),
      action = "store_true",
      default = FALSE,
      help = "Summarize the application log by status"
    )
  )

  parser <- optparse::OptionParser(
    usage = glue::glue(
      "cv review [id='latest'] [--period PERIOD='latest'] ",
      "[--field FIELD='all'] [--status STATUS='all'] [--report]",
    ),
    option_list = opt_list,
    description = "Review application details.",
    epilogue = "
See also:
    {cv check}
        Generate keyword reports.
    "
  )
  args_parsed <- optparse::parse_args(
    parser,
    args = args,
    positional_arguments = TRUE
  )
  pos_args <- args_parsed$args
  opts <- args_parsed$options
  
  if (opts$report) {
    autocv::get_status_report(
      app_period = opts$period
    )
  } else {
    id <- ifelse(length(pos_args) > 0, pos_args[1], "all")
    autocv::get_app_info(
      id = id,
      app_period = opts$period
    )
  }
}


#' CLI handler for 'update'.
#'
#' @family cli-handlers
#' @export
cli_apply <- function(args) {
  parser <- optparse::OptionParser(
    usage = "cv apply [id='latest']",
    description = "Apply to a given job (set date_applied and status).",
    epilogue = "
See also:
    {cv review}
        Review application details.
    "
  )
  args_parsed <- optparse::parse_args(
    parser,
    args = args,
    positional_arguments = TRUE
  )
  pos_args <- args_parsed$args
  # opts <- args_parsed$options

  id <- ifelse(length(pos_args) > 0, pos_args[1], "latest")
  autocv::apply_to(
    app_id = id
  )
}


#' CLI handler for 'open'.
#'
#' @family cli-handlers
#' @export
cli_open <- function(args) {
  opt_list <- list(
    optparse::make_option(
      c("-p", "--period"),
      type = "character",
      default = "latest",
      help = "Application period [default %default]",
      metavar = "period"
    ),
    optparse::make_option(
      c("-b", "--base"),
      action = "store_true",
      default = FALSE,
      help = glue::glue(
        "Count skills in the base résumé ",
        "instead of those for a specific period/job"
      )
    )
  )

  parser <- optparse::OptionParser(
    usage = "cv open [id='latest'] [--period PERIOD='latest'] [--base]",
    option_list = opt_list,
    description = "Open a job output directory.",
    epilogue = "
See also:
    {cv review}
        Review application details.
    "
  )
  args_parsed <- optparse::parse_args(
    parser,
    args = args,
    positional_arguments = TRUE
  )
  pos_args <- args_parsed$args
  opts <- args_parsed$options
  
  if (opts$base) {
    output_path <- get_path_to("output")
    alert_opening(output_path)
    fs::file_show(output_path)

  } else {
    id <- ifelse(length(pos_args) > 0, pos_args[1], "latest")

    output_path <- autocv::get_app_info(
      id = id,
      app_period = opts$period,
      field="output_path"
    )$output_path[1]

    autocv::alert_opening(output_path)
    fs::file_show(output_path)
  }
}

# TODO: cli_delete: delete_app() (block if not latest or if status is applied)

# TODO: cli_help: just output notes on latex, conditional formatting:
# Note vspace, formatting wc/skills / quantifiers/aliases.
# PERF: cli_update: update_app_info() (assume latest period, take many fields)
