# CLI handlers for core autocv functionality.


#' CLI handler for 'build'.
#'
#' @family cli-handlers
#' @export
cli_build <- function(args) {
  # opt_list <- list(
  #   optparse::make_option(
  #     c("-i", "--input"),
  #     type = "character",
  #     help = "Input file"
  #   ),
  #   optparse::make_option(
  #     c("-o", "--output"),
  #     type = "character",
  #     help = "Output file"
  #   )
  # )
  # opt_list <- list()

  parser <- optparse::OptionParser(
    usage = "cv build",
    # option_list = opt_list,
    description = "
Details:
    STEP 1: Build an application directory.

    1) Opens './input/job_metadata.yml' and prompts you to enter
    metadata for a job of interest, including a unique identifier
    the current application period, and hyperlinks to the job posting.
    Upon continuing, adds a logfile entry for the given application period
    inside './applications/' and constructs the job
    directory from your base data files (.input/{{cover,resume}}_data.xlsx).

    2) Attempts to download the job posting and waits for you to
    make changes to the retrieved text as desired. Upon continuing,
    generates keyword reports for your reference.

    NOTE: This step requires `autocv::build_base_directory` to have run,
    creating the required template data files, stylesheets, and notebooks
    inside the './input' directory.
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
      c("--period"),
      type = "character",
      default = "latest",
      help = "Application period (default=%default)",
      metavar = "period"
    ),
    optparse::make_option(
      c("--base"),
      action = "store_true",
      default = FALSE,
      help = "Edit the base application data instead of a specific period/job"
    )
  )

  parser <- optparse::OptionParser(
    usage = "cv edit [id='latest'] [--period PERIOD='latest'] [--base]",
    option_list = opt_list,
    description = "
Details:
    STEP 2: Edit application data files.

    Opens './application/<period>/<job>/{{resume,cover}}_data_<job>.xlsx'
    for editing purposes. Tailor resume entries, bullet points, keywords,
    and cover letter sections as desired.

    If the --base flag is supplied, instead opens
    './input/{{resume,cover}}_data.xlsx'. Modify your base application data,
    adding positions, projects, and skills as they accumulate.

    NOTE: This step depends on `cv build`.
    "
  )
  args_parsed <- optparse::parse_args(
    parser,
    args = args,
    positional_arguments = TRUE
  )
  pos_args <- args_parsed$args
  opts <- args_parsed$options
  
  # if (!opts$base & length(pos_args) == 0) {
  #   stop(glue::glue("
  #     Error: Require <id> argument unless --base is specified.
  #     Run with --help for usage.
  #   ")
  # }
  if (opts$base) {
    autocv::edit_base()
  } else {
    id <- ifelse(length(pos_args) > 0, pos_args[1], "latest")
    autocv::edit_app(id = id, app_period = opts$period)
  }
}


#' CLI handler for 'render'.
#'
#' @family cli-handlers
#' @export
cli_render <- function(args) {
  # TODO: App (default) flags: cover=TRUE,email=TRUE,use_bullets=TRUE (for cov)
  # TODO: --base|-b flags: cover=TRUE, report_counts=TRUE
  # TODO: --linkedin|-li flags: report_counts=TRUE
  # --nocounts,-nc --nobullets,-nb, --nocover,-nc, --noemail,-ne
  opt_list <- list(
    optparse::make_option(
      c("--period"),
      type = "character",
      default = "latest",
      help = "Application period (default=%default)",
      metavar = "period"
    ),
    optparse::make_option(
      c("--base"),
      action = "store_true",
      default = FALSE,
      help = "Edit the base application data instead of a specific period/job"
    )
  )
  
  parser <- optparse::OptionParser(
    usage = "cv render [id='latest'] [--period PERIOD='latest'] [--base]",
    option_list = opt_list,
    description = "
Details:
    STEP 3: Render your application documents.

    XXX

    If the --base flag is supplied, instead XXX

    If the --linkedin flag is supplied, instead XXX

    NOTE: This step should follow `cv edit`, which depends on `cv build`.
    "
  )
  args_parsed <- optparse::parse_args(
    parser,
    args = args,
    positional_arguments = TRUE
  )
  pos_args <- args_parsed$args
  opts <- args_parsed$options
  
  # if (!opts$base & length(pos_args) == 0) {
  #   stop(glue::glue("
  #     Error: Require <id> argument unless --base is specified.
  #     Run with --help for usage.
  #   ")
  # }
  if (opts$base) {
    autocv::render_base()
  } else {
    id <- ifelse(length(pos_args) > 0, pos_args[1], "latest")
    autocv::render_app(
      app_id = id,
      app_period = opts$period,
      use_bullets = FALSE
    )
  }
}
# TODO: cli_check: check_skills(), count_terms_base()
# TODO: cli_report: get_app_info() get_status_report()
# TODO: cli_update: update_app_info() update_datestamp() apply_to()
# TODO: cli_open: open_app()
# TODO: cli_delete: delete_app()
