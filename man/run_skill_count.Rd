% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check.R
\name{run_skill_count}
\alias{run_skill_count}
\alias{check_skills}
\alias{count_terms_base}
\title{Obtain keyword counts for a given doc/term list and generate reports.}
\usage{
run_skill_count(
  app_id = "latest",
  doc = c("posting", "resume"),
  term_list_filename = c("skill_list.txt", "keyword_list.txt"),
  term_list_dir = "resources",
  log = load_log(),
  orderby = c("counts", "doc", "source"),
  filterby = c("both", "count", "matches"),
  overwrite = FALSE,
  is_posting_count = FALSE
)

check_skills(
  app_id = "latest",
  app_period = "latest",
  log = load_log(),
  orderby = c("doc", "counts", "source"),
  overwrite = FALSE,
  check_resume = TRUE
)

count_terms_base(
  input_dir = "output",
  term_list_filename = "skill_list.txt",
  term_list_dir = "resources",
  orderby = c("counts", "doc", "source"),
  filterby = c("both", "count", "matches"),
  use_abridged = FALSE
)
}
\description{
\code{run_skill_count} returns keyword counts for a given document/term list.

\code{check_skills} runs \code{run_skill_count} on a posting and compares to a resume.

\code{count_terms_base} returns keyword counts for a base resume.
}
\seealso{
Other report: 
\code{\link{count_terms}()},
\code{\link{report_skill_metrics}()},
\code{\link{run_skill_report}()}
}
\concept{report}
