% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prepare.R
\name{load_application_data}
\alias{load_application_data}
\title{Load application data.}
\usage{
load_application_data(
  target = c("app", "base"),
  filename = c("resume_data.xlsx", "cover_data.xlsx"),
  sheet = c("entries", "skills", "contact_info", "text_blocks"),
  data_dir = "input",
  app_id = "latest",
  app_period = "latest"
)
}
\description{
Load application data.
}
\seealso{
Other data: 
\code{\link{autocv_resources}()},
\code{\link{load_job_info}()},
\code{\link{load_log}()}
}
\concept{data}
