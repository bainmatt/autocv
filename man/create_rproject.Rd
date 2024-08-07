% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/build.R
\name{create_rproject}
\alias{create_rproject}
\title{Write a default .Rproject file to a new project directory and open.}
\usage{
create_rproject(root_path, project_name)
}
\description{
Write a default .Rproject file to a new project directory and open.
}
\examples{
create_rproject(root_path = tempdir(), project_name = "test_project")

}
\seealso{
Other build-dev: 
\code{\link{as_filename}()},
\code{\link{construct_app_metadata}()},
\code{\link{copy_files}()},
\code{\link{create_rprofile}()},
\code{\link{get_latest_entry}()},
\code{\link{get_valid_opts}()},
\code{\link{in_tmp_env}()},
\code{\link{open_doc_and_wait}()},
\code{\link{set_project_paths}()},
\code{\link{str_to_filename}()},
\code{\link{truncate_string}()},
\code{\link{validate_id}()},
\code{\link{validate_status}()},
\code{\link{write_app_metadata}()}
}
\concept{build-dev}
