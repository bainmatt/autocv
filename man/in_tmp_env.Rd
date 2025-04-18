% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/build.R
\name{in_tmp_env}
\alias{in_tmp_env}
\title{Establish a temporary working tree for testing app-building functionality.}
\usage{
in_tmp_env(expr)
}
\description{
This function creates a temporary \code{autocv} project directory with a
working tree mirroring that of the user's, which is defined in their
.Rprofile file. This temporary directory serves as an environment
for testing the following application-building functionality:
\itemize{
\item building applications
\item rendering applications
\item accessing application logs
\item running application keyword checks
}
}
\examples{
# Set temporary path environment variables ----------------------------------
in_tmp_env(message("Temporary root directory: ", Sys.getenv("ROOT")))
message("Reset root directory: ", Sys.getenv("ROOT"))

in_tmp_env(message("Temporary templates loc: ", get_path_to("templates")))
message("Reset templates loc: ", get_path_to("templates"))

# Wrap multi-line expressions -----------------------------------------------
in_tmp_env({
  x <- c(1, 2, 3)
  for (i in x) {
    print(i)
  }
})

# Handle input/output operations in the temporary environment ---------------
in_tmp_env({
  tmpdir <- get_path_to("input")
  if (!dir.exists(tmpdir)) {
    dir.create(tmpdir, recursive = TRUE)
  }

  fil <- file.path(tmpdir, "test.txt")
  con <- file(fil, open = "w")
  writeLines("this is a test", con = con)
  close(con)

  cat(paste0("In ", cli::col_blue(fil), ":\n", readLines(fil)))
})

}
\seealso{
Other build-dev: 
\code{\link{as_filename}()},
\code{\link{construct_app_metadata}()},
\code{\link{copy_files}()},
\code{\link{create_rprofile}()},
\code{\link{create_rproject}()},
\code{\link{get_latest_entry}()},
\code{\link{get_valid_opts}()},
\code{\link{open_doc_and_wait}()},
\code{\link{set_project_paths}()},
\code{\link{str_to_filename}()},
\code{\link{truncate_string}()},
\code{\link{validate_id}()},
\code{\link{validate_status}()},
\code{\link{write_app_metadata}()}
}
\concept{build-dev}
