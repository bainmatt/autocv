# Routines to prepare template datasets included with `autocv`.

library(autocv)
library(usethis)

# Load the datasets
example_position_data <- autocv::load_application_data(
  filename = "resume_data.xlsx",
  sheet = "entries",
  skip = 1
)
example_skill_data <- autocv::load_application_data(
  filename = "resume_data.xlsx",
  sheet = "skills",
  skip = 2
)
example_contact_data <- autocv::load_application_data(
  filename = "cover_data.xlsx",
  sheet = "contact_info"
)
example_text_data <- autocv::load_application_data(
  filename = "cover_data.xlsx",
  sheet = "text_blocks"
)
example_job_info <- autocv::load_job_info(
  filename = "job_metadata.yml"
)
example_posting <- readLines(
  file.path(autocv::get_path_to("input"), "posting.txt")
)

# Save the datasets in the data/ directory
usethis::use_data(example_position_data, overwrite = FALSE)
usethis::use_data(example_skill_data, overwrite = FALSE)
usethis::use_data(example_contact_data, overwrite = FALSE)
usethis::use_data(example_text_data, overwrite = FALSE)
usethis::use_data(example_job_info, overwrite = FALSE)
usethis::use_data(example_posting, overwrite = FALSE)
