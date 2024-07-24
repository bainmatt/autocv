# Routines to prepare template datasets included with `autocv`.

library(autocv)
library(usethis)

# Load the datasets
example_position_data <- autocv::load_application_data(
  target = "base",
  filename = "template_resume_data.xlsx",
  sheet = "entries",
  # skip = 1,
  data_dir = "templates"
)
example_skill_data <- autocv::load_application_data(
  target = "base",
  filename = "template_resume_data.xlsx",
  sheet = "skills",
  # skip = 2,
  data_dir = "templates"
)
example_contact_data <- autocv::load_application_data(
  target = "base",
  filename = "template_cover_data.xlsx",
  sheet = "contact_info",
  data_dir = "templates"
)
example_text_data <- autocv::load_application_data(
  target = "base",
  filename = "template_cover_data.xlsx",
  sheet = "text_blocks",
  data_dir = "templates"
)
example_job_metadata <- autocv::load_job_info(
  filename = "template_job_metadata.yml",
  dir = "templates"
)
example_posting <- readLines(
  file.path(autocv::get_path_to("resources"), "posting.txt")
)

# Save the datasets in the data/ directory
usethis::use_data(example_position_data, overwrite = TRUE)
usethis::use_data(example_skill_data, overwrite = TRUE)
usethis::use_data(example_contact_data, overwrite = TRUE)
usethis::use_data(example_text_data, overwrite = TRUE)
usethis::use_data(example_job_metadata, overwrite = TRUE)
usethis::use_data(example_posting, overwrite = TRUE)
