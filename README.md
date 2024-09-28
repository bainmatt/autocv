# autocv

<!-- badges: start -->
[![build](https://github.com/bainmatt/autocv/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bainmatt/autocv/actions/workflows/R-CMD-check.yaml)
[![docs](https://github.com/bainmatt/autocv/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/bainmatt/autocv/actions/workflows/pkgdown.yaml)
<!-- badges: end -->

The `autocv` package provides a simple, unified workflow for constructing 
a curriculum vitae (and related job application documents)
from data stored in spreadsheets.

## Overview

### User Interface

The core of the user interface is encapsulated in
the following sequence of commands for
building and fine-tuning a job application:

1. [Build](https://bainmatt.github.io/autocv/reference/build_app_directory.html)
an application directory.
Given a job of interest, construct a directory containing the
template data files and folders required to build a tailored job application.

2. [Edit](https://bainmatt.github.io/autocv/reference/open_app.html)
the application data files.
Open the data files for a given job of interest and select
the information (sections, entries, skills, ordering)
to include or exclude from your application.

3. [Render](https://bainmatt.github.io/autocv/reference/render_app.html)
your application documents.
For a given job of interest, use the tailored data files
to generate the requested application documents
(plain text and/or PDF resume, PDF or HTML curriculum vitae,
cover letter, etc.).

4. [Check](https://bainmatt.github.io/autocv/reference/run_skill_count.html)
application keywords.
Given a term bank of relevant job keywords as well as a job posting and
the application documents for a given job of interest, extract keywords
from your job application and cross-check them against the job posting.

5. [Review](https://bainmatt.github.io/autocv/reference/get_app_info.html)
job application data.
Retrieve information about your existing job applications.

### API reference

Find the documentation for these functions
[here](https://bainmatt.github.io/autocv/reference/),
along with documentation for additional functions
included with the package so that developers may
tweak and extend functionality as desired.

### Data reference

Included with the package are several template data files which the user
should modify to reflect their work experience, skills, application text,
and metadata for any job of interest. The core data files are:

1. [`example_skill_data`](https://bainmatt.github.io/autocv/reference/example_skill_data.html):
A table with rows representing job-relevant skills, along with ratings of
skill level and flags indicating whether or not to include each
skill in the present job application.

2. [`example_position_data`](https://bainmatt.github.io/autocv/reference/example_position_data.html):
A table with rows representing professional roles,
degrees and certificates earned, professional projects, etc.,
with optional fields including title, company, timeline, and description text.

3. [`example_contact_data`](https://bainmatt.github.io/autocv/reference/example_contact_data.html):
A table with rows containing personal contact information and associated links
(address, email, personal website, etc.).

4. [`example_text_data`](https://bainmatt.github.io/autocv/reference/example_text_data.html):
A table with rows containing the textual building blocks of a job application,
such as a resume bio or a cover letter.

5. [`example_job_metadata`](https://bainmatt.github.io/autocv/reference/example_job_metadata.html):
A named list with fields containing the details of a given job application,
such as the company, position, associated URLs, and a unique identifier.

6. [`example_posting`](https://bainmatt.github.io/autocv/reference/example_posting.html):
A string containing the text of an example job posting.

### Example documents

The autocv package includes functions and dynamic spreadsheets for generating
the following documents:

1. [PDF resume](https://bainmatt.github.io/autocv/reference/render_resume.html)
Either a general ("base") resume for the role you are pursuing or
a tailored resume, built upon the base, modified to suit a specific job
to which you are applying.
Uses the notebook `resume.Rmd` (see
[template](https://github.com/bainmatt/autocv/blob/main/inst/templates/template_resume.Rmd))
and the spreadsheet `resume_data.xlsx` (see
[template](https://github.com/bainmatt/autocv/blob/main/inst/templates/template_resume_data.xlsx), 
as well as the corresponding example datasets
[example_skill_data](https://bainmatt.github.io/autocv/reference/example_skill_data.html) and
[example_position_data](https://bainmatt.github.io/autocv/reference/example_position_data.html)).
View an example
[here](https://github.com/bainmatt/autocv/blob/main/vignettes/output/resume_yourname_AB.pdf).

2. [PDF cover letter](https://bainmatt.github.io/autocv/reference/render_cover.html):
A cover letter containing the company and position information for a
job of interest.
Uses the spreadsheet `cover_data.xlsx` (see
[template](https://github.com/bainmatt/autocv/blob/main/inst/templates/template_cover_data.xlsx),
as well as the corresponding example datasets
[example_contact_data](https://bainmatt.github.io/autocv/reference/example_contact_data.html) and
[example_text_data](https://bainmatt.github.io/autocv/reference/example_text_data.html)).

3. [Plain text resume](https://bainmatt.github.io/autocv/reference/render_resume.html):
A resume containing the same information as the PDF resume
and relying on the same template files but
rendered in a plain text format for simplicity and ease of data entry.
View an example
[here](https://github.com/bainmatt/autocv/blob/main/vignettes/output/resume_yourname_AB.txt).

4. [Plain text cover letter](https://bainmatt.github.io/autocv/reference/render_cover.html):
A cover letter containing the same information as the PDF cover letter
and relying on the same template files but
rendered in a plain text format for simplicity and ease of data entry.

5. [HTML curriculum vitae](https://bainmatt.github.io/autocv/reference/render_cv_as_html.html):
An informal, web-friendly curriculum vitae containing an ongoing record
of your professional history, a graphical breakdown of your skill set,
and a space to include an image.
Relies on the notebook `cv.Rmd` (see
[template](https://github.com/bainmatt/autocv/blob/main/inst/templates/template_cv.Rmd))
and the same spreadsheet files as the PDF and plain text resume.
View an example [here](https://bainmatt.github.io/autocv/cv_yourname.html).

6. [PDF curriculum vitae](https://bainmatt.github.io/autocv/reference/render_cv_as_html.html):
A curriculum vitae containing the same information as the HTML version
and relying on the same template files but
rendered as a PDF document for offline distribution.

7. [Professional profile curriculum vitae](https://bainmatt.github.io/autocv/reference/render_resume.html):
A streamlined curriculum vitae, rendered as a PDF using the PDF resume styling,
based on concise information, provided in the data spreadsheet,
intended for compatibility with an online professional profile
such as LinkedIn.
Relies on the same template files as the PDF and plain text resume.

## Installation

This package is not on CRAN. If you would like to use it, you can install
it directly from GitHub by running the following in your console:

    remotes::install_github("bainmatt/autocv")

## Usage

Follow [this](https://bainmatt.github.io/autocv/articles/demo-resume.html) demo
illustrating the basic end-to-end workflow to get started.

## Acknowledgements

This project is inspired by Nick Strayer’s [data-driven cv][ddcv]
template ([exemplar][nickstrayer]), which extends the [pagedown][pagedown]
`html_resume` template. Further inspiration and design refinements
were borrowed from Matt Leary’s [resume][ddcv-mleary] repository
([examplar][mleary]). 
Thank you to the creators of these projects.

[pagedown]: https://github.com/rstudio/pagedown/tree/main	"pagedown package"
[ddcv]: https://github.com/nstrayer/datadrivencv/tree/master/inst/templates	"dd template"
[ddcv-mleary]: https://github.com/mleary/resume	"Matt Leary template"
[nickstrayer]: https://nickstrayer.me/cv/	"Nick Strayer’s data-driven CV"
[mleary]: https://mleary.github.io/resume/	"Matt Leary’s data-driven CV"
