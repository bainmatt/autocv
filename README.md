# autocv

<!-- badges: start -->
[![build](https://github.com/bainmatt/autocv/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bainmatt/autocv/actions/workflows/R-CMD-check.yaml)
[![docs](https://github.com/bainmatt/autocv/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/bainmatt/autocv/actions/workflows/pkgdown.yaml)
<!-- badges: end -->

The autocv package provides a simple, unified workflow for constructing 
a curriculum vitae (and related job application documents)
from data stored in spreadsheets.

##### User Interface

The core of the user interface is encapsulated in
the following sequence of commands:

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

##### Reference

Find the documentation for these functions
[here](https://bainmatt.github.io/autocv/reference/),
along with documentation for additional functions
included with the package so that developers may
tweak and extend functionality as desired.

##### Data files

Included with the package are several template data files which the user
should modify to reflect their work experience, skills, application text,
and metadata for any job of interest. The core data files are:

1. [example_skill_data](https://bainmatt.github.io/autocv/reference/example_skill_data.html):
Contains ...

WIP. finish this section

<!--
using a central spreadsheet housing resume data (work history, education, skills)

Using a functional programming approach to
automate repetitive elements of the job application process, `autocv`
provides an efficient, reproducible, easily customized and extended workflow,
!-->

# Installation

This package is not on CRAN. If you would like to use it, you can install
it directly from GitHub by running the following in your console:

    remotes::install_github("bainmatt/autocv")

# Usage

Follow [this](./articles/example-cv.html) vignette in the autocv documentation 
to get started.

WIP. add disclaimer to vignette (custom div) that parts may be out of date

<!--
Alternatively, if you're in a hurry, the most straightforward way to get
up and running is to: 

1. Install the project
2. Copy [these](link) spreadsheets into your data/ directory
3. Copy [these](link) stylesheets into your inst/extdata directory
4. Open `resume_data.xlsx` and `cover_data.xlsx` and modify the entries
5. Set your paths and job info in `job_data.xlsx`
6. Run the commands in the [Makefile](link)
!-->

# Acknowledgements

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
