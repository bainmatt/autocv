# autocv

<!-- badges: start -->
[![build](https://github.com/bainmatt/autocv/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bainmatt/autocv/actions/workflows/R-CMD-check.yaml)
[![docs](https://github.com/bainmatt/autocv/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/bainmatt/autocv/actions/workflows/pkgdown.yaml)
<!-- badges: end -->

This is a package which aims to provide a unified workflow for constructing 
job application documents (such as a curriculum vitae, or CV),
from data stored in spreadsheets.

The central workflow is provided as a user interface consisting of the
following commands:

1. ***Build** an application directory*.
Given a job of interest, construct a directory containing all required
documents, including template data files, to build a tailored job application.

TODO: extend this

2. 

Find a reference for these functions [here](), along with the documentation
for all related functions provided with the package for interested developers
to tweak and extend package functionality as desired.

Find a reference for all the relevant data files [here](). At the core
are the following documents, provided with the package as templates:

TODO: briefly describe these

1. 

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

TODO: include disclaimer in vignette (custom div) that it may be out of date

Follow [this](./articles/example-cv.html) vignette in the autocv documentation 
to get started.

TODO: clean this up or remove

Alternatively, if you're in a hurry, the most straightforward way to get
up and running is to: 

1. Install the project
2. Copy [these](link) spreadsheets into your data/ directory
3. Copy [these](link) stylesheets into your inst/extdata directory
4. Open `resume_data.xlsx` and `cover_data.xlsx` and modify the entries
5. Set your paths and job info in `job_data.xlsx`
6. Run the commands in the [Makefile](link)

# Acknowledgements

This project is inspired by, and heavily borrows from, Nick Strayer’s 
excellent [data-driven cv][ddcv] template ([exemplar][nickstrayer]), 
which builds upon the [pagedown][pagedown] `html_resume` template. 
Further inspiration and design refinements were borrowed from 
Matt Leary’s [resume][ddcv-mleary] repository ([examplar][mleary]). 
Thank you to the creators of these projects.

[pagedown]: https://github.com/rstudio/pagedown/tree/main	"pagedown package"
[ddcv]: https://github.com/nstrayer/datadrivencv/tree/master/inst/templates	"dd template"
[ddcv-mleary]: https://github.com/mleary/resume	"Matt Leary template"
[nickstrayer]: https://nickstrayer.me/cv/	"Nick Strayer’s data-driven CV"
[mleary]: https://mleary.github.io/resume/	"Matt Leary’s data-driven CV"
