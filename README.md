# autocv

This package houses the code and template data files used to construct 
variations of my data-driven curriculum vitae. 

This approach uses the R programming language to automate repetitive 
elements of the job application process, resulting in an efficient,
reproducible, easily customized and extended workflow.

# Installation

This package is not on CRAN. If you would like to use it, you can install
it directly from GitHub by running the following in your console:

    remotes::install_github("bainmatt/autocv")

# Usage

Follow [this](./articles/example-cv.html) vignette in the autocv documentation 
to get started.

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
