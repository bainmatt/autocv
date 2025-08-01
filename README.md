# autocv

<!-- badges: start -->
[![build](https://github.com/bainmatt/autocv/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bainmatt/autocv/actions/workflows/R-CMD-check.yaml)
[![docs](https://github.com/bainmatt/autocv/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/bainmatt/autocv/actions/workflows/pkgdown.yaml)
<!-- badges: end -->

The `autocv` package provides a straightforward, unified, data-driven workflow
for constructing, tailoring, and maintaining a curriculum vitae (and related
job application documents) from data stored in spreadsheets.

## Overview

### User interface

The core interface of `autocv` is encapsulated in the following functions:

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
(plain text and/or PDF résumé, PDF or HTML curriculum vitae,
cover letter, etc.).

4. [Check](https://bainmatt.github.io/autocv/reference/run_skill_count.html)
application keywords.
Given a term bank of relevant job keywords as well as a job posting and
the application documents for a given job of interest, extract keywords
from your job application and cross-check them against the job posting.

5. [Review](https://bainmatt.github.io/autocv/reference/get_app_info.html)
job application data.
Retrieve information about your existing job applications.

### Command-line interface

The `autocv` package comes with a built-in CLI called `cv` which exposes
its core functionality in a compact, user-friendly way:

```bash
>>> cv --help
Usage: cv <command> [options]

Available commands:
  build     Build an application directory.
  edit      Edit the application data files.
  render    Render your application documents.
  check     Check application keywords.
  review    Review job application data.
  apply     Set the status of a submitted application.
  open      Open a job output directory.

Run 'cv <command> --help' for command-specific options.
```

<details><summary>build</summary>

```bash
>>> cv build --help
Usage: cv build
[STEP 1/4] Build an application directory.

Options:
    -h, --help
        Show this help message and exit


Details:
    1.  Opens 'input/job_metadata.yml' and prompts you to enter
        metadata for a job of interest, including:
        - a unique identifier `id`
        - the current application period `period`
        - hyperlinks to the job posting

        Upon confirmation, adds a logfile entry to the application directory
        for the given period and constructs the job directory from your base
        data files:

        {applications/<period>/<job>/}

    2.  Attempts to download the job posting and waits for you to
        make edits to the retrieved text. Once confirmed, generates keyword
        reports for your reference.

See also:
    {autocv::build_base_directory}
        Creates the required stylesheets, notebooks, and base data
        files inside the 'input/' directory, including:

        {cover_data.xlsx}
        {resume_data.xlsx}
        {qa.yml}
        {job_metadata.yml}
```

</details>

<details><summary>edit</summary>

```bash
>>> cv edit --help
Usage: cv edit [id='latest'] [--period PERIOD='latest'] [--doc DOCUMENT='resume'] [--base]
[STEP 2/4] Edit application data files.

Options:
    -p PERIOD, --period=PERIOD
        Application period [default latest]

    -d DOCUMENT, --doc=DOCUMENT
        Document to edit: one of all, resume, cover, qa. Ignored with --base. [default resume]

    -b, --base
        Edit the base application data instead of a specific period/job

    -h, --help
        Show this help message and exit


Details:
    a.  Opens the following documents inside the
        'application/<period>/<job>/input/' directory for editing purposes:

        {posting_<id>.txt}
        {resume_data_<id>.xlsx}       [--doc all|resume]
        {cover_data_<id>.xlsx}        [--doc all|cover]
        {qa_<id>.yml}                 [--doc all|qa]
    
        Modify these files to tailor résumé entries, bullet points, keywords,
        and cover letter sections for a specific job application. Modify the
        following fields to change the rendered outputs:

        include
            Toggle included résumé and cover letter entries.
    
    b.  With the --base flag, opens the following documents inside the root
        'input/' directory:

        {resume_data.xlsx}            [--doc all|resume]
        {cover_data.xlsx}             [--doc all|cover]
        {qa.yml}                      [--doc all|qa]
    
        Modify these files to maintain your base list of positions, projects,
        and skills (from which job-specific files are derived) as they grow
        over time. Modify the following fields to change the rendered outputs:

        in_base
            Toggle included base résumé entries.

        core_tools
        current_tech
        key_competency
            Modify CV-specific details.

        short_summary
        tool_*
        competency_*
            Modify LinkedIn-specific details.

See also:
    {cv build}
        Must be run as a prerequisite to construct the application directory.
```

</details>

<details><summary>render</summary>

```bash
>>> cv render --help
Usage: cv render [id='latest'] [--period PERIOD='latest'] [--doc DOCUMENT='resume'] [--base] [--cv] [--linkedin]
[STEP 3/4] Render application documents.

Options:
    -p PERIOD, --period=PERIOD
        Application period. Ignored with --base, --cv, or --linkedin. [default latest]

    -d DOCUMENT, --doc=DOCUMENT
        Document to render: one of all, resume, cover. Ignored with --base, --cv, or --linkedin. [default resume]

    -s, --show
        Open the rendered documents instead of showing them in the file explorer

    -a, --achievements
        Use a list of achievements in place of the cover letter body text. Ignored unless --doc cover|all supplied.

    -b, --base
        Render the base résumé (as both a PDF and plain text file) instead of those for a specific period/job

    -c, --cv
        Render the base, web-optimized CV (as both an HTML and PDF file) instead of those for a specific period/job

    -l, --linkedin
        Render the base résumé (as both a PDF and plain text file) using the `short_summary` and `linkedin_*` fields instead of those for a specific period/job

    -h, --help
        Show this help message and exit


Details:
    a.  Using the applicable input data files and stylesheets (see `cv edit`
        and `cv base`), generates the following documents inside the
        'application/<period>/<job>/output/' directory:

        {resume_<yourname>_<id>.pdf}    [--doc all|resume]
        {resume_<yourname>_<id>.txt}    [--doc all|resume]
        {cover_<yourname>_<id>.pdf}     [--doc all|cover]
        {cover_<yourname>_<id>.txt}     [--doc all|cover]
        {email_<yourname>_<id>.txt}     [--doc all|cover]

    b.  With the --base flag, renders the following documents and saves
        them inside the root 'output/' directory:

        {resume_<yourname>.pdf}
        {resume_<yourname>.txt}

    c.  With the --cv flag, generates the following documents inside
        the root 'output/' directory:

        {cv_<yourname>.html}
        {cv_<yourname>.pdf}

    d.  With the --linkedin flag, generates the following documents inside
        the root 'output/' directory:

        {resume_<yourname>_linkedin.pdf}
        {resume_<yourname>_linkedin.txt}

See also:
    {cv edit}
        Run this first to tailor data files as desired.
```

</details>

<details><summary>check</summary>

```bash
>>> cv check --help
Usage: cv check [id='latest'] [--period PERIOD='latest'] [--base] [--linkedin]
[STEP 4/4] Check application keywords.

Options:
    -p PERIOD, --period=PERIOD
        Application period. Ignored with --base or --linkedin. [default latest]

    -b, --base
        Count skills in the base résumé instead of those for a specific period/job

    -l, --linkedin
        Count skills in the LinkedIn-appropriate base résumé instead of those for a specific period/job

    -h, --help
        Show this help message and exit


Details:
    a.  Extract job-relevant terms from a résumé and cross-check them against
        the job posting. The term bank is a plain text file with each term
        (case-sensitive) defined on a new line. By default, it is located
        inside the 'inst/extdata/resources/' directory:
    
        {skill_list.txt}
        {keyword_list.txt}

        This step generates CSV files inside the
        'application/<period>/<job>/output/' directory, namely:

        {keyword_counts_<id>.csv}
        {skill_counts_posting_<id>.csv}
        {skill_counts_resume_<id>.csv}
        {skill_report_<id>.csv}

    b.  With the --base flag, only prints skill counts based on the base
        résumé With the --linkedin flag, only prints skill counts based on
        the LinkedIn-appropriate base résumé

See also:
    {cv render}
        Run this first to generate the résumé to check.
```

</details>

<details><summary>review</summary>

```bash
>>> cv review --help
Usage: cv review [id='latest'] [--period PERIOD='latest'] [--field FIELD='all'] [--status STATUS='all'] [--report]
Review application details.

Options:
    -p PERIOD, --period=PERIOD
        Application period [default latest]

    -r, --report
        Summarize the application log by status

    -h, --help
        Show this help message and exit


See also:
    {cv check}
        Generate keyword reports.
```

</details>

### API reference

Find the documentation for these functions in the
[reference](https://bainmatt.github.io/autocv/reference/),
along with documentation for additional functions
included with the package so that developers may
tweak and extend functionality as desired.

### Data reference

The `autocv` package includes template data files which the user
should modify to reflect their work experience, skills, application text,
and metadata for any job of interest. The core data files are:

1. [`example_skill_data`](https://bainmatt.github.io/autocv/reference/example_skill_data.html):
A table with rows representing job-relevant skills, along with ratings of
skill level and flags indicating whether to include each
skill in the present job application.

1. [`example_position_data`](https://bainmatt.github.io/autocv/reference/example_position_data.html):
A table with rows representing professional roles,
degrees and certificates earned, professional projects, etc.,
with optional fields including title, company, timeline, and description text.

1. [`example_contact_data`](https://bainmatt.github.io/autocv/reference/example_contact_data.html):
A table with rows containing personal contact information and associated links
(address, email, personal website, etc.).

1. [`example_text_data`](https://bainmatt.github.io/autocv/reference/example_text_data.html):
A table with rows containing the textual building blocks of a job application,
such as a résumé bio or a cover letter.

1. [`example_job_metadata`](https://bainmatt.github.io/autocv/reference/example_job_metadata.html):
A named list with fields containing the details of a given job application,
such as the company, position, associated URLs, and a unique identifier.

1. [`example_posting`](https://bainmatt.github.io/autocv/reference/example_posting.html):
A string containing the text of an example job posting.

### Example documents

The `autocv` package includes functions and dynamic spreadsheets for generating
the following documents:

1. [PDF resume](https://bainmatt.github.io/autocv/reference/render_resume.html)
Either a general ('base') résumé for the role you are pursuing or
a tailored résumé, built upon the base, modified to suit a specific job
to which you are applying.
Uses the notebook `resume.Rmd` (see
[template](https://github.com/bainmatt/autocv/blob/main/inst/templates/template_resume.Rmd))
and the spreadsheet `resume_data.xlsx` (see
[template](https://github.com/bainmatt/autocv/blob/main/inst/templates/template_resume_data.xlsx),
as well as the corresponding example datasets
[example_skill_data](https://bainmatt.github.io/autocv/reference/example_skill_data.html) and
[example_position_data](https://bainmatt.github.io/autocv/reference/example_position_data.html)).
View an
[example](https://github.com/bainmatt/autocv/blob/main/vignettes/output/resume_yourname_AB.pdf).

1. [PDF cover letter](https://bainmatt.github.io/autocv/reference/render_cover.html):
A cover letter containing the company and position information for a
job of interest.
Uses the spreadsheet `cover_data.xlsx` (see
[template](https://github.com/bainmatt/autocv/blob/main/inst/templates/template_cover_data.xlsx),
as well as the corresponding example datasets
[example_contact_data](https://bainmatt.github.io/autocv/reference/example_contact_data.html) and
[example_text_data](https://bainmatt.github.io/autocv/reference/example_text_data.html)).

1. [Plain text résumé](https://bainmatt.github.io/autocv/reference/render_resume.html):
A resume containing the same information as the PDF résumé
and relying on the same template files but
rendered in a plain text format for simplicity and ease of data entry.
View an [example](https://github.com/bainmatt/autocv/blob/main/vignettes/output/resume_yourname_AB.txt).

1. [Plain text cover letter](https://bainmatt.github.io/autocv/reference/render_cover.html):
A cover letter containing the same information as the PDF cover letter
and relying on the same template files but
rendered in a plain text format for simplicity and ease of data entry.

1. [HTML curriculum vitae](https://bainmatt.github.io/autocv/reference/render_cv_as_html.html):
An informal, web-friendly curriculum vitae containing an ongoing record
of your professional history, a graphical breakdown of your skill set,
and a space to include an image.
Relies on the notebook `cv.Rmd` (see
[template](https://github.com/bainmatt/autocv/blob/main/inst/templates/template_cv.Rmd))
and the same spreadsheet files as the PDF and plain text résumé.
View an [example](https://bainmatt.github.io/autocv/cv_yourname.html).

1. [PDF curriculum vitae](https://bainmatt.github.io/autocv/reference/render_cv_as_html.html):
A curriculum vitae containing the same information as the HTML version
and relying on the same template files but
rendered as a PDF document for offline distribution.

1. [Professional profile curriculum vitae](https://bainmatt.github.io/autocv/reference/render_resume.html):
A streamlined curriculum vitae, rendered as a PDF using the PDF resume styling,
based on concise information, provided in the data spreadsheet,
intended for compatibility with an online professional profile
such as LinkedIn.
Relies on the same template files as the PDF and plain text résumé.

## Installation

This package is not on CRAN. If you would like to use it, you can install
it directly from GitHub by running the following in your console:

```bash
remotes::install_github("bainmatt/autocv")
```

## Usage

Follow [this](https://bainmatt.github.io/autocv/articles/demo-resume.html) demo
illustrating the basic end-to-end workflow to get started.

## Acknowledgements

This project is inspired by Nick Strayer’s [data-driven cv][ddcv]
template ([exemplar][nickstrayer]), which extends the [pagedown][pagedown]
`html_resume` template. Further inspiration and design refinements
are borrowed from Matt Leary’s [resume][ddcv-mleary] repository
([exemplar][mleary]). Thank you to the creators of these projects.

[pagedown]: https://github.com/rstudio/pagedown/tree/main "pagedown package"
[ddcv]: https://github.com/nstrayer/datadrivencv/tree/master/inst/templates "dd template"
[ddcv-mleary]: https://github.com/mleary/resume "Matt Leary template"
[nickstrayer]: https://nickstrayer.me/cv/ "Nick Strayer’s data-driven CV"
[mleary]: https://mleary.github.io/resume/ "Matt Leary’s data-driven CV"
