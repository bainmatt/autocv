# This script contains documentation for datasets included with `autocv`.

#' Sample skill data
#'
#' A synthetic dataset containing information about job-relevant skills,
#'  including skill ratings and inclusion/exclusion flags.
#' 
#' @format A data frame with 113 rows and 14 columns:
#' \describe{
#'   \item{category}{Name of the category
#'    under which to classify the skill (character).}
#'    
#'   \item{skill}{Name of the skill (character).}
#'   
#'   \item{in_base}{Flag indicating whether the skill should be included
#'    in the skills section of your base resume,
#'    either "x" or NA (character).}
#'    
#'   \item{include}{Flag indicating whether the skill should be included
#'    in the skills section of your tailored resume,
#'    either "x" or NA (character).}
#'    
#'   \item{level}{Level of proficiency, from 0-5 (float).}
#'   
#'   \item{core_tools}{Flag indicating whether the skill should be included
#'    in the "Core tools" section of your CV,
#'    either "x" or NA (character).}
#'    
#'   \item{current_tech}{Flag indicating whether the skill should be included
#'    in the "Current tech" section of your CV,
#'    either "x" or NA (character).}
#'    
#'   \item{key_competency}{Flag indicating whether the skill
#'    should be included in the "Key competencies" section of your CV,
#'    either "x" or NA (character).}
#'    
#'   \item{in_stack}{Flag indicating whether the skill is in your tech stack,
#'    assuming the skill is a tools or technology,
#'    either "x" or NA (character).}
#'    
#'   \item{is_a_tool}{Flag indicating whether the skill
#'    is a tool or technology, either "x" or NA (character).}
#'    
#'   \item{in_resume}{Logical value indicating whether the skill is mentioned
#'    in the body text of your resume; auto-populated (logical).}
#'    
#'   \item{in_profile}{Logical value indicating whether the skill is mentioned
#'    in the skills section of your abridged resume;
#'    auto-populated (logical).}
#'    
#'   \item{category_id}{Number representing the chronological position
#'    in the resume skills section of the category
#'    to which the skill belongs; auto-populated (positive integer).}
#'    
#'   \item{alias}{Name of the alias given to the category
#'    to which the skill belongs; auto-populated (character).}
#' }
#' 
#' @source Generated for demonstration purposes.
#' 
#' @examples
#' data(example_skill_data)
#' head(example_skill_data)
#' 
#' @keywords internal
"example_skill_data"


#' Sample position data
#'
#' A synthetic dataset containing information about previous and current
#'  professional roles and experiences, including work, education,
#'  certifications, projects, and publications.
#' 
#' @format A data frame with 11 rows and 35 columns:
#' \describe{
#'   \item{section}{Name of the section under which
#'    the entry should appear in the resume (character).}
#'    
#'   \item{title}{Name or title of the role, experience,
#'    or project (character).}
#'    
#'   \item{in_base}{Flag indicating whether the entry should be included
#'    in your base resume, either "x" or NA (character).}
#'    
#'   \item{include}{Flag indicating whether the entry should be included
#'    in your tailored resume, either "x" or NA (character).}
#'    
#'   \item{institution}{Name of the institution associated with
#'    the role or experience (character).}
#'   
#'   \item{loc}{Name of the location associated with
#'    the role or experience (character).}
#'   
#'   \item{start}{Start date (YYYY-MM-DD) of the role or experience;
#'    set to 1900-01-01 (the origin) to indicate that the provided
#'    end date (see below) is a target
#'    (will be rendered as "Expected - `end`" (date).}
#'   
#'   \item{end}{End date (YYYY-MM-DD) of the role or experience;
#'    set to NA to indicate that the role is ongoing
#'    (will be rendered as "`start` - Present") (date).}
#'   
#'   \item{link}{Web address to display next to the position name;
#'    optional (character).}
#'   
#'   \item{link_text}{The (clickable) text to display in place
#'    of the web address, if `link` supplied (character).}
#'   
#'   \item{short_summary}{Concise summary of the role or experience
#'    and the associated responsibilities, intended for use
#'    in a professional profile such as LinkedIn (character).}
#'   
#'   \item{description_i}{Bullet point to include under the entry,
#'    where `i` can be an integer from 1 to 5 representing the
#'    bullet's ordering (all character).}
#'   
#'   \item{skill_ix_iy}{Name of a (comma-separated) skill to include
#'    in parentheses next to bullet point `ix`, in the order given by `iy`,
#'    where `ix` and `iy` can each be an integer from 1 to 5 (all character).}
#'   
#'   \item{tool_1, tool_2, tool_3}{Name of a tool or technology required
#'    for the role or experience, intended for use
#'    in a professional profile such as LinkedIn (all character).}
#'    
#'   \item{competency_1, competency_2, competency_3}{Name of a skill
#'    (other than a tool or technology) required
#'    for the role or experience, intended for use
#'    in a professional profile such as LinkedIn (all character).}
#' }
#' 
#' @source Generated for demonstration purposes.
#' 
#' @examples
#' data(example_position_data)
#' head(example_position_data)
#' 
#' @keywords internal
"example_position_data"


#' Sample contact data
#'
#' A synthetic dataset containing personal contact information and
#'  associated links, including a home address, email, and personal websites.
#' 
#' @format A data frame with 9 rows and 5 columns:
#' \describe{
#'   \item{loc}{Name of the contact info field (character).}
#'   
#'   \item{order}{Ordinal position in which to display the item
#'    in your resume (positive integer).}
#'    
#'   \item{icon}{Name of a Font Awesome icon to display next to the item;
#'    optional (character).}
#'    
#'   \item{address_text}{Text to display for the item,
#'    in place of the `address`, if provided (see below) (character).}
#'    
#'   \item{address}{Link or path to the website, web address, or file to
#'    be redirected to when the corresponding `address_text` is clicked;
#'    optional (character).}
#' }
#' 
#' @source Generated for demonstration purposes.
#' 
#' @examples
#' data(example_contact_data)
#' head(example_contact_data)
#' 
#' @keywords internal
"example_contact_data"


#' Sample text data
#'
#' A synthetic dataset containing the textual building blocks of
#'  job application components such as a resume bio or a cover letter.
#' 
#' @format A data frame with 32 rows and 7 columns:
#' \describe{
#'   \item{loc}{Name of the text block (character).}
#'   
#'   \item{include}{Flag indicating whether the item should be included
#'    in your tailored resume, either "x" or NA (character).}
#'    
#'   \item{text}{The actual text to display for the item (character).}
#'   
#'   \item{order}{Ordinal position in which to display the item
#'    in your application documents (positive integer).}
#'    
#'   \item{word_limit}{Limit to impose on the number of words
#'    for the text block (positive integer).}
#'   
#'   \item{word_count}{Computed number of words
#'    for the text block; auto-populated (positive integer).}
#' }
#' 
#' @source Generated for demonstration purposes.
#' 
#' @examples
#' data(example_text_data)
#' head(example_text_data)
#' 
#' @keywords internal
"example_text_data"


#' Sample job metadata
#'
#' A synthetic dataset containing information about a hypothetical job.
#' 
#' @format A named list with 11 fields:
#' \describe{
#'   \item{name}{Your name (character).}
#'   
#'   \item{period}{Name of the current job application period, chosen
#'    by the user when creating the first application for the period;
#'    must be a valid directory name (character).}
#'    
#'   \item{id}{Unique identifier (relative to all other identifiers for the
#'    current period) to associate with the current application (character).}
#'    
#'   \item{base_id}{Identifier corresponding to an existing job application
#'    upon which to base the current application. If provided,
#'    the template data files for the current application
#'    directory will be copied from the `base_id` directory rather
#'    than the autocv template directory;
#'    must be an existing id for the current period; optional (character).}
#'    
#'   \item{company}{Name of the company to which you are
#'    applying (character).}
#'   
#'   \item{position}{Name of the position for which you are
#'    applying (character).}
#'   
#'   \item{portal_url}{URL of the job application portal; optional (character).}
#'   
#'   \item{posting_url}{URL of the job posting page (character).}
#'   
#'   \item{linkedin_url}{URL of the LinkedIn posting for the job application;
#'    optional (character).}
#'   
#'   \item{recruiter_email}{Email of the recruiter for the job;
#'    optional (character).}
#'   
#'   \item{notes}{Any notes to include in the log for the current
#'    job application; optional (character).}
#' }
#' 
#' @source Generated for demonstration purposes.
#' 
#' @examples
#' data(example_job_metadata)
#' head(example_job_metadata)
#' 
#' @keywords internal
"example_job_metadata"


#' Sample job posting
#'
#' A hypothetical job posting.
#' 
#' @format A character vector of length 1
#' 
#' @source Generated for demonstration purposes.
#' 
#' @examples
#' data(example_posting)
#' head(example_posting)
#' 
#' @keywords internal
"example_posting"
