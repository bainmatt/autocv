# This script contains documentation for datasets included with `autocv`.

#' Sample skill data
#'
#' A synthetic dataset containing information about job-relevant skills,
#'  including skill ratings and inclusion/exclusion flags.
#' 
#' @format A data frame with 113 rows and 14 columns:
#' \describe{
#'   \item{category}{Name of the category
#'    under which to classify the skill (character)}
#'    
#'   \item{skill}{Name of the skill (character)}
#'   
#'   \item{in_base}{Flag indicating whether the skill should be included
#'    in the skills section of your base resume,
#'    either "x" or NA (character)}
#'    
#'   \item{include}{Flag indicating whether the skill should be included
#'    in the skills section of tailored resumes,
#'    either "x" or NA (character)}
#'    
#'   \item{level}{Level of proficiency, from 0--5 (float)}
#'   
#'   \item{core_tools}{Flag indicating whether the skill should be included
#'    in the "Core tools" section of your CV,
#'    either "x" or NA (character)}
#'    
#'   \item{current_tech}{Flag indicating whether the skill should be included
#'    in the "Current tech" section of your CV,
#'    either "x" or NA (character)}
#'    
#'   \item{key_competency}{Flag indicating whether the skill
#'    should be included in the "Key competencies" section of your CV,
#'    either "x" or NA (character)}
#'    
#'   \item{in_stack}{Flag indicating whether the skill is in your tech stack,
#'    assuming the skill is a tools or technology,
#'    either "x" or NA (character)}
#'    
#'   \item{is_a_tool}{Flag indicating whether the skill
#'    is a tool or technology, either "x" or NA (character)}
#'    
#'   \item{in_resume}{Logical value indicating whether the skill is mentioned
#'    in the body text of your resume (logical)}
#'    
#'   \item{in_profile}{Logical value indicating whether the skill is mentioned
#'    in the skills section of your abridged resume (logical)}
#'    
#'   \item{category_id}{Number representing the chronological position
#'    in the resume skills section of the category
#'    to which the skill belongs (positive integer)}
#'    
#'   \item{alias}{Name of the alias given to the category
#'    to which the skill belongs (character)}
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
#' A synthetic dataset containing information about ...
#' 
#' @format A data frame with x rows and y columns:
#' \describe{
#'   \item{field}{description}
#'   \item{skill_1, skill_2, ..., skill_5}{description (all character)}
#'   \item{skill_ix_iy}{ ,
#'    where `ix` and `iy` can each be an integer from 1 to 5 (all character)}
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
#' A synthetic dataset containing information about ...
#' 
#' @format A data frame with x rows and y columns:
#' \describe{
#'   \item{field}{description}
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
#' A synthetic dataset containing information about ...
#' 
#' @format A data frame with x rows and y columns:
#' \describe{
#'   \item{field}{description}
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
#' A synthetic dataset containing information about ...
#' 
#' @format A named list with x fields:
#' \describe{
#'   \item{field}{description}
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


#' Sample posting
#'
#' A synthetic dataset containing information about ...
#' 
#' @format A ...
#' 
#' @source Generated for demonstration purposes.
#' 
#' @examples
#' data(example_posting)
#' head(example_posting)
#' 
#' @keywords internal
"example_posting"
