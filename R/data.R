#' Sample brief dataset for eclipseplot
#'
#' A small dataset containing risk of bias assessments for testing.
#'
#' @format A data frame with 5 rows and 13 variables:
#' \describe{
#'   \item{pmid}{PubMed ID}
#'   \item{study}{Study citation label}
#'   \item{item1_step1, item1_step2, item2_step1, item2_step2, item3_step1, item3_step2, item4_step1, item4_step2, item5_step1, item5_step2, item6_step2}{Risk of bias for the core items}
#' }
"sample_brief"

#' Sample long dataset for eclipseplot
#'
#' A larger dataset with more studies and optional items.
#'
#' @format A data frame with 26 rows and 13 variables.
"sample_long"

#' Mock messy data for eclipsedf testing
#'
#' A dataset containing messy headers and non-standard values to test the
#' cleaning and standardization capabilities of the eclipsedf function.
#'
#' @format A data frame with 4 rows and 13 variables of varied naming conventions.
"messy"
