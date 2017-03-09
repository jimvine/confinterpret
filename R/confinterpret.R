# (c) Jim Vine
# Author: Jim Vine
# Function to derive qualitative interpretations of confidence intervals




# Need to think about different CIs.
# Not all are comparing 2 groups.
# So 'groups' might not make sense.
# Might be comparing one population to a particular value.
#
# Possibly make S3.
# For htest objects, look inside to find the ci


# =============================================================================
# Interpret Confidence Intervals
# =============================================================================


#' Descriptive interpretations of confidence intervals.
#'
#' Produces descriptive interpretations of confidence intervals, depending
#' on the type of test specified by an \code{\link{interpretation_set}}.
#'
#' Helpful wrapper functions are provided for some commonly used types of
#' test:
#'
#' \describe{
#'   \item{Superiority tests}{\code{\link{interpret_superiority}}}
#'   \item{Non-inferiority tests}{\code{\link{interpret_noninferiority}}}
#'   \item{Equivalence tests}{\code{\link{interpret_equivalence}}}
#' }
#'
#' @param ci
#'   A single row from a matrix of the type returned by \code{confint()},
#'   containing the confidence interval for the parameter estimate. The two
#'   columns provide the lower and upper confidence limits.
#' @param interpretation_set
#'   List-based object that specifies the boundaries between regions that each
#'   of the confidence limits can fill in, and the interpretations to be
#'   returned in each of the cases.
#' @param boundaries
#'   Vector of numbers specifying the values for each of the boundaries defined
#'   in the \code{interpretation_set}.
#' @param comparison_labels
#'   Character vector specifying the labels to be used within the
#'   interpretation to describe the comparison. Required if the
#'   \code{interpretation_set} includes a $placeholders entry. Null otherwise.
#'
#' @return A list object with elements stating the interpretation in different
#'   formats.
#' @examples
#' # Establish a test confidence interval
#' ci_test <- matrix(c(-0.1,0.1),
#'                   nrow = 1, dimnames = list("estimate",
#'                                             c("2.5 %","97.5 %")))
#' confinterpret(ci_test, interpretations_superiority, 0,
#'   comparison_labels = c(comparison_intervention = "Treatment as usual",
#'                         tested_intervention = "New treatment"))
#'
#' @export
#'
confinterpret <- function(ci,
                          interpretation_set,
                          boundaries,
                          comparison_labels = NULL) {


  # TODO: Implement some sort of equivalent to beneficial_outcome
  #       (If so, ensure it's flagged in the example, or uses a default.)
  #       ? Also in the boundaries increasing order checker.
  # @param beneficial_outcome
  #   Is the outcome to be treated as beneficial (i.e., a higher value of the
  #   outcome is superior)?


  # Validation checks =========================================================

  # Checker: validate interpretation_set
  validate_interpretation_set(interpretation_set)


  # Checker: Length of comparison_labels same as length of
  #          interpretation_set$placeholders
  if (length(comparison_labels) != length(interpretation_set$placeholders)) {
    stop(paste("The number of comparison_labels provided  does not match the",
               "number needed for this interpretation_set."))
  }

  # Check: no duplicate names in comparison_labels
  if (anyDuplicated(names(comparison_labels)) > 0) {
    stop(paste("One or more duplicate names found in comparison_labels.",
               "All elements should have unique names."))
  }

  # Check: are the names of the labels the same as those expected?
  # (setequal should be fine here as long as we are also checking for
  # duplicates in each of the things being compared.)
  if (!setequal(names(interpretation_set$placeholders),
                names(comparison_labels))) {
    stop(paste("The comparison_labels provided are not named the same as",
               "expected for this interpretation_set. Expected:",
               names(interpretation_set$placeholders)))
  }


  # Checker: Are boundaries in increasing order?

  if (is.unsorted(boundaries)) {
    stop(paste("The boundaries provided are not in increasing order."))
  }


  # Checker: Length of boundaries same as length of
  #          interpretation_set$boundary_names
  if (length(boundaries) != length(interpretation_set$boundary_names)) {
    stop(paste("The number of boundaries does not match the number needed",
               "for this interpretation_set."))
  }

  # TODO: Check ci length 2

    ci_lower <- ci[[1]]
  ci_upper <- ci[[2]]

  # Checker: Are lower and upper ends of CI the right way round?
  if (ci_upper < ci_lower) {
    stop(paste("Upper end of confidence interval must not be less than",
               "lower end."))
  }



  # Prepare regions ===========================================================

  # Establish a list of the top ends of each region. The same as the
  # boundaries, plus Inf to make a top end for the last boundary.
  # Needed for the min(which()) approach used below to find which region
  # each of the two CI ends are in.
  region_tops <- c(boundaries, Inf)
  number_regions <- length(region_tops)



  # Establish the interpretation ==============================================

  # Find the region by looking for the first boundary that it is lower than.
  region_lower <- min(which(ci_lower < region_tops))
  region_upper <- min(which(ci_upper < region_tops))

  # Calculate the interpretation that relates to this pair of regions.
  interpretation_number <- region_upper +
    sum(0 : (number_regions - 1)) -
    sum(0 : (number_regions - region_lower))

  # Extract the relevant interpretation fromt the interpretation_set.
  interpretation <- interpretation_set$interpretations[[interpretation_number]]


  # Perform replacements on labelled text -------------------------------------

  # Only necessary if there are comparison_labels. (It is possible to have
  #   interpretation_set objects with no placeholders, which don't need
  #   replacements.)
  if(!is.null(comparison_labels)) {
    for (label_name in names(comparison_labels)){
      interpretation <- gsub(interpretation_set$placeholders[[label_name]],
                             comparison_labels[[label_name]],
                             interpretation, fixed = TRUE)
    }
  }

  # Done ----------------------------------------------------------------------

  return(interpretation)
}


