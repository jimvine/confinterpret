# (c) Jim Vine
# Author: Jim Vine
# Function to derive qualitative interpretations of confidence intervals




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
#' The low_to_high parameter can be set to FALSE to facilitate the situation
#' where the boundaries are ordered high-to-low. This enables the same
#' \code{interpretation_set} object to be used for both beneficial and harmful
#' outcomes. For an \code{interpretation_set} that has been defined as if
#' higher numbers are better (for example, proportion of participants
#' recovering from a particular illness after treatment) then the inferiority
#' interpretations will be listed first and the superiority ones last. To use
#' this with a negative outcome (for example, proportion of participants
#' catching an illness after a preventative measure), provide the boundaries
#' in high-to-low order and use \code{low_to_high = FALSE}. This will also
#' work where a single boundary is specified, and will act to 'reverse' the
#' interpretations.
#'
#' The use of low_to_high only affects the order of the boundaries (and
#' the regions these implicitly define). It does \strong{not} affect the
#' ordering of the confidence interval: the numerically lower confidence limit
#' should be listed first either way.
#'
#' @param ci
#'   A single row from a matrix of the type returned by \code{confint()},
#'   containing the confidence interval for the parameter estimate. The two
#'   columns provide the lower and upper confidence limits.
#' @param interpretation_set
#'   List-based object that specifies the boundaries between regions that each
#'   of the confidence limits can fall in, and the interpretations to be
#'   returned in each of the cases.
#' @param boundaries
#'   Vector of numbers specifying the values for each of the boundaries defined
#'   in the \code{interpretation_set}. Normally provided in low-to-high order, but
#'   see the \code{low_to_high} parameter for options.
#' @param comparison_labels
#'   Character vector specifying the labels to be used within the
#'   interpretation to describe the comparison. Required if the
#'   \code{interpretation_set} includes a $placeholders entry. Null otherwise.
#' @param low_to_high
#'   Are the boundaries ordered low-to-high (TRUE) or high-to-low (FALSE)?
#'   This can be used to reverse the assessment, including in the cases where
#'   only one boundary is supplied. See Details.
#'
#' @return A list object of class \code{interpretation_result} with elements
#'   stating the interpretation in different formats, plus the parameters
#'   used to generate the interpretation.
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
                          comparison_labels = NULL,
                          low_to_high = TRUE) {


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

  if (!is.logical(low_to_high)) {
    stop("low_to_high must be a logical (TRUE or FALSE).")
  }

  # Checker: Are boundaries in increasing order?
  # (Or decreasing if low_to_high is FALSE)

  if (low_to_high) {
    if (is.unsorted(boundaries)) {
      stop(paste("If low_to_high is TRUE the boundaries must be",
                 "provided in increasing order."))
    }
  } else {
    if (is.unsorted(rev(boundaries))) {
      stop(paste("If low_to_high is FALSE the boundaries must be",
                 "provided in decreasing order."))
    }
  }


  # Checker: Length of boundaries same as length of
  #          interpretation_set$boundary_names
  if (length(boundaries) != length(interpretation_set$boundary_names)) {
    stop(paste("The number of boundaries does not match the number needed",
               "for this interpretation_set."))
  }

  # Check that ci is specified
  if(is.null(ci)) {
    stop(paste("The ci argument must be provided."))
  }

  # Check exactly two elements in the ci argument.
  if(length(ci) != 2) {
    stop(paste("Exactly 2 elements required in ci, one for the lower",
               "confidence limit, the other for the upper confidence limit.",
               "The object received had too",
               ifelse(length(ci) > 2, "many", "few"), "elements."))
  }

  # Check no duplicated elements in the ci argument.
  if(ci[1] == ci[2]) {
    stop(paste0("The two elements in ci must be unique. Both contain: ",
                ci[1]))
  }


  ci_lower <- ci[[1]]
  ci_upper <- ci[[2]]

  # Checker: Are lower and upper ends of CI the right way round?
  if (ci_upper < ci_lower) {
    stop(paste("Upper end of confidence interval must not be less than",
               "lower end."))
  }




  # Prepare regions ===========================================================

  # Establish a list of the ends of each region. The same as the
  # boundaries, plus Inf to make a top end for the last boundary.
  # (Or -Inf for low_to_high = FALSE)
  # Needed for the min(which()) approach used below to find which region
  # each of the two CI ends are in.

  if (low_to_high) {
    region_ends <- c(boundaries, Inf)
  } else {
    region_ends <- c(boundaries, -Inf)
  }

  number_regions <- length(region_ends)



  # Establish the interpretation ==============================================


  if (low_to_high) {
    # Find the region by looking for the first boundary that it is lower than.
    region_lower <- min(which(ci_lower < region_ends))
    region_upper <- min(which(ci_upper < region_ends))
  } else {
    # Find the region by looking for the first boundary that it is higher than.
    region_upper <- min(which(ci_lower > region_ends))
    region_lower <- min(which(ci_upper > region_ends))
  }

  # Calculate the interpretation that relates to this pair of regions.
  interpretation_number <- region_upper +
    sum(0 : (number_regions - 1)) -
    sum(0 : (number_regions - region_lower))

  # Extract the relevant interpretation from the interpretation_set.
  interpretation <- interpretation_set$interpretations[[interpretation_number]]

  interpretation_result <- interpretation_result(interpretation = interpretation,
                                                 ci = ci,
                                                 interpretation_set = interpretation_set,
                                                 interpretation_set_name = deparse(substitute(interpretation_set)),
                                                 boundaries = boundaries,
                                                 comparison_labels = comparison_labels,
                                                 low_to_high = low_to_high)

  # Done ======================================================================

  return(interpretation_result)
}




