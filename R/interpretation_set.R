# (c) Jim Vine
# Author: Jim Vine
# interpretation_set definition (constructor, validator and helper functions)



#' Interpretation Sets
#'
#' A class to define a set of interpretations for confidence intervals,
#' depending on where the lower and upper confidence limits sit.
#'
#' The set of boundaries specified in an \code{interpretation_set} can be
#' thought of as establishing a number of regions within which the lower and
#' upper confidence limits can sit. There is 1 more region than the number of
#' boundaries, since the set of regions is effectively 'less than boundary 1',
#' 'between boundary 1 : n-1 and boundary 2 : n' and 'above boundary n'.
#'
#' The valid combinations are those where the upper confidence limit is in a
#' region greater than or equal to the region of the lower confidence limit.
#' This establishes \code{sum(1 : n)} valid combinations, where n is the number
#' of regions (i.e., the number of boundaries + 1). An interpretation needs to
#' be provided for each of these combinations.
#'
#' Interpretations are provided in order. The order is based on first specifying
#' all the cases where the lower confidence limit is in the bottom region, and
#' each of the regions for the upper confidence limit (again, starting from the
#' bottom and increasing to the top); next come all of the cases where the
#' lower confidence limit is in the second-from-bottom region (in this case the
#' valid regions for the UCL will start at the second-from-bottom and go up to
#' the top region); and so on. So for a 2 region (1 boundary) situation, the
#' interpretations should be provided in the following order:
#'    \tabular{lll}{
#'    Order   \tab Lower confidence level   \tab Upper confidence level \cr
#'    1 \tab Region 1 \tab Region 1 \cr
#'    2 \tab Region 1 \tab Region 2 \cr
#'    3 \tab Region 2 \tab Region 2 \cr
#'    }
#' For a 3 region (2 boundary) situation, the interpretations should be provided
#' in this order:
#'    \tabular{lll}{
#'    Order   \tab Lower confidence level   \tab Upper confidence level \cr
#'    1 \tab Region 1 \tab Region 1 \cr
#'    2 \tab Region 1 \tab Region 2 \cr
#'    3 \tab Region 1 \tab Region 3 \cr
#'    4 \tab Region 2 \tab Region 2 \cr
#'    5 \tab Region 2 \tab Region 3 \cr
#'    6 \tab Region 3 \tab Region 3 \cr
#'    }
#'
#' Values for \code{placeholders} can be specified to enable sections of text
#' in interpretations to be replaced automatically. This can be used, for
#' example, to allow names or descriptions of intervention to be passed to
#' \code{\link{confinterpret}}, so that these can be returned in the final
#' interpretation rather than a generic description. \code{confinterpret}
#' uses \code{gsub} with \code{fixed=TRUE} to do substitutions for
#' \code{placeholders} entries, so values should be selected that will match
#' accordingly (and will not match extra items). Use of a non-alphanumeric
#' character within a placeholder can help to
#'
#'
#' @param boundary_names
#'   Character vector of boundary names. The length of this vector (i.e.,
#'   the number of boundary names listed) determines the number of boundaries
#'   for use with this interpretation_set, which also determines the number of
#'   interpretations that must be provided (see Details).
#' @param placeholders
#'   Vector of named character elements, where each item contains a string
#'   that is used within the interpretations as a placeholder, enabling a
#'   specific value to be susbtituted.
#'   Can be null
#' @param interpretations
#'   An ordered list of interpretations, one for each valid combination of
#'   confidence limits. See Details for information on the number and expected
#'   ordering of \code{interpretations} in a given \code{interpretation_set}.
#'
#' @export
#'
interpretation_set <- function(boundary_names,
                               placeholders = NULL,
                               interpretations) {
  # Just calls the constructor function and the validator function.
  validate_interpretation_set(
    new_interpretation_set(boundary_names, placeholders, interpretations))
}

# Probably not worth exporting this as can use the interpretation_set()
#   function to create new interpretation_set objects.
new_interpretation_set <- function(boundary_names,
                                   placeholders,
                                   interpretations) {
  stopifnot(is.list(interpretations),
            (is.atomic(boundary_names) || is.list(boundary_names)),
            (is.atomic(placeholders) || is.list(placeholders)))
  structure(
    list(
      boundary_names = boundary_names,
      placeholders = placeholders,
      interpretations = interpretations
    ),
    class = "interpretation_set")
}

#' Validator for interpretation_set objects
#'
#' Checks some features of the passed object to see whether they are as
#' expected for the class. See \code{\link{interpretation_set}} documentation
#' for definition of the class.
#'
#' @param interpretation_set
#'   An object to be checked to see whether it is a valid interpretation_set.
#' @return The interpretation_set object that was input, if no errors are
#'   found.
#' @export
#'
validate_interpretation_set <- function(interpretation_set) {

  # Checker: has class been set?

  if (!inherits(interpretation_set, "interpretation_set")) {
    stop(paste("Invalid interpretation_set. The object is not of class",
               "interpretation_set."))
  }

  # Checker: is it a list object with the right elements?

  if (!is.list(interpretation_set) ||
      is.null(interpretation_set$boundary_names) ||
      is.null(interpretation_set$interpretations)) {
    stop(paste("Invalid interpretation_set. Should be a list object containing",
               "at least $boundary_names and $interpretations."))
  }



  # Checker: Number of interpretations correct for number of boundaries
  # Valid interpretation_set object should have:
  #   number of regions = number boundaries + 1
  #   number of options = sum( 1 : number of regions )
  #     Because the lower confidence limit can validly be in any of the
  #     regions, and for each of those cases the upper confidence limit
  #     can be in any region equal to or above the region the lower limit is
  #     in. For example, for 3 regions, LCL can be 1, 2 or 3. Then:
  #       If LCL = 1, there are 3 valid options (UCL = 1, 2 or 3).
  #       If LCL = 2, there are 2 valid options (UCL = 2 or 3).
  #       If LCL = 3, there is 1 valid option (UCL = 3).
  #     Giving 3+2+1 options total.
  # sum (1 : n) can be calculated by arithmetic progression rules. n*(n+1)/2

  number_regions <- length(interpretation_set$boundary_names) + 1
  number_interpretations <- length(interpretation_set$interpretations)

  if (number_interpretations != (number_regions * (number_regions + 1) / 2)) {
    stop(paste("Invalid interpretation_set. The number of interpretations",
               "specified is not correct for the number of boundaries."))
  }

  # Check: no duplicates in placeholder names.
  if (anyDuplicated(names(interpretation_set$placeholders)) > 0) {
    stop(paste("One or more duplicate names found in",
               "interpretation_set$placeholders.",
               "All elements should have unique names."))
  }


  # Check: no duplicates in placeholder values.
  if (anyDuplicated(interpretation_set$placeholders) > 0) {
    stop(paste("One or more duplicate values found in",
               "interpretation_set$placeholders.",
               "All elements should have unique values."))
  }
  # Return the primary input.
  interpretation_set
}
