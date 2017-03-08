# (c) Jim Vine
# Author: Jim Vine
# interpretation_set definition (constructor, validator and helper functions)


#' @export
#'
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


#' @export
#'
validate_interpretation_set <- function(interpretation_set) {


  number_regions <- length(interpretation_set$boundary_names) + 1
  number_interpretations <- length(interpretation_set$interpretations)

  # Checker: Number of interpretations correct for number of boundaries
  # Valid interpretation_set object should have:
  #   regions = boundaries + 1
  #   options = sum( 1 : regions )
  # (That's an arithmetic progression. n*(n+1)/2 )
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
}
