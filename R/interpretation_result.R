# (c) Jim Vine
# Author: Jim Vine
# interpretation_result definition (constructor, validator and helper functions)



#' Interpretation result
#'
#' A class to define the result that is returned by an interpretation conducted
#' by \code{\link{confinterpret}}.
#'
#' The parameters are the ones that were used in conducting the interpretation
#' (typically using \code{confinterpret} or one of its convenience wrapper
#' functions). See \code{\link{confinterpret}} for more details on how these
#' parameters were used in conducting the interpretation.
#'
#' @param interpretation
#'   A list object from a an \code{\link{interpretation_set}} providing the
#'   qualitative interpretation.
#' @param ci
#'   The confidence interval that was interpreted.
#' @param interpretation_set
#'   The \code{\link{interpretation_set}} object that was used to conduct the
#'   interpretation.
#' @param interpretation_set_name
#'   The name of the \code{interpretation_set} that was used for the
#'   interpretation.
#' @param boundaries
#'   The boundaries parameter that was used for the interpretation.
#' @param comparison_labels
#'   Labels that were used to describe the groups that were compared in the
#'   interpretation.
#' @param low_to_high
#'   Whether the boundaries were provided in low-to-high or high-to-low order.
#'
#' @return A list object of class \code{interpretation_result} with elements
#'   stating the interpretation in different formats
#'   (\code{$interpretation_short}, \code{$interpretation}, and
#'   \code{$interpretation_md}) and \code{$parameters}. \code{$parameters} is
#'   list object detailing the parameters that were used to generate the
#'   interpretation, and contains
#'   \code{$ci},
#'   \code{$interpretation_set},
#'   \code{$interpretation_set_name},
#'   \code{$boundaries},
#'   \code{$comparison_labels} and
#'   \code{$low_to_high}.
#'
#' @export
#'
interpretation_result <- function(interpretation,
                                  ci,
                                  interpretation_set,
                                  interpretation_set_name =
                                    deparse(substitute(interpretation_set)),
                                  boundaries,
                                  comparison_labels,
                                  low_to_high) {
  # Just calls the constructor function and the validator function.
  validate_interpretation_result(
    new_interpretation_result(interpretation,
                              ci,
                              interpretation_set,
                              interpretation_set_name,
                              boundaries,
                              comparison_labels,
                              low_to_high))
}


# Probably not worth exporting this as can use the interpretation_result()
#   function to create new interpretation_result objects.
new_interpretation_result <- function(interpretation,
                                      ci,
                                      interpretation_set,
                                      interpretation_set_name =
                                        deparse(substitute(interpretation_set)),
                                      boundaries,
                                      comparison_labels,
                                      low_to_high) {


  # Perform replacements on labelled text =====================================

  # Only necessary if there are comparison_labels. (It is possible to have
  #   interpretation_set objects with no placeholders, which don't need
  #   replacements.)
  if(!is.null(comparison_labels)) {
    for (i in 1 : length(interpretation)) {
      for (label_name in names(comparison_labels)) {
        interpretation[i] <- gsub(interpretation_set$placeholders[[label_name]],
                                  comparison_labels[[label_name]],
                                  interpretation[i], fixed = TRUE)
      }
    }
  }

  parameters <- list(ci = ci,
                     interpretation_set = interpretation_set,
                     interpretation_set_name = interpretation_set_name,
                     boundaries = boundaries,
                     comparison_labels = comparison_labels,
                     low_to_high = low_to_high)

  # stopifnot(is.list(interpretations),
  #           (is.atomic(boundary_names) || is.list(boundary_names)),
  #           (is.atomic(placeholders) || is.list(placeholders)))
  structure(
    list(interpretation_short = interpretation$interpretation_short,
         interpretation = interpretation$interpretation,
         interpretation_md = interpretation$interpretation_md,
         parameters = parameters),
    class = "interpretation_result")

  # interpretation$parameters <- list(
  #   ci = ci,
  #   interpretation_set_name = deparse(substitute(interpretation_set)),
  #   boundaries = boundaries,
  #   comparison_labels = comparison_labels,
  #   low_to_high = low_to_high
  # )
  #
  # class(interpretation) <- "interpretation_result"

}



#' Validator for interpretation_result objects
#'
#' Checks some features of the passed object to see whether they are as
#' expected for the class. See \code{\link{interpretation_result}}
#' documentation for definition of the class.
#'
#' @param x
#'   An object to be checked to see whether it is a valid
#'   interpretation_result.
#' @return The interpretation_result object that was input, if no errors are
#'   found.
#' @export
#'
validate_interpretation_result <- function(x) {

  # Check that an item is a valid result as should come from confinterpret()

  if(!inherits(x, "interpretation_result")) {
    stop("Not an object of class interpretation_result.")
  }

  # Checker: is it a list object with the right elements?

  if (!is.list(x) ||
      is.null(x$interpretation_short) ||
      is.null(x$interpretation) ||
      is.null(x$interpretation_md) ||
      is.null(x$parameters)) {
    stop(paste("Invalid interpretation result. Should be a list object",
               "containing at least $interpretation_short, $interpretation,",
               "$interpretation_md and $parameters."))
  }

  # Checker: is $parameters a list object with the right elements?

  if (!is.list(x$parameters) ||
      is.null(x$parameters$ci) ||
      is.null(x$parameters$interpretation_set) ||
      is.null(x$parameters$interpretation_set_name) ||
      is.null(x$parameters$boundaries) ||

      is.null(x$parameters$low_to_high)) {
    stop(paste("Invalid interpretation result. Should contain a list object",
               "$parameters, which should contain at least",
               "$ci,",
               "$interpretation_set,",
               "$interpretation_set_name,",
               "$boundaries,",
               " and",
               "$low_to_high."))
  }

  if(is.null(x$parameters$comparison_labels) &&
     !is.null(x$parameters$interpretation_set$placeholders)) {
    stop(paste("If the interpretation_set included $placeholders then",
               "$parameters$comparison_labels should not be NULL."))
  }

  validate_interpretation_set(x$parameters$interpretation_set)

  # Return the primary input.
  x

}
