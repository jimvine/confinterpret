# (c) Jim Vine
# Author: Jim Vine
# interpretation_set and confinterpret wrapper function to conduct non-inferiority
#   tests

#' Interpretation set for non-inferiority tests
#'
#' An \code{\link{interpretation_set}} object used for conducting
#' non-inferiority tests. A convenient wrapper function,
#' \code{\link{interpret_noninferiority}}, is provided, making use of
#' this object.
#'
#' @inherit interpretations_superiority details
#'
#' @export
#'
interpretations_noninferiority <- structure(list(
  boundary_names = c("Non-inferiority margin",
                     "Actual null"),
  # region_names   = c("Inferiority",
  #                    "Non-inferiority",
  #                    "Superiority"),
  placeholders = c(comparison_intervention = "$comp",
                   tested_intervention = "$test"),
  interpretations = list(
    # A
    list(interpretation_short = "Inferior",
         interpretation       = "$test inferior to $comp",
         interpretation_md    = "$test **inferior** to $comp"),
    # B
    list(interpretation_short = "Inconclusive",
         interpretation       = paste("Inconclusive:",
                                      "$test not shown to be non-inferior to $comp"),
         interpretation_md    = paste("**Inconclusive**:",
                                      "$test not shown to be non-inferior to $comp")),
    # C
    list(interpretation_short = "Inconclusive",
         interpretation       = paste("Inconclusive:",
                                      "$test not shown to be superior or",
                                      "non-inferior to $comp"),
         interpretation_md    = paste("**Inconclusive**:",
                                      "$test not shown to be superior or",
                                      "non-inferior to $comp")),
    # D
    list(interpretation_short = "Non-inferior",
         interpretation       = "$test non-inferior to $comp",
         interpretation_md    = "$test **non-inferior** to $comp"),
    # E
    list(interpretation_short = "Non-inferior",
         interpretation       = paste("$test non-inferior to $comp",
                                      "but not shown to be superior"),
         interpretation_md    = paste("$test **non-inferior** to $comp",
                                      "but not shown to be superior")),
    # F
    list(interpretation_short = "Superior",
         interpretation       = "$test superior to $comp",
         interpretation_md    = "$test **superior** to $comp")
  )), class = "interpretation_set")


#' Non-inferiority test interpretations of confidence intervals.
#'
#' Conduct non-inferiority tests on confidence intervals using a standard set
#' of interpretations. Takes a confidence interval around an effect size
#' measure, for example from the results from a randomised controlled trial
#' comparing  the outcome for an intervention group to a control group.
#'
#' Non-inferiority tests are typically specified in analysis plans where a new
#' intervention is being compared to an existing one, especially if it has some
#' benefit other than the effect being measured. For example, the new
#' intervention might be cheaper than the old one, or have fewer side effects.
#' In these circumstances, the new intervention may not need to prove itself
#' more effective than the old one, but just to be not substantially worse -
#' i.e., non-inferior.
#'
#' When conducting non-inferiority tests, a non-inferiority margin is defined.
#' This is effectively the leeway of small, practically insignificant
#' differences by which the new intervention is allowed to under-perform the
#' old one and still be considered non-inferior.
#'
#' The non-inferiority margin is defined as being a small amount on the
#' inferior side of an actual null result. If using
#' \code{beneficial_outcome = TRUE} (the default), the non-inferiority margin
#' will extend below \code{actual_null}; if \code{beneficial_outcome = FALSE}
#' it extends above it.
#'
#' You are able to supply descriptive names of the interventions being
#' compared, and these will be inserted into the resultant interpretation.
#' If the comparison / baseline intervention does not have a convenient name
#' (such as "Placebo"), some of these might be suitable:
#' \itemize{
#'  \item{"Business as usual"}
#'  \item{"Treatment as usual"}
#'  \item{"No intervention"}
#' }
#' (Whilst these may work well as short descriptions for outputting from
#' this function, in your reporting you will still normally want to provide
#' information about what exactly those in a comparison group got.)
#'
#' This function is provided in the form of a convenience wrapper for
#' \code{\link{confinterpret}}, using
#' \code{\link{interpretations_noninferiority}} as its
#' \code{\link{interpretation_set}}.
#'
#' @param ni_margin Numerical value specifying the non-inferiority
#'    margin to be used. Provided as a positive number; the value of
#'    \code{beneficial_outcome} defines whether it is added to or subtracted
#'    from the \code{actual_null} value to position the boundary. See Details.
#' @param actual_null The value that precisely zero difference would have in
#'   the parameter being examined. For an absolute measure this will typically
#'   be 0. For a relative measure it will typically be 1. This is the starting
#'   point that the \code{ni_margin} is applied to in order to establish the
#'   point for comparison.
#' @inheritParams confinterpret
#' @inheritParams interpret_superiority
#' @inherit confinterpret return
#' @examples
#' # Establish a test confidence interval
#' ci_test <- matrix(c(-0.05, 0.05),
#'                   nrow = 1, dimnames = list("estimate",
#'                                             c("2.5 %", "97.5 %")))
#' interpret_noninferiority(ci_test, 0, 0.1, c("Treatment as usual",
#'                                             "New treatment"))
#'
#' @export
#'
interpret_noninferiority <- function(ci, actual_null = 0, ni_margin = 0.1,
                                     groups = c("Control intervention",
                                                "Test intervention"),
                                     beneficial_outcome = TRUE) {

  if(beneficial_outcome) {
    noninf_boundary <- actual_null - ni_margin
  } else {
    noninf_boundary <- actual_null + ni_margin
  }
  boundaries <- c(noninf_boundary, actual_null)


  comparison_labels <- c(comparison_intervention = groups[1],
                         tested_intervention = groups[2])

  confinterpret(ci, interpretations_noninferiority, boundaries,
                comparison_labels, beneficial_outcome)
}
