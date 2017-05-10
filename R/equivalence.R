# (c) Jim Vine
# Author: Jim Vine
# interpretation_set and confinterpret wrapper function to conduct equivalence
#   tests


#' Interpretation set for equivalence tests
#'
#' An \code{\link{interpretation_set}} object used for conducting equivalence
#' tests. A convenient wrapper function, \code{\link{interpret_equivalence}},
#' is provided, making use of this object.
#'
#' @export
#'
interpretations_equivalence <- structure(list(
  boundary_names = c("Equivalence / inferiority",
                     "Equivalence / superiority"),
  # region_names   = c("Inferiority",
  #                    "Equivalence",
  #                    "Superiority"),
  placeholders = c(comparison_intervention = "$comp",
                   tested_intervention = "$test"),
  interpretations = list(
    # A
    list(interpretation_short = "Non-equivalent",
         interpretation       = "Non-equivalent: $test inferior to $comp",
         interpretation_md    = "**Non-equivalent**: $test inferior to $comp"),
    # B
    list(interpretation_short = "Inconclusive",
         interpretation       = paste("Inconclusive:",
                                      "$test not shown to be equivalent to $comp"),
         interpretation_md    = paste("**Inconclusive**:",
                                      "$test not shown to be equivalent to $comp")),
    # C
    list(interpretation_short = "Inconclusive",
         interpretation       = paste("Inconclusive:",
                                      "$test not shown to be equivalent to $comp"),
         interpretation_md    = paste("**Inconclusive**:",
                                      "$test not shown to be equivalent to $comp")),
    # D
    list(interpretation_short = "Equivalent",
         interpretation       = "$test equivalent to $comp",
         interpretation_md    = "$test **equivalent** to $comp"),
    # E
    list(interpretation_short = "Inconclusive",
         interpretation       = paste("Inconclusive:",
                                      "$test not shown to be equivalent to $comp"),
         interpretation_md    = paste("**Inconclusive**:",
                                      "$test not shown to be equivalent to $comp")),
    # F
    list(interpretation_short = "Non-equivalent",
         interpretation       = "Non-equivalent: $test superior to $comp",
         interpretation_md    = "**Non-equivalent**: $test superior to $comp")
  )), class = "interpretation_set")




#' Equivalence test interpretations of confidence intervals.
#'
#' Conduct equivalence tests on confidence intervals using a standard set of
#' interpretations. Takes a confidence interval around an effect size measure,
#' for example from the results from a randomised controlled trial comparing
#' the outcome for an intervention group to a control group.
#'
#' Equivalence tests can be specified in analysis plans when the aim is to
#' check whether a new intervention performs the same as an old one.
#' The test is most appropriate
#' where the aim is not to result in a better or worse outcome, but the same as
#' under the previous intervention. One
#' particular use is for testing new versions of medicines, such as generic
#' versions of drugs after the branded version's patent protection has ended.
#' In this situation, if the generic manufacturer is correctly producing the
#' medicine it should result in neither better nor worse outcomes than the
#' branded medicine.
#'
#' When conducting equivalence tests, an equivalence margin is specified. This
#' is the region around a true null (i.e., no difference) result that is deemed
#' to be within a reasonable range. It is commonly selected to include the
#' range of differences that would be of no practical significance.
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
#' \code{\link{interpretations_equivalence}} as its
#' \code{\link{interpretation_set}}.
#'
#' @param eq_margin Numerical value specifying the equivalence margin
#'   to be used.
#' @param actual_null The value that precisely zero difference would have in
#'   the parameter being examined. For an absolute measure this will typically
#'   be 0. For a relative measure it will typically be 1. This is the starting
#'   point that the \code{eq_margin} is applied to in order to establish the
#'   region for comparison.
#' @inheritParams confinterpret
#' @inheritParams interpret_superiority
#' @inherit confinterpret return
#' @export
#'
interpret_equivalence <- function(ci, actual_null = 0, eq_margin = 0.1,
                                  groups = c("Control intervention",
                                             "Test intervention"),
                                  beneficial_outcome = TRUE) {


  if(beneficial_outcome) {
    lower_equivalence <- actual_null - eq_margin
    upper_equivalence <- actual_null + eq_margin
  } else {
    lower_equivalence <- actual_null + eq_margin
    upper_equivalence <- actual_null - eq_margin
  }


  boundaries <- c(lower_equivalence, upper_equivalence)

  comparison_labels <- c(comparison_intervention = groups[1],
                         tested_intervention = groups[2])

  confinterpret(ci, interpretations_equivalence, boundaries, comparison_labels,
                beneficial_outcome)

}
