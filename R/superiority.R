# (c) Jim Vine
# Author: Jim Vine
# interpretation_set and confinterpret wrapper function to conduct superiority tests


#' Interpretation set for superiority tests
#'
#' An \code{\link{interpretation_set}} object used for conducting superiority
#' tests. A convenient wrapper function, \code{\link{interpret_superiority}},
#' is provided, making use of this object.
#'
#' @export
#'
interpretations_superiority <- structure(list(
  boundary_names = c("Null value"),
  # region_names   = c("Inferiority",
  #                    "Superiority"),
  placeholders = c(comparison_intervention = "$comp",
                   tested_intervention = "$test"),
  # Don't think the list version adds anything much.
  # (Would allow $ referencing as well as [ and [[. Not sure that adds.)
  #
  # placeholders = list(comparison_intervention = "$comp",
  #                  tested_intervention = "$test"),
  interpretations = list(
    list(interpretation_short = "Inferior",
         interpretation       = "$test inferior to $comp",
         interpretation_md    = "$test **inferior** to $comp"),
    list(interpretation_short = "Inconclusive",
         interpretation       = paste("Inconclusive:",
                                      "$test not shown to be superior to $comp"),
         interpretation_md    = paste("**Inconclusive**:",
                                      "$test not shown to be superior to $comp")),
    list(interpretation_short = "Superior",
         interpretation       = "$test superior to $comp",
         interpretation_md    = "$test **superior** to $comp")
  )), class = "interpretation_set")


#' Superiority test interpretations of confidence intervals.
#'
#' Conduct superiority tests on confidence intervals using a standard set of
#' interpretations. Takes a confidence interval around an effect size measure,
#' for example from the results from a randomised controlled trial comparing
#' the outcome for an intervention group to a control group.
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
#' (Whislt these may work well as short descriptions for outputting from
#' this function, in your reporting you will still normally want to provide
#' information about what exactly those in a comparison group got.)
#'
#' This function is provided in the form of a convenience wrapper for
#' \code{\link{confinterpret}}, using
#' \code{\link{interpretations_superiority}} as its
#' \code{\link{interpretation_set}}.
#'
#' @param null_value
#'   The value that precisely zero difference would have in
#'   the parameter being examined. For an absolute measure this will typically
#'   be 0. For a relative measure it will typically be 1. For superiority tests
#'   this is the point value that the confidence interval is compared at.
#' @param groups
#'   A character vector of length 2 containing short descriptive names of the
#'   groups being compared, such as the names of the interventions being
#'   compared if the confidence interval is derived from an outcome effect
#'   size measure in a randomised controlled trial. Give the name of the
#'   intervention given to the comparison or control group first and the new
#'   or tested intervention second.
#' @param beneficial_outcome Is the outcome to be treated as beneficial
#'   (i.e., a higher value of the outcome is superior)? For harmful
#'   outcomes (where lower numbers are better), set this to FALSE. If, for
#'   example, the outcome is measuring something like prevalence of patients
#'   recovering from a disease, that is likely to be beneficial; if it is
#'   measuring the prevalence of patients falling ill with a disease it is
#'   likely to be \strong{not} beneficial.
#'
#' @inheritParams confinterpret
#' @export
#'
interpret_superiority <- function(ci, null_value = 0,
                                  groups = c("Control intervention",
                                             "Test intervention"),
                                  beneficial_outcome = TRUE) {
  # TODO: Check null_value single finite etc.


  comparison_labels <- c(comparison_intervention = groups[1],
                         tested_intervention = groups[2])

  confinterpret(ci, interpretations_superiority, null_value, comparison_labels,
                beneficial_outcome)
}

