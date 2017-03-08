# (c) Jim Vine
# Author: Jim Vine
# interpretation_set and confinterpret wrapper function to conduct equivalence
#   tests



interpretations_equivalence <- structure(list(
  boundary_names = c("lower_equivalence",
                     "upper_equivalence"),
  # region_names   = c("inferiority",
  #                    "equivalence",
  #                    "superiority"),
  placeholders = c(comparison_intervention = "$comp",
                   tested_intervention = "$test"),
  interpretations = list(
    #F
    list(interpretation_short = "Non-equivalent",
         interpretation       = "Non-equivalent: $test inferior to $comp",
         interpretation_md    = "**Non-equivalent**: $test inferior to $comp"),
    #E
    list(interpretation_short = "Inconclusive",
         interpretation       = paste("Inconclusive:",
                                      "$test not shown to be equivalent to $comp"),
         interpretation_md    = paste("**Inconclusive**:",
                                      "$test not shown to be equivalent to $comp")),
    #C
    list(interpretation_short = "Inconclusive",
         interpretation       = paste("Inconclusive:",
                                      "$test not shown to be equivalent to $comp"),
         interpretation_md    = paste("**Inconclusive**:",
                                      "$test not shown to be equivalent to $comp")),
    #D
    list(interpretation_short = "Equivalent",
         interpretation       = "$test equivalent to $comp",
         interpretation_md    = "$test **equivalent** to $comp"),
    #B
    list(interpretation_short = "Inconclusive",
         interpretation       = paste("Inconclusive:",
                                      "$test not shown to be equivalent to $comp"),
         interpretation_md    = paste("**Inconclusive**:",
                                      "$test not shown to be equivalent to $comp")),
    #A
    list(interpretation_short = "Non-equivalent",
         interpretation       = "Non-equivalent: $test superior to $comp",
         interpretation_md    = "**Non-equivalent**: $test superior to $comp")
  )), class = "interpretation_set")




#' Equivalence test interpretations of confidence intervals.
#'
#' @param eq_margin Numerical value specifying the equivalence margin
#'   to be used.
#' @param actual_null The value that precisely zero difference would have in
#'   the parameter being examined. For an absolute measure this will typically
#'   be 0. For a relative measure it will typically be 1. This is the starting
#'   point that the ni_margin is applied to in order to establish the point /
#'   region for comparison.
#' @export
#'
interpret_equivalence <- function(ci, actual_null = 0, eq_margin = 0.1,
                                  groups = c("Control intervention",
                                             "Test intervention")) {

  # TODO: Check eq_margin positive

  lower_equivalence <- actual_null - eq_margin
  upper_equivalence <- actual_null + eq_margin
  boundaries <- c(lower_equivalence, upper_equivalence)

  comparison_labels <- c(comparison_intervention = groups[1],
                         tested_intervention = groups[2])

  confinterpret(ci, interpretations_equivalence, boundaries, comparison_labels)

}
