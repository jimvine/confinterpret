# (c) Jim Vine
# Author: Jim Vine
# interpretation_set and confinterpret wrapper function to conduct non-inferiority
#   tests


interpretations_noninferiority <- structure(list(
  boundary_names = c("noninf_margin",
                     "actual_null"),
  # region_names   = c("inferiority",
  #                    "non-inferiority",
  #                    "superiority"),
  placeholders = c(comparison_intervention = "$comp",
                   tested_intervention = "$test"),
  interpretations = list(
    #F
    list(interpretation_short = "Inferior",
         interpretation       = "$test inferior to $comp",
         interpretation_md    = "$test **inferior** to $comp"),
    #E
    list(interpretation_short = "Inconclusive",
         interpretation       = paste("Inconclusive:",
                                      "$test not shown to be non-inferior to $comp"),
         interpretation_md    = paste("**Inconclusive**:",
                                      "$test not shown to be non-inferior to $comp")),
    #C
    list(interpretation_short = "Inconclusive",
         interpretation       = paste("Inconclusive:",
                                      "$test not shown to be superior or",
                                      "non-inferior to $comp"),
         interpretation_md    = paste("**Inconclusive**:",
                                      "$test not shown to be superior or",
                                      "non-inferior to $comp")),
    #D
    list(interpretation_short = "Non-inferior",
         interpretation       = "$test non-inferior to $comp",
         interpretation_md    = "$test **non-inferior** to $comp"),
    #B
    list(interpretation_short = "Non-inferior",
         interpretation       = paste("$test non-inferior to $comp",
                                      "but not shown to be superior"),
         interpretation_md    = paste("$test **non-inferior** to $comp",
                                      "but not shown to be superior")),
    #A
    list(interpretation_short = "Superior",
         interpretation       = "$test superior to $comp",
         interpretation_md    = "$test **superior** to $comp")
  )), class = "interpretation_set")


#' Non-inferiority test interpretations of confidence intervals.
#'
#' @param ni_margin Numerical value specifying the non-inferiority
#'    margin to be used.
#' @param actual_null The value that precisely zero difference would have in
#'   the parameter being examined. For an absolute measure this will typically
#'   be 0. For a relative measure it will typically be 1. This is the starting
#'   point that the ni_margin is applied to in order to establish the point /
#'   region for comparison.
#' @export
#'
interpret_noninferiority <- function(ci, actual_null = 0, ni_margin = 0.1,
                                     groups = c("Control intervention",
                                                "Test intervention")) {
  # TODO: sort directionality / is_beneficial in here

  noninf_boundary <- actual_null - ni_margin
  boundaries <- c(noninf_boundary, actual_null)

  comparison_labels <- c(comparison_intervention = groups[1],
                         tested_intervention = groups[2])

  confinterpret(ci, interpretations_noninferiority, boundaries,
                comparison_labels)
}
