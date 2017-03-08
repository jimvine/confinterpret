# (c) Jim Vine
# Author: Jim Vine
# interpretation_set and confinterpret wrapper function to conduct superiority tests


# TODO: Do I want to @export these?
# I think yes. Make them available for printing and plotting.
# @export
interpretations_superiority <- structure(list(
  boundary_names = c("null_value"),
  # region_names   = c("inferiority",
  #                  "superiority"),
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
#' @param null_value The value that precisely zero difference would have in
#'   the parameter being examined. For an absolute measure this will typically
#'   be 0. For a relative measure it will typically be 1. For superiority tests
#'   this is the point value that the confidence interval is compared at.
#' @export
#'
interpret_superiority <- function(ci, null_value = 0,
                                  groups = c("Control intervention",
                                             "Test intervention")) {
  # TODO: Check null_value single finite etc.


  comparison_labels <- c(comparison_intervention = groups[1],
                         tested_intervention = groups[2])

  confinterpret(ci, interpretations_superiority, null_value, comparison_labels)
}

