library(confinterpret)
context("Boundaries in wrong order")

test_that("boundaries in wrong order give error", {

  ci_above <- matrix(c(0.15, 0.25),nrow = 1,
                     dimnames = list("estimate", c("2.5 %","97.5 %")))

  expect_error(
    confinterpret(ci = ci_above,
                  interpretation_set = interpretations_noninferiority,
                  boundaries = c(0.1, 0),
                  comparison_labels = c(comparison_intervention = "A",
                                        tested_intervention = "B"),
                  low_to_high = TRUE),
    "If low_to_high is TRUE the boundaries must be provided in increasing order.")


  expect_error(
    confinterpret(ci = ci_above,
                  interpretation_set = interpretations_noninferiority,
                  boundaries = c(0, 0.1),
                  comparison_labels = c(comparison_intervention = "A",
                                        tested_intervention = "B"),
                  low_to_high = FALSE),
    "If low_to_high is FALSE the boundaries must be provided in decreasing order.")

})
