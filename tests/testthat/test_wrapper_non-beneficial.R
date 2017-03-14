library(confinterpret)
context("CI tests - wrapper functions, non-beneficial outcome")

test_that("CI above zero and margins returns expected results", {

  ci_above <- matrix(c(0.15, 0.25),nrow = 1,
                     dimnames = list("estimate", c("2.5 %","97.5 %")))

  expect_equal(interpret_superiority(ci_above,
                                     beneficial_outcome = FALSE)$interpretation_short,
               "Inferior")
  expect_equal(interpret_noninferiority(ci_above,
                                        beneficial_outcome = FALSE)$interpretation_short,
               "Inferior")
  expect_equal(interpret_equivalence(ci_above,
                                     beneficial_outcome = FALSE)$interpretation_short,
               "Non-equivalent")

})

test_that("CI spanning zero returns expected results", {

  ci_across <- matrix(c(-0.05, 0.05),nrow = 1,
                      dimnames = list("estimate", c("2.5 %","97.5 %")))

  expect_equal(interpret_superiority(ci_across,
                                     beneficial_outcome = FALSE)$interpretation_short,
               "Inconclusive")
  expect_equal(interpret_noninferiority(ci_across,
                                        beneficial_outcome = FALSE)$interpretation_short,
               "Non-inferior")
  expect_equal(interpret_equivalence(ci_across,
                                     beneficial_outcome = FALSE)$interpretation_short,
               "Equivalent")

})


test_that("CI just below zero returns expected results", {

  ci_below <- matrix(c(-0.15, -0.05),nrow = 1,
                     dimnames = list("estimate", c("2.5 %","97.5 %")))

  expect_equal(interpret_superiority(ci_below,
                                     beneficial_outcome = FALSE)$interpretation_short,
               "Superior")
  expect_equal(interpret_noninferiority(ci_below,
                                        beneficial_outcome = FALSE)$interpretation_short,
               "Superior")
  expect_equal(interpret_equivalence(ci_below,
                                     beneficial_outcome = FALSE)$interpretation_short,
               "Inconclusive")

})

test_that("CI substantially below zero returns expected results", {

  ci_below2 <- matrix(c(-0.25, -0.15),nrow = 1,
                      dimnames = list("estimate", c("2.5 %","97.5 %")))

  expect_equal(interpret_superiority(ci_below2,
                                     beneficial_outcome = FALSE)$interpretation_short,
               "Superior")
  expect_equal(interpret_noninferiority(ci_below2,
                                        beneficial_outcome = FALSE)$interpretation_short,
               "Superior")
  expect_equal(interpret_equivalence(ci_below2,
                                     beneficial_outcome = FALSE)$interpretation_short,
               "Non-equivalent")

})
