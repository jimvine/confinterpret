library(confinterpret)
context("interpretation_set objects")

test_that("interpretations_superiority is valid interpretation_set object", {
  expect_equal(validate_interpretation_set(interpretations_superiority),
               interpretations_superiority)
})

test_that("interpretations_noninferiority is valid interpretation_set object", {
  expect_equal(validate_interpretation_set(interpretations_noninferiority),
               interpretations_noninferiority)
})

test_that("interpretations_equivalence is valid interpretation_set object", {
  expect_equal(validate_interpretation_set(interpretations_equivalence),
               interpretations_equivalence)
})
