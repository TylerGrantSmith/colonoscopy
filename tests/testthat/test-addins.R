testthat::test_that("scope_selection", {
  testthat::with_mock(
    "colonoscopy::check_rstudio" = function() FALSE,
    testthat::expect_error(colonoscopy::scope_selection())
  )
})
