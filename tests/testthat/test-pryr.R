test_that("where works", {
  e <- rlang::new_environment()
  f <- rlang::new_environment(parent = e)
  e$test <- ""
  expect_identical(where("test", f), e)

  expect_error(where(1))
  expect_error(where(c("scope", "unscope")))
  expect_error(where("DoEsNtExIsT", rlang::empty_env()), "Can't find DoEsNtExIsT")
})
