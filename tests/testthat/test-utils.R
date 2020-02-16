context("utils")

test_that("get_pkg_name", {

  expect_null(get_pkg_name(NULL))
  expect_null(get_pkg_name(.GlobalEnv))

  test_env <- new.env()

  attr(test_env, 'name') <- "package:test"
  expect_identical(get_pkg_name(test_env), "test")

  attr(test_env, 'name') <- "namespace:test"
  expect_identical(get_pkg_name(test_env), "test")
})

test_that("is_exported", {
  expect_true (is_exported ("pack",       getNamespace("packr")))
  expect_false(is_exported("is_exported", getNamespace("packr")))
})
