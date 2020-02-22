context("utils")

test_that("find_pkg_name", {

  test_env <- new.env()

  expect_identical(find_pkg_name("scope", rlang::ns_env("colonoscopy")), "colonoscopy")
  expect_identical(find_pkg_name("is_null", rlang::ns_env("colonoscopy")), "rlang")
})

test_that("is_exported", {
  expect_true (is_exported("adist",    getNamespace("utils")))
  expect_false(is_exported("argNames", getNamespace("utils")))
})
