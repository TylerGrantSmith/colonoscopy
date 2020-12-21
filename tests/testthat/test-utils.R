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

test_that("extract_footer works with all nonspace characters", {
  expect_identical(extract_footer("a\n"), "\n")
  expect_identical(extract_footer("}\n"), "\n")
  expect_identical(extract_footer("#\n"), "\n")
})

test_that("extract_header works with all nonspace characters", {
  expect_identical(extract_header("\na"), "\n")
  expect_identical(extract_header("\n{"), "\n")
  expect_identical(extract_header("\n#"), "\n")
})

test_that("find_nm_in_imports handles non-namespace environments", {
  expect_null(find_nm_in_imports("x", environment()))
})
