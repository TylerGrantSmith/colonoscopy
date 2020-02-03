context("utils")

test_that("make_imported_call", {
  expect_identical(make_exported_call("packr", "unpack"), quote(packr::unpack))
})

test_that("make_exported_call", {
  expect_identical(make_internal_call("packr", "unpack_"), quote(packr:::unpack_))
})

test_that("is_assignment", {
  expect_true(is_assignment(quote(x <- 2)))
  expect_true(is_assignment(quote(x <<- 2)))
  expect_true(is_assignment(str2lang("x = 2")))
})


test_that("is_exported", {
  expect_true(is_exported())
})

test_that("get_pkg_name", {

  expect_null(get_pkg_name(NULL))
  expect_null(get_pkg_name(.GlobalEnv))

  test_env <- new.env()

  environmentName(test_env) <- "package:test"
  expect_identical(get_pkg_name(test_env), "test")

  environmentName(test_env) <- "namespace:test"
  expect_identical(get_pkg_name(test_env), "test")
})

test_that("is_exported", {
  expect_true(is_exported("pack"), getNamespace("packr"))
  expect_false(is_exported("is_exported"), getNamespace("packr"))
})
