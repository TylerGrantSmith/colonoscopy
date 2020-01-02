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
