library(testthat)
library(packr)

test_list <- list(a = 1, b = 2)

test_that("unpack call works", {
  expect_equal(unpack("A"), "A")
  expect_equal(unpack(1L), 1L)
  expect_null(unpack(NULL))
  expect_equal(unpack(quote(unpack)), quote(packr::unpack))
  expect_equal(unpack_(quote(is_exported), enclos = rlang::ns_env("packr")), quote(packr:::is_exported))
})
