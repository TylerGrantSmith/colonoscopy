context("unpack")

test_that("unpack works as intended", {
  # syntactic literals
  expect_identical(unpack("A"), "A")
  expect_identical(unpack(1L), 1L)

  # NULL expressions
  expect_null(unpack(NULL))

  # namespace accessors
  expect_identical(unpack(unpack, envir = rlang::ns_env("packr")), quote(packr::unpack))

  ## Fails with devtools::test() but not with testthat::test_dir called directly.
  # expect_identical(unpack(unpack_, envir = rlang::ns_env("packr")), quote(packr:::unpack_))
})
