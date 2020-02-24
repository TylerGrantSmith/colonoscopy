test_that("scope exported names", {
    expect_identical(scope("unscope")$text, "colonoscopy::unscope")
  }
)

test_that("scope works", {
  expect_error(scope(NULL), "`x` cannot be NULL.")
}
)

test_that("scope.character works", {
  expect_error(scope.character("test", list()), "`envir`` must be an environment")
})


test_that("scope.function works", {
  expect_identical(scope(utils::alarm)$text,
                   "function ()\n{\n    cat(\"\\a\")\n    flush.console()\n}")
  expect_identical(scope(deparse(utils::alarm))$text,
                   "function ()\n{\n    cat(\"\\a\")\n    utils::flush.console()\n}")

})

test_that("scope properly handles closures not classed as functions", {
  f <- function() "closure"
  class(f) <- "not_a_function_anymore"

  expect_identical(scope(f)$text, "function() \"closure\"")
})

test_that("scope functions",
  expect_identical(
    with_mock(`colonoscopy::unscope` = function() head, scope(unscope)$text),
    "function() utils::head"
  )
)

test_that("scope scopes rhs of formals",
  expect_identical(
    with_mock(`colonoscopy::unscope` = function(a = head) head, scope(unscope)$text),
    "function(a = utils::head) utils::head"
  )
)

test_that("scope masks formal arguments in body",
  expect_identical(
    with_mock(
      `colonoscopy::unscope` = function(tail = head) tail ,scope(unscope)$text
    ),
    "function(tail = utils::head) tail"
  )
)

test_that("scope leaves constants unchanged", {
    expect_identical(scope("1")$text, "1")
  }
)

test_that("scope doesn't alter assigned names", {

    e <- rlang::env_clone(rlang::ns_env("colonoscopy"))
    expect_identical(scope("unscope <- NULL",  e)$text, "unscope <- NULL")
    e <- rlang::env_clone(rlang::ns_env("colonoscopy"))
    expect_identical(scope("NULL -> unscope",  e)$text, "NULL -> unscope")

    e <- rlang::env_clone(rlang::ns_env("colonoscopy"))
    expect_identical(scope("unscope = NULL",   e)$text, "unscope = NULL")


    e <- rlang::new_environment(parent = rlang::env_clone(rlang::ns_env("colonoscopy")))
    expect_identical(scope("unscope <<- NULL", e)$text, "unscope <<- NULL")
    e <- rlang::new_environment(parent = rlang::env_clone(rlang::ns_env("colonoscopy")))
    expect_identical(scope("NULL ->> unscope", e)$text, "NULL ->> unscope")
  }
)
