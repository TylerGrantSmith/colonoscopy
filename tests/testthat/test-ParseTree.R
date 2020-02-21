context("ParseTree")

test_that(
  "new",
  {
    expect_error(ParseTree$new(NULL), "ParseData cannot be initialized with NULL.")
  }
)

test_that(
  "footer and header",
  {
    browser()
    default_text <- " \n\t \n  1   \n \t\n"
    pt <- ParseTree$new(default_text)
    expect_equal(pt$.__enclos_env__$private$header, " \n\t \n")
    expect_equal(pt$.__enclos_env__$private$footer, "   \n \t\n")

  }
)

test_that(
  "handles all whitespace input",
  {
    default_text <- "\n \t  \n\n"
    pt <- ParseTree$new(default_text)
    expect_equal(pt$.__enclos_env__$private$header, default_text)
    expect_equal(pt$.__enclos_env__$private$footer, character(0))
    expect_equal(pt$text, default_text)

    # capture.output ignores the final newline?
    expect_output(pt$print(), "\\n \\t  \\n")
  }
)

