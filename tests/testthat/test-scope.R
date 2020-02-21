test_that(
  "scope exported names",
  {
    expect_output(
      with_mock(
        `colonoscopy::unscope` = function() head ,
        print(scope("unscope"))
      ),
      "colonoscopy::unscope"
    )
  }
)

test_that(
  "scope functions",
  expect_output(
    with_mock(
      `colonoscopy::unscope` = function() head ,
      print(scope(unscope))
    ),
    "function\\(\\) utils::head"
  )
)

test_that(
  "scope scopes rhs of formals"
  ,
  expect_output(
    with_mock(
      `colonoscopy::unscope` = function(a = head) head ,
      print(scope(unscope))
    ),
    "function\\(a = utils::head\\) utils::head"
  )
)

test_that(
  "scope masks formal arguments in body"
  ,
  expect_output(
    with_mock(
      `colonoscopy::unscope` = function(tail = head) tail ,
      print(scope(unscope))
    ),
    "function\\(tail = utils::head\\) tail"
  )
)

test_that(
  "scope leaves constants unchanged",
  {
    expect_output(print(scope("1")), "1")
  }
)
