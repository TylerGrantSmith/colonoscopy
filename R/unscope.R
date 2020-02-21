#' Remove \code{`::`} and \code{`:::`} operators
#'
#' @description Deletes all package access operators.
#' @param x A character vector of parseable R code, or a function.
#'
#' @param ... Optional arguments when x is a function.
#'
#' @export
unscope <- function(x, ...) {
  UseMethod("unscope")
}

#' @export
#' @keywords internal
unscope.default <- function(x, ...) {
  tryCatch(x <- as.character(x),
           error = function(e)
             abort("Unable to convert `x` to a character."))
  unscope.character(x)
}

#' @export
#' @keywords internal
unscope.character <- function(x, ...) {
  ParseTreeUnscoper$new(text = x)
}

#' @export
#' @keywords internal
unscope.function <- function(x, useSource = TRUE, ...) {
  control = c("keepInteger", "keepNA")

  if (is_primitive(x)) {
    return(as.character(substitute(x)))
  }

  if (!is.null(attr(x, "srcref")))
    return(unscope(attr(x,"srcref")))

  if (useSource)
    control <- append(control, "useSource")

  unscope(deparse(x, width.cutoff = 59, control = control))
}
