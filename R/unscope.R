#' Remove \code{`::`} and \code{`:::`} operators
#'
#' @description TBD
#' @param x
#'
#' @param ...
#'
#' @export
#'
#' @examples
#'
#' unscope("2+2")
#'
#' unscope("utils::head(colonoscopy::scope)")
unscope <- function(x, ...) {
  UseMethod("unscope")
}


#' @export
#' @keywords internal
unscope.default <- function(x) {
  tryCatch(x <- as.character(x),
           error = function(e)
             abort("Unable to convert `x` to a character."))
  unscope.character(x)
}

#' @export
#' @keywords internal
unscope.character <- function(x) {
  ParseTreeUnscoper$new(text = x)
}

#' @export
#' @keywords internal
unscope.function <- function(x, ...) {
  abort("Cannot `unscope` functions.  Use `unscope_function` instead.")
}

#' Unscope a function
#'
#' @export
unscope_function <- function(f, useSource = T) {
  control = c("keepInteger", "keepNA")

  if (is_primitive(f)) {
    return(as.character(substitute(x)))
  }

  if (!is.null(attr(f, "srcref")))
    return(unscope(attr(f,"srcref")))

  if (useSource)
    control <- append(control, "useSource")

  unscope(deparse(f, width.cutoff = 59, control = control))
}
