#' @export
unscope <- function(x, ...) {
  UseMethod("unscope")
}


#' @rdname unscope
#' @export
unscope.default <- function(x) {
  tryCatch(x <- as.character(x),
           error = function(e)
             abort("Unable to convert `x` to a character."))
  unscope.character(x)
}

#' @rdname unscope
#' @export
unscope.character <- function(x) {
  header <- regmatches(x, regexec("^\\s+", x))[[1]]
  footer <- regmatches(x, regexec("\\s+$", x))[[1]]

  ptp <- ParseTreeUnscoper$new(text = x)
  paste0(header, ptp$text, footer, collapse = "")
}

unscope.function <- function(x, useSource = T) {
  control = c("keepInteger", "keepNA")

  if (!is.null(attr(x, "srcref")))
    return(unscope(attr(x,"srcref")))

  if (useSource)
    control <- append(control, "useSource")

  unscope(deparse(x, width.cutoff = 59, control = control))
}
