#' Make explicit package dependencies
#'
#' \code{unpack} returns an expression with all symbols resolving to loaded namespaces by
#' explicitly adding the namespace access notation.
#'
#' @param x An expression to process.  Input is automatically quoted, use !! to unquote if
#' you have already captured an expression object.
#'
#' @param envir An environment in which the expression should be evaluated.
#' @examples
#'
#' # Unchanged
#' unpack(1)
#' unpack("a")
#'
#' # Modified
#' library(packr)
#' unpack(unpack)
unpack <- function(x, envir, ...) {
    UseMethod("unpack")
}

#' @rdname unpack
#' @export
unpack.default <- function(x, envir, ...) {
  tryCatch(x <- as.character(x),
           error = function(e) abort("Unable to convert x to a character"))

  unpack.character(x, envir)
}

#' @rdname unpack
#' @export
unpack.character <- function(x, envir = caller_env(), ...) {
  if (!is_environment(envir)) {
    abort("envir must be an environment")
  }

  header <- regmatches(x, regexec("^\\s+", x))[[1]]
  footer <- regmatches(x, regexec("\\s+$", x))[[1]]
  ptu <- ParseTreeUnpacker$new(text = x, envir = envir)
  ptu$unpack()
  paste0(header, ptu$text, footer, collapse = "")
}

#' @rdname unpack
#' @export
unpack.function <- function(x, envir = caller_env(), use_fn_env = TRUE, useSource = T,...) {
  control = c("keepInteger", "keepNA")

  if (use_fn_env)
    envir <- environment(x)

  if (!is.null(attr(x, "srcref")))
    return(unpack(attr(x,"srcref"), envir))

  if (useSource)
    control <- append(control, "useSource")

  unpack(deparse(x, width.cutoff = 59, control = control), envir)
}
#
