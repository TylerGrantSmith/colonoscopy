#' Make explicit package dependencies
#'
#' \code{unpack} returns an expression with all symbols resolving to loaded namespaces by
#' explicitly adding the namespace access notation.
#'
#' @param x An expression to process.  Input is automatically quoted, use !! to unquote if
#' you have already captured an expression object.
#'
#' @param envir An environment in which the expression should be evaluated.
#' @export
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

unpack.srcref <- function(x, envir, ...) {
  unpack(attr(x, "srcfile"), envir, ...)
}

unpack.srcfile <- function(x, envir, ...) {
  abort("Not implemented for this class")
}

unpack.expression <- function(x, envir, ...) {
  abort("Not implemented for this class")
}

unpack.character <- function(x, envir = caller_env(), ...) {
  if (!is_environment(envir)) {
    abort("envir must be an environment")
  }

  ptu <- ParseTreeUnpacker$new(text = x, envir = envir)
  ptu$unpack()
  cat(ptu$text)
}

unpack.function <- function(x, envir, use_fn_env = TRUE, useSource = TRUE, ...) {

  control = c("keepInteger", "keepNA")

  if (use_fn_env)
    envir <- environment(x)

  if (!is.null(attr(x, "srcref")))
    unpack(attr(x,"srcref"), envir)

  if (useSource)
    control <- append(control, "useSource")

  unpack(deparse(x, width.cutoff = 59, control = control), envir)
}

