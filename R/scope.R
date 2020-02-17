#' Make explicit package dependencies
#'
#' \code{scope} returns an expression with all symbols resolving to loaded namespaces by
#' explicitly adding the namespace access notation.
#'
#' @param x An expression to process.  Input is automatically quoted, use !! to unquote if
#' you have already captured an expression object.
#'
#' @param envir An environment in which the expression should be evaluated.
#' @examples
#'
#' # Unchanged
#' scope(1)
#' scope("a")
#'
#' # Modified
#' library(colonoscopy)
#' scope(scope)
#' @export
scope <- function(x, envir = caller_env(), ...) {
    if (is_null(x))
      abort("`x` cannot be NULL.")
    UseMethod("scope")
}

#' @rdname scope
#' @export
scope.default <- function(x, envir = caller_env(), ...) {
  tryCatch(x <- as.character(x),
           error = function(e) abort("Unable to convert x to a character"))

  scope.character(x, envir)
}

#' @rdname scope
#' @export
scope.character <- function(x, envir = caller_env(), ...) {
  if (!is_environment(envir)) {
    abort("`envir`` must be an environment")
  }
  ParseTreeScoper$new(text = x, envir = envir, keep.source = T)
}

#' @rdname scope
#' @export
scope.function <- function(x, ...) {
  abort("Cannot `scope` functions.  Use `scope_function` instead.")
}

#' @export
scope_function <- function(f, envir = get_env(f) %||% caller_env(), useSource = T,...) {
  control = c("keepInteger", "keepNA")

  if (!is_null(attr(f, "srcref")))
    return(scope(attr(f,"srcref"), envir = envir))

  if (useSource)
    control <- append(control, "useSource")

  scope(deparse(f, width.cutoff = 59, control = control), envir = envir)
}
