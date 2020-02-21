#' Make package dependencies explicit
#'
#' Parses the input, and explicitly adds namespace access where appropriate.
#'
#' @param x A character vector of parseable code or a function.
#'
#' @param envir An environment in which the expression should be evaluated.
#' Defaults to the enclosing environment if \code{x} is a function, otherwise the calling environment.
#'
#' @return Returns a [ParseData] object which prints the modified code.
#'
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
#' @keywords internal
scope.default <- function(x, envir = caller_env(), ...) {
  tryCatch(x <- as.character(x),
           error = function(e) abort("Unable to convert x to a character"))

  scope.character(x, envir)
}

#' @rdname scope
#' @export
#' @keywords internal
scope.character <- function(x, envir = caller_env(), ...) {
  if (!is_environment(envir)) {
    abort("`envir`` must be an environment")
  }

  ParseTreeScoper$new(text = x, envir = envir)
}

#' @rdname scope
#' @export
#' @keywords internal
scope.function <- function(x, envir = get_env(x) %||% caller_env(), useSource = TRUE,...) {
  control = c("keepInteger", "keepNA")

  if (!is_null(attr(x, "srcref")))
    return(scope(attr(x,"srcref"), envir = envir))

  if (useSource)
    control <- append(control, "useSource")

  scope(deparse(x, width.cutoff = 59, control = control), envir = envir)
}
