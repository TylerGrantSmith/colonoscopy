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
#' @importFrom rlang caller_env is_null abort
scope <- function(x, envir = caller_env(), ...) {
    if (is_null(x))
      abort("`x` cannot be NULL.")
    UseMethod("scope")
}

#' @rdname scope
#' @export
#' @importFrom rlang caller_env
scope.default <- function(x, envir = caller_env(), ...) {
  tryCatch(x <- as.character(x),
           error = function(e) abort("Unable to convert x to a character"))

  scope.character(x, envir)
}

#' @rdname scope
#' @export
#' @importFrom rlang is_environment caller_env
scope.character <- function(x, envir = caller_env(), ...) {
  if (!is_environment(envir)) {
    abort("`envir`` must be an environment")
  }

  header <- regmatches(x, regexec("^\\s+", x))[[1]]
  footer <- regmatches(x, regexec("\\s+$", x))[[1]]
  ptu <- ParseTreeScoper$new(text = x, envir = envir, keep.source = T)
  paste0(header, ptu$text, footer, collapse = "")
}

#' @rdname scope
#' @export
#' @importFrom rlang caller_env is_null base_env get_env is_primitive
scope.function <- function(x, envir = get_env(x), useSource = T,...) {
  control = c("keepInteger", "keepNA")

  if (is_primitive(x)) {
    return(as.character(substitute(x)))
  }

  if (!is_null(attr(x, "srcref")))
    return(scope(attr(x,"srcref"), envir = envir))

  if (useSource)
    control <- append(control, "useSource")

  scope(deparse(x, width.cutoff = 59, control = control), envir = envir)
}
#
