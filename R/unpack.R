unpack_assignment <- function(x, envir) {
  if(is_symbol(x[[2]])) {
    switch(
      as.character(x[[1]]),
      '='= ,
      '<-' = env_bind(envir, !!deparse(x[[2]]) := x[[3]], .eval_env = envir),
      '<<-' = env_bind(tryCatch(where(deparse(x[[2]]), parent.env(envir)),
                                error = function(e) envir),
                       !!deparse(x[[2]]) := x[[3]], .eval_env = envir) # not sure this is right
    )
  }

  return(as.call(list(x[[1]],
                      unpack_(x[[2]], envir),
                      unpack_(x[[3]], envir))))
}

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
unpack <- function(x, envir = caller_env(), ...) {
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

unpack.character <- function(x, envir, ...) {
  if (!is_environment(envir)) {
    abort("envir must be an environment")
  }

  ptu <- ParseTreeUnpacker$new(text = x, envir = envir)
  ptu$unpack()
  cat(ptu$text)
}

unpack.function <- function(x, envir, use_fn_env = TRUE, useSource, ...) {

  if (use_fn_env) {
    envir <- environment(x)
  }

  if (!is.null(attr(x, "srcref"))) {
    unpack(attr(x,"srcref"), envir)
  }

  control = c("keepInteger", "keepNA")
  if (useSource) control <- append(control, "useSource")

  unpack(deparse(x, width.cutoff = 59, control = control))
}

unpack()
