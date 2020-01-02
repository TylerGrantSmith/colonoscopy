unpack_ <- function(x, envir) {
  if(is_null(envir)) {
    envir = new_environment()
  }

  if(is_syntactic_literal(x)) {
    return(x)
  }

  if(is_symbol(x)) {
    return(unpack_symbol(x, envir))
  }

  if(is_call(x)) {
    return(unpack_call(x, envir))
  }

  if(length(x) > 1) {
    return(lapply(x, unpack_, envir))
  }

  stop("Cannot unpack ", x, " of class ", class(x))
}


unpack_symbol <- function(x, envir) {
  xc <- as.character(x)
  x_env <- tryCatch(where(xc, envir), error = function(e) NULL)


  if(is_null(x_env)) {
    return(x)
  }

  if(grepl("^imports:", env_name(x_env)))  {
    x_env <- get_obj_env(xc, x_env)
  }

  pkg_name <- get_pkg_name(x_env)

  if(is_null(pkg_name) || pkg_name == 'base') {
    return(x)
  }

  if(xc %in% getNamespaceExports(pkg_name)) {
    return(make_exported_call(pkg_name, x))
  } else {
    return(make_internal_call(pkg_name, x))
  }

  return(x)
}

unpack_call <- function(x, envir) {

  if(is_null(x)) {
    return(x)
  }

  if(is_assignment(x)) {
    return(unpack_assignment(x, envir))
  }

  if(is_pipe(x)) {
    return(unpack_pipe(x, envir))
  }

  if(is_list_access(x)) {
    return(unpack_list_access(x, envir))
  }

  if(is_ns_access(x)) {
    return(x)
  }

  if(is_function_def(x)) {
    enclos = new_environment(parent = envir)
    fmls <- unpack_pairlist(x[[2]], enclos)
    body <- unpack_call(x[[3]], enclos)
    return(make_function_call(fmls, body))
  }

  return(as.call(lapply(x, unpack_, envir)))
}

unpack_pairlist <- function(pl, envir) {
  pl_bound <- pl[!sapply(pl, is_missing)]
  do.call(env_bind_lazy, c(.env = envir, pl_bound))
  lapply(pl, unpack_, envir)
}


unpack_list_access <- function(x, envir) {
  ds <- switch(length(x),
               list(unpack_list_access(x[[1]], envir)),
               list(unpack_list_access(x[[1]], envir), unpack_(x[[2]], envir)),
               list(x[[1]], unpack_(x[[2]], envir), x[[3]])
  )
  return(as.call(ds))
}

unpack_pipe <- function(p, envir) {
  as.call(list(p[[1]],unpack_(p[[2]], envir), unpack_(p[[3]], envir)))
}

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
unpack <- function(x, envir = caller_env()) {
  expr <- enexpr(x)
  unpack_(expr, envir)
}

#' Make explicit package dependencies in a function definition
#'
#' @param f A function
#'
#' @param enclos The environment in which the function is defined.  Defaults to
#'   \code{environment(f)}
#' @export
#'
#' @examples
#' unpack_function(unpack_function)
unpack_function <- function(f, enclos = environment(f)) {
  if(is_primitive(f)) {
    return(f)
  }

  envir <- new_environment(parent = enclos)

  fmls <- unpack_pairlist(formals(f), envir)
  body <- unpack_call(body(f), envir)

  make_function_call(fmls, body)
}
