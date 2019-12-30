#' unpack
#' @export
unpack <- function(x) {
  unpack_(x)
}

#' unpack_
#' @export
unpack_ <- function(x, envir, enclos, ...) {
  UseMethod("unpack_")
}

#' @export
unpack_.quosure <- function(x, envir = caller_env(), enclos = environment(x)) {
  unpack_(quo_squash(x), envir, enclos)
}

#' @export
unpack_.function <- function(x, envir = environment(x), enclos = .global_packr_env$exec_env) {
  if(is.primitive(x)) return(x)

  current_exec_env = .global_packr_env$exec_env
  on.exit({.global_packr_env$exec_env = current_exec_env})

  .global_packr_env$exec_env = new_environment(as.list(formals(x)), current_exec_env)

  fmls <- unpack_.pairlist(formals(x), envir, .global_packr_env$exec_env)
  body <- unpack_.call(body(x), envir, .global_packr_env$exec_env)

  make_function_call(fmls, body)
}


#' @export
unpack_.pairlist <- function(x, envir = current_env(), enclos = environment(x)) {
  as.pairlist(lapply(x, unpack_, envir, enclos))
}

#' @export
#' @rdname unpack_
unpack_.list <- function(x, envir = current_env(), enclos = environment(x)) {
  lapply(x, unpack_, envir, enclos)
}

#' @export
unpack_.call <- function(x, envir = current_env(), enclos = environment(x)) {

  xu <- unroll_call(x)

  if(as.character(xu) == "function") return(unpack_.function(eval(x, envir, enclos)))

  if(as.character(xu) %in% .global_packr_env$list_access_symbols) return(unpack_list_access(x, envir, enclos))
  if(as.character(xu) %in% .global_packr_env$ns_access_symbols)   return(unpack_ns_access(x, envir, enclos))
  if(as.character(xu) %in% .global_packr_env$assignment_symbols)  return(unpack_assignment_access(x, envir, enclos))
  if(as.character(xu) %in% .global_packr_env$pipe_symbols && getOption("packr.pipe")) return(unpack_pipe(x, envir, enclos))
  if(as.character(xu) %in% .global_packr_env$dt_symbols && getOption("packr.dt")) return(unpack_assignment_access(x, envir, enclos))

  ds <- lapply(x, unpack_, envir, enclos)
  cs <- as.call(ds)
  cs
}



#' @export
unpack_.name <- function(x, envir = current_env(), enclos = new_environment(parent = envir)){
  if(is.null(x) || x == "") {
    return(x)
  }

  if(exists(as.character(x), enclos %||% empty_env())) {
    xenv <- pryr::where(as.character(x), enclos %||% empty_env())
  } else if(exists(as.character(x), envir)) {
    xenv <- pryr::where(as.character(x), envir)
  } else {
    return(x)
  }

    if(identical(xenv, .global_packr_env$exec_env)) {
      return(x)
    }

  if(grepl("^imports:", env_name(xenv)))  {
    xenv <- get_obj_env(x, xenv)
  }

  pkg <- get_pkg_name(xenv)
  exported <- is_exported(x, pkg)

  if(is.null(pkg) || isBaseNamespace(xenv)  || identical(xenv, base_env())) return(x)
  if(exported) make_exported_call(pkg, x) else make_internal_call(pkg, x)
}

#' @export
unpack_.default <- function(x, envir = current_env(), enclos = new_environment(parent = caller_env())) {

  if(is.call(x)) {
    return(unpack_.call(x, envir, enclos))
  }

  if(is_syntactic_literal(x)) {
    return(x)
  }

  stop(sprintf("Cannot unpack %s of class %s", deparse(substitute(x)), class(x)[1]))
}

unpack_ns_access <- function(x, envir, enclos) {
  if(length(x) == 3) return(x)

  return(as.call(c(x[[1]], unpack_(as.list(x[-1]), envir, enclos))))
}

unpack_list_access <- function(x, envir, enclos) {
  ds <- switch(length(x),
               list(unpack_list_access(x[[1]], envir, enclos)),
               list(unpack_list_access(x[[1]], envir, enclos), unpack_(x[[2]], envir, enclos)),
               list(x[[1]], unpack_(x[[2]], envir, enclos), x[[3]])
  )
  return(as.call(ds))
}

unpack_assignment_access <- function(x, envir, enclos) {
  lhs <- x[[2]] # not sure this is right
  rhs <- x[[3]]

  ds <- as.call(list(x[[1]], lhs, unpack_(rhs, envir, enclos)))
  env_bind(enclos, !!deparse(lhs) := missing_arg())

  return(ds)
}

unpack_pipe <- function(x, envir, enclos) {
  pipe <- x[[1]]
  lhs  <- x[[2]]
  rhs  <- x[[3]]

  ds <- as.call(list(pipe,
                     unpack_(lhs, envir, enclos),
                     unpack_(rhs, envir, enclos)))
  return(ds)
}
