pack_ <- function(x, envir) {
  if(is_null(envir)) {
    envir = new_environment()
  }

  if(is_syntactic_literal(x) || is_symbol(x)) {
    return(x)
  }

  if(is_call(x)) {
    return(pack_call(x, envir))
  }

  if (is_list(x)) {
    return(purrr::map(x, pack_, envir))
  }

  stop("Cannot unpack ", x, " of class ", class(x))
}


pack_call <- function(x, envir) {
  if (length(x[[1]]) != 1) {
    return(as.call(lapply(x, pack_, envir)))
  }

  if (as.character(x[[1]]) %in% c('::', ':::')) {
    pkg <- x[[2]]
    nm <- x[[3]]

    register_dependency(pkg, nm)

    return(nm)
  }

  return(as.call(lapply(x, pack_, envir)))
}

register_dependency <- function(pkg, nm) {
  pkg = as.character(pkg)
  nm = as.character(nm)

  if (exists(pkg, dep_env)) {
    dep_env[[pkg]][[nm]] = nm
  } else {
    dep_env[[pkg]] = rlang::new_environment()
    assign(nm, nm, dep_env[[pkg]])
  }
}

pack_pairlist <- function(x, envir) {
  lapply(x, pack_, envir)
}


#' Remove all namespace accessors from an expression
#'
#' @param x An expression to process.  Input is automatically quoted, use !! to unquote if
#' you have already captured an expression object.
#'
#' @param envir An environment in which the expression should be evaluated.
#' @export
#' @examples
#' # Unchanged
#' pack(1)
#' pack("a")
#'
#' # Modified
#' pack(packr::pack)
pack <- function(x, envir = NULL) {
  expr <- enexpr(x)
  pack_(expr, envir)
}

#' Remove all namespace accessors from a function definition
#'
#' @param f A function
#'
#' @param enclos The environment in which the function is defined.  Defaults to
#'   \code{environment(f)}
#' @export
#'
#' @examples
#' test_function <- function() { packr::pack }
#' pack_function(test_function)
pack_function <- function(f, enclos = environment(f)) {
  if(is_primitive(f)) {
    return(f)
  }

  envir <- new_environment(parent = enclos)

  fmls <- as.pairlist(pack_pairlist(formals(f), envir))
  body <- pack_call(body(f), envir)

  make_function_call(fmls, body)
}
