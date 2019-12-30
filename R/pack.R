#' pack
#'
#' @param x ([function] | [call] | [name] | [list] | [pairlist] | ... )\cr
#'   Input code
#'
#' @export
pack <- function(x) {
  UseMethod("pack_")
}

#' @export
pack_ <- function(x, envir = caller_env(), enclos = new_environment(parent = empty_env()), ..pkgs = new_environment()) {
  UseMethod("pack_")
}

#' @export
pack_.default <- function(x, envir = caller_env(), enclos = new_environment(parent = empty_env()), ..pkgs = new_environment()) {
  if(is.call(x)) {
    return(pack_.call(x, envir, enclos, ..pkgs))
  }

  if(is_syntactic_literal(x)) {
    return(x)
  }

  stop(sprintf("%s with class %s is not pack-able", deparse(substitute(x)), class(x)[1]))
}


#' @export
pack_.call <- function(x, envir = caller_env(), enclos = new_environment(parent = empty_env()), ..pkgs = new_environment()) {
  xu <- unroll_call(x)

  if(as.character(xu) == "function") return(pack_.function(eval(x, envir, enclos), envir, enclos, ..pkgs))
  if(as.character(xu) %in% .global_packr_env$assignment_symbols) {
    return(pack_assignment_access(x, envir, enclos, ..pkgs))
  }
  if(as.character(xu) %in% .global_packr_env$ns_access_symbols) {
    if(is.name(x[[1]])) {
      register_dependency(..pkgs, x[[2]], x[[3]])
      x = x[[3]]
    } else {
      register_dependency(..pkgs, x[[1]][[2]], x[[1]][[3]])
      x[[1]] = x[[1]][[3]]
    }
    return(pack_(x, envir, enclos, ..pkgs))
  }
  ds <- lapply(x, pack_, envir = envir, enclos = enclos, ..pkgs = ..pkgs)
  cs <- as.call(ds)
  cs
}

#' @export
pack_.name <- function(x, envir = caller_env(), enclos = environment(x), ..pkgs = new_environment()) {
  cx <- as.character(x)

  if(cx == "") return(x)
  if(!exists(cx, enclos)) return(x)

  pkg_env <- get_obj_env(cx, enclos)#pryr::where(as.character(cx), envir)
  pkg_name <- get_pkg_name(pkg_env)

  if(!is.null(pkg_name)) {
    register_dependency(..pkgs, pkg_name, as.character(x))
  }

  return(x)
}


#' @export
pack_.function <- function(x, envir = environment(x), enclos = new_environment(parent = caller_env()), ..pkgs = new_environment()) {

  current_exec_env = .global_packr_env$exec_env
  on.exit({.global_packr_env$exec_env = current_exec_env})

  .global_packr_env$exec_env = new_environment(as.list(formals(x)), current_exec_env)

  args <- pack_(formals(x), envir = envir, enclos = .global_packr_env$exec_env, ..pkgs = ..pkgs)
  body <- pack_(body(x), envir = envir, enclos = .global_packr_env$exec_env, ..pkgs = ..pkgs)
  call2('function', args, body)
}

#' @export
pack_.expression <- function(x, envir = caller_env(), enclos = environment(x), ..pkgs = new_environment()) {
  as.expression(pack_.list(x, envir, enclos, ..pkgs))
}

#' @export
pack_.pairlist <- function(x, envir = caller_env(), enclos = environment(x), ..pkgs = new_environment()) {
  upl <- pack_(as.list(x), envir = envir, enclos = enclos, ..pkgs = ..pkgs)
  as.pairlist(upl)
}

#' @export
pack_.list <- function(x, envir = caller_env(), enclos = environment(x), ..pkgs = new_environment()) {
  lapply(x, pack_, envir = envir, enclos = enclos, ..pkgs = ..pkgs)
}

#' pack_track
#' @export
pack_track <- function(x, envir = caller_env(), enclos = new_environment(parent = caller_env())) {
  ..pkgs = new_environment()
  # doesnt quite work right with functions
  res = pack_(x, envir, enclos, ..pkgs = ..pkgs)
  return(list(res = res, pkgs = lapply(as.list(..pkgs), as.list)))
}

pack_assignment_access <- function(x, envir, enclos, ..pkgs) {
  lhs <- x[[2]] # not sure this is right
  rhs <- x[[3]]

  ds <- as.call(list(x[[1]], lhs, pack_(rhs, envir, enclos, ..pkgs)))
  env_bind(enclos, !!deparse(lhs) := missing_arg())

  return(ds)
}
