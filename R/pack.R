pack <- function(x, envir = NULL) {
  expr <- enexpr(x)
  pack_(expr, envir)
}

pack_ <- function(x, envir) {
  if(is_null(envir)) {
    envir = new_environment()
  }

  if(is_call(x)) {
    return(pack_call(x, envir))
  }

  if(is_syntactic_literal(x) || is_symbol(x)) {
    return(x)
  }

  if(length(x) > 1) {
    return(lapply(x, pack_, envir))
  }

  return(NULL)
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

pack_function <- function(f, enclos = NULL) {
  if(is_primitive(f)) {
    return(f)
  }

  enclos <- enclos %||% environment(f)

  envir <- new_environment(parent = enclos)

  fmls <- as.pairlist(pack_pairlist(formals(f), envir))
  body <- pack_call(body(f), envir)

  make_function_call(fmls, body)
}

pack_track <- function(f, envir = NULL, ..pkgs) {

}
