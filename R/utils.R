get_obj_env <- function(x, env) {
  tryCatch(environment(get0(as.character(x), env)),
           error = function(e) NULL
  )
}

get_pkg_name <- function(env) {

  if(is.null(env)) return(NULL)
  if(any(sapply(.global_packr_env$skip_envs, identical, env))) {
    return(NULL)
  }

  en <- env_name(env)
  if (grepl("^package:", en))
    return(sub("^package:", "", en))
  if (grepl("^namespace:", en))
    return(sub("^namespace:", "", en))

  return(get_pkg_name(env_parent(env)))
}

make_function_call <- function(args, body) {
  call("function", as.pairlist(args), body)
}

is_exported <- function(x, ns) {
  if(is.character(ns)) ns <- getNamespace(ns)
  if(!isNamespace(ns)) return(FALSE)
  as.character(x) %in% getNamespaceExports(ns)
}

make_exported_call <- function(pkg, name) {
  call("::", as.name(pkg), as.name(name))
}

make_internal_call <- function(pkg, name) {
  call(":::", as.name(pkg), as.name(name))
}

is.syntactic <- function(x) {
  make.names(x) == x
}

register_dependency <- function(env, pkg, name) {
  pkg = as.character(pkg)
  name = as.character(name)

  if (exists(pkg, env)) {
    env[[pkg]][[name]] = name
  } else {
    env[[pkg]] = rlang::new_environment()
    assign(name, name, env[[pkg]])
  }
}

unroll_call <- function(x) {
  if(is.call(x)) x <- unroll_call(x[[1]])

  x
}
