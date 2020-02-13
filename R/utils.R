get_obj_env <- function(x, env) {
  environment(get0(x, env))
}

get_pkg_name <- function(env) {
  if(is_null(env)) return(NULL)

  en <- env_name(env)

  if (grepl("^package:", en))
    return(sub("^package:", "", en))

  if (grepl("^namespace:", en))
    return(sub("^namespace:", "", en))

  return(NULL)
}

is_exported <- function(x, ns) {
  if(is_character(ns)) ns <- ns_env(ns)
  if(!is_namespace(ns)) return(FALSE)
  as_character(x) %in% ls(pkg_env(ns))
}

make_exported_call <- function(pkg, name) {
  call("::", as.name(pkg), as.name(name))
}

make_internal_call <- function(pkg, name) {
  call(":::", as.name(pkg), as.name(name))
}

getNamespaceExportsAndLazyData <- function(ns) {
  ns <- asNamespace(ns)
  if (isBaseNamespace(ns))
    names(.BaseNamespaceEnv)
  else
    c(names(.getNamespaceInfo(ns, "exports")),
      names(.getNamespaceInfo(ns, "lazydata")))
}


str_to_lang <- function(s) {
  if (s != "") {
    str2lang(s)
  } else {
    missing_arg()
  }
}
