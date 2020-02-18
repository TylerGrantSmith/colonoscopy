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
  if(is.character(ns)) ns <- getNamespace(ns)
  if(!isNamespace(ns)) return(FALSE)
  as.character(x) %in% getNamespaceExportsAndLazyData(ns)
}

getNamespaceExportsAndLazyData <- function(ns) {
  ns <- asNamespace(ns)
  if (isBaseNamespace(ns))
    names(.BaseNamespaceEnv)
  else
    c(names(.getNamespaceInfo(ns, "exports")),
      names(.getNamespaceInfo(ns, "lazydata")))
}

#' @importFrom rlang with_bindings eval_bare
with_self_bindings <- function(...) {
  self <- get("self", caller_env())
  with_bindings(..., .env = self$.__enclos_env__$self)
}
