get_obj_env <- function(x, env) {
  tryCatch(environment(get0(as.character(x), env)),
           error = function(e) NULL
  )
}

get_pkg_name <- function(env) {
  if(is.null(env)) return(NULL)

  en <- env_name(env)

  if (grepl("^package:", en))
    return(sub("^package:", "", en))

  if (grepl("^namespace:", en))
    return(sub("^namespace:", "", en))

  return(NULL)
}

is_assignment <- function(x) {
  deparse(x[[1]]) %in% c("=", "<-", "<<-")
}

is_exported <- function(x, ns) {
  if(is.character(ns)) ns <- getNamespace(ns)
  if(!isNamespace(ns)) return(FALSE)
  as.character(x) %in% getNamespaceExports(ns)
}

is_function_def <- function(x) {
  x[[1]] == as.name('function')
}

is_list_access <- function(x) {
  deparse(x[[1]]) %in% c("$")
}

is_ns_access <- function(x) {
  deparse(x[[1]]) %in% c("::",":::")
}

is_pipe <- function(x) {
  deparse(x[[1]]) %in% c('%>%')
}

make_function_call <- function(args, body) {
  call("function", as.pairlist(args), body)
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
