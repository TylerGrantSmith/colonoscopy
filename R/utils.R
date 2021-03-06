get_obj_env <- function(x, env) {
  environment(get0(x, env))
}

find_nm_in_imports <- function(nm, ns) {
  imports <- ns$.__NAMESPACE__.$imports

  if (is_null(imports)) return(NULL)

  pkg <- unique(names(which(sapply(imports, function(x) any(x == nm)))))
  if (length(pkg))
    pkg
  else
    NULL
}

find_pkg_name <- function(nm, env) {
  if (isBaseNamespace(env))
    return("base")

  x <- get0(nm, env)

  if (inherits(x, "lazy_scope") || inherits(x, "scoped") || is_null(x)) {
    return(NULL)
  }

  .getNamespaceInfo(environment(x), "spec")[["name"]] %||%
    find_nm_in_imports(nm, env) %||%
    tryCatch(ns_env_name(env), error = function(e) "")
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

with_self_bindings <- function(...) {
  self <- get("self", caller_env())
  with_bindings(..., .env = self$.__enclos_env__$self)
}

parse_safely <- function(text, ...) {
  parsed <- with_handlers(
    parse(text = text, ...),
    error   = function(e) e,
    warning = function(w) w
  )

  if (inherits(parsed, "error")) {
    abort(parsed$message)
  } else if (inherits(parsed, "warning")) {
    warn(parsed$message)
  }
  parsed
}

#' @importFrom data.table rbindlist
add_root_row <- function(pd) {
  rbindlist(list(pd, list(terminal = FALSE,
                          id = 0,
                          parent = -Inf,
                          text = "")),
            fill = TRUE)

}

#' @importFrom data.table as.data.table
get_parse_data <- function(text, ...) {
  parse_safely(text, ..., keep.source = TRUE) %>%
    utils::getParseData() %>%
    as.data.table() %>%
    add_root_row()
}

extract_header <- function(x) {
  regmatches(x, regexpr("^\\s*(\\r?\\n)", x))
}

extract_footer <- function(x) {
  regmatches(x, regexpr("(?<=\\S)\\s*$", x, perl = TRUE))
}
