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

#' @importFrom rlang abort warn
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
  parse_safely(text, ...) %>%
    utils::getParseData() %>%
    as.data.table() %>%
    add_root_row()
}

extract_header <- function(x) {
  regmatches(x, regexpr("^\\s*(\\r?\\n)", x))
}

extract_footer <- function(x) {
  regmatches(x, regexpr("(?<=\\w)\\s*$", x, perl = TRUE))
}
