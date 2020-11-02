get_obj_env <- function(x, env) {
  environment(get0(x, env))
}

find_nm_in_imports <- function(nm, env) {
  imports <- collapse_ns_imports(env)
  pkg <- imports %>% dplyr::filter(value == nm) %>% dplyr::pull(name)
  if (length(pkg))
    pkg
  else
    NULL
}

collapse_ns_imports <- function(ns) {
  imports <- ns$.__NAMESPACE__.$imports

  c(purrr::discard(imports, is.list) %>% purrr::map_chr(unname),
    purrr::keep(imports, is.list) %>%
      purrr::map(~purrr::set_names(.[2], .[1])) %>%
      purrr::flatten()) %>%
    tibble::enframe() %>%
    tidyr::unnest(value) %>%
    dplyr::distinct()
}

find_pkg_name <- function(nm, env) {
  if (isBaseNamespace(env))
    return("base")

  x <- get(nm, env)

  if (inherits(x, "lazy_scope") || is.null(x)) {
    return(NULL)
  }

  .getNamespaceInfo(environment(x), "spec")[["name"]] %||%
    find_nm_in_imports(nm, env) %||%
    tryCatch(rlang::ns_env_name(env), error = function(e) "")
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
