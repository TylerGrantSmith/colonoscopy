ParseTreeScoper <- R6::R6Class(
  "ParseTreeScoper",
  inherit = ParseTree,
  portable = FALSE,

  public = list(
    initialize = function(..., envir = NULL) {
      super$initialize(...)
      envir <<- envir
      scope()
    }
  )
)

ParseTreeScoper$set(
  "public",
  "scope",

  function() {
    recursive_scope(0L)
  }
)

ParseTreeScoper$set(
  "public",
  "recursive_scope",
  function(new_root) {

    current_root <- root
    root <<- new_root

    scope_assignment()

    if (any(parse_data$token == "FUNCTION")) {
      scope_function()
    } else {
      scope_exprs()
    }

    root <<- current_root

  }
)

ParseTreeScoper$set(
  "private",
  "scope_exprs",
  function() {

    # need to ignore symbols preceded by $


    skip_ids <- integer()
    if (any(parse_data$token %in% c("NS_GET", "NS_GET_INT", "'$'"))) {
      for (.id in ids) {
        if (parse_data[,.(id, pt = shift(token, 1L, '', "lag"))][id == .id, pt] %in% c("NS_GET", "NS_GET_INT", "'$'")) {
          skip_ids <- append(skip_ids, .id)
        }
      }
    }

    sym_ids <- parse_data[token %in% symbol_tokens]$id
    sym_ids <- setdiff(sym_ids, skip_ids)

    for (id in sym_ids) {
      scope_symb(id)
    }

    expr_ids <- parse_data[terminal == FALSE]$id
    expr_ids <- setdiff(expr_ids, skip_ids)

    for (id in expr_ids) {
      recursive_scope(id)
    }
  }
)

ParseTreeScoper$set(
  "private",
  "scope_symb",
  function(.id) {
      nm <- .text_env[[as.character(.id)]]
      set(parse_data_full, which(parse_data_full$id == .id), "text", scope_symbol(nm, envir))
  }
)


ParseTreeScoper$set(
  "private",
  "scope_assignment",
  function() {

    assign_filter <- parse_data$token %in% c("LEFT_ASSIGN", "RIGHT_ASSIGN", "EQ_ASSIGN")

    if (!any(assign_filter)) {
      return()
    }

    if (sum(assign_filter) > 1) {
      abort("Unexpected number of assignments in parsed assignment expression.")
    }

    assign_row <- which(assign_filter)

    offset <- ifelse(parse_data$token[assign_row] == "RIGHT_ASSIGN", 1L, -1L)

    assigned_row <- assign_row + offset

    assigned_id <- parse_data$id[assigned_row]

    sub_pt <- parse_data_full[parent == assigned_id]

    if (identical(sub_pt$token, "SYMBOL")) {

      env <- envir #sub_pt$envir[[1]]
      nm  <- sub_pt$text[[1]]

      # Hacky non-accurate workaround for double arrow assignment
      if (parse_data$text[assign_row] %in% c('<<-', '->>') )
        env <- env_parent(env)

      env_bind(.env = env, !!nm := assigned_id)
    }
  }
)

ParseTreeScoper$set(
  "private",
  "scope_formals",
  function() {

    # drop the body expression.  should be the last one always...?
    pl <- head(parse_data, -1)

    expr_ids <- pl[token == "expr"]$id

    # Lazy bind pre-bound arguments
    bound_fml_nms <- pl[, .(token, nm = shift(text, 1L, type = "lag" ))][token == "EQ_FORMALS", nm]
    bound_fml_ids <- pl[, .(token, as = shift(id,   1L, type = "lead"))][token == "EQ_FORMALS", as]
    fmls <- lapply(bound_fml_ids, `class<-`, "lazy_scope")
    names(fmls) <- bound_fml_nms
    env_bind_lazy(envir, !!!fmls)

    # Bind remaining arguments
    unbound_fml_nms <- setdiff(pl[token == "SYMBOL_FORMALS", text], bound_fml_nms)
    fmls <- rep_named(unbound_fml_nms, list(NULL))
    env_bind(envir, !!!fmls)
  }
)

ParseTreeScoper$set(
  "private",
  "scope_body",
  function() {
    recursive_scope(tail(parse_data$id, 1L))
  }
)

ParseTreeScoper$set(
  "private",
  "scope_function",
  function() {

    enclos <- new_environment(parent = envir)
    cur_env <- envir
    envir <<- enclos

    scope_formals()
    scope_body()

    el <- as.list(enclos)
    scope_el <- which(vapply(el, function(x) inherits(x, "scope"),logical(1)))
    while(length(scope_el) > 0) {
      for(i in scope_el) {
        recursive_scope(el[[i]])
        class(enclos[[names(el)[[i]]]]) <- "scopeed"
      }
      el <- as.list(enclos)
      scope_el <- which(vapply(el, function(x) inherits(x, "scope"),logical(1)))
    }

    envir <<- cur_env
  }
)


scope_symbol <- function(x, envir) {
  x_c   <- as.character(x)
  x_env <- tryCatch(where(x_c, envir), error = function(e) NULL)

  if(is_null(x_env)) { return(x) }

  if(grepl("^imports:", env_name(x_env)))  {
    x_env <- get_obj_env(x_c, x_env)
  }

  pkg_name <- get_pkg_name(x_env) %||% ""

  if (pkg_name == "base") { return(x) }

  if (pkg_name == "") {
    y <- get(x_c, x_env)

    # mark lazy data for scopeing
    if (inherits(y, "lazy_scope")) {
        class(x_env[[x_c]]) <- "scope"
    }

    return(x)
  }

  if (is_exported(x_c, pkg_name)) {
    paste0(pkg_name, "::", x_c)
  } else {
    paste0(pkg_name, ":::", x_c)
  }
}
