ParseTreeUnpacker <- R6::R6Class(
  "ParseTreeUnpacker",
  inherit = ParseTree,
  portable = FALSE,

  public = list(
    initialize = function(..., envir = NULL) {
      envir <<- envir
      super$initialize(...)
    }
  )
)

ParseTreeUnpacker$set(
  "public",
  "unpack",

  function() {
    recursive_unpack(0L)
  }
)

ParseTreeUnpacker$set(
  "public",
  "recursive_unpack",
  function(new_root) {

    current_root <- root
    root <<- new_root

    unpack_assignment()

    if (any(parse_data$token == "FUNCTION")) {
      unpack_function()
    } else {
      unpack_exprs()
    }

    root <<- current_root

  }
)

ParseTreeUnpacker$set(
  "private",
  "unpack_exprs",
  function() {
    sym_ids <- parse_data[token %in% symbol_tokens]$id

    for (id in sym_ids) {
      unpack_symb(id)
    }

    expr_ids <- parse_data[terminal == FALSE]$id

    for (id in expr_ids) {
      recursive_unpack(id)
    }
  }
)

ParseTreeUnpacker$set(
  "private",
  "unpack_symb",
  function(.id) {
      nm <- as.name(.text_env[[as.character(.id)]])
      set(parse_data_full, which(parse_data_full$id == .id), "text", deparse(unpack_symbol(nm, envir)))
  }
)


ParseTreeUnpacker$set(
  "private",
  "unpack_assignment",
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

ParseTreeUnpacker$set(
  "private",
  "unpack_formals",
  function() {

    # drop the body expression.  should be the last one always...?
    pl <- head(parse_data, -1)

    expr_ids <- pl[token == "expr"]$id

    # Lazy bind pre-bound arguments
    bound_fml_nms <- pl[, .(token, nm = shift(text, 1L, type = "lag" ))][token == "EQ_FORMALS", nm]
    bound_fml_ids <- pl[, .(token, as = shift(id,   1L, type = "lead"))][token == "EQ_FORMALS", as]
    fmls <- lapply(bound_fml_ids, `class<-`, "lazy_unpack")
    names(fmls) <- bound_fml_nms
    env_bind_lazy(envir, !!!fmls)

    # Bind remaining arguments
    unbound_fml_nms <- setdiff(pl[token == "SYMBOL_FORMALS", text], bound_fml_nms)
    fmls <- rep_named(unbound_fml_nms, list(NULL))
    env_bind(envir, !!!fmls)
  }
)

ParseTreeUnpacker$set(
  "private",
  "unpack_body",
  function() {
    recursive_unpack(tail(parse_data$id, 1L))
  }
)

ParseTreeUnpacker$set(
  "private",
  "unpack_function",
  function() {

    enclos <- new_environment(parent = envir)
    cur_env <- envir
    envir <<- enclos

    unpack_formals()
    unpack_body()

    el <- as.list(enclos)
    unpack_el <- which(vapply(el, function(x) inherits(x, "unpack"),logical(1)))
    while(length(unpack_el) > 0) {
      for(i in unpack_el) {
        recursive_unpack(el[[i]])
        class(enclos[[names(el)[[i]]]]) <- "unpacked"
      }
      el <- as.list(enclos)
      unpack_el <- which(vapply(el, function(x) inherits(x, "unpack"),logical(1)))
    }

    envir <<- cur_env
  }
)


unpack_symbol <- function(x, envir) {
  xc <- as.character(x)
  x_env <- tryCatch(pryr::where(xc, envir), error = function(e) NULL)

  if(is_null(x_env)) {
    return(x)
  }

  if(grepl("^imports:", env_name(x_env)))  {
    x_env <- get_obj_env(xc, x_env)
  }

  pkg_name <- get_pkg_name(x_env) %||% ""

  if (pkg_name == "base")
    return(x)

  if (pkg_name == "") {
    y <- get(xc, x_env)
    if (inherits(y, "lazy_unpack")) {
      # mark lazy data for unpacking
      class(x_env[[xc]]) <- "unpack"

      return(x)
    } else {
      return(x)
    }
  }


  if(xc %in% getNamespaceExportsAndLazyData(pkg_name)) {
    return(make_exported_call(pkg_name, x))
  } else {
    return(make_internal_call(pkg_name, x))
  }

  return(x)
}
