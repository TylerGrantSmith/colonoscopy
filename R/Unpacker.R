# pt <- ParseTreeUnpacker$new(text = "test <- function(b = mutate, mutate = 3) {\n  \n  # comment\n  \n  a=mtcars %>% mutate(mpg = 2) %>% \n    group_by(cyl); a <- 3 -> c\n}\n\n")

ParseTreeUnpacker <- R6::R6Class(
  "ParseTreeUnpacker",
  inherit = ParseTree,
  portable = FALSE,

  private = list(
    .eval_env = empty_env()
  ),

  public = list(
    initialize = function(..., envir = NULL) {
      super$initialize(...)
    }
  ),

  active = list(
    eval_env = function(value) {
      if (missing(value)) {
        return(private$.eval_env)
      }

      .eval_env <- value
      envir <<- value
    }
  )
)

ParseTreeUnpacker$set(
  "public",
  "unpack",

  function(envir = caller_env()) {
    eval_env <<- new_environment(parent = envir)
    root <<- root_id
    recursive_unpack()
    root <<- root_id
  }
)

ParseTreeUnpacker$set(
  "public",
  "recursive_unpack",
  function() {

    if (is_skipped(root)) {
      return()
    }

    # make sure not to revisit
    skip(root)

    unpack_assignment()
    unpack_function()
    unpack_exprs()

  }
)

ParseTreeUnpacker$set(
  "private",
  "unpack_exprs",
  function() {

    sym_ids <- parse_data[token %in% symbol_tokens]$id

    for (id in sym_ids) {
      unpack_symb(id)
      skip(id)
    }

    expr_ids <- parse_data[token == "expr"]$id

    for (id in expr_ids) {
      root <<- id
      recursive_unpack()
    }
  }
)

ParseTreeUnpacker$set(
  "private",
  "unpack_symb",
  function(.id) {
    if (!is_skipped(.id)) {
      nm <- as.name(parse_data[id == .id, text])
      envir <- parse_data[id == .id, envir][[1]]
      set(parse_data_full, which(parse_data_full$id == .id), "text", deparse(unpack_symbol(nm, envir)))
    }
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
    assignment_token <- parse_data[assign_filter]$token

    assigned_row <- switch(assignment_token,
                          EQ_ASSIGN =,
                          LEFT_ASSIGN = assign_row - 1,
                          RIGHT_ASSIGN = assign_row + 1)

    assigned_id <- parse_data[assigned_row, id]
    sub_pt <- parse_data_full[parent == assigned_id]

    if (identical(sub_pt$token, "SYMBOL")) {
      env <- sub_pt$envir[[1]]
      nm  <- sub_pt$text[[1]]
      env_bind(.env = env, !!nm := assigned_id)
      parse_data_full[parent == assigned_id, skip := TRUE]
    }
  }
)

ParseTreeUnpacker$set(
  "private",
  "unpack_function",
  function() {

    if (!any(parse_data$token == "FUNCTION")) {
      return()
    }


    enclos <- new_environment(parent = envir)
    parse_data_full[id %in% child_ids, envir := .(enclos)]

    for (.id in ids) {
      if (is_skipped(.id)) {
        next
      }

      if (parse_data[id == .id]$token == "SYMBOL_FORMALS") {
        nm <- parse_data[id == .id]$text
        env_bind_lazy(enclos, !!nm := .id)
        skip(.id)
      }

      if (!is_terminal(.id)) {
        ptc <- clone()
        ptc$root <- .id
        ptc$recursive_unpack()
      }
    }
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

  pkg_name <- get_pkg_name(x_env)

  if(is_null(pkg_name) || pkg_name == 'base') {
    return(x)
  }

  if(xc %in% getNamespaceExportsAndLazyData(pkg_name)) {
    return(make_exported_call(pkg_name, x))
  } else {
    return(make_internal_call(pkg_name, x))
  }

  return(x)
}
