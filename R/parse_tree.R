ParseTree <- R6::R6Class(
  "ParseTree",

  private = list(
    .parse_output = NULL,
    .parse_data = NULL
  ),

  public = list(
    initialize = function(...) {
      private$.parse_output <- parse(...)

      private$.parse_data <- as.data.table(getParseData(private$.parse_output))
    }
  ),

  active = list(
    source = function() {
      attr(private$.parse_output, "wholeSrcref")
    },

    srcfile = function() {
      attr(private$.parse_data, "srcfile")
    },

    parse_data = function() {
      private$.parse_data
    },

    root_id = function() {
      self$parse_data[self$parse_data$parent == 0, id]
    }
  )
)


Unpacker <- R6::R6Class(
  "ParseTreeUnpacker",
  inherit = "ParseTree",
  public = list(
    initialize = function(x, envir = NULL) {

    },

    unpack = function() {
      unpack_(self$parse_data, self$root_id)
    }
  )
)

expand_parse_data <- function(pd) {
  terminal <- line_start <- line1 <- line2 <- col1 <- col2 <-
    indent <- space <- blank_lines <- token <- NULL

  filter <- pd$terminal
  pd[filter, line_start := row.names(.SD) == 1L, line1]
  pd[line_start == TRUE, indent := (col1 - 1)][is.na(indent), indent := 0]

  pd[, space := col1 - data.table::shift(col2, 1, type = "lag") - 1, line1][is.na(space), space := 0]
  pd[, blank_lines := data.table::shift(line1, 1, type = "lead") - line2][is.na(blank_lines), blank_lines := 0]
  pd[, comment := token == "COMMENT"]
  pd[, inline_comment := comment & !line_start]
  pd[, deco_text := text]

  pd[, deco_text := purrr::map2(deco_text, indent,      ~paste0(strrep(' ', .y), .x))]
  pd[, deco_text := purrr::map2(deco_text, space,       ~paste0(strrep(' ', .y), .x))]
  # pd[comment == TRUE, deco_text := purrr::map2(text, inline_comment, mask_comment)]
  pd[, deco_text := purrr::map2(deco_text, blank_lines, ~paste0(.x, strrep('\n', .y)))]

  deco_text <- paste0(pd$deco_text, collapse = "")
}

symbol_tokens <- c(
  "SYMBOL",
  "SYMBOL_FUNCTION_CALL"
)
# pt <- ParseTree$new(text = "test <- function(b = mutate, mutate = 3) {\n  \n  # comment\n  \n  mtcars %>% mutate(mpg = 2) %>% \n    group_by(cyl)\n}\n\n")
# parse_data <- pt$parse_data
# parse_data_copy <- copy(parse_data)
# parse_data <- copy(parse_data_copy)

unpack <- function(parse_data, envir = caller_env()) {
    unpack_expr(parse_data, 0L, envir)
}

unpack_expr <- function(parse_data, root_id, envir) {

  # unpack_formals(parse_data, id, envir)
  # unpack_assignment(parse_data, id, envir)

  sym_ids <- parse_data[(parent == root_id) & (token %in% symbol_tokens), id]

  for (id in sym_ids) {
    unpack_symb(parse_data, id)
  }

  expr_ids <- parse_data[(parent == root_id) & (token == "expr"), id]

  for (id in expr_ids) {
    unpack_expr(parse_data, id)
  }
}

unpack_symb <- function(parse_data, .id, assigned) {
  if (!assigned) {
    parse_data[id == .id, text := sapply(text, unpack_symbol, global_env())]
  }
}

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
