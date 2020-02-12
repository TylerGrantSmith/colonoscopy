ParseTree <- R6::R6Class(
  "ParseTree",
  portable = FALSE,

  private = list(
    .parse_output = NULL,
    .parse_data = NULL,
    .parse_data_filtered = NULL,

    .token_env = NULL,
    .terminal_env = NULL,
    .text_env = NULL,
    .hash_env = NULL,

    .root_filter = NULL,
    .root_id = NULL,
    .envir = NULL
  ),

  public = list(
    initialize = function(...) {
      private$.parse_output <- parse(...)

      pd <- as.data.table(getParseData(private$.parse_output))

      # add "master_root" row
      pd <- rbindlist(list(pd, list(id = 0L, parent = -Inf, text = "")), fill = T)

      setkeyv(pd, "id")

      private$.parse_data <- pd
      self$root <- 0L

      private$.text_env <- new_environment(setNames(pd$text, pd$id))
      private$.token_env <- new_environment(setNames(pd$token, pd$id))
      private$.terminal_env <- new_environment(setNames(pd$terminal, pd$id))

    },


    skip = function(ids) {
      .parse_data[id %in% ids, skip := TRUE]
    },

    is_skipped = function(.id) {
      .parse_data[id == .id, skip]
    },

    is_terminal = function(.id) {
      .parse_data[id == .id, terminal]
    },

    reset_root = function() {
      root <- 0L
    }
  ),

  active = list(

    child_ids = function() {

      child_ids <- integer()

      get_children <- function(pd) {
        if (nrow(pd) == 0) return()

        child_ids <<- c(child_ids, pd$id)
        non_terminal_ids <- pd[terminal == F]$id
        for (i in seq_along(non_terminal_ids)) {

          get_children(private$.parse_data[parent %in% non_terminal_ids[[i]]])

        }
      }

      get_children(private$.parse_data_filtered)
    },

    source = function() {
      attr(.parse_output, "wholeSrcref")
    },

    envir = function(value) {
      if (missing(value)) {
        return(private$.envir)
      }
      private$.envir <- value
    },

    srcfile = function() {
      attr(.parse_data, "srcfile")
    },

    parse_data_full = function() {
      .parse_data
    },

    parse_data = function() {
      .parse_data_filtered
    },

    ids = function() {
      .parse_data_filtered$id
    },

    master_root_id = function() {
      .parse_data[parent == 0L, id]
    },

    root = function(value) {
      if (missing(value)) {
        return(.root_id)
      }
      private$.root_filter <- .parse_data$parent == value
      private$.parse_data_filtered <- private$.parse_data[private$.root_filter]
      setkey(private$.parse_data_filtered, "id")
      private$.root_id <- value
    },

    text = function() {
      expand_parse_data(parse_data_full)
    }
  )
)


expand_parse_data <- function(pd) {
  terminal <- line_start <- line1 <- line2 <- col1 <- col2 <-
    indent <- space <- blank_lines <- token <- NULL

  filter <- pd$terminal
  pd[filter, line_start := row.names(.SD) == 1L, line1]
  pd[line_start == TRUE, indent := (col1 - 1)][is.na(indent), indent := 0]

  pd[filter, space := col1 - data.table::shift(col2, 1L, NA_integer_, "lag") - 1, line1][is.na(space), space := 0]
  pd[filter, blank_lines := data.table::shift(line1, 1L, NA_integer_, "lead") - line2][is.na(blank_lines), blank_lines := 0]
  pd[filter, deco_text := text]

  pd[filter, deco_text := purrr::map2_chr(deco_text, indent, ~paste0(strrep(' ', .y), .x))]
  pd[filter, deco_text := purrr::map2_chr(deco_text, space,  ~paste0(strrep(' ', .y), .x))]
  pd[filter, deco_text := purrr::map2_chr(deco_text, blank_lines, ~paste0(.x, strrep('\n', .y)))]

  deco_text <- paste0(pd[filter]$deco_text, collapse = "")
  deco_text
}

symbol_tokens <- c(
  "SYMBOL",
  "SYMBOL_FUNCTION_CALL"
)
