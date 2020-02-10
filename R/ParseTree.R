ParseTree <- R6::R6Class(
  "ParseTree",
  portable = FALSE,

  private = list(
    .parse_output = NULL,
    .parse_data = NULL,
    .root_filter = NULL,
    .root_id = NULL,
    .envir = NULL
  ),

  public = list(
    initialize = function(...) {
      private$.parse_output <- parse(...)
      private$.parse_data <- as.data.table(getParseData(private$.parse_output))
      private$.parse_data[, skip := FALSE]
      private$.parse_data[, mod := NA_character_]
      private$.parse_data[, envir := list(new_environment(parent = base_env()))]

      self$root <- 0L
    },


    skip = function(ids) {
      private$.parse_data[id %in% ids, skip := TRUE]
    },

    is_skipped = function(.id) {
      private$.parse_data[id == .id, skip]
    },

    is_terminal = function(.id) {
      private$.parse_data[id == .id, terminal]
    }
  ),

  active = list(
    root_id = function() {
      private$.parse_data[parent == 0L, id]
    },

    child_ids = function() {

      child_ids <- integer()

      get_children <- function(pd) {
        if (nrow(pd) == 0) return()
        child_ids <<- c(child_ids, pd$id)

        pd <- pd[terminal == F]

        for (i in 1:nrow(pd)) {

          get_children(parse_data_full[parent %in% pd[i, id]])

        }
      }

      get_children(parse_data)

      sort(child_ids)
    },

    source = function() {
      attr(private$.parse_output, "wholeSrcref")
    },

    envir = function(value) {
      if (missing(value)) {
        return(private$.envir)
      }

      if (!is_environment(value)) abort("Not an environment")

      private$.envir <- value
    },

    srcfile = function() {
      attr(private$.parse_data, "srcfile")
    },

    parse_data_full = function() {
      private$.parse_data
    },

    parse_data = function() {
      private$.parse_data[private$.root_filter]
    },

    ids = function() {
      parse_data$id
    },

    root = function(value) {
      if (missing(value)) {
        return(private$.root_id)
      }

      private$.root_filter <- private$.parse_data$parent == value
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
