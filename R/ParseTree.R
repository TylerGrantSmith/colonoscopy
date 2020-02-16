ParseTree <- R6::R6Class(
  "ParseTree",
  portable = FALSE,

  private = list(
    .parse_output = NULL,
    .parse_data = NULL,
    .parse_data_filtered = NULL,

    .text_env = NULL,
    .root_filter = NULL,
    .root_id = NULL,
    .envir = NULL
  ),

  public = list(
    initialize = function(...) {
      .parse_output <- parse(...)

      pd <- as.data.table(utils::getParseData(.parse_output))

      # add id = 0 row
      pd <- rbindlist(list(pd, list(terminal = F,
                                    id = 0L,
                                    parent = -Inf,
                                    text = "")), fill = T)

      # key on text order but add secondary index on id
      setindexv(pd, "id")
      setkeyv(pd, c("line1", "col1", "line2", "col2"))

      .parse_data <<- pd
      .text_env <<- rlang::new_environment(setNames(pd$text, pd$id))

      root <<- 0L
    },


    is_terminal = function(.id) {
      .parse_data[id == .id, terminal]
    },

    reset_root = function() {
      root <- 0L
    }
  ),

  active = list(
    envir = function(value) {
      if (missing(value)) {
        return(.envir)
      }
      .envir <<- value
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
      .parse_data$id[.parse_data$parent == 0L]
    },

    root = function(value) {
      if (missing(value)) {
        return(.root_id)
      }
      .root_id <<- value
      .root_filter <<- .parse_data$parent == value
      .parse_data_filtered <<- private$.parse_data[private$.root_filter]

      setindexv(.parse_data_filtered, "id")
      setkeyv(.parse_data_filtered, c("line1", "col1", "line2", "col2"))
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

  deco_text <- paste0(pd$deco_text[filter], collapse = "")
  deco_text
}

symbol_tokens <- c(
  "SYMBOL",
  "SYMBOL_FUNCTION_CALL"
)
