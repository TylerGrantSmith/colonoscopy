
ParseTree <- R6::R6Class(
  "ParseTree",

  private = list(
    # ---- Private Fields ----
    .text = NULL,
    header = '',
    footer = '',
    pd = NULL,
    parse_data_filtered = NULL,

    root_id = NULL,
    cache = list(),
    text_env = NULL,
    .envir = NULL,

    # ---- Private Methods ----
    cache_parse_data = function() {
      nonterminal_ids <- private$pd$id[private$pd$terminal == F]

      for (id in nonterminal_ids) {
        mask <- private$pd$parent == id
        private$cache[[id + 1]] <- private$pd[mask]
      }
    },

    expand_parse_data = function(pd) {
      if (is_null(pd)) {
        return("")
      }

      terminal <- line_start <- line1 <- line2 <- col1 <- col2 <-
        indent <- space <- blank_lines <- token <- NULL

      filter <- pd$terminal
      pd[filter, line_start := row.names(.SD) == 1L, line1]
      pd[line_start == TRUE, indent := (col1 - 1)][is.na(indent), indent := 0]

      pd[filter, space := col1 - shift(col2, 1L, NA_integer_, "lag") - 1, line1][is.na(space), space := 0]
      pd[filter, blank_lines := shift(line1, 1L, NA_integer_, "lead") - line2][is.na(blank_lines), blank_lines := 0]
      pd[filter, deco_text := text]

      pd[filter, deco_text := map2_chr(deco_text, indent, ~paste0(strrep(' ', .y), .x))]
      pd[filter, deco_text := map2_chr(deco_text, space,  ~paste0(strrep(' ', .y), .x))]
      pd[filter, deco_text := map2_chr(deco_text, blank_lines, ~paste0(.x, strrep('\n', .y)))]

      deco_text <- paste0(pd$deco_text[filter], collapse = "")
      deco_text
    }


  ),

  public = list(
    # ---- Public Fields ----
    symbol_tokens = c("SYMBOL", "SYMBOL_FUNCTION_CALL"),

    # ---- Public Methods ----

    initialize = function(text, ...) {
      if (is_null(text))
        abort("ParseData cannot be initialized with NULL.")

      private$.text  <- text

      private$header <- extract_header(text)
      private$footer <- extract_footer(text)
      private$pd     <- get_parse_data(text = text, ...)

      self$set_parse_data_keys()
      private$cache_parse_data()
      private$text_env <- new_environment(stats::setNames(private$pd$text, private$pd$id))


      self$root <- 0L
    },

    print = function(...) {
      cat(self$text)
      invisible(self)
    },

    set_parse_data_keys = function(index = "id", keys = c("line1", "col1", "line2", "col2")) {
      setindexv(private$pd, index)
      setkeyv(private$pd, keys)
    }

  ),

  active = list(

    # ---- Active Fields ----

    envir = function(value) {
      if (missing(value)) {
        return(private$.envir)
      }
      private$.envir <- value
    },

    ids = function() {
      private$parse_data_filtered$id
    },

    text = function() {
      paste0(private$header, private$expand_parse_data(private$pd), private$footer)
    },

    root = function(value) {
      if (missing(value)) {
        return(private$root_id)
      }
      private$root_id <- value
      private$parse_data_filtered <- private$cache[[value + 1]]
    }
  )
)

as.character.ParseTree <- function(pt) {
  pt$text
}
