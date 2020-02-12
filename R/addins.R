ContextTracker <- R6::R6Class(
  "ContextTracker",
  portable = FALSE,
  cloneable = FALSE,

  public = list(

    initialize = function() {
      private$.context <- rstudioapi::getActiveDocumentContext()
    },

    set_selections = function() {
      for (i in 1:length(selection)) {
        if (!is_empty_selection(selection[[i]])) {
          next
        }

        private$.context$selection[[i]]$range$start[[2]] <- get_token_start(self$context, self$context$selection[[i]])
        private$.context$selection[[i]]$range$end[[2]]   <- get_token_end(self$context,   self$context$selection[[i]])
      }

      rstudioapi::setSelectionRanges(purrr::map(private$.context$selection, purrr::pluck, 'range'), private$.context$id)
      private$.context <- rstudioapi::getActiveDocumentContext()
    }
  ),

  private = list(
    patterns = c(start = "[a-zA-Z._0-9]+$",
                 end   = "^[a-zA-Z._0-9]+"),
    .context = NULL
  ),

  active = list(
    context = function(ct) {
      if (missing(ct)) {
        return(.context)
      }
      private$.context <- ct
    },

    id = function() { context$id },

    buffer = function() {
      purrr::map(context$selection, purrr::pluck, 'text')
    },

    ranges = function() {
      purrr::map(context$selection, purrr::pluck, 'range')
    },

    selection = function() context$selection
  )
)

ContextTracker$set(
  "public",
  "replace_in_context",
  function(.text) {
    if(is.null(.text)) return()

    ranges <- self$ranges
    buffer <- self$buffer
    id <- self$id

    # probably a better way to do this?
    text <- lapply(lapply(.text, unlist), paste0, collapse = "\n")

    purrr::map2(ranges,
                text,
                rstudioapi::insertText,
                id = id)
  }
)


check_rstudio <- function() {
  requireNamespace("rstudioapi", quietly = T) && rstudioapi::isAvailable()
}

is_empty_selection <- function(selection) {
  identical(selection$range$start, selection$range$end)
}

get_token_start <- function(context, selection) {
  row <- selection$range$start[[1]]
  col <- selection$range$start[[2]]
  buffer <- context$contents[[row]]

  line_head <- substr(buffer, 1L, col - 1L)

  start <- regexec(.global_packr_env$pattern_start, line_head)[[1]]

  if(start == -1) {
    start <- col
  }

  start
}

get_token_end <- function(context, selection) {
  row <- selection$range$start[[1]]
  col <- selection$range$start[[2]]
  buffer <- context$contents[[row]]

  line_tail = substr(buffer, col, nchar(buffer))

  offset = attr(regexec(.global_packr_env$pattern_end, line_tail)[[1]], "match.length")

  if(offset == -1) {
    offset = 0
  }

  col + offset
}

unpack_selection <- function(envir = caller_env()) {
  check_rstudio()
  context_tracker <- ContextTracker$new()
  context_tracker$set_selections()
  # parsed_text <- parse_selection(context_tracker)

  # outside
  ut <- lapply(context_tracker$buffer, #parsed_text,
               function(x) lapply(x, unpack, envir))

  if(is.null(ut)) return()

  invisible(context_tracker$replace_in_context(ut))
}

comment_start <- ".comment_start"
comment_end   <- ".comment_end"
blank_line <- sprintf('invisible(.blank_line)')
inline_comment <- sprintf('invisible("%s%s")',
                          comment_start,
                          comment_end)

# text = "
#     cat <-   parse( text ) #comment
#
# # test
# "
#
# parse(text="cat <- parse(text = invisible('%beginspace% 43') %endspace% comment) %beginspace% 43 %inlinecomment% '#test'")
#
# mask_selection <- function(text) {
#   terminal <- line_start <- line1 <- line2 <- col1 <- col2 <-
#     indent <- space <- blank_lines <- token <- NULL
#
#   pd <- data.table::as.data.table(utils::getParseData(parse(text = text)))
#
#   pd <- data.table::as.data.table(pd)
#   pd <- pd[terminal == TRUE]
#   pd[, line_start := row.names(.SD) == 1L, line1]
#   pd[line_start == TRUE, indent := (col1 - 1)][is.na(indent), indent := 0]
#
#   pd[, space := col1 - data.table::shift(col2, 1, type = "lag") - 1, line1][is.na(space), space := 0]
#   pd[, blank_lines := data.table::shift(line1, 1, type = "lead") - line2][is.na(blank_lines), blank_lines := 0]
#   pd[, comment := token == "COMMENT"]
#   pd[, inline_comment := comment & !line_start]
#   pd[, deco_text := text]
#
#   pd[, deco_text := purrr::map2(deco_text, indent,      ~paste0(strrep(' ', .y), .x))]
#   pd[, deco_text := purrr::map2(deco_text, space,       ~paste0(strrep(' ', .y), .x))]
#   # pd[comment == TRUE, deco_text := purrr::map2(text, inline_comment, mask_comment)]
#   pd[, deco_text := purrr::map2(deco_text, blank_lines, ~paste0(.x, strrep('\n', .y)))]
#
#   deco_text <- paste0(pd$deco_text, collapse = "")
# }
#
# quote_comment <- function(text) {
#   paste0("\"", text, "\"")
# }
#
# inline_comment_token <- ".__inline_comment__."
# comment_token <- ".__comment___."
# comment_end <- ".comment_end'"
#
# inline_comment_pattern <- paste0("\\n?", inline_comment_token, "\\(\"([^\"]*)\"\\)")
# comment_pattern <- paste0("\\n?", comment_token, "\\(\"([^\"]*)\"\\)")
#
# mask_comment <- function(text, inline) {
#   text <- quote_comment(text)
#   if (inline) {
#     text <- paste0("\n", inline_comment_token, "(", text, ")")
#   } else {
#     text <- paste0(comment_token, "(", text, ")")
#   }
#
#   text
# }
#
# unmask_comments <- function(text) {
#   text <- gsub(inline_comment_pattern, " \\1", text)
#   text <- gsub(comment_pattern, "\\1", text)
#   text
# }
#
# mask_spaces <- function(text, spaces) {
#   # text <- paste0(space_marker,)
# }

pack_selection <- function(envir = caller_env(), enclos = new_environment(parent = empty_env())) {

  parsed_text <- parse_selection()
  ut = pack(!!parsed_text)

  if(is.null(ut)) return()

  invisible(replace_in_context(ut))

}

