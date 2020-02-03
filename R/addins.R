ContextTracker <- R6::R6Class(
  "ContextTracker",

  public = list(

  ),

  private = list(
    patterns = c(start = "[a-zA-Z._0-9]+$",
                 end   = "^[a-zA-Z._0-9]+"),
    frozen_context = NULL
  ),

  active = list(
    context = function() { rstudioapi::getActiveDocumentContext() },
    buffer = function() {
      purrr::map(self$context$selection, purrr::pluck, 'text')
    },
    ranges = function() {
      purrr::map(self$context$selection, purrr::pluck, 'range')
    }
  ),
  cloneable = FALSE
)

check_rstudio <- function() {
  requireNamespace("rstudioapi", quietly = T) && rstudioapi::isAvailable()
}

set_selections <- function() {
  context <- context_tracker$context
  selections <- context$selection

  # expand empty selections to token at cursor
  for (i in 1:length(selections)) {
    if (!is_empty_selection(selections[[i]])) {
      next
    }

    selections[[i]]$range$start[[2]] <- get_token_start(context,selections[[i]])
    selections[[i]]$range$end[[2]]   <- get_token_end(context, selections[[i]])
  }

  rstudioapi::setSelectionRanges(purrr::map(selections, purrr::pluck, 'range'), context$id)
}

is_empty_selection <- function(selection) {
  identical(selection$range$start, selection$range$end)
}

is_selection <- function(context = .global_packr_env$context) {
  !all(sapply(context$selection, is_empty_range))
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

extract_padding <- function(buffer) {
  buffer %>%
    purrr::map(stringr::str_split, "\n", simplify = T) %>%
    purrr::map(purrr::keep, nzchar) %>%
    purrr::map(stringr::str_extract, "^(\\s)+") %>%
    purrr::map(purrr::modify_if, is.na, ~"")
}

replace_in_context <- function(.text) {
  if(is.null(.text)) return()

  ranges <- context_tracker$ranges
  buffer <- context_tracker$buffer #%>% map(trimws)
  padding <- extract_padding(buffer)
  id <- context_tracker$context$id

  text <- .text %>%
    purrr::map_depth(2, deparse) %>%
    purrr::map(unlist) %>%
    purrr::map(unmask_comments) %>%
    purrr::map(trimws)

  text <- purrr::map2(padding, text, stringr::str_c, collapse = "\n")

  map2(ranges,
       text,
       rstudioapi::insertText,
       id = id)
}

parse_selection <- function() {
  check_rstudio()
  set_selections()
  buffer <- context_tracker$buffer
  buffer %>% purrr::map(mask_selection) %>% purrr::map(parse_exprs)
}

unpack_selection <- function(envir = caller_env()) {
  parsed_text <- parse_selection()

  # outside
  ut <- unpack(!!parsed_text, envir)

  if(is.null(ut)) return()

  invisible(replace_in_context(ut))
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

mask_selection <- function(text) {
  pd <- data.table::as.data.table(utils::getParseData(parse(text = text)))

  pd <- data.table::as.data.table(pd)
  pd <- pd[terminal == TRUE]
  pd[, line_start := row.names(.SD) == 1L, line1]
  pd[line_start == TRUE, indent := (col1 - 1)][is.na(indent), indent := 0]

  pd[, space := col1 - data.table::shift(col2, 1, type = "lag") - 1, line1][is.na(space), space := 0]
  pd[, blank_lines := data.table::shift(line1, 1, type = "") - line2][is.na(blank_lines), blank_lines := 0]
  pd[, comment := token == "COMMENT"]
  pd[, inline_comment := comment & !line_start]
  pd[, deco_text := text]

  pd[, deco_text := purrr::map2(deco_text, indent,      ~paste0(strrep(' ', .y), .x))]
  pd[, deco_text := purrr::map2(deco_text, space,       ~paste0(strrep(' ', .y), .x))]
  pd[comment == TRUE, deco_text := purrr::map2(text, inline_comment, mask_comment)]
  pd[, deco_text := purrr::map2(deco_text, blank_lines, ~paste0(.x, strrep('\n', .y)))]

  deco_text <- paste0(pd$deco_text, collapse = "")
}

quote_comment <- function(text) {
  paste0("\"", text, "\"")
}


inline_comment <- "%%inline_comment%%"
comment_start <- ".comment_start"
comment_end <- ".comment_end'"

inline_comment_pattern <- paste0(inline_comment, "\"([^\"]*)\"")
block_comment_pattern <- sprintf('invisible\\(\\"\\%s([^"]*)\\%s\\"\\)', comment_start, comment_end)

mask_comment <- function(text, inline) {
  text <- quote_comment(text)
  if (inline) {
    text <- paste0(inline_comment, text)
  } else {
    text <- paste0("invisible(",comment_start, text, comment_end, ")")
  }

  text
}

unmask_comments <- function(text) {
  text <- gsub(inline_comment_pattern, " \\1", text)
  text <- gsub(block_comment_pattern, "\\1", text)
  text
}

mask_spaces <- function(text, spaces) {
  # text <- paste0(space_marker,)
}

pack_selection <- function(envir = caller_env(), enclos = new_environment(parent = empty_env())) {

  parsed_text <- parse_selection()
  ut = pack(!!parsed_text)

  if(is.null(ut)) return()

  invisible(replace_in_context(ut))

}

