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

  start <- regexec("[a-zA-Z._0-9:]+$", line_head)[[1]]

  if(start == -1L) {
    start <- col
  }

  start
}

get_token_end <- function(context, selection) {
  row <- selection$range$start[[1]]
  col <- selection$range$start[[2]]
  buffer <- context$contents[[row]]

  line_tail = substr(buffer, col, nchar(buffer))

  offset = attr(regexec("^[a-zA-Z._0-9:]+", line_tail)[[1]], "match.length")

  if(offset == -1L) {
    offset = 0L
  }

  col + offset
}

unpack_selection <- function(envir = caller_env()) {
  check_rstudio()
  context_tracker <- ContextTracker$new()
  context_tracker$set_selections()

  ut <- lapply(context_tracker$buffer, #parsed_text,
               function(x) lapply(x, unpack, envir))

  if(is.null(ut)) return()

  invisible(context_tracker$replace_in_context(ut))
}

pack_selection <- function() {
  check_rstudio()
  context_tracker <- ContextTracker$new()
  context_tracker$set_selections()
  ut <- lapply(context_tracker$buffer,function(x) lapply(x, pack))

  if(is.null(ut)) return()

  invisible(context_tracker$replace_in_context(ut))

}

