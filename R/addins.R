check_rstudio <- function() {
  is_installed("rstudioapi") && rstudioapi::isAvailable()
}

is_empty_selection <- function(selection) {
  identical(selection$range$start, selection$range$end)
}

get_token_start <- function(context, selection) {
  row <- selection$range$start[[1]]
  col <- selection$range$start[[2]]
  buffer <- context$contents[[row]]

  line_head <- substr(buffer, 1, col - 1)

  start <- regexec("[a-zA-Z._0-9:]+$", line_head)[[1]]

  if(start == -1) {
    start <- col
  }

  start
}

get_token_end <- function(context, selection) {
  row       <- selection$range$start[[1]]
  col       <- selection$range$start[[2]]
  buffer    <- context$contents[[row]]
  line_tail <- substr(buffer, col, nchar(buffer))
  offset    <- attr(regexec("^[a-zA-Z._0-9:]+", line_tail)[[1]], "match.length")
  offset    <- max(offset, 0)

  col + offset
}

#' @importFrom purrr map_depth
scope_selection <- function(envir = caller_env()) {
  if(!check_rstudio()) {
    abort(paste(
      "Either `rstudioapi` is missing or you are attempting",
      "to run this function from outside RStudio."))
  }

  context_tracker <- ContextTracker$new()

  out <- map_depth(context_tracker$buffer, 2, scope, envir)

  if(is_null(out)) return()

  context_tracker$replace_in_context(out)
}

#' @importFrom purrr map_depth
unscope_selection <- function() {
  check_rstudio()
  context_tracker <- ContextTracker$new()

  out <- map_depth(context_tracker$buffer, 2, unscope, envir)

  if(is_null(out)) return()

  context_tracker$replace_in_context(out)

}

