check_rstudio <- function() {
  requireNamespace("rstudioapi", quietly = T) && rstudioapi::isAvailable()
}

.set_context <- function() {
  if(check_rstudio()) {
    type <- "rstudio"
  } else {
    type <- "console"
  }

  switch(type,
         rstudio = .set_rstudio_context(),
         console = .set_console_context())
}


.set_rstudio_context <- function(with_selection = FALSE) {
  context   = rstudioapi::getActiveDocumentContext()
  selection = rstudioapi::primary_selection(context)
  row       = selection$range$end[[1]]

  if(with_selection) {
    .set_context_in_env(type     = "rstudio",
                        id       = context$id,
                        row      = row,
                        buffer   = selection$text,
                        cursor_pos = selection$range$end[[2]])
  } else {
    .set_context_in_env(type       = "rstudio",
                        id         = context$id,
                        row        = row,
                        buffer     = context$contents[[row]],
                        cursor_pos = selection$range$end[[2]])
  }
}

.set_console_context <- function() {
  stop("Not implemented yet outside of RStudio.")
}

.set_context_in_env <- function(type = NULL, id = NULL, row = NULL, buffer = "", cursor_pos = 1L) {
  .global_packr_env$context$type = type
  .global_packr_env$context$id = id
  .global_packr_env$context$row = row
  .global_packr_env$context$buffer = buffer
  .global_packr_env$context$cursor_pos = cursor_pos

  .set_token_start()
  .set_token_end()
  .set_token()
}


.set_token_start <- function() {
  line_head = substr(.global_packr_env$context$buffer,
                     1L,
                     .global_packr_env$context$cursor_pos-1)

  start = regexec(.global_packr_env$pattern_start, line_head)[[1]]

  if(start == -1) {
    start = .global_packr_env$context$cursor_pos
  }

  .global_packr_env$context$cursor_start = start
}

.set_token_end <- function() {

  line_tail = substr(.global_packr_env$context$buffer,
                     .global_packr_env$context$cursor_pos,
                     nchar(.global_packr_env$context$buffer))

  offset = attr(regexec(.global_packr_env$pattern_end, line_tail)[[1]], "match.length")
  if(offset == -1) {
    offset = 0
  }

  .global_packr_env$context$cursor_end = .global_packr_env$context$cursor_pos + offset - 1
}

.set_token <- function() {
  .global_packr_env$context$token = substring(.global_packr_env$context$buffer,
                                              .global_packr_env$context$cursor_start,
                                              .global_packr_env$context$cursor_end)
}


.replace_in_context <- function(text) {
  if(is.null(text)) return()

  row   = .global_packr_env$context$row
  start = .global_packr_env$context$cursor_start
  end   = .global_packr_env$context$cursor_end
  id    = .global_packr_env$context$id

  replace_range = rstudioapi::as.document_range(
    c(rstudioapi::as.document_position(c(row, start)),
      rstudioapi::as.document_position(c(row, end + 1))))

  rstudioapi::insertText(replace_range, text, id)
}



unpack_cursor <- function(envir = caller_env()) {
  .set_context()

  ut = unpack_token_at_cursor(envir)

  if(is.null(ut)) return()

  invisible(.replace_in_context(deparse(ut)))
}

unpack_selection <- function(envir = caller_env()) {
  .set_context()

  parsed_text = parse(text = .global_packr_env$context$buffer)
  ut = pack(parsed_text)

  if(is.null(ut)) return()

  invisible(.replace_in_context(ut))

}

unpack_token_at_cursor <- function(envir = caller_env()) {
  token <- .global_packr_env$context$token  #.rs.guessToken(line, cursorPos)

  if(token == "")
    return()

  ns_access = grepl(":", token)

  if(ns_access)
    return()

  ut = unpack_symbol(parse_expr(token), envir)

  return(ut)
}
pack_cursor <- function(envir = caller_env()) {
  pattern_start <- .global_packr_env$pattern_start
  pattern_end   <- .global_packr_env$pattern_end
  on.exit({
    .global_packr_env$pattern_start       <- pattern_start
    .global_packr_env$pattern_end         <- pattern_end
  })

  .global_packr_env$pattern_start       <- "[a-zA-Z._0-9:]+$"
  .global_packr_env$pattern_end         <- "^[a-zA-Z._0-9:]+"
  .set_context()

  ut = pack_token_at_cursor(envir,FALSE)

  invisible(.replace_in_context(ut))
}


pack_selection <- function(envir = caller_env(), enclos = new_environment(parent = empty_env())) {
  .set_context()

  parsed_text = parse(text = .global_packr_env$context$buffer)
  ut = pack(parsed_text)

  if(is.null(ut)) return()

  invisible(.replace_in_context(ut))

}

# pack_cursor_roxygen <- function(envir = caller_env()) {
#   pattern_start <- .global_packr_env$pattern_start
#   pattern_end   <- .global_packr_env$pattern_end
#   on.exit({
#     .global_packr_env$pattern_start       <- pattern_start
#     .global_packr_env$pattern_end         <- pattern_end
#   })
#
#   .global_packr_env$pattern_start       <- "[a-zA-Z._0-9:]+$"
#   .global_packr_env$pattern_end         <- "^[a-zA-Z._0-9:]+"
#   .set_context()
#   ut = pack_token_at_cursor(envir, TRUE)
#   invisible(.replace_in_context(ut))
# }

pack_token_at_cursor <- function(envir = caller_env(), roxygen = FALSE) {
  token <- .global_packr_env$context$token  #.rs.guessToken(line, cursorPos)

  if(token == "")
    return()

  up = pack_format(rlang::parse_expr(token), roxygen, envir)

  return(up)
}

pack_format <- function(text, roxygen, envir) {
  if(roxygen) {
    stop("Functionality not yet implemented")
    # out = pack_track(text)
    # headers <- create_header(out$pkgs)
    # text = paste0(headers, expr_text(out$res))
  } else {
    out = pack_(text,envir)
    text = expr_text(out)
  }

  return(text)
}

# pack_selection_roxygen <- function() {
#
#   context = rstudioapi::getSourceEditorContext()
#   selection = rstudioapi::primary_selection(context)
#   selection_text = selection$text
#   up <- pack(parse_expr(selection_text))
#   headers <- create_header(up$pkgs)
#   text = paste0(headers, rlang::expr_text(up$res))
#
#   rstudioapi::insertText(location = selection$range,
#                          text = text)
# }

create_header <- function(pkgs) {
  importfrom = "#' @importFrom"
  header = ''
  for(pkg in names(pkgs)) {
    header = paste(header, importfrom, pkg, paste0(pkgs[[pkg]], collapse = ", "),'\n')
  }
  header
}
