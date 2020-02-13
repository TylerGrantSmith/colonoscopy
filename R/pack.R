#' @export
pack <- function(x) {
  UseMethod("pack")
}


#' @rdname pack
#' @export
pack.default <- function(x) {
  tryCatch(x <- as.character(x),
           error = function(e) abort("Unable to convert x to a character"))

  pack.character(x)
}

#' @rdname pack
#' @export
pack.character <- function(x, ...) {
  header <- regmatches(x, regexec("^\\s+", x))[[1]]
  footer <- regmatches(x, regexec("\\s+$", x))[[1]]
  ptp <- ParseTreePacker$new(text = x)
  ptp$pack()
  paste0(header, ptp$text, footer, collapse = "")
}


pack.function <- function(x, use_fn_env = TRUE, useSource = T,...) {
  control = c("keepInteger", "keepNA")

  if (use_fn_env)
    envir <- environment(x)

  if (!is.null(attr(x, "srcref")))
    return(pack(attr(x,"srcref")))

  if (useSource)
    control <- append(control, "useSource")

  pack(deparse(x, width.cutoff = 59, control = control))
}
#
