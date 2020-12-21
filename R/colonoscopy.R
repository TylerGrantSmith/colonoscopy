#' Exposing or hiding package dependencies in R code.
#'
#' colonoscopy allows you to transform R code by adding or
#' removing namespace access operators (`::` and `:::`) in accordance
#' with the search path in an environment.
#'
#' RStudio users can find addins "Scope selection" and "Unscope selection"
#' to apply the respective function and replace the selected code.
#'
#' @examples
#' scope("1+1")
#' scope("tail")
#' scope("scope")
#' scope(scope)
#'
#' @rawNamespace import(rlang, except = ":=")
#' @importFrom magrittr "%>%"
#' @importFrom data.table ":=" as.data.table rbindlist set setkeyv setindexv shift "%chin%"
#' @importFrom purrr map map_chr map2 map2_chr pmap walk pwalk pluck
"_PACKAGE"
