ParseTreeUnscoper <- R6::R6Class(
  "ParseTreeUnscoper",
  inherit = ParseTree,
  portable = FALSE,

  private = list(
    .imports = NULL  # place to save packed functions
  ),

  public = list(
    initialize = function(...) {
      super$initialize(...)
      unscope()
    },

    print = function(...) {
      cat("Unscoped output:\n")
      super$print()
    }
  )
)

ParseTreeUnscoper$set(
  "public",
  "unscope",
  function(add_roxygen = FALSE) {
    colon_rows <- which(parse_data_full$token %in% c("NS_GET", "NS_GET_INT"))

    if (length(colon_rows)==0) {
      return()
    }

    package_rows <- colon_rows - 1

    parse_data_full[c(package_rows, colon_rows), text := ""]
    browser()
  }
)
