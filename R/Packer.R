ParseTreePacker <- R6::R6Class(
  "ParseTreePacker",
  inherit = ParseTree,
  portable = FALSE,

  private = list(
    .imports = NULL  # place to save packed functions
  ),

  public = list(
    initialize = function(...) {
      super$initialize(...)
    }
  )
)

ParseTreePacker$set(
  "public",
  "pack",
  function() {
    colon_rows <- which(parse_data_full$token %in% c("NS_GET", "NS_GET_INT"))

    if (length(colon_rows) == 0) {
      return()
    }

    package_rows <- colon_rows - 1L
    name_rows    <- colon_rows + 1L

    colon_ids   <- parse_data_full$id[colon_rows]
    package_ids <- parse_data_full$id[package_rows]
    name_ids    <- parse_data_full$id[name_rows]

    parse_data_full[c(colon_ids, package_ids), text := ""]
  }
)
