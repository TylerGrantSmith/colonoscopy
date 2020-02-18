ParseTreeUnscoper <- R6::R6Class(
  "ParseTreeUnscoper",
  inherit = ParseTree,

  private = list(),

  public = list(
    initialize = function(...) {
      super$initialize(...)
      super$set_parse_data_keys(key = c("terminal", "line1", "col1", "line2", "col2"))
      self$unscope()
    },

    unscope = function() {
      colon_rows <- which(private$pd$token %in% c("NS_GET", "NS_GET_INT"))

      if (length(colon_rows)==0) {
        return()
      }

      package_rows <- colon_rows - 1
      name_rows <- colon_rows + 1

      private$pd[c(package_rows, colon_rows), text := ""]
    }
  )
)
