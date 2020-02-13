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

