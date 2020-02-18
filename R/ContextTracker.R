ContextTracker <- R6::R6Class(
  "ContextTracker",
  portable = FALSE,
  cloneable = FALSE,

  private = list(
    .context = NULL
  ),

  public = list(

    initialize = function() {
      .context <<- rstudioapi::getActiveDocumentContext()

      set_selections()
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
    },

    replace_in_context = function(.text) {
      if(is_null(.text)) return()

      # probably a better way to do this?
      text <- .text %>% map(unlist) %>% map_chr(paste0, collapse = "\n")

      map2(ranges,
           text,
           rstudioapi::insertText,
           id = id)
    }
  ),

  active = list(
    context = function(ct) {
      if (missing(ct)) {
        return(.context)
      }

      private$.context <- ct
    },

    id = function() {
      context$id
    },

    buffer = function() {
      map(selection, pluck, 'text')
    },

    ranges = function() {
      map(selection, pluck, 'range')
    },

    selection = function() {
      context$selection
    }
  )
)
