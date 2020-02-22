ParseTreeScoper <- R6::R6Class(
  "ParseTreeScoper",
  inherit = ParseTree,

  public = list(
    initialize = function(..., envir = NULL) {
      super$initialize(...)
      self$envir <- envir
      self$scope()
    }
  )
)

ParseTreeScoper$set(
  "public",
  "scope",
  function() {
    private$recursive_scope(0L)
  }
)

ParseTreeScoper$set(
  "private",
  "scope_all",
  function() {
    private$scope_assignment()

    if (any(private$parse_data_filtered$token == "FUNCTION")) {
      private$scope_function()
    } else {
      private$scope_exprs()
    }
  }
)

ParseTreeScoper$set(
  "private",
  "recursive_scope",
  function(new_root) {
    with_self_bindings(private$scope_all(), root = new_root)
  }
)

ParseTreeScoper$set(
  "private",
  "scope_exprs",
  function() {
    skip_ids <- integer()
    if (any(private$parse_data_filtered$token %in% c("NS_GET", "NS_GET_INT", "'$'",  "RIGHT_ASSIGN", "LEFT_ASSIGN","EQ_ASSIGN"))) {
      for (.id in self$ids) {
        if (private$parse_data_filtered[,.(id, pt = shift(token, 1L, '', "lag"))][id == .id, pt] %in% c("NS_GET", "NS_GET_INT", "'$'","RIGHT_ASSIGN")) {
          skip_ids <- append(skip_ids, .id)
          next
        }
        if (private$parse_data_filtered[,.(id, pt = shift(token, 1L, '', "lead"))][id == .id, pt] %in% c("LEFT_ASSIGN", "EQ_ASSIGN")) {
          skip_ids <- append(skip_ids, .id)
          next
        }
      }
    }

    sym_ids <- private$parse_data_filtered$id[private$parse_data_filtered$token %chin% self$symbol_tokens]
    sym_ids <- setdiff(sym_ids, skip_ids)

    for (id in sym_ids) {
      private$scope_symb(id)
    }

    expr_ids <- private$parse_data_filtered$id[private$parse_data_filtered$terminal == FALSE]
    expr_ids <- setdiff(expr_ids, skip_ids)

    for (id in expr_ids) {
      private$recursive_scope(id)
    }
  }
)

ParseTreeScoper$set(
  "private",
  "scope_symb",
  function(.id) {
    nm <- private$text_env[[as.character(.id)]]
    set(
      x = private$pd,
      i = which(private$pd$id == .id),
      j = "text",
      value = private$scope_symbol(nm)
    )
  }
)


ParseTreeScoper$set(
  "private",
  "scope_assignment",
  function() {
    assign_filter <- private$parse_data_filtered$token %in% c("LEFT_ASSIGN", "RIGHT_ASSIGN", "EQ_ASSIGN")

    if (!any(assign_filter)) {
      return()
    }
    assign_rows <- which(assign_filter)
    offsets <- ifelse(private$parse_data_filtered$token[assign_rows] == "RIGHT_ASSIGN", 1L, -1L)
    is_double_arrow <- private$parse_data_filtered$text[assign_rows] %in% c("<<-", "->>")
    assigned_rows <- assign_rows + offsets
    assigned_ids <- private$parse_data_filtered$id[assigned_rows]
    assigned_pds <- map(assigned_ids + 1, ~private$cache[[.x]])

    assign_symbol <- function (pd, id, is_double_arrow) {
      if (identical(pd$token, "SYMBOL")) {
        nm <- pd$text[1]

        # Hacky non-accurate workaround for double arrow assignment
        if (is_double_arrow) {
          env <- env_parent(self$envir)
        } else {
          env <- self$envir
        }

        env_bind(.env = env, !!nm := id)
      }
    }

    pmap(list(assigned_pds, assigned_ids, is_double_arrow), assign_symbol)

  }
)

ParseTreeScoper$set(
  "private",
  "scope_formals",
  function() {
    # drop the body expression.  should be the last one always...?
    pl <- head(private$parse_data_filtered, -1)

    expr_ids <- pl$id[pl$token == "expr"]

    # Lazy bind pre-bound arguments
    bound_fml_nms <- pl[, .(token, nm = shift(text, 1L, type = "lag" ))][token == "EQ_FORMALS", nm]
    bound_fml_ids <- pl[, .(token, as = shift(id,   1L, type = "lead"))][token == "EQ_FORMALS", as]
    fmls <- lapply(bound_fml_ids, `class<-`, "lazy_scope")
    names(fmls) <- bound_fml_nms
    env_bind_lazy(self$envir, !!!fmls)

    # Bind remaining arguments
    unbound_fml_nms <- setdiff(pl[token == "SYMBOL_FORMALS", text], bound_fml_nms)
    fmls <- rep_named(unbound_fml_nms, list(NULL))
    env_bind(self$envir, !!!fmls)

    expr_ids <- pl$id[pl$token == "expr"]

    for (id in expr_ids) {
      private$recursive_scope(id)
    }
  }
)

ParseTreeScoper$set(
  "private",
  "scope_body",
  function() {
    private$recursive_scope(utils::tail(private$parse_data_filtered$id, 1L))
  }
)

ParseTreeScoper$set(
  "private",
  "scope_function",
  function() {
    enclos <- new_environment(parent = self$envir)

    with_self_bindings(
      envir = enclos,

      {
        private$scope_formals()
        private$scope_body()

        el <- as.list(enclos)
        scope_el <- which(vapply(el, function(x) inherits(x, "scope"),logical(1)))
        while(length(scope_el) > 0) {
          for(i in scope_el) {
            private$recursive_scope(el[[i]])
            class(enclos[[names(el)[[i]]]]) <- "scoped"
          }
          el <- as.list(enclos)
          scope_el <- which(vapply(el, function(x) inherits(x, "scope"),logical(1)))
        }
      }
    )
  }
)

ParseTreeScoper$set(
  "private",
  "scope_symbol",
  function(x) {
    nm   <- as.character(x)
    where_env <- tryCatch(where(nm, self$envir), error = function(e) NULL)

    if (identical(where_env, private$.envir_initial)) { return(x) }
    if (is_null(where_env)) { return(x) }

    pkg_name <- find_pkg_name(nm, where_env)

    if (pkg_name == "base") { return(x) }

    if (pkg_name == "") {
      y <- get(nm, where_env)

      # mark lazy data for scope-ing
      if (inherits(y, "lazy_scope")) {
        class(where_env[[nm]]) <- "scope"
      }

      return(x)
    }

    if (is_exported(nm, pkg_name)) {
      paste0(pkg_name, "::", nm)
    } else {
      paste0(pkg_name, ":::", nm)
    }
  }
)
