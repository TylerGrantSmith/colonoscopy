ParseTreeScoper <- R6::R6Class(
  "ParseTreeScoper",
  inherit = ParseTree,

  public = list(
    initialize = function(..., envir = NULL) {
      super$initialize(...)
      self$envir <- envir
      private$visited_ids <- environment()
      private$recursive_scope(0L)
    }
  )
)

ParseTreeScoper$set(
  "private",
  "recursive_scope",
  function(root) {
    with_self_bindings(private$scope_all(), root = root)
  }
)

ParseTreeScoper$set(
  "private",
  "scope_all",
  function() {
    root_sym <- as.character(private$root_id)

    if (get0(root_sym, private$visited_ids) %||% FALSE)
      return()

    if (any(private$parse_data_filtered$token == "FUNCTION")) {
      private$scope_function()
    } else {
      private$scope_exprs()
    }
    assign(root_sym, TRUE, envir = private$visited_ids)
  }
)
ParseTreeScoper$set(
  "private",
  "scope_symbs",
  function(skip_ids) {
    sym_ids <- private$parse_data_filtered$id[private$parse_data_filtered$token %chin% self$symbol_tokens]
    sym_ids <- setdiff(sym_ids, skip_ids)

    for (id in sym_ids) {
      private$scope_symb(id)
    }

  }
)

ParseTreeScoper$set(
  "private",
  "scope_exprs",
  function() {
    skip_ids <- integer()
    # browser()
    if (any(private$parse_data_filtered$token %in% c("NS_GET", "NS_GET_INT", "'$'",  "RIGHT_ASSIGN", "LEFT_ASSIGN","EQ_ASSIGN"))) {
      for (.id in self$ids) {
        if (private$parse_data_filtered[,.(id, pt = shift(token, 1L, '', "lag"))][id == .id, pt] %in% c("NS_GET", "NS_GET_INT", "'$'","RIGHT_ASSIGN")) {
          skip_ids <- append(skip_ids, .id)
          next
        }
        if (private$parse_data_filtered[,.(id, pt = shift(token, 1L, '', "lead"))][id == .id, pt] %in% c("LEFT_ASSIGN", "EQ_ASSIGN")) {
          private$scope_assignment()
          private$recursive_scope(.id)
          skip_ids <- append(skip_ids, .id)
          next
        }
      }
    }

    terminals <- private$parse_data_filtered$terminal == TRUE

    private$scope_symbs(skip_ids)
    expr_ids <- private$parse_data_filtered$id[!terminals]
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
    assign_filter <- private$parse_data_filtered$token %in% c("LEFT_ASSIGN",
                                                              "RIGHT_ASSIGN",
                                                              "EQ_ASSIGN")
    if (!any(assign_filter)) {
      return()
    }
    assign_rows <- which(assign_filter)
    offsets <- 2L*(private$parse_data_filtered$token[assign_rows] == "RIGHT_ASSIGN") - 1L
    is_double_arrow <- private$parse_data_filtered$text[assign_rows] %in% c("<<-", "->>")
    assigned_rows <- assign_rows + offsets
    assigned_ids <- private$parse_data_filtered$id[assigned_rows]
    assigned_pds <- private$cache[assigned_ids + 1]

    unassigned_rows <- assign_rows - offsets
    unassigned_ids <- private$parse_data_filtered$id[unassigned_rows]
    unassigned_pds <- private$cache[unassigned_ids + 1]

    assign_symbol <- function (pd, id, is_double_arrow) {
      if (identical(pd$token, "SYMBOL")) {
        nm <- pd$text[[1]]

        # Hacky non-accurate workaround for double arrow assignment
        if (is_double_arrow) {
          env <- env_parent(self$envir)
        } else {
          env <- self$envir
        }

        class(id) <- "scoped"

        env_bind(.env = env, !!nm := id)
      }
    }

    walk(unassigned_ids, private$recursive_scope)
    pwalk(list(assigned_pds, assigned_ids, is_double_arrow), assign_symbol)

    # unassigned_ids
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
    enclos <- child_env(self$envir)

    with_self_bindings(
      envir = enclos,

      {
        private$scope_formals()
        private$scope_body()

        el <- as.list(enclos)
        scope_el <- which(vapply(el, inherits, FALSE, "scope"))
        while(length(scope_el) > 0) {
          for(i in scope_el) {
            private$recursive_scope(el[[i]])
            class(enclos[[names(el)[[i]]]]) <- "scoped"
          }
          el <- as.list(enclos)
          scope_el <- which(vapply(el, inherits, FALSE, "scope"))
        }
      }
    )
  }
)

ParseTreeScoper$set(
  "private",
  "scope_symbol",
  function(x) {
    nm <- as.character(x)
    where_env <- tryCatch(where(nm, self$envir), error = function(e) NULL)

    if (is_null(where_env)) {
      return(x)
    }

    if (identical(where_env, private$.envir_initial)) {
      return(x)
    }

    pkg_name <- find_pkg_name(nm, where_env)

    if (is_null(pkg_name)) {
      y <- get(nm, where_env)

      if (inherits(y, "lazy_scope")) {
        class(where_env[[nm]]) <- "scope"
      }

      return(x)
    }

    if (pkg_name == "base") { return(x) }

    if (is_exported(nm, pkg_name)) {
      paste0(pkg_name, "::", nm)
    } else {
      paste0(pkg_name, ":::", nm)
    }
  }
)
