#' pryr::where
where <- function (name, env = parent.frame())
{
  stopifnot(is.character(name), length(name) == 1)
  env <- to_env(env)
  if (identical(env, emptyenv())) {
    stop("Can't find ", name, call. = FALSE)
  }
  if (exists(name, env, inherits = FALSE)) {
    env
  }
  else {
    where(name, parent.env(env))
  }
}

#' pryr::to_env
to_env <- function (x, quiet = FALSE)
{
  if (is.environment(x)) {
    x
  }
  else if (is.list(x)) {
    list2env(x)
  }
  else if (is.function(x)) {
    environment(x)
  }
  else if (length(x) == 1 && is.character(x)) {
    if (!quiet)
      message("Using environment ", x)
    as.environment(x)
  }
  else if (length(x) == 1 && is.numeric(x) && x > 0) {
    if (!quiet)
      message("Using environment ", search()[x])
    as.environment(x)
  }
  else {
    stop("Input can not be coerced to an environment",
         call. = FALSE)
  }
}
