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
