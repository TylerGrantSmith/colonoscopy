# Make sure data.table knows we know we're using it
.datatable.aware = TRUE

# Prevent R CMD check from complaining about the use of pipe expressions
# standard data.table variables
if (getRversion() >= "2.15.1")
  utils::globalVariables(c(".", ".I", ".N", ".SD"), utils::packageName())

globalVariables(c(".global_packr_env"), "packr") #ignore this function in R CMD checks, since it is part of RStudio runtime

.global_packr_env <- new.env(parent = empty_env())
.global_packr_env$skip_envs = list(empty_env(),
                                   global_env(),
                                   base_env(),
                                   getNamespace("base"))
.global_packr_env$exec_env <- new.env(parent = empty_env())
.global_packr_env$assignment_symbols  <- c("<-", "=", "<<-")
.global_packr_env$ns_access_symbols   <- c("::",":::")
.global_packr_env$list_access_symbols <- c("$")
.global_packr_env$pipe_symbols        <- c("%>%", "%T>%", "%<>%", "%$%")
.global_packr_env$dt_symbols          <- c(":=")
.global_packr_env$pattern_start       <- "[a-zA-Z._0-9:]+$"
.global_packr_env$pattern_end         <- "^[a-zA-Z._0-9:]+"
.global_packr_env$context             <- list(type         = NULL,
                                              id           = NULL,
                                              row          = NULL,
                                              buffer       = NULL,
                                              cursor_pos   = NULL,
                                              cursor_start = NULL,
                                              cursor_end   = NULL)
dep_env <- new_environment()
