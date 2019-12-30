# Make sure data.table knows we know we're using it
.datatable.aware = TRUE


.onLoad <- function(libname, pkgname) {

  packr.opts = list(packr.pipe = TRUE,
                    packr.dt = FALSE)
  toset <- !names(packr.opts) %in% names(options())
  if(any(toset)) options(packr.opts[toset])
}
