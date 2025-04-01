.onAttach <- function(libname, pkgname) {
  please_cite <- capture.output(print(citation(pkgname)))
  please_cite <- paste(please_cite, collapse = "\n")
  packageStartupMessage(
    please_cite,
    comain = NULL, appendLF = TRUE
  )
}
