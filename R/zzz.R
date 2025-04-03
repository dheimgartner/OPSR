.onAttach <- function(libname, pkgname) {
  please_cite <- utils::capture.output(print(utils::citation(pkgname)))
  please_cite <- paste(please_cite, collapse = "\n")
  packageStartupMessage(
    please_cite,
    comain = NULL, appendLF = TRUE
  )
}
