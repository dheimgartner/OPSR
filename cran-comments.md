## Update

* Minor updates for current CRAN release OPSR 0.1.2 (in particular adding paper vignette)
* Changes are documented in NEWS.md

## R CMD check results

I am using 'packageStartupMessage'. The 'print' call (formatting the citation) is wrapped in
'utils::capture.output'.

0 errors | 1 warnings | 3 notes

❯ checking compiled code ... OK
   WARNING
  ‘qpdf’ is needed for checks on size reduction of PDFs

❯ checking for future file timestamps ... NOTE
  unable to verify current time

❯ checking R code for possible problems ... NOTE
  File ‘OPSR/R/zzz.R’:
    .onAttach calls:
      print(utils::citation(pkgname))
  
  Package startup functions should use ‘packageStartupMessage’ to
    generate messages.
  See section ‘Good practice’ in '?.onAttach'.

❯ checking HTML version of manual ... NOTE
  Skipping checking HTML validation: no command 'tidy' found

## Revdeps

No reverse dependencies found
