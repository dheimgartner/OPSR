# OPSR (development version)

* `print.summary.opsr()` now shows GOF indicators.
* Added `timeuse_data` from the TimeUse+ study.
* `extract.opsr()`: R2 for each regime in `texreg` functions (and don't repeat GOFs if `beside = TRUE`).
* Nicer summary printing and setting fixed values in null model to `NA`.
* Added `.loglik` arg to `opsr()` to return log-likelihood vector.
* Detects and warns on singularity issues.
* Resolved memory leak.
* Corrected outcome-specific R2 computations.
* Added `type = "Xb"` in `predict.opsr()`.
* Works for only 2 treatment regimes.

# OPSR 0.1.2

* Patch with minor updates on documentation.

# OPSR 0.1.1

* Patch with missing links.

# OPSR 0.1.0

* Initial CRAN submission.
