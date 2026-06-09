# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What This Package Does

OPSR is an R package implementing **Ordered Probit Switching Regression** — a Heckman-type selection model where the treatment/selection variable is ordinal (not binary) and outcomes are continuous. Each ordinal regime gets its own outcome equation. The motivating application is telework frequency (ordinal treatment) and vehicle miles driven (continuous outcome).

## Common Commands

```r
# Install dependencies
make install_deps

# Build and check (CRAN-style)
make check

# Install locally
make install

# Clean build artifacts
make clean
```

```r
# Inside an R session — recompile C++ and reload:
devtools::load_all()

# Regenerate documentation from roxygen2 comments:
devtools::document()

# Run all tests:
devtools::test()

# Run a single test file:
testthat::test_file("tests/testthat/test-opsr.R")

# Run one named test:
testthat::test_file("tests/testthat/test-opsr.R", filter = "basic model converges")

# Check package without rebuilding:
devtools::check()
```

## Architecture

### Estimation flow

```
opsr()           — formula interface (R/opsr.R)
  └─ opsr_2step()  — Heckman 2-step for starting values (R/opsr_2step.R)
  └─ opsr.fit()    — calls maxLik::maxLik (R/opsr_fit.R)
       └─ loglik_cpp()  — C++ log-likelihood, parallelised with OpenMP (src/loglik.cpp)
            └─ loglik_j()   — per-regime contribution (src/loglik.cpp)
            └─ opsr_prepare_coefs()  — unpacks named theta vector into per-regime structs (src/utils.cpp)
```

The **parameter vector `theta`** is named and structured as:
- `kappa1`, `kappa2`, … — thresholds of the ordered probit selection model
- `s_<varname>` — selection equation coefficients (`gamma`)
- `o<j>_<varname>` — outcome equation coefficients for regime `j` (`beta_j`)
- `sigma<j>` — outcome equation error std dev for regime `j`
- `rho<j>` — error correlation between selection and outcome for regime `j`

The C++ code parses these name prefixes directly (`opsr_prepare_coefs` in `src/utils.cpp`). Renaming parameter conventions breaks the C++ parser.

### Formula convention

```r
ys | yo ~ selection_terms | outcome_terms_regime1 | outcome_terms_regime2 | ...
```

- `ys` — integer ordinal selection variable (must start at 1, no gaps)
- `yo` — continuous outcome
- First RHS part — selection equation (intercept excluded automatically for identification)
- Subsequent RHS parts — one per regime, or a single shared specification

Parsing uses the `Formula` package; `model.part()` extracts each piece.

### Key R files

| File | Purpose |
|---|---|
| `R/opsr.R` | User-facing `opsr()`: validates formula, builds `W`/`X`/`Y` lists, calls `opsr.fit()`, attaches metadata to fit object |
| `R/opsr_fit.R` | `opsr.fit()`: thin wrapper around `maxLik::maxLik` with the C++ log-likelihood |
| `R/opsr_2step.R` | Heckman 2-step starting values via `MASS::polr` (selection) + OLS with Mills ratio (outcome) |
| `R/predict.R` | `predict.opsr()`: returns response, prob, mills ratio, Heckman correction, or `Xb` |
| `R/opsr_te.R` | `opsr_te()` + `summary.opsr.te()`: average treatment effects via counterfactual prediction |
| `R/opsr_null_model.R` | Null model (no correction) for pseudo-R² computation |
| `R/opsr_step.R` | Stepwise variable selection wrapper |
| `R/anova.R` | `anova.opsr()` — likelihood ratio tests between nested models |
| `R/texreg.R` | `extract.opsr()` for `texreg`/`screenreg` table export |
| `R/summary_opsr.R` | `summary.opsr()`: Wald tests, pseudo-R², runtime, regime counts |
| `R/utils.R` | Internal helpers: `get_z`, `get_y`, `censor`, `fitted.opsr`, `residuals.opsr`, `r2` |

### C++ layer (`src/`)

- `loglik.cpp` / `loglik.h` — per-regime log-likelihood `loglik_j()`; `loglik_cpp()` is the exported entry point used by `opsr.fit()`
- `utils.cpp` / `utils.h` — `opsr_prepare_coefs()` (parses named theta), `make_theta_array()`, `opsr_check_omp()`, `opsr_max_threads()`
- OpenMP parallelism: each regime's likelihood is computed in a separate thread. Do not pass `nThreads > nReg`.
- `src/Makevars` links against LAPACK/BLAS/FLIBS and OpenMP via `SHLIB_OPENMP_CXXFLAGS`.

### Tests (`tests/testthat/`)

Fixtures are pre-simulated datasets in `tests/testthat/fixtures/` (loaded via `load_sim_dat()` in `helper.R`). The helper also exports `sim4()` for generating 4-regime data. Tests cover: convergence, parameter recovery (within tolerance 1e-1), formula variants, treatment effects, texreg extraction, anova, and predict types.

### Data

- `data/telework_data.rda` — real survey data (telework frequency + VMD), the main example dataset
- `data/timeuse_data.rda` — secondary example dataset
- `data-raw/` — scripts that produced the `.rda` files

### Development scripts

`dev/` contains exploratory/scratch scripts (not part of the package). `dev.R` is the main development scratchpad. `R/examples/` holds runnable `\example{}` scripts referenced from roxygen docs.
