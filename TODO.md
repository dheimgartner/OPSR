# Todo

> See: https://github.com/dheimgartner/OPSR/issues/42

- [x] Add convergence diagnostics (standard second order conditions) to `summary.opsr()`
but probably in a separate `opsr_diagnostics()` function:
  - [x] Maximum absolute gradient.
  - [x] Whether the Hessian is positive definite.
  - [x] Possibly eigenvalues or conditioning diagnostics (?).
- [x] What simulation exercise did Lokshin perform (see `literature`)?
- [x] Replicate (or extend).
  - [x] What is challenging for identification?
- [x] Implement metrics to provide empirical evidence for identification.
- [ ] Rewrite paper.
  - [ ] Remove [OK] [WARNING] flags from the diagnostics.
  - [ ] Remove --- 1. LR test: ... --- and => `---` from print() for exclusion test object.
  - [ ] Remove `;` in 3.2, 4. (and other sections).
  - [ ] Use 1000 replications in the simulation study!
  - [ ] Make table 2 and 3 conform with the other tables (caption below).
  - [ ] Add "Run" to the table.
  - [ ] Run 3 is omitted from the paper (included in the stored object) to match the numbering of Chiburis and Lokshin (2007). (?)
  - [ ] Exclusion restriction is justified twice => remove one.
  - [ ] The hessian is not negative definite in the case study! => check specification in issue again.
  - [ ] The LR-test does not reject the null...
  - [ ] Why is Figure 2 truncated? shouln't the curve span [-0.99, 0.99]?
  - [ ] Case study needs a rewrite (after the final specification has been identified).
- [ ] Make sure man is in sync.
  - [ ] Is the `simulation_study` dataset documented?
- [ ] Update on CRAN.
- [ ] Resubmit: When you have uploaded Revisions for all required files above 
(including your point-to-point response), you need to inform the editors that 
your revision is complete. To do so click the Add discussion button below, using 
the subject "Resubmission" and adding all involved section editors and journal 
editors (if any) as participants.
