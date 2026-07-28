## Summary

This is a resubmission. flametree 0.1.3 is the version currently on CRAN
(published 2021-11-29); this release (0.2.0) adds new functionality
(reintroduces `prune` support for `flametree_grow()`), tightens input
validation across several functions, and includes a number of documentation
fixes. See NEWS.md for the full list of user-facing changes.

## Test environments

* local: Ubuntu 24.04, R 4.6.1
* GitHub Actions:
  - macOS-latest, R release
  - windows-latest, R release
  - ubuntu-latest, R devel
  - ubuntu-latest, R release
  - ubuntu-latest, R oldrel-1
* win-builder (devel and release) [TODO: run `devtools::check_win_devel()` /
  `check_win_release()` before submission and paste results here]
* R-hub v2 [TODO: run `rhub::rhub_check()` before submission and paste
  results here]

## R CMD check results

0 errors | 0 warnings | 0 notes

## Downstream dependencies

flametree has no reverse dependencies on CRAN.
