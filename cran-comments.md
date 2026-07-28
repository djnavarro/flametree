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
* win-builder:
  - R-release (Windows Server 2022 x64)
  - R-devel (Windows Server 2022 x64)
* R-hub v2 (via GitHub Actions):
  - ubuntu-release (R-devel, r-release-linux-x86_64)
  - ubuntu-gcc12 (R-devel, Debian/Ubuntu gcc)
  - macos (R-devel)
  - windows (R-devel)
  - nosuggests (R-devel, Fedora, Suggests packages not installed)

## R CMD check results

0 errors | 0 warnings | 0 notes on all platforms checked (local,
GitHub Actions CI, R-hub, and win-builder release/devel).

Note: an earlier build of 0.2.0 triggered a NOTE from CRAN's automatic
incoming pretest ("Examples with CPU (user + system) or elapsed time >
10s", for `flametree_save`). This was caused by that function's examples
building and rendering two separate five-tree plots. Fixed by reusing a
single plot across both example calls and using smaller `trees`/`time`
values; example runtime is now well under a second. Reconfirmed via a
fresh win-builder run after the fix.

## Downstream dependencies

flametree has no reverse dependencies on CRAN.
