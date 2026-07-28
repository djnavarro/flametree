## Summary

This is a resubmission. flametree 0.1.3 is the version currently on CRAN
(published 2021-11-29); this release (0.2.0) adds new functionality
(reintroduces `prune` support for `flametree_grow()`), tightens input
validation across several functions, and includes a number of documentation
fixes. See NEWS.md for the full list of user-facing changes.

A previous submission of this same 0.2.0 tarball was rejected by CRAN's
automatic incoming pretest for a long-running-examples NOTE on
`flametree_save()`. That has been fixed (see below) and reconfirmed across
all test environments; this is the corrected resubmission of 0.2.0.

## Test environments

* local: Ubuntu 24.04, R 4.6.1
* GitHub Actions:
  - macOS-latest, R release
  - windows-latest, R release
  - ubuntu-latest, R devel
  - ubuntu-latest, R release
  - ubuntu-latest, R oldrel-1
* win-builder:
  - R-release (Windows Server 2022 x64) — checked prior to the fix below;
    was already clean
  - R-devel (Windows Server 2022 x64) — reconfirmed clean after the fix
* R-hub v2 (via GitHub Actions), reconfirmed clean after the fix:
  - ubuntu-release (R-devel, r-release-linux-x86_64)
  - ubuntu-gcc12 (R-devel, Debian/Ubuntu gcc)
  - macos (R-devel)
  - windows (R-devel)
  - nosuggests (R-devel, Fedora, Suggests packages not installed)

## R CMD check results

0 errors | 0 warnings | 0 notes on all platforms above.

Previous attempt: CRAN's incoming pretest flagged "Examples with CPU (user
+ system) or elapsed time > 10s" for `flametree_save`, caused by its
examples building and rendering two separate five-tree plots. Fixed by
reusing a single plot across both example calls and reducing `trees`/
`time`; example runtime is now well under a second.

## Downstream dependencies

flametree has no reverse dependencies on CRAN.
