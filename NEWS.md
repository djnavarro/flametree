# flametree 0.1.3.9000

* Sets up spell checking for the package documentation, using the `spelling` package. Sets `Language: en-GB` in `DESCRIPTION` (the package consistently uses British spellings such as "colour" and "visualise"), and adds an `inst/WORDLIST` of accepted technical terms. Along the way, fixes a handful of genuine typos found by the spell check: "tesselation" &rarr; "tessellation", "wdith" &rarr; "width", and "abranching" &rarr; "a branching" (in `flametree_plot()` and `flametree_grow()`'s documentation).
* Further documentation clarity pass: fixes a copy-paste error in the `@param y` description for the `sparks` functions (previously said "horizontal" for both `x` and `y`); removes a stray backtick in `flametree_grow()`'s `@return` docs; documents that `flametree_plot(style = "themegray")` also accepts the British spelling `"themegrey"`; updates a stale comment in `checks.R` that referred to `prune` as removed, when it has since been reintroduced; and, in the vignettes, mentions the `prune` spark argument (previously omitted) and fixes a couple more typos ("wheras", a missing article, and another instance of "tesselation") that weren't caught by the spell check because vignettes are currently excluded from it.
* Rewords the `Description` field in `DESCRIPTION`: the growth algorithm is a stochastic branching process, not a formal L-system (which implies a deterministic rewriting grammar), so the earlier description was technically misleading. Also fixes a grammatical mismatch in the same field ("generating the data structures and visualise them" &rarr; "...and visualising them").
* `ft__check_colour()` now validates that the `background` and `palette` arguments to `flametree_plot()` are colours that R actually recognises (via `grDevices::col2rgb()`), rather than only checking that they are character vectors. Invalid colours (e.g. `"##f6b6a6"`) now throw an informative error from `flametree_plot()` itself, rather than failing deep inside grid graphics code (#17).
* `flametree_grow()` now warns when the requested `time`, `split`, and `trees` combination is expected to produce a very large data frame (row count grows as `trees * split ^ time`), since this was the most common cause of "cannot allocate vector" memory errors (#18).
* Reintroduces `prune` support, which had been removed prior to the 0.1.3 release. `flametree_grow()` gains a `prune` argument: a spark function (like `seg_col`, `seg_wid`, `shift_x`, and `shift_y`) that controls the probability that a given shoot stops growing early. The default, `spark_nothing()`, disables pruning and reproduces prior behaviour exactly. Pruned shoots keep the segment they already grew, and that segment is marked as a leaf (`id_leaf`), but it produces no further descendants. Also relaxes `split` to allow a value of 1, which disables branching altogether and produces a single winding path per tree (#19).
* The `trees` argument to `flametree_grow()` is now validated (it previously had no checks at all, so invalid input such as `trees = 2.5` or `trees = -1` could silently misbehave or fail with a cryptic low-level error).
* `flametree_save()` now checks that `plot` is a ggplot object. Previously, passing something other than a ggplot object (including `NULL`) either failed with a cryptic error or, in the case of `NULL`, silently saved an unrelated plot via `ggplot2::last_plot()` instead of erroring.

# flametree 0.1.3

* Removes an unnecessary dependency on the paletteer package.
* Changes the maintainer email address to one under long-term personal control, rather than a university address.
* Adds this `NEWS.md` file to track user-facing changes to the package going forward.

# flametree 0.1.2

* Tightens validation of the arguments to `flametree_grow()` and `flametree_plot()`.
* Updates and extends the documentation for `flametree_grow()` and the `sparks` functions, and lightly reworks the "voronoi" style documentation.
* Adds a Voronoi-style example image to the README, and removes an unused vignette.

# flametree 0.1.1

* Fixes a bug in the "wisp" style where segments were grouped by `id_path` instead of `id_pathtree`, causing segments from different trees that happened to share an `id_path` value to be incorrectly joined together (#14).

# flametree 0.1

* First CRAN release.
* Consolidates what had been several separate, inconsistent plotting systems (`ashtree`, `wisptree`, `voronoitree`, and the original `flametree` functions) into the single, unified interface that the package still uses: `flametree_grow()` to generate the underlying data, and `flametree_plot()` to render it in a chosen `style` ("plain", "minimal", "themegray", "voronoi", "wisp", or "nativeflora").
* Introduces the "spark function" system (`spark_linear()`, `spark_decay()`, `spark_random()`, `spark_nothing()`) for controlling segment colour, segment width, and horizontal/vertical displacement during growth.
* Adds `id_leaf` to the data structure returned by `flametree_grow()`, identifying terminal segments.
* Renames all unexported functions to use a consistent `ft__` prefix, and adds an initial test suite and package documentation site (via pkgdown).

# flametree 0.0.1

* Early development version, predating the unified `flametree_grow()`/`flametree_plot()` interface introduced in 0.1. Establishes the core data structure for representing a grown tree, and simplifies the internal API and default parameter values.
