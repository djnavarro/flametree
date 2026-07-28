# Changelog

## flametree 0.1.3.9000

- `ft__check_colour()` now validates that `background` and `palette`
  inputs to
  [`flametree_plot()`](https://flametree.djnavarro.net/reference/flametree_plot.md)
  are colours that R actually recognises (via
  [`grDevices::col2rgb()`](https://rdrr.io/r/grDevices/col2rgb.html)),
  rather than only checking they are character vectors. Invalid colours
  (e.g. `"##f6b6a6"`) now throw an informative error instead of failing
  deep inside grid graphics code
  ([\#17](https://github.com/djnavarro/flametree/issues/17)).
- [`flametree_grow()`](https://flametree.djnavarro.net/reference/flametree_grow.md)
  now warns when the requested `time`, `split`, and `trees` combination
  is expected to produce a very large data frame (row count grows as
  `trees * split ^ time`), since this was the most common cause of
  “cannot allocate vector” memory errors
  ([\#18](https://github.com/djnavarro/flametree/issues/18)).
- Reintroduces `prune` support, removed prior to the 0.1.3 release.
  [`flametree_grow()`](https://flametree.djnavarro.net/reference/flametree_grow.md)
  gains a `prune` argument, a spark function (like `seg_col`, `seg_wid`,
  `shift_x`, and `shift_y`) that controls the probability that a given
  shoot stops growing early. The default,
  [`spark_nothing()`](https://flametree.djnavarro.net/reference/sparks.md),
  disables pruning and reproduces prior behaviour exactly. Pruned shoots
  keep the segment they already grew and are marked as leaves
  (`id_leaf`). Also relaxes `split` to allow a value of 1, which
  disables branching altogether and produces a single winding path per
  tree ([\#19](https://github.com/djnavarro/flametree/issues/19)).

## flametree 0.1.3

CRAN release: 2021-11-29

- Removes unnecessary dependency on paletteer.
- Changes maintainer email to one I’ll have long term control over.
- Added a `NEWS.md` file to track changes to the package.
