# flametree 0.1.3.9000

* `ft__check_colour()` now validates that `background` and `palette` inputs to `flametree_plot()` are colours that R actually recognises (via `grDevices::col2rgb()`), rather than only checking they are character vectors. Invalid colours (e.g. `"##f6b6a6"`) now throw an informative error instead of failing deep inside grid graphics code (#17).
* `flametree_grow()` now warns when the requested `time`, `split`, and `trees` combination is expected to produce a very large data frame (row count grows as `trees * split ^ time`), since this was the most common cause of "cannot allocate vector" memory errors (#18).

# flametree 0.1.3

* Removes unnecessary dependency on paletteer.
* Changes maintainer email to one I'll have long term control over.
* Added a `NEWS.md` file to track changes to the package.
