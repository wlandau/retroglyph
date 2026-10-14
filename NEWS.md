# retroglyph development version

* Reconstructed censoring times now land on the censoring tick marks the figure draws, instead of at the even spacing `IPDfromKM` invents. The algorithm by Guyot et al. (2012) recovers how many patients were censored in each risk table interval but nothing about where, and `IPDfromKM::getIPD()` spreads them uniformly; a tick mark says exactly where, and `retro_data_censor()` has been detecting them with nothing downstream reading the column. Neither `IPDfromKM::preprocess()` nor `getIPD()` accepts observed censoring times, and `preprocess()` prunes tied-time rows, so a per-point flag cannot reach the reconstruction that way. What `getIPD()` does respond to is the risk table: an interval narrow enough to hold one censoring leaves it nowhere else to go. So `retro_survival_series()` now reconstructs twice - once conventionally, then again on the same trace with a risk table augmented by a bracketing pair of entries at each censoring's assigned tick position. The number at risk at each added entry is walked down from the interval's own starting count by the events the trace shows and the censorings assigned, rather than read off the first pass, whose uniformly spread censoring times are the very thing being replaced; the counts the model read off the figure are never moved. The risk table alone decides how many patients were censored, by elimination against the first-pass events, so the number of ticks never enters the arithmetic and a series whose ticks are partly hidden by overplotting still reconstructs the full count. A series with no detected ticks augments to the risk table it started with, so the second pass reproduces the first exactly. The conventional single-pass reconstruction is now `retro_survival_reconstruct()`, called once per pass, so both passes are provably the same computation on different inputs. Two limitations are inherited from tick detection and surface in output for the first time: `retro_censor_centers()` is still stubbed to always report two groups, so a figure drawing no ticks has its tallest columns flagged anyway, and a vertical step drop is flagged at its outer columns.
* New `retro_data_censor()` detects censoring tick marks in the distilled image and labels which pixels of each traced curve sit under one, in a new logical `censored` column that `retro_do_data()` returns as `censoring` and carries through `retro_data_scale()`. A tick is drawn in the series' own color, so its ink belongs to the series but not to the thin medial-axis trace `retro_data_path()` returns: per pixel column, the ink's longest contiguous vertical run less the trace's own gives an excess height - only line width for an ordinary column, the tick's full height where one is drawn. Runs rather than outright ranges, so that a detached speck of the same color elsewhere in the column cannot stretch the measurement across the gap and read as a tick. `Ckmeans.1d.dp` clusters those heights rather than `stats::kmeans()`, since univariate k-means solves exactly by dynamic programming and reported censoring times have to reproduce run to run. Two limitations remain: `retro_censor_centers()` is stubbed to always answer two groups, so a figure drawing no ticks has its tallest columns mislabeled, and a vertical step drop is flagged at its outer columns, where the trace is thin but the ink runs the whole length of the drop. Both now reach the reconstructed censoring times, since `retro_survival_series()` reads the column.
* `retro_color_pixels()` is now the single definition of "the image's pixels" - the raster, the hex color matrix, and the detected background in one call - and `retro_data_path()` and `retro_data_panel()` both read through it instead of repeating the same four lines. `retro_data_censor()` compares the colors it finds against the `color` column `retro_data_path()` recorded, so a different normalization at either end would measure every excess height against the wrong ink. Sharing the read makes that agreement structural rather than a convention. Behavior at both existing sites is unchanged.
* A full risk table is no longer required (#2). `retroglyph` now asks only for at least one number at risk per data series, read off the figure or supplied by the user, mixing the two freely. They are `(count, series, time)` readings and no shape is special-cased, so totals, a starting and ending sample size, or any mix of time points across series are all ordinary input. This needed no change to the reconstruction itself, only to the prompting and documentation that had described a risk table as mandatory.
* `retro_agent_class$compare()` and `$export()` accept `data = "censoring"`, and `retro_app()` offers it as a third choice alongside the reconstructed survival data and the recaptured pixel trace (#3). It refits the same Kaplan-Meier curves as `data = "survival"` but draws them one pixel wide with a vertical tick at every reconstructed censoring time. The thinning is what makes the ticks checkable: a tick a couple of pixels tall disappears inside a line dilated to the source figure's own line width.
* Censoring tick marks in `compare(data = "censoring")` and `export(data = "censoring")` are now sized relative to the canvas, at 2% of the image height, instead of a fixed 2 pixels up and down. A 5-pixel tick is legible on a small canvas but a speck on a large one - on the README figure, 700 pixels tall, it was under 1% of the height and hard to check against the source. The fixed 2 pixels remains the floor, so a small canvas is unaffected.
* `retro_image_layer_trace()` and `retro_image_layer_survival()` now dilate a curve to `(line_width - 1) %/% 2` pixels on either side of its center, instead of `floor(line_width / 2)`. The dilation is symmetric about the center pixel, so the drawn thickness is always `2 * half_width + 1` and only odd thicknesses are representable. The old mapping rounded an even measurement up, rendering every even-width source curve one pixel too thick - the README figure's curve measures 4 pixels and was reconstructed at 5. The new mapping rounds down, so the reconstruction errs thin rather than thick, and odd widths are unaffected.

# retroglyph 0.0.9

* `retro_color_rgb()` and `retro_color_rgba()` now parse each color with `grDevices::col2rgb()` and re-format it with `grDevices::rgb()`, instead of slicing and concatenating the string. `magick::image_raster()` returns color specifications rather than guaranteed hex, and for a fully transparent pixel it returns the name `"transparent"`, which the old string handling turned into the invalid `"transpa"` and `"transpaff"`. The 0.0.8 fix addressed this by flattening at the entry point, but several internal steps build images from character matrices and never pass through `retro_image_png()`, so the error persisted on platforms whose `ImageMagick` build retains an alpha channel. Fixing the helpers themselves covers every path. Output is unchanged for hex input.
* Clarify in `retro_image_png()` that flattening onto white is what makes a partially transparent source image tractable: the blend a reader sees is the color to digitize, so compositing is not interchangeable with discarding the alpha channel.

# retroglyph 0.0.8

* `retro_image_png()` now flattens the source image onto an opaque white background, and `retro_agent_class$register()` routes every format through it instead of copying PNG files verbatim. `magick::image_raster()` reports a fully transparent pixel as the color name `"transparent"` rather than as hex, which `retro_color_rgb()` truncated to `"transpa"` and `retro_color_rgba()` extended to `"transpaff"`, both invalid color names that stopped the pipeline. Removing the alpha channel once at the entry point means no later step encounters a non-hex pixel value.

# retroglyph 0.0.7

* Collapse tool call cards in `retro_app()` by default instead of expanding them.
* Top-align and left-align the Step 2 greeting in `retro_app()` instead of centering it vertically.

# retroglyph 0.0.6

* Fix the cancel button in `retro_app()`: the chat turn now runs in a `shiny::ExtendedTask`, so a cancel click can actually reach `stream_controller()` and stop the stream instead of being processed only after the response already finished. Since the tools now run outside a reactive context, they read `state` via `shiny::isolate()`.
* `retro_chat_replay_simulation()$stream_async()` now returns a promise instead of the finished text. A finished string is not a stream, so `retro_app()` rendered the replay as an already-complete message and the stop button never appeared.
* Give `retro_app()` a `theme` argument for `bslib` theming, and put a light/dark mode toggle in the top right of the main panel.
* Add a preload hint for `shinychat`'s JS and CSS bundle in `retro_app()`, so the browser starts fetching it in parallel with other blocking `<script>` tags instead of only after they finish, and the greeting renders sooner.
* Improve the error messages of the quantize and label tools so the model knows when the user forgot to supply a source image.

# retroglyph 0.0.5

move to shinychat 0.5.0.

# retroglyph 0.0.4

* Use a simulated Kaplan-Meier image for examples and tests.
* Reduce content returned from tools, especially `retro_tool_label()`. Mostly this is a workaround to avoid <https://github.com/tidyverse/ellmer/issues/1136>, but it also somewhat declutters the context window. 
* Add a `type` argument to `quantiles()` (`"survival"` default, or `"incidence"`) controlling how `probabilities` is interpreted, default `probabilities` to descending (`c(0.75, 0.5, 0.25)`), and always report both `survival` and `incidence` columns in the output, sorted chronologically regardless of input order or `type`.
* Add new methods `probabilities()` and `counts()` to the agent class.
* Add `retro_app()`, a Shiny app interface for `retroglyph`.

# retroglyph 0.0.3

* Make `retro_data_path()` more robust to poorly classified anti-aliasing debris: before
  choosing a curve's rightmost pixel, drop connected components of the
  target color smaller than 8 times the line width.

# retroglyph 0.0.2

* Document AI Registry and Cyber SAE approvals.

# retroglyph 0.0.1

* First version.
