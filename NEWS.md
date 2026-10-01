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
