#' @title Render an image with axes overlaid.
#' @keywords internal
#' @noRd
#' @description Add axes and axis ticks back on an image to help a human
#'   review it. Axis lines and tick marks are solid, single-color,
#'   axis-aligned runs of pixels, so they are written directly into the
#'   image's pixel raster (see [retro_ruler_axes()] and
#'   [retro_ruler_tick_lines()]) - the same 1-based pixel indices
#'   [retro_data_panel()] detects land exactly where they are named, with
#'   no coordinate conversion and no anti-aliasing blur. Tick labels are
#'   text, so they still go through a `magick::image_draw()` device (see
#'   [retro_ruler_labels()]): fitting a label to its OCR bounding box (via
#'   [retro_ruler_cex()]) relies on R graphics' own font metrics, which
#'   only exist on an active device.
#' @return `NULL` (invisibly). Called for its side effect of writing
#'   an image file.
#' @param input Character scalar, path to the cleaned or layered image file.
#' @param output Character scalar, path where the ruler image will be
#'   written.
#' @param data_panel A single-row tibble with integer columns `x1`, `x2`,
#'   `y1`, `y2` (pixel bounds of the panel) and `pad_x1`, `pad_y2` (the
#'   y-axis's and x-axis's own measured line-thickness pads), from
#'   [retro_data_panel()].
#' @param data_label A tibble from [retro_do_label()] with columns `label`,
#'   `x`, `y` (centroids), `x1`, `x2`, `y1`, `y2` (bounding box edges).
#'   Used to locate the original text position and infer font size for
#'   each tick label.
#' @param x_axis A tibble with columns `label` (character) and `value`
#'   (numeric), one row per x-axis tick. From
#'   `retro_data_calibrate()$x_axis`.
#' @param y_axis A tibble with columns `label` (character) and `value`
#'   (numeric), one row per y-axis tick. From
#'   `retro_data_calibrate()$y_axis`.
#' @examples
#'   input <- system.file("simulation.png", package = "retroglyph")
#'   panel_file <- tempfile(fileext = ".png")
#'   classified <- tempfile(fileext = ".png")
#'   cleaned <- tempfile(fileext = ".png")
#'   ruler <- tempfile(fileext = ".png")
#'   axes_x <- tibble::tibble(
#'     label = c("F", "M"), value = c(0, 35),
#'     x = c(376, 1154.5), y = c(570, 570),
#'     x1 = c(370L, 1143L), x2 = c(382L, 1166L),
#'     y1 = c(561L, 561L), y2 = c(579L, 579L)
#'   )
#'   axes_y <- tibble::tibble(
#'     label = c("A", "E"), value = c(0.8, 0.2),
#'     x = c(340, 339), y = c(116, 427),
#'     x1 = c(324L, 324L), x2 = c(355L, 354L),
#'     y1 = c(107L, 418L), y2 = c(126L, 436L)
#'   )
#'   data_panel <- retroglyph:::retro_data_panel(
#'     input = source,
#'     x_axis = axes_x,
#'     y_axis = axes_y
#'   )
#'   retroglyph:::retro_image_mask(
#'     input = source,
#'     output = panel_file,
#'     x1 = data_panel$x1 + 5,
#'     x2 = data_panel$x2 - 5,
#'     y1 = data_panel$y1 + 5,
#'     y2 = data_panel$y2 - 5,
#'     mask = TRUE,
#'     initial = FALSE
#'   )
#'   retroglyph:::retro_image_classify(
#'     input = panel_file,
#'     output = classified,
#'     background = "#FFFFFF",
#'     series = c("#DC3030", "#3030A0")
#'   )
#'   retroglyph:::retro_image_clean(
#'     input = classified,
#'     output = cleaned,
#'     panel = data_panel
#'   )
#'   x_axis <- tibble::tibble(label = c("A", "B"), value = c(0, 12))
#'   y_axis <- tibble::tibble(label = c("C", "D"), value = c(0, 1))
#'   data_label <- tibble::tibble(
#'     label = c("A", "B", "C", "D"),
#'     word = c("0", "5", "0.2", "0.4"),
#'     confidence = c(90, 88, 92, 91),
#'     x = c(384, 486, 340, 340),
#'     x1 = c(379, 480, 324, 324),
#'     x2 = c(389, 490, 355, 355),
#'     y = c(573, 573, 324, 428),
#'     y1 = c(561, 561, 314, 418),
#'     y2 = c(580, 580, 334, 438)
#'   )
#'   retroglyph:::retro_image_ruler(
#'     input = cleaned,
#'     output = ruler,
#'     data_panel = data_panel,
#'     data_label = data_label,
#'     x_axis = x_axis,
#'     y_axis = y_axis
#'   )
#'   if (interactive()) {
#'     browseURL(ruler)
#'   }
retro_image_ruler <- function(
  input,
  output,
  data_panel,
  data_label,
  x_axis,
  y_axis
) {
  stopifnot(
    "input must be a single string" = is.character(input) &&
      length(input) == 1L,
    "input must exist" = file.exists(input),
    "output must be a single string" = is.character(output) &&
      length(output) == 1L,
    "x_axis must be a data frame with columns label and value" = is.data.frame(
      x_axis
    ) &&
      all(c("label", "value") %in% names(x_axis)) &&
      nrow(x_axis) >= 2L,
    "y_axis must be a data frame with columns label and value" = is.data.frame(
      y_axis
    ) &&
      all(c("label", "value") %in% names(y_axis)) &&
      nrow(y_axis) >= 2L,
    "data_panel must be a data frame with columns x1, x2, y1, y2, pad_x1, pad_y2" = is.data.frame(
      data_panel
    ) &&
      all(
        c("x1", "x2", "y1", "y2", "pad_x1", "pad_y2") %in% names(data_panel)
      ),
    "data_label must be a data frame with columns label, x, y, x1, x2, y1, y2" = is.data.frame(
      data_label
    ) &&
      all(c("label", "x", "y", "x1", "x2", "y1", "y2") %in% names(data_label))
  )
  edges <- retro_ruler_edges(data_panel)
  tick_length <- retro_ruler_tick_length(data_panel)
  raster <- magick::image_raster(magick::image_read(input), tidy = FALSE)
  color <- retro_ruler_color(raster)
  raster <- retro_ruler_axes(raster, edges, color)
  raster <- retro_ruler_tick_lines(
    raster,
    edges,
    data_label,
    x_axis,
    tick_position = "top",
    tick_length = tick_length,
    color = color
  )
  raster <- retro_ruler_tick_lines(
    raster,
    edges,
    data_label,
    y_axis,
    tick_position = "right",
    tick_length = tick_length,
    color = color
  )
  magick::image_read(raster) |>
    magick::image_write(path = output)
  image <- magick::image_read(output) |>
    magick::image_draw()
  retro_ruler_labels(data_label, x_axis, color)
  retro_ruler_labels(data_label, y_axis, color)
  grDevices::dev.off()
  magick::image_write(image, path = output)
  invisible(NULL)
}

#' @title Extract panel edges as a plain list of integers
#' @keywords internal
#' @noRd
#' @description `data_panel` stores the panel boundary as the detected axis
#'   line positions. This converts its `x1`, `x2`, `y1`, `y2` columns to a
#'   plain integer list for the drawing helpers below.
#' @param data_panel A single-row tibble with columns `x1`, `x2`, `y1`, `y2`.
#' @return A list with integer elements `x1`, `x2`, `y1`, `y2`.
#' @examples
#'   data_panel <- tibble::tibble(x1 = 50L, x2 = 250L, y1 = 30L, y2 = 200L)
#'   edges <- retroglyph:::retro_ruler_edges(data_panel)
#'   print(edges)
retro_ruler_edges <- function(data_panel) {
  list(
    x1 = as.integer(data_panel$x1),
    x2 = as.integer(data_panel$x2),
    y1 = as.integer(data_panel$y1),
    y2 = as.integer(data_panel$y2)
  )
}

#' @title Compute a shared tick-mark length from the panel's own axis pads
#' @keywords internal
#' @noRd
#' @description Tick marks are drawn at a fixed length rather than one
#'   derived from each tick label's OCR bounding box, so that tick length
#'   never depends on label font size or how close a label happens to sit
#'   to the axis. The length reuses measurements [retro_data_panel()]
#'   already made: the y-axis's own pad (`pad_x1`, always positive) and
#'   the x-axis's own pad (`pad_y2`, always negative - `abs()` recovers
#'   its magnitude), taking the larger of the two so a thick axis line on
#'   either side still gets a visibly long tick, then doubling it for a
#'   comfortable visual length. This mirrors the `outward_margin` idea
#'   `retro_data_panel()` already uses internally for an analogous
#'   "how far to extend when there's nothing more specific to measure"
#'   problem.
#' @param data_panel A single-row tibble with columns `pad_x1`, `pad_y2`.
#' @return Integer scalar, the shared tick length in pixels.
#' @examples
#'   data_panel <- tibble::tibble(pad_x1 = 5L, pad_y2 = -3L)
#'   retroglyph:::retro_ruler_tick_length(data_panel)
#'   # 2 * max(5, abs(-3)) = 10
retro_ruler_tick_length <- function(data_panel) {
  2L * as.integer(max(data_panel$pad_x1, abs(data_panel$pad_y2)))
}

#' @title Draw the panel's x-axis and y-axis lines directly into a pixel raster
#' @keywords internal
#' @noRd
#' @description Writes two straight runs of solid color directly into
#'   `raster`: the x-axis (the panel's bottom edge, row `edges$y2`,
#'   columns `edges$x1` to `edges$x2`) and the y-axis (the panel's left
#'   edge, column `edges$x1`, rows `edges$y1` to `edges$y2`). Writing
#'   pixels directly - rather than drawing with `graphics::segments()`
#'   under `magick::image_draw()` - means the same 1-based pixel indices
#'   [retro_data_panel()] detects land exactly where they are named, with
#'   no device-coordinate offset and no anti-aliasing blur.
#' @param raster A native raster matrix, from
#'   `magick::image_raster(image, tidy = FALSE)`.
#' @param edges A list with integer elements `x1`, `x2`, `y1`, `y2`
#'   giving the pixel bounds of the panel's true axis lines, e.g. from
#'   [retro_ruler_edges()].
#' @param color Character scalar, hex color for the axis lines, from
#'   [retro_ruler_color()].
#' @return `raster`, with axis-line pixels set to `color`.
#' @examples
#'   edges <- list(x1 = 5L, x2 = 35L, y1 = 3L, y2 = 17L)
#'   raster <- matrix("#ffffffff", nrow = 20L, ncol = 40L)
#'   raster <- retroglyph:::retro_ruler_axes(raster, edges, color = "#000000")
#'   print(all(raster[17L, 5L:35L] == "#000000ff"))
#'   print(all(raster[3L:17L, 5L] == "#000000ff"))
retro_ruler_axes <- function(raster, edges, color) {
  color <- retro_color_rgba(color)
  raster[edges$y2, edges$x1:edges$x2] <- color
  raster[edges$y1:edges$y2, edges$x1] <- color
  raster
}

#' @title Draw tick-mark line segments directly into a pixel raster
#' @keywords internal
#' @noRd
#' @description For each row of `axis`, finds the matching OCR-detected
#'   label in `data_label` and writes its tick mark straight into
#'   `raster` as a run of `tick_length` solid-color pixels, starting at
#'   the axis edge and running outward (away from the panel) - a
#'   vertical run for `tick_position = "top"` (the x-axis), a horizontal
#'   run for `tick_position = "right"` (the y-axis). The run sits at the
#'   label's own centroid, rounded to the nearest pixel. Unlike text, a
#'   straight, single-color tick mark needs no graphics device: writing
#'   pixel colors directly avoids the coordinate offset that
#'   `magick::image_draw()` (used only for the tick labels; see
#'   [retro_ruler_labels()]) would otherwise require.
#' @param raster A native raster matrix, from
#'   `magick::image_raster(image, tidy = FALSE)`.
#' @param edges A list with integer elements `x1`, `x2`, `y1`, `y2`
#'   giving the pixel bounds of the panel's true axis lines, e.g. from
#'   [retro_ruler_edges()].
#' @param data_label A tibble from [retro_do_label()] with columns
#'   `label`, `x`, `y` (centroids).
#' @param axis A tibble with columns `label` (character) and `value`
#'   (numeric), one row per tick mark, e.g.
#'   `retro_data_calibrate()$x_axis` or `$y_axis`.
#' @param tick_position Character scalar, `"top"` for x-axis ticks
#'   (vertical runs starting at the panel's bottom edge, `edges$y2`,
#'   extending downward) or `"right"` for y-axis ticks (horizontal runs
#'   starting at the panel's left edge, `edges$x1`, extending leftward).
#' @param tick_length Integer scalar, the run length in pixels, from
#'   [retro_ruler_tick_length()].
#' @param color Character scalar, hex color for the tick marks, from
#'   [retro_ruler_color()].
#' @return `raster`, with tick-mark pixels set to `color`.
#' @examples
#'   edges <- list(x1 = 5L, x2 = 35L, y1 = 3L, y2 = 25L)
#'   data_label <- tibble::tibble(
#'     label = c("A", "B"),
#'     x = c(10, 30), y = c(27, 27)
#'   )
#'   axis <- tibble::tibble(label = c("A", "B"), value = c(0, 10))
#'   raster <- matrix("#ffffffff", nrow = 30L, ncol = 40L)
#'   raster <- retroglyph:::retro_ruler_tick_lines(
#'     raster = raster,
#'     edges = edges,
#'     data_label = data_label,
#'     axis = axis,
#'     tick_position = "top",
#'     tick_length = 4L,
#'     color = "#000000"
#'   )
#'   print(all(raster[25L:29L, 10L] == "#000000ff"))
retro_ruler_tick_lines <- function(
  raster,
  edges,
  data_label,
  axis,
  tick_position,
  tick_length,
  color
) {
  axis_edge <- if (tick_position == "top") {
    edges$y2
  } else {
    edges$x1
  }
  color <- retro_color_rgba(color)
  for (i in seq_len(nrow(axis))) {
    label_row <- data_label[data_label$label == axis$label[i], ]
    if (tick_position == "top") {
      column <- as.integer(round(label_row$x))
      rows <- seq.int(axis_edge, min(nrow(raster), axis_edge + tick_length))
      raster[rows, column] <- color
    } else {
      row <- as.integer(round(label_row$y))
      cols <- seq.int(max(1L, axis_edge - tick_length), axis_edge)
      raster[row, cols] <- color
    }
  }
  raster
}

#' @title Draw tick labels onto the active graphics device
#' @keywords internal
#' @noRd
#' @description For each row of `axis`, finds the matching OCR-detected
#'   label in `data_label` and draws its text at its original OCR
#'   centroid, scaled to fit its OCR bounding box (via
#'   [retro_ruler_cex()]). Must be called while a `magick::image_draw()`
#'   device is active, as in [retro_image_ruler()].
#'
#'   Unlike the axis and tick-mark lines (drawn directly into the pixel
#'   raster; see [retro_ruler_axes()] and [retro_ruler_tick_lines()]),
#'   text has to go through `graphics::text()`, since fitting a label to
#'   its bounding box relies on R graphics' own font metrics
#'   (`strheight()`/`strwidth()`, used by [retro_ruler_cex()]), which
#'   only exist on an active device. `magick::image_draw()` opens a
#'   0-based device (confirmed empirically: passing coordinate `v` lands
#'   on 1-based pixel index `v + 1`), so drawing coordinates need a `- 1`
#'   shift to land on the 1-based pixel coordinates `data_label` uses.
#' @param data_label A tibble from [retro_do_label()] with columns
#'   `label`, `word`, `x`, `y` (centroids), and `x1`, `x2`, `y1`, `y2`
#'   (bounding box edges).
#' @param axis A tibble with columns `label` (character) and `value`
#'   (numeric), one row per tick label to draw, e.g.
#'   `retro_data_calibrate()$x_axis` or `$y_axis`.
#' @param color Character scalar, hex color for the label text, from
#'   [retro_ruler_color()].
#' @return `NULL` (invisibly). Called for its side effect of drawing
#'   label text onto the active graphics device.
#' @examples
#'   data_label <- tibble::tibble(
#'     label = c("A", "B"),
#'     word = c("0", "10"),
#'     x = c(10, 30), y = c(27, 27),
#'     x1 = c(8L, 27L), x2 = c(12L, 33L),
#'     y1 = c(26L, 26L), y2 = c(28L, 28L)
#'   )
#'   axis <- tibble::tibble(label = c("A", "B"), value = c(0, 10))
#'   image <- magick::image_read(matrix("#ffffff", nrow = 30, ncol = 40)) |>
#'     magick::image_draw()
#'   retroglyph:::retro_ruler_labels(
#'     data_label = data_label,
#'     axis = axis,
#'     color = "#000000"
#'   )
#'   grDevices::dev.off()
retro_ruler_labels <- function(data_label, axis, color) {
  for (i in seq_len(nrow(axis))) {
    label_row <- data_label[data_label$label == axis$label[i], ]
    box_height <- as.integer(label_row$y2) - as.integer(label_row$y1) + 1L
    box_width <- as.integer(label_row$x2) - as.integer(label_row$x1) + 1L
    graphics::text(
      x = label_row$x - 1,
      y = label_row$y - 1,
      labels = label_row$word,
      cex = retro_ruler_cex(label_row$word, box_width, box_height),
      col = color,
      adj = c(0.5, 0.5)
    )
  }
}

#' @title Choose a font scale that fits a label to its OCR bounding box
#' @keywords internal
#' @noRd
#' @description X-axis and y-axis tick labels are often set in very
#'   different, unpredictable font sizes. Rather than assume a fixed
#'   pixel-per-`cex` ratio, measure the label's own rendered height and
#'   width at `cex = 1` (via `strheight()`/`strwidth()`, which reflect
#'   the actual font metrics of the active graphics device) and scale so
#'   neither dimension overflows the label's detected OCR bounding box.
#' @param word Character scalar, the label text to be drawn.
#' @param box_width Numeric scalar, width of the OCR bounding box, pixels.
#' @param box_height Numeric scalar, height of the OCR bounding box, pixels.
#' @return Numeric scalar, the `cex` to pass to `graphics::text()`.
#' @examples
#'   retroglyph:::retro_ruler_cex(word = "10", box_width = 20, box_height = 12)
retro_ruler_cex <- function(word, box_width, box_height) {
  # magick::image_draw() flips the y user-coordinate system to match PNG
  # (top-left origin) conventions, so strheight() returns a negative
  # value here; abs() recovers the actual glyph height.
  height_cex <- box_height /
    abs(graphics::strheight(word, units = "user", cex = 1))
  width_cex <- box_width /
    abs(graphics::strwidth(word, units = "user", cex = 1))
  min(height_cex, width_cex)
}

#' @title Choose the ruler overlay color with the highest contrast to the
#'   image background
#' @keywords internal
#' @noRd
#' @description The ruler (axis lines, tick marks, tick labels) used to be
#'   drawn in a hardcoded black, which is invisible against a dark
#'   background. Detects the image's actual background color (via
#'   [retro_background_color()], the same detector [retro_image_mask()]
#'   and [retro_data_panel()] use) and returns whichever grayscale
#'   extreme - black or white - has the larger brightness contrast
#'   against it (via [retro_ocr_brightness()]). Checking only the two
#'   extremes is sufficient: no grayscale value between them can sit
#'   farther from the background's brightness than the nearer extreme
#'   already does.
#' @param raster A native raster matrix, from
#'   `magick::image_raster(image, tidy = FALSE)`, read before the ruler
#'   is drawn into it.
#' @return Character scalar hex color, `"#000000"` or `"#ffffff"`.
#' @examples
#'   dark <- matrix("#111111ff", nrow = 4L, ncol = 4L)
#'   retroglyph:::retro_ruler_color(dark)
#'   # "#ffffff" - white contrasts more than black against a dark background
#'   light <- matrix("#eeeeeeff", nrow = 4L, ncol = 4L)
#'   retroglyph:::retro_ruler_color(light)
#'   # "#000000" - black contrasts more than white against a light background
retro_ruler_color <- function(raster) {
  background <- retro_background_color(raster, quantize = FALSE)
  background_brightness <- retro_ocr_brightness(background)
  grayscale <- c("#000000", "#ffffff")
  contrast <- abs(retro_ocr_brightness(grayscale) - background_brightness)
  grayscale[which.max(contrast)]
}
