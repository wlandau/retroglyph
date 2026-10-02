#' @title Detect plotting panel lines and boundaries
#' @keywords internal
#' @noRd
#' @description Detect the x-axis (bottom) and y-axis (left) lines
#'   in a Kaplan-Meier image, then look for a top and a right bounding
#'   line as well - the top and right edges of a plotting-area box, when
#'   the source image draws one. Two labeled calibration ticks per axis
#'   (pixel position plus data value) establish where to search: the
#'   y-axis line sits just to the right of its tick labels' right edges,
#'   and the x-axis line sits just above its tick labels' top edges. The
#'   function dichotomizes the image to a logical matrix and searches a
#'   narrow one-sided band extending from those tick-label edges toward
#'   the expected axis line for the long horizontal/vertical run of
#'   foreground pixels that represents each axis line. The x-axis and
#'   y-axis lines are required - not finding one is an error (see
#'   [retro_panel_position_y_axis()], [retro_panel_position_x_axis()]).
#'   The top and right lines are optional: many Kaplan-Meier images draw
#'   only the two required axes with no enclosing box, so not finding a
#'   top or right line is a normal outcome, not an error (see
#'   [retro_panel_position_top()], [retro_panel_position_right()]).
#' @details Coordinates follow PNG conventions: (1, 1) is the top-left
#'   corner. In the returned tibble, `x1` is the left column, `x2` is
#'   the right column, `y1` is the top row, and `y2` is the bottom row.
#'   Each lands exactly on its detected line: `x1`/`y2` on the required
#'   y-axis/x-axis lines, and `x2`/`y1` on the optional right/top lines
#'   if found, or on the y-axis/x-axis line's own right/top extent
#'   otherwise. None of the four are padded - padding is reported
#'   separately, in `pad_x1`, `pad_x2`, `pad_y1`, `pad_y2`, so a caller
#'   that wants the true line positions (e.g. [retro_image_ruler()], to
#'   redraw them for human review) can use `x1`/`x2`/`y1`/`y2` directly,
#'   while a caller that wants only the data (e.g. [retro_do_distill()])
#'   adds the matching pad to each edge.
#'
#'   Each edge pads independently, off its own detected line's own
#'   thickness, measured separately (see [retro_panel_line_pad()])
#'   rather than one pad shared across all four edges. The required
#'   y-axis and x-axis lines always pad inward (toward the panel
#'   center), since they are always found. The optional top and right
#'   lines pad inward, for the same reason, when found - but pad
#'   outward (away from the panel center) when not found: with no
#'   detected line to clear, there is nothing to pad inward from, and
#'   inward padding would then only risk clipping data that sits flush
#'   with that edge. The outward pad in that case is the larger of the
#'   y-axis's and x-axis's own measured pads (reusing measurements
#'   already made, rather than a new font-size-dependent guess),
#'   clipped so it never pads past the edge of the image.
#'
#'   The search band width is `ceiling(window * median_height)`,
#'   where `median_height` is the median bounding-box height across all
#'   four calibration ticks (a pixel-scale proxy for one character's
#'   height in this image). `window` is not intended to be tuned per
#'   image - it is a fixed default meant to work across most images, only
#'   adjusted if evidence from many images suggests otherwise. The same
#'   width sizes the top/right search bands, one-sided from the
#'   y-axis/x-axis line's own extent outward toward the image edge.
#' @return A single-row tibble with columns:
#'   * `x1` — Integer, left column of the panel (1-based); the detected
#'     y-axis line.
#'   * `x2` — Integer, right column of the panel (1-based); the detected
#'     right bounding line, if found, else the x-axis line's own
#'     rightmost detected pixel.
#'   * `y1` — Integer, top row of the panel (1-based); the detected top
#'     bounding line, if found, else the y-axis line's own topmost
#'     detected pixel.
#'   * `y2` — Integer, bottom row of the panel (1-based); the detected
#'     x-axis line.
#'   * `pad_x1` — Integer, always positive: pixels to add to `x1` to
#'     clear the y-axis line's own thickness (moves right, inward).
#'   * `pad_x2` — Integer, signed: negative (moves left, inward) when a
#'     right bounding line was found; positive (moves right, outward)
#'     otherwise.
#'   * `pad_y1` — Integer, signed: positive (moves down, inward) when a
#'     top bounding line was found; negative (moves up, outward)
#'     otherwise.
#'   * `pad_y2` — Integer, always negative: pixels to add to `y2` to
#'     clear the x-axis line's own thickness (moves up, inward).
#'
#'   A caller that wants only the data crops with `x1 + pad_x1`,
#'   `x2 + pad_x2`, `y1 + pad_y1`, `y2 + pad_y2`.
#' @param input Character scalar, path to the input image file.
#' @param x_axis Data frame with exactly 2 rows, one per x-axis calibration
#'   tick, and columns `value` (numeric data value), `x`, `y` (pixel
#'   centroid), `x1`, `x2`, `y1`, `y2` (pixel bounding box). See
#'   [retro_data_calibrate()] for how this is assembled from a model's
#'   tick selection and OCR findings.
#' @param y_axis Data frame with exactly 2 rows, one per y-axis calibration
#'   tick, same columns as `x_axis`.
#' @param window Numeric scalar, greater than 0. Multiple of the median
#'   calibration-tick bounding-box height defining the width of the
#'   one-sided pixel band searched from the tick-label edges toward each
#'   axis line. Not restricted to the 0-1 range. This is an internal
#'   tuning constant, not something callers are expected to vary per image.
#' @examples
#'   input <- system.file("simulation.png", package = "retroglyph")
#'   # Two x-axis ticks (time = 0 and 35) and two y-axis ticks (survival
#'   # = 0.8 and 0.2), with pixel positions read from this image's OCR.
#'   x_axis <- tibble::tibble(
#'     label = c("F", "M"), value = c(0, 35),
#'     x = c(376, 1154.5), y = c(570, 570),
#'     x1 = c(370L, 1143L), x2 = c(382L, 1166L),
#'     y1 = c(561L, 561L), y2 = c(579L, 579L)
#'   )
#'   y_axis <- tibble::tibble(
#'     label = c("A", "E"), value = c(0.8, 0.2),
#'     x = c(340, 339), y = c(116, 427),
#'     x1 = c(324L, 324L), x2 = c(355L, 354L),
#'     y1 = c(107L, 418L), y2 = c(126L, 436L)
#'   )
#'   retroglyph:::retro_data_panel(input, x_axis, y_axis)
retro_data_panel <- function(
  input,
  x_axis,
  y_axis,
  window = 4
) {
  bbox_columns <- c("value", "x", "y", "x1", "x2", "y1", "y2")
  stopifnot(
    "input must be a single string" = is.character(input) &&
      length(input) == 1L,
    "input must exist" = file.exists(input),
    "x_axis must have exactly 2 rows" = nrow(x_axis) == 2L,
    "y_axis must have exactly 2 rows" = nrow(y_axis) == 2L,
    "x_axis must have columns value, x, y, x1, x2, y1, y2" = all(
      bbox_columns %in% names(x_axis)
    ),
    "y_axis must have columns value, x, y, x1, x2, y1, y2" = all(
      bbox_columns %in% names(y_axis)
    ),
    "x_axis$value must not be NA and its two ticks must differ" = !anyNA(
      x_axis$value
    ) &&
      x_axis$value[1L] != x_axis$value[2L],
    "y_axis$value must not be NA and its two ticks must differ" = !anyNA(
      y_axis$value
    ) &&
      y_axis$value[1L] != y_axis$value[2L],
    "window must be a single number greater than 0" = is.numeric(window) &&
      length(window) == 1L &&
      !is.na(window) &&
      window > 0
  )
  foreground <- retro_components_foreground_mask(magick::image_read(input))
  anchor <- retro_panel_anchor(x_axis, y_axis)
  band_width <- retro_panel_band(x_axis, y_axis, window)
  median_height <- retro_panel_median_height(x_axis, y_axis)
  y_axis_position <- retro_panel_position_y_axis(
    foreground = foreground,
    anchor = anchor,
    band_width = band_width,
    input = input,
    median_height = median_height
  )
  x_axis_position <- retro_panel_position_x_axis(
    foreground = foreground,
    anchor = anchor,
    band_width = band_width,
    input = input,
    median_height = median_height
  )
  # Raw panel bounding box from the two required axis lines.
  x1 <- max(
    min(x_axis_position$endpoints$x),
    min(y_axis_position$endpoints$x)
  )
  y2 <- min(
    max(x_axis_position$endpoints$y),
    max(y_axis_position$endpoints$y)
  )
  x2 <- max(x_axis_position$endpoints$x)
  y1 <- min(y_axis_position$endpoints$y)
  # Outward pad, used by the optional top/right lines below if not found:
  # the larger of the two required axes' own measured pads.
  outward_margin <- max(y_axis_position$pad, x_axis_position$pad)
  top <- retro_panel_position_top(
    foreground = foreground,
    x1 = x1,
    x2 = x2,
    y1 = y1,
    band_width = band_width,
    input = input,
    median_height = median_height,
    outward_margin = outward_margin
  )
  right <- retro_panel_position_right(
    foreground = foreground,
    x2 = x2,
    y1 = y1,
    y2 = y2,
    band_width = band_width,
    input = input,
    median_height = median_height,
    outward_margin = outward_margin
  )
  tibble::tibble(
    x1 = x1,
    x2 = right$x2,
    y1 = top$y1,
    y2 = y2,
    pad_x1 = y_axis_position$pad,
    pad_x2 = right$pad_x2,
    pad_y1 = top$pad_y1,
    pad_y2 = -x_axis_position$pad
  )
}

#' @title Compute the tick-label-edge anchors for axis search bands
#' @keywords internal
#' @noRd
#' @description Each axis line sits just beyond the outer edge of its own
#'   tick labels: the y-axis line is to the right of its labels, and the
#'   x-axis line is above its labels. This function returns those edges as
#'   integer pixel positions to anchor the one-sided search bands.
#' @param x_axis Data frame with 2 rows and column `y1` (top edge of each
#'   x-axis tick label's bounding box).
#' @param y_axis Data frame with 2 rows and column `x2` (right edge of
#'   each y-axis tick label's bounding box).
#' @return A list with elements:
#'   * `y_axis_column` — Integer, the rightmost right-edge of the y-axis
#'     tick labels. The y-axis line is at or to the right of this column.
#'   * `x_axis_row` — Integer, the topmost top-edge of the x-axis tick
#'     labels. The x-axis line is at or above this row.
#' @examples
#'   x_axis <- tibble::tibble(y1 = c(83L, 83L))
#'   y_axis <- tibble::tibble(x2 = c(17L, 17L))
#'   retroglyph:::retro_panel_anchor(x_axis, y_axis)
#'   # Returns list(y_axis_column = 17L, x_axis_row = 83L).
retro_panel_anchor <- function(x_axis, y_axis) {
  list(
    y_axis_column = as.integer(max(y_axis$x2)),
    x_axis_row = as.integer(min(x_axis$y1))
  )
}

#' @title Compute the median calibration-tick bounding-box height
#' @keywords internal
#' @noRd
#' @description A pixel-scale proxy for one character's height in this
#'   image, taken as the median bounding-box height across all four
#'   calibration ticks (two per axis).
#' @param x_axis Data frame with 2 rows and columns `y1`, `y2`.
#' @param y_axis Data frame with 2 rows and columns `y1`, `y2`.
#' @return Numeric scalar.
#' @examples
#'   # All four calibration ticks are 10 pixels tall.
#'   x_axis <- tibble::tibble(y1 = c(100, 100), y2 = c(110, 110))
#'   y_axis <- tibble::tibble(y1 = c(50, 50), y2 = c(60, 60))
#'   retroglyph:::retro_panel_median_height(x_axis, y_axis)
#'   # Returns 10.
retro_panel_median_height <- function(x_axis, y_axis) {
  heights <- c(x_axis$y2 - x_axis$y1, y_axis$y2 - y_axis$y1)
  stats::median(heights)
}

#' @title Compute the axis search band width
#' @keywords internal
#' @noRd
#' @description The width (in pixels) of the one-sided band searched from
#'   the tick-label edge toward each axis line, derived from the median
#'   bounding-box height across all four calibration ticks.
#' @param x_axis Data frame with 2 rows and columns `y1`, `y2`.
#' @param y_axis Data frame with 2 rows and columns `y1`, `y2`.
#' @param window Numeric scalar, multiple of the median tick height.
#' @return Integer scalar, at least 1.
#' @examples
#'   # All four calibration ticks are 10 pixels tall.
#'   x_axis <- tibble::tibble(y1 = c(100, 100), y2 = c(110, 110))
#'   y_axis <- tibble::tibble(y1 = c(50, 50), y2 = c(60, 60))
#'   retroglyph:::retro_panel_band(x_axis, y_axis, window = 2)
#'   # Median tick height is 10, so the band width is
#'   # ceiling(2 * 10) = 20.
retro_panel_band <- function(x_axis, y_axis, window) {
  median_height <- retro_panel_median_height(x_axis, y_axis)
  max(1L, as.integer(ceiling(window * median_height)))
}

#' @title Locate the y-axis line and its own padding
#' @keywords internal
#' @noRd
#' @description Searches a column band extending rightward from the y-axis
#'   tick labels' right edges for the y-axis (left) line - the required
#'   vertical line whose absence is an error - then measures that line's
#'   own thickness to compute how far the panel should pad inward to clear
#'   it (the skeletonization trick, via [retro_panel_line_pad()]).
#' @param foreground Logical matrix (height x width), `TRUE` for
#'   foreground pixels.
#' @param anchor A list with element `y_axis_column` (integer), the
#'   rightmost right-edge of the y-axis tick labels, from
#'   [retro_panel_anchor()].
#' @param band_width Integer scalar, the one-sided search band width from
#'   [retro_panel_band()].
#' @param input Character scalar, path to the input image file (passed
#'   through to [retro_panel_line_pad()]).
#' @param median_height Numeric scalar, the median calibration-tick
#'   bounding-box height, from [retro_panel_median_height()].
#' @return A list with elements:
#'   * `endpoints` — A 2-row data frame with columns `x`, `y`, the
#'     endpoints of the detected y-axis line (see
#'     [retro_panel_find_axis()]).
#'   * `pad` — Integer, the y-axis line's own measured thickness pad.
#' @examples
#'   foreground <- matrix(FALSE, nrow = 100, ncol = 100)
#'   foreground[10:80, 20] <- TRUE
#'   pixel_matrix <- matrix("#ffffffff", nrow = 100, ncol = 100)
#'   pixel_matrix[10:80, 20] <- "#000000ff"
#'   input <- tempfile(fileext = ".png")
#'   magick::image_write(magick::image_read(pixel_matrix), input)
#'   retroglyph:::retro_panel_position_y_axis(
#'     foreground = foreground,
#'     anchor = list(y_axis_column = 17L, x_axis_row = 83L),
#'     band_width = 24L,
#'     input = input,
#'     median_height = 6
#'   )
retro_panel_position_y_axis <- function(
  foreground,
  anchor,
  band_width,
  input,
  median_height
) {
  width <- ncol(foreground)
  low <- anchor$y_axis_column
  high <- min(width, anchor$y_axis_column + band_width)
  if (low > width) {
    stop(
      "Could not find the y-axis. The tick-label edge (column ",
      anchor$y_axis_column,
      ") falls outside the image. Check the y-axis calibration ticks.",
      call. = FALSE
    )
  }
  endpoints <- retro_panel_find_axis(
    foreground = foreground,
    columns = low:high,
    axis = "y"
  )
  pad <- retro_panel_line_pad(input, endpoints, median_height)
  list(endpoints = endpoints, pad = pad)
}

#' @title Locate the x-axis line and its own padding
#' @keywords internal
#' @noRd
#' @description Searches a row band extending upward from the x-axis tick
#'   labels' top edges for the x-axis (bottom) line - the required
#'   horizontal line whose absence is an error. Horizontal lines are found
#'   by reusing the same column-scanning machinery as the y-axis: the
#'   foreground matrix is transposed and column-reversed so that
#'   bottom-to-top becomes left-to-right, [retro_panel_find_axis()]
#'   searches it exactly as it would a vertical line, and the result is
#'   mapped back to original coordinates. Then measures the line's own
#'   thickness to compute how far the panel should pad inward to clear it
#'   (the skeletonization trick, via [retro_panel_line_pad()]).
#' @param foreground Logical matrix (height x width), `TRUE` for
#'   foreground pixels.
#' @param anchor A list with element `x_axis_row` (integer), the topmost
#'   top-edge of the x-axis tick labels, from [retro_panel_anchor()].
#' @param band_width Integer scalar, the one-sided search band width from
#'   [retro_panel_band()].
#' @param input Character scalar, path to the input image file (passed
#'   through to [retro_panel_line_pad()]).
#' @param median_height Numeric scalar, the median calibration-tick
#'   bounding-box height, from [retro_panel_median_height()].
#' @return A list with elements:
#'   * `endpoints` — A 2-row data frame with columns `x`, `y`, the
#'     endpoints of the detected x-axis line, in original (untransformed)
#'     coordinates.
#'   * `pad` — Integer, the x-axis line's own measured thickness pad.
#' @examples
#'   foreground <- matrix(FALSE, nrow = 100, ncol = 100)
#'   foreground[80, 20:90] <- TRUE
#'   pixel_matrix <- matrix("#ffffffff", nrow = 100, ncol = 100)
#'   pixel_matrix[80, 20:90] <- "#000000ff"
#'   input <- tempfile(fileext = ".png")
#'   magick::image_write(magick::image_read(pixel_matrix), input)
#'   retroglyph:::retro_panel_position_x_axis(
#'     foreground = foreground,
#'     anchor = list(y_axis_column = 17L, x_axis_row = 83L),
#'     band_width = 24L,
#'     input = input,
#'     median_height = 6
#'   )
retro_panel_position_x_axis <- function(
  foreground,
  anchor,
  band_width,
  input,
  median_height
) {
  height <- nrow(foreground)
  transformed <- t(foreground)[, seq(height, 1L), drop = FALSE]
  high <- anchor$x_axis_row
  low <- max(1L, anchor$x_axis_row - band_width)
  if (high < 1L) {
    stop(
      "Could not find the x-axis. The tick-label edge (row ",
      anchor$x_axis_row,
      ") falls outside the image. Check the x-axis calibration ticks.",
      call. = FALSE
    )
  }
  transformed_columns <- sort(height + 1L - (low:high))
  transformed_endpoints <- retro_panel_find_axis(
    foreground = transformed,
    columns = transformed_columns,
    axis = "x"
  )
  # Map back: transformed[i, j] corresponds to original[height + 1 - j, i].
  endpoints <- data.frame(
    x = transformed_endpoints$y,
    y = rep(height + 1L - transformed_endpoints$x[1L], 2L)
  )
  pad <- retro_panel_line_pad(input, endpoints, median_height)
  list(endpoints = endpoints, pad = pad)
}

#' @title Locate an optional top bounding line and its padding
#' @keywords internal
#' @noRd
#' @description Some Kaplan-Meier images draw a full box around the
#'   plotting area; its top line can extend above where the y-axis line
#'   itself stops being drawn. This searches for that line, one-sided,
#'   using the same transpose trick as [retro_panel_position_x_axis()]:
#'   rows from `band_width` pixels above `y1` down through `y1` itself (a
#'   real top border can only be at or above `y1`, the y-axis line's own
#'   topmost detected pixel - never below it), restricted to columns
#'   `x1:x2` so an unrelated horizontal mark elsewhere in the row band
#'   (e.g. a title rule) is not mistaken for the panel's border. Uses
#'   [retro_panel_find_line()] directly, rather than
#'   [retro_panel_find_axis()], since not finding a line here is a normal
#'   outcome, not an error.
#'
#'   If found, the line's own thickness is measured (the skeletonization
#'   trick, via [retro_panel_line_pad()]) and the panel pads inward
#'   (down) to clear it. If not found, `y1` is left unchanged and the
#'   panel instead pads outward (up) by `outward_margin`, to leave room
#'   for data that might sit flush with that edge - clipped so it never
#'   pads above row 1.
#' @param foreground Logical matrix (height x width), `TRUE` for
#'   foreground pixels.
#' @param x1 Integer scalar, left column of the panel.
#' @param x2 Integer scalar, right column of the panel.
#' @param y1 Integer scalar, top row of the panel, from the y-axis
#'   line's own topmost detected pixel.
#' @param band_width Integer scalar, the search band width from
#'   [retro_panel_band()].
#' @param input Character scalar, path to the input image file (passed
#'   through to [retro_panel_line_pad()]).
#' @param median_height Numeric scalar, the median calibration-tick
#'   bounding-box height, from [retro_panel_median_height()].
#' @param outward_margin Integer scalar, the outward pad to use if no top
#'   line is found - the larger of the y-axis's and x-axis's own
#'   measured pads (see [retro_data_panel()]).
#' @return A list with elements:
#'   * `y1` — Integer, the detected top line's row, or the unchanged
#'     input `y1` if none was found.
#'   * `pad_y1` — Integer, signed: positive (inward) if a line was
#'     found, negative (outward) otherwise.
#' @examples
#'   foreground <- matrix(FALSE, nrow = 100, ncol = 100)
#'   foreground[10:80, 20] <- TRUE
#'   foreground[80, 20:90] <- TRUE
#'   foreground[5, 20:90] <- TRUE
#'   pixel_matrix <- matrix("#ffffffff", nrow = 100, ncol = 100)
#'   pixel_matrix[foreground] <- "#000000ff"
#'   input <- tempfile(fileext = ".png")
#'   magick::image_write(magick::image_read(pixel_matrix), input)
#'   retroglyph:::retro_panel_position_top(
#'     foreground = foreground,
#'     x1 = 20L,
#'     x2 = 90L,
#'     y1 = 10L,
#'     band_width = 24L,
#'     input = input,
#'     median_height = 6,
#'     outward_margin = 3L
#'   )
#'   # Finds the top border at row 5, above the y-axis line's own
#'   # topmost pixel at row 10, and pads inward (down) to clear it.
retro_panel_position_top <- function(
  foreground,
  x1,
  x2,
  y1,
  band_width,
  input,
  median_height,
  outward_margin
) {
  height <- nrow(foreground)
  width <- ncol(foreground)
  restricted <- foreground
  if (x1 > 1L) {
    restricted[, seq_len(x1 - 1L)] <- FALSE
  }
  if (x2 < width) {
    restricted[, seq(x2 + 1L, width)] <- FALSE
  }
  transformed <- t(restricted)[, seq(height, 1L), drop = FALSE]
  low <- max(1L, floor(y1 - band_width))
  transformed_columns <- sort(height + 1L - (low:y1))
  transformed_endpoints <- retro_panel_find_line(
    foreground = transformed,
    columns = transformed_columns
  )
  if (is.null(transformed_endpoints)) {
    return(list(y1 = y1, pad_y1 = -min(outward_margin, y1 - 1L)))
  }
  endpoints <- data.frame(
    x = transformed_endpoints$y,
    y = rep(height + 1L - transformed_endpoints$x[1L], 2L)
  )
  pad <- retro_panel_line_pad(input, endpoints, median_height)
  list(y1 = endpoints$y[1L], pad_y1 = pad)
}

#' @title Locate an optional right bounding line and its padding
#' @keywords internal
#' @noRd
#' @description The mirror of [retro_panel_position_top()] for the right
#'   edge - no transpose needed, since a right bounding line is vertical,
#'   the same orientation [retro_panel_position_y_axis()] already
#'   searches for. Searches columns from `x2` itself out to `band_width`
#'   pixels beyond it (a real right border can only be at or right of
#'   `x2`, the x-axis line's own rightmost detected pixel - never left of
#'   it), restricted to rows `y1:y2`. Uses [retro_panel_find_line()]
#'   directly, since not finding a line here is a normal outcome, not an
#'   error.
#'
#'   If found, the line's own thickness is measured (via
#'   [retro_panel_line_pad()]) and the panel pads inward (left) to clear
#'   it. If not found, `x2` is left unchanged and the panel instead pads
#'   outward (right) by `outward_margin`, to leave room for data that
#'   might sit flush with that edge - clipped so it never pads past the
#'   image's right edge.
#' @param foreground Logical matrix (height x width), `TRUE` for
#'   foreground pixels.
#' @param x2 Integer scalar, right column of the panel, from the x-axis
#'   line's own rightmost detected pixel.
#' @param y1 Integer scalar, top row of the panel.
#' @param y2 Integer scalar, bottom row of the panel.
#' @param band_width Integer scalar, the search band width from
#'   [retro_panel_band()].
#' @param input Character scalar, path to the input image file (passed
#'   through to [retro_panel_line_pad()]).
#' @param median_height Numeric scalar, the median calibration-tick
#'   bounding-box height, from [retro_panel_median_height()].
#' @param outward_margin Integer scalar, the outward pad to use if no
#'   right line is found - the larger of the y-axis's and x-axis's own
#'   measured pads (see [retro_data_panel()]).
#' @return A list with elements:
#'   * `x2` — Integer, the detected right line's column, or the
#'     unchanged input `x2` if none was found.
#'   * `pad_x2` — Integer, signed: negative (inward) if a line was
#'     found, positive (outward) otherwise.
#' @examples
#'   foreground <- matrix(FALSE, nrow = 100, ncol = 100)
#'   foreground[10:80, 20] <- TRUE
#'   foreground[80, 20:90] <- TRUE
#'   foreground[10:80, 95] <- TRUE
#'   pixel_matrix <- matrix("#ffffffff", nrow = 100, ncol = 100)
#'   pixel_matrix[foreground] <- "#000000ff"
#'   input <- tempfile(fileext = ".png")
#'   magick::image_write(magick::image_read(pixel_matrix), input)
#'   retroglyph:::retro_panel_position_right(
#'     foreground = foreground,
#'     x2 = 90L,
#'     y1 = 10L,
#'     y2 = 80L,
#'     band_width = 24L,
#'     input = input,
#'     median_height = 6,
#'     outward_margin = 3L
#'   )
#'   # Finds the right border at column 95, right of the x-axis line's
#'   # own rightmost pixel at column 90, and pads inward (left) to
#'   # clear it.
retro_panel_position_right <- function(
  foreground,
  x2,
  y1,
  y2,
  band_width,
  input,
  median_height,
  outward_margin
) {
  height <- nrow(foreground)
  width <- ncol(foreground)
  restricted <- foreground
  if (y1 > 1L) {
    restricted[seq_len(y1 - 1L), ] <- FALSE
  }
  if (y2 < height) {
    restricted[seq(y2 + 1L, height), ] <- FALSE
  }
  high <- min(width, ceiling(x2 + band_width))
  endpoints <- retro_panel_find_line(
    foreground = restricted,
    columns = x2:high
  )
  if (is.null(endpoints)) {
    return(list(x2 = x2, pad_x2 = min(outward_margin, width - x2)))
  }
  pad <- retro_panel_line_pad(input, endpoints, median_height)
  list(x2 = endpoints$x[1L], pad_x2 = -pad)
}

#' @title Measure a detected line's thickness and compute its pad
#' @keywords internal
#' @noRd
#' @description Estimates how many pixels to pad past a detected line to
#'   clear its own thickness. Isolates the line from the rest of the
#'   image by masking a band around it - as wide as the median
#'   calibration-tick height (see [retro_panel_median_height()]),
#'   spanning the line's own detected extent - quantizes that band to
#'   strictly two colors with no dithering so anti-aliased edge pixels
#'   collapse cleanly to foreground or background, and measures line
#'   thickness the same way [retro_data_path()] measures curve width -
#'   pixel area over skeleton length. The result adds 2 pixels as a
#'   deliberate safety margin: a pad that is too small still leaves a
#'   sliver of the line in the panel, while a pad that is too large only
#'   costs a few pixels of otherwise-empty margin.
#'
#'   Whether the line is vertical or horizontal is read directly off
#'   `endpoints`: a vertical line has one repeated `x` and two distinct
#'   `y` values (its row extent); a horizontal line has one repeated `y`
#'   and two distinct `x` values (its column extent) - the same shape
#'   every caller already has on hand, from [retro_panel_find_axis()] or
#'   [retro_panel_find_line()].
#' @param input Character scalar, path to the input image file.
#' @param endpoints A data frame with 2 rows and columns `x`, `y` - the
#'   pixel endpoints of one detected line.
#' @param median_height Numeric scalar, the median calibration-tick
#'   bounding-box height, from [retro_panel_median_height()].
#' @return Integer scalar, the measured line thickness in pixels, plus 2.
#' @examples
#'   # A 100x100 image with a single-pixel-wide vertical line at column
#'   # 20, rows 10-80.
#'   pixel_matrix <- matrix("#ffffffff", nrow = 100, ncol = 100)
#'   pixel_matrix[10:80, 20] <- "#000000ff"
#'   input <- tempfile(fileext = ".png")
#'   magick::image_write(magick::image_read(pixel_matrix), input)
#'   endpoints <- data.frame(x = c(20L, 20L), y = c(10L, 80L))
#'   retroglyph:::retro_panel_line_pad(input, endpoints, median_height = 6)
#'   # A single-pixel-wide line has line width 1, plus 2 = 3.
retro_panel_line_pad <- function(input, endpoints, median_height) {
  image <- magick::image_read(input)
  info <- magick::image_info(image)
  width <- info$width
  height <- info$height
  half_width <- max(1L, as.integer(ceiling(median_height / 2)))
  vertical <- endpoints$x[1L] == endpoints$x[2L]
  if (vertical) {
    column <- endpoints$x[1L]
    band_x1 <- max(1L, column - half_width)
    band_x2 <- min(width, column + half_width)
    band_y1 <- min(endpoints$y)
    band_y2 <- max(endpoints$y)
  } else {
    row <- endpoints$y[1L]
    band_y1 <- max(1L, row - half_width)
    band_y2 <- min(height, row + half_width)
    band_x1 <- min(endpoints$x)
    band_x2 <- max(endpoints$x)
  }
  masked <- tempfile(fileext = ".png")
  on.exit(unlink(masked))
  retro_image_mask(
    input = input,
    output = masked,
    x1 = band_x1,
    x2 = band_x2,
    y1 = band_y1,
    y2 = band_y2,
    mask = TRUE,
    initial = FALSE
  )
  quantized <- retro_components_quantize(magick::image_read(masked))
  raster <- magick::image_raster(quantized, tidy = FALSE)
  pixel_matrix <- as.matrix(raster)
  pixel_matrix[] <- retro_color_rgb(pixel_matrix)
  background <- retro_background_color(raster, quantize = FALSE)
  line_width <- retro_path_line_width(pixel_matrix, background)
  as.integer(line_width + 2L) # 2 extra pixels to account for anti-aliasing
}

#' @title Find an axis line by scanning a given column band
#' @keywords internal
#' @noRd
#' @description Search the given columns for the one with the longest
#'   vertical run of `TRUE` values (via [retro_panel_find_line()]),
#'   erroring if none clears the noise floor - the axis line is
#'   required, unlike the optional top/right bounding lines (see
#'   [retro_panel_position_top()] and [retro_panel_position_right()],
#'   which call [retro_panel_find_line()] directly instead).
#' @param foreground Logical matrix. `TRUE` indicates foreground pixels.
#' @param columns Integer vector, columns to search (already clipped to
#'   the image and anchored off the tick-label edges by the caller), in
#'   ascending order.
#' @param axis Character scalar, `"x"` or `"y"`, used in error messages.
#' @return A data frame with columns `x` and `y` and two rows (endpoints).
#' @examples
#'   # A 5x6 mask: columns 3 and 4 both have a 4-pixel run of foreground
#'   # (rows 1-4), tied for the longest run; column 1 has a shorter,
#'   # 2-pixel run.
#'   foreground <- matrix(FALSE, nrow = 5, ncol = 6)
#'   foreground[1:4, 3] <- TRUE
#'   foreground[1:4, 4] <- TRUE
#'   foreground[1:2, 1] <- TRUE
#'   result <- retroglyph:::retro_panel_find_axis(
#'     foreground, columns = 1:6, axis = "y"
#'   )
#'   print(result)
#'   # The tied columns (3 and 4) average to 3.5, rounded to 4.
retro_panel_find_axis <- function(foreground, columns, axis) {
  endpoints <- retro_panel_find_line(foreground, columns)
  if (is.null(endpoints)) {
    stop(
      "Could not find the ",
      axis,
      "-axis. No sufficiently long run of foreground pixels was found in ",
      "the search band near the tick labels. Try re-checking the ",
      "axis calibration ticks (labels and values) - misplaced tick labels ",
      "shift the search band away from the true axis line.",
      call. = FALSE
    )
  }
  endpoints
}

#' @title Find the longest run of foreground pixels across a column band
#' @keywords internal
#' @noRd
#' @description Search the given columns for the one with the longest
#'   vertical run of `TRUE` values. A line is typically several pixels
#'   thick, so more than one column ties for the longest run; average
#'   the tied columns so the detected line follows the true centerline
#'   of the stroke instead of one side of it. Return the endpoints of
#'   the run at that averaged column, or `NULL` if the best run does not
#'   clear `noise_floor` - "no line found" is a normal outcome for the
#'   optional top/right bounding lines (see [retro_panel_position_top()],
#'   [retro_panel_position_right()]), unlike the required axis lines,
#'   where [retro_panel_find_axis()] turns a `NULL` result into an error.
#'
#'   A small fixed `noise_floor` is brittle: in some source images, a
#'   Kaplan-Meier curve's own flat run (a step function's plateau,
#'   especially the long one right at the start) is easily several
#'   pixels long, long enough to clear a small fixed floor and get
#'   mistaken for an axis or bounding line. The default instead scales
#'   with `foreground`'s own row count - a real axis or bounding line
#'   spans a substantial fraction of the image, so requiring at least a
#'   third of that dimension rejects stray data runs while still
#'   accepting genuine lines. This works for every caller, whether
#'   `foreground` is in original orientation (so its row count is the
#'   image height, for a vertical line search) or transposed (so its row
#'   count is the image width, for a horizontal line search via the
#'   transpose trick) - either way, `nrow(foreground)` is the dimension
#'   the detected run actually extends along.
#' @param foreground Logical matrix. `TRUE` indicates foreground pixels.
#' @param columns Integer vector, columns to search, in ascending order.
#' @param noise_floor Integer scalar, the minimum run length that counts
#'   as a detected line.
#' @return A data frame with columns `x` and `y` and two rows (endpoints),
#'   or `NULL` if no column's longest run reaches `noise_floor`.
#' @examples
#'   # A 5x6 mask: columns 3 and 4 both have a 4-pixel run of foreground
#'   # (rows 1-4), tied for the longest run; column 1 has a shorter,
#'   # 2-pixel run, below the default noise floor.
#'   foreground <- matrix(FALSE, nrow = 5, ncol = 6)
#'   foreground[1:4, 3] <- TRUE
#'   foreground[1:4, 4] <- TRUE
#'   foreground[1:2, 1] <- TRUE
#'   result <- retroglyph:::retro_panel_find_line(foreground, columns = 1:6)
#'   print(result)
#'   # No column reaches a run of 3 in this mask.
#'   sparse <- matrix(FALSE, nrow = 5, ncol = 6)
#'   sparse[1:2, 1] <- TRUE
#'   retroglyph:::retro_panel_find_line(sparse, columns = 1:6)
#'   # Returns NULL.
#'   # A 100-row mask: a run of 20 clears the old fixed floor of 3, but
#'   # not a third of the image height (34) - correctly rejected as a
#'   # stray data run rather than a real line.
#'   tall <- matrix(FALSE, nrow = 100, ncol = 6)
#'   tall[1:20, 3] <- TRUE
#'   retroglyph:::retro_panel_find_line(tall, columns = 1:6)
#'   # Returns NULL.
retro_panel_find_line <- function(
  foreground,
  columns,
  noise_floor = max(3L, nrow(foreground) %/% 3L)
) {
  longest <- vapply(
    columns,
    function(column) retro_panel_max_run(foreground[, column]),
    integer(1L)
  )
  best <- max(longest)
  if (best < noise_floor) {
    return(NULL)
  }
  tied <- columns[longest == best]
  column <- as.integer(round(mean(tied)))
  foreground_rows <- which(foreground[, column])
  data.frame(
    x = c(column, column),
    y = c(min(foreground_rows), max(foreground_rows))
  )
}

#' @title Longest run of TRUE values in a logical vector
#' @keywords internal
#' @noRd
#' @param x Logical vector.
#' @return Integer scalar.
#' @examples
#'   retroglyph:::retro_panel_max_run(c(TRUE, TRUE, FALSE, TRUE, TRUE, TRUE))
#'   # Longest run of TRUE values is 3 (the trailing run).
#'   retroglyph:::retro_panel_max_run(c(FALSE, FALSE))
#'   # No TRUE values at all - returns 0.
retro_panel_max_run <- function(x) {
  if (!any(x)) {
    return(0L)
  }
  runs <- rle(x)
  max(runs$lengths[runs$values])
}
