#' @title Scale path pixel data into data coordinates
#' @keywords internal
#' @noRd
#' @description Convert each curve's already-thin pixel trace from
#'   [retro_data_path()] into data-space values: calibrate both axes to
#'   data-space using the axis tick labels and locations, then normalize the
#'   result into a single canonical shape - a 0-1 survival curve starting at
#'   `(x = 0, y = 1)` and decreasing on average from there.
#'   A percentage-scale plot is divided down to a
#'   proportion, and a cumulative-incidence plot is complemented into its
#'   survival equivalent - both inferred once from every series' pixels
#'   pooled together (see [retro_scale_max_y()] and
#'   [retro_scale_increasing()]). [retro_data_path()] already returns a
#'   thin, skeletonized trace, so there is no step-fitting or median
#'   collapsing left to do here - this is pure pixel-to-data scaling.
#' @return A tibble with columns:
#'   * `series` — Character, the series name.
#'   * `x` — Numeric, the data-space x-coordinate. `0` is always the first
#'     value within a series.
#'   * `y` — Numeric, the data-space y-coordinate, always on a 0-1 survival
#'     scale: `1` is always the first value within a series, and the trend is
#'     decreasing after that.
#'   * `max_y` — Numeric, `1` or `100`: whether the *original* plot's
#'     y-axis read as a 0-1 proportion or a 0-100 percentage. Same value on
#'     every row; `x`/`y` are already normalized, so this describes the
#'     source plot rather than how to interpret them. Kept only so
#'     [retro_agent_class]$compare() can redraw the refit curve back onto
#'     the original axis.
#'   * `increasing` — Logical, `TRUE` if the *original* plot trended upward
#'     over time (cumulative incidence) rather than downward (survival).
#'     Same value on every row; `y` is already complemented into survival
#'     scale, so this also describes the source plot only, again for
#'     [retro_agent_class]$compare()'s redraw.
#'
#'   One row per skeleton pixel per series,
#'   plus the prepended time-axis origin.
#'   A vertical step-drop's several pixels at (or near) one `x` all survive
#'   here rather than being collapsed to a single value. Sorted by series,
#'   then `x` ascending, then `y` descending within tied/near-tied `x` (so a
#'   drop reads top-to-bottom, consistent with `y` never increasing). Color
#'   is not included here - look it up from the legend (e.g.
#'   `state$data_legend`) instead.
#' @param data_path A tibble with columns `x` (integer, pixel column), `y`
#'   (integer, pixel row), and `color` (character, hex color). From
#'   [retro_data_path()].
#' @param data_label A tibble from [retro_do_label()] with columns
#'   `label`, `x`, `y` (pixel centroids), among others.
#' @param x_axis A tibble from `retro_data_calibrate()$x_axis` with
#'   columns `label` (character) and `value` (numeric). Must have at
#'   least 2 rows.
#' @param y_axis A tibble from `retro_data_calibrate()$y_axis` with
#'   columns `label` (character) and `value` (numeric). Must have at
#'   least 2 rows.
#' @param legend A tibble with columns `series` and `color` (e.g. from
#'   `state$data_legend`, produced by [retro_tool_distill()]), used to
#'   map each path pixel's color to its series name.
#' @examples
#'   data_label <- tibble::tibble(
#'     label = c("A", "B", "C", "D"),
#'     word = c("0", "10", "0.0", "1.0"),
#'     confidence = rep(90, 4),
#'     x = c(2, 8, 1, 1),
#'     y = c(10, 10, 8, 2),
#'     x1 = c(1L, 7L, 0L, 0L),
#'     x2 = c(3L, 9L, 2L, 2L),
#'     y1 = c(9L, 9L, 7L, 1L),
#'     y2 = c(11L, 11L, 9L, 3L)
#'   )
#'   x_axis <- tibble::tibble(label = c("A", "B"), value = c(0, 10))
#'   y_axis <- tibble::tibble(label = c("C", "D"), value = c(0, 1))
#'   data_path <- tibble::tibble(
#'     x = c(2L, 2L, 3L, 5L, 8L),
#'     y = c(3L, 4L, 5L, 5L, 6L),
#'     color = rep("#dc3030", 5),
#'     line_width = 1L,
#'     width = 10L,
#'     height = 12L
#'   )
#'   legend <- tibble::tibble(series = "Placebo", color = "#dc3030")
#'   retroglyph:::retro_data_scale(
#'     data_path = data_path,
#'     data_label = data_label,
#'     x_axis = x_axis,
#'     y_axis = y_axis,
#'     legend = legend
#'   )
retro_data_scale <- function(
  data_path,
  data_label,
  x_axis,
  y_axis,
  legend
) {
  stopifnot(
    inherits(data_path, "tbl_df"),
    inherits(data_label, "tbl_df"),
    inherits(x_axis, "tbl_df"),
    inherits(y_axis, "tbl_df"),
    inherits(legend, "tbl_df"),
    all(c("color", "series") %in% names(legend)),
    nrow(x_axis) >= 2L,
    nrow(y_axis) >= 2L,
    all(x_axis$label %in% data_label$label),
    all(y_axis$label %in% data_label$label)
  )
  # Map path colors to series names and validate against the legend.
  data_path$series <- legend$series[match(data_path$color, legend$color)]
  unmatched <- is.na(data_path$series)
  if (any(unmatched)) {
    bad_colors <- unique(data_path$color[unmatched])
    stop(
      "Path contains colors not in legend: ",
      paste(bad_colors, collapse = ", ")
    )
  }
  # Use the tick mark values and locations to find the scale of the data,
  # and calibrate every pixel once - data_path is already a thin,
  # skeletonized trace, so there is no per-series fitting step left that
  # would need its own calibration pass.
  x_pixel <- data_label$x[match(x_axis$label, data_label$label)]
  x_calibration <- retro_scale_calibration(
    pixel = x_pixel,
    value = x_axis$value
  )
  y_pixel <- data_label$y[match(y_axis$label, data_label$label)]
  y_calibration <- retro_scale_calibration(
    pixel = y_pixel,
    value = y_axis$value
  )
  data_path$x <- x_calibration$slope * data_path$x + x_calibration$intercept
  data_path$y <- y_calibration$slope * data_path$y + y_calibration$intercept
  # A Kaplan-Meier plot's scale (0-1 or 0-100) and orientation (survival or
  # cumulative incidence) are whole-plot properties, read once off every
  # series' pixels pooled together rather than re-derived per series.
  max_y <- retro_scale_max_y(data_path$y)
  increasing <- retro_scale_increasing(data_path$x, data_path$y)
  # Normalize into the canonical shape: a 0-1 survival curve starting at
  # (0, 1) and decreasing from there, regardless of how the source plot
  # read. This applies uniformly to every series, so it happens once here
  # rather than being recomputed per series.
  data_path$y <- data_path$y / max_y
  if (increasing) {
    data_path$y <- 1 - data_path$y
  }
  # Every series with at least one claimed pixel gets its own time-axis origin,
  # independent of whether any of its pixels survive the x >= 0 filter
  # below.
  series_names <- unique(data_path$series)
  time_axis_origin <- data.frame(x = 0, y = 1, series = series_names)
  in_range <- data_path$x >= 0
  observed <- data.frame(
    x = data_path$x[in_range],
    y = data_path$y[in_range],
    series = data_path$series[in_range]
  )
  scaled <- rbind(time_axis_origin, observed)
  # Sort by series (alphabetical), then x ascending; break ties (e.g. a
  # vertical drop's several skeleton pixels sharing one x) by y descending,
  # so a drop reads top-to-bottom - consistent with y being non-increasing
  # over time.
  scaled <- scaled[order(scaled$series, scaled$x, -scaled$y), ]
  scaled$max_y <- max_y
  scaled$increasing <- increasing
  fields <- c("series", "x", "y", "max_y", "increasing")
  tibble::as_tibble(scaled[, fields])
}

#' @title Fit a linear pixel-to-data mapping from tick mark positions
#' @keywords internal
#' @noRd
#' @description Fits a least-squares line `value = slope * pixel +
#'   intercept` through the given pixel/value pairs (typically OCR tick
#'   mark pixel positions and their known axis values), using the
#'   centered-sums form of simple linear regression. Requires at least 2
#'   points, not all with identical pixel coordinates.
#' @param pixel Numeric vector, pixel-space coordinates of the tick
#'   marks (e.g. OCR centroid `x` or `y` positions). Must have length
#'   >= 2, and not all elements may be identical.
#' @param value Numeric vector, the same length as `pixel`, giving the
#'   data-space axis value at each tick mark.
#' @return A list with numeric elements `slope` and `intercept`,
#'   defining the linear map from pixel coordinate to data-space value.
#' @examples
#'   calibration <- retroglyph:::retro_scale_calibration(
#'     pixel = c(0, 10, 20), value = c(0, 100, 200)
#'   )
#'   print(calibration)
#'   # slope = 10, intercept = 0
retro_scale_calibration <- function(pixel, value) {
  n <- length(pixel)
  if (n < 2L) {
    stop("At least 2 calibration points are required.")
  }
  pixel_centered <- pixel - mean(pixel)
  sum_of_squares <- sum(pixel_centered^2)
  if (sum_of_squares == 0) {
    stop("All pixel coordinates are identical; cannot fit calibration.")
  }
  slope <- sum(pixel_centered * (value - mean(value))) / sum_of_squares
  intercept <- mean(value) - slope * mean(pixel)
  list(slope = slope, intercept = intercept)
}

#' @title Infer whether the y-axis reads as a proportion or a percentage
#' @keywords internal
#' @noRd
#' @description A Kaplan-Meier y-axis is plotted on a 0-1 proportion scale
#'   or a 0-100 percentage scale; nothing about the digitized curve says
#'   which, so it is inferred from the largest calibrated y value across
#'   every series pooled together. A value that could only come from a
#'   percentage scale (bigger than any plausible proportion) means 100;
#'   otherwise 1.
#' @param y Numeric vector, data-space y-coordinates, e.g. every series'
#'   calibrated pixel `y` combined.
#' @return Numeric scalar, `100` or `1`.
#' @examples
#'   retroglyph:::retro_scale_max_y(c(1, 0.9, 0.5, 0.2))
#'   # 1 - a proportion scale
#'   retroglyph:::retro_scale_max_y(c(100, 90, 50, 20))
#'   # 100 - a percentage scale
retro_scale_max_y <- function(y) {
  if (max(y) > 1.5) 100 else 1
}

#' @title Infer whether a trace trends upward (cumulative incidence) or
#'   downward (survival)
#' @keywords internal
#' @noRd
#' @description A traditional Kaplan-Meier curve falls over time
#'   (survival); its complement, cumulative incidence, rises over time.
#'   Nothing about the digitized trace names which one was plotted, so the
#'   direction is inferred by fitting a simple linear trend and checking
#'   its sign. Every curve on one plot shares the same y-axis convention
#'   (see the package's assumptions), so pooling every series' `x`/`y`
#'   together only reinforces the direction signal rather than muddying
#'   it.
#' @details A curve with no trend to read comes back as `FALSE`:
#'   survival, the package's default orientation. Two cases reach that
#'   default. A degenerate path, one pixel or every pixel in a single
#'   column, yields a slope of `NA`, and the result feeds an `if()` in
#'   [retro_data_scale()], so it has to be a usable logical rather than a
#'   missing value. A perfectly flat curve yields a slope that is zero
#'   only up to rounding, and its sign is then whichever way the last bits
#'   fell: a flat trace calibrated from the same tick marks has been
#'   measured at both `+1.4e-17` and `-1.4e-17`. Testing the fitted rise
#'   across the observed x range against a tolerance, rather than testing
#'   the raw slope against zero, keeps that noise from deciding the
#'   answer. It matters more than the reported column suggests, because
#'   the direction also decides whether [retro_data_scale()] complements
#'   `y` into survival scale before returning it.
#' @param x Numeric vector, data-space x-coordinates (time), e.g. every
#'   series' calibrated pixel `x` combined.
#' @param y Numeric vector, the same length as `x`, data-space
#'   y-coordinates.
#' @return Logical scalar, `TRUE` if the fitted trend is upward
#'   (cumulative incidence), `FALSE` if downward (survival) or if there is
#'   no trend to fit.
#' @examples
#'   retroglyph:::retro_scale_increasing(x = 0:4, y = c(1, 0.9, 0.8, 0.7, 0.6))
#'   # FALSE - survival, falling over time
#'   retroglyph:::retro_scale_increasing(x = 0:4, y = c(0, 0.1, 0.2, 0.3, 0.4))
#'   # TRUE - cumulative incidence, rising over time
#'   retroglyph:::retro_scale_increasing(x = 5, y = 5)
#'   # FALSE - no trend to fit
#'   retroglyph:::retro_scale_increasing(x = 0:4, y = rep(0.5, 5))
#'   # FALSE - flat, so the slope is rounding noise either way
retro_scale_increasing <- function(x, y) {
  slope <- unname(stats::coef(stats::lm(y ~ x))[[2]])
  rise <- slope * diff(range(x))
  tolerance <- sqrt(.Machine$double.eps) * max(1, max(abs(y)))
  isTRUE(rise > tolerance)
}
