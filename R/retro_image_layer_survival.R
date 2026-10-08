#' @title Render the reconstructed survival curve with a specified layer order
#' @keywords internal
#' @noRd
#' @description Refit each series' Kaplan-Meier curve from reconstructed
#'   individual patient data, express it back on the plot's own scale and
#'   orientation, transform it into pixel space, and paint it onto a
#'   canvas — painting curve layers in a specified order of precedence.
#'   The first color in `layers` is drawn on top (highest precedence);
#'   subsequent colors are layered beneath. Writes the result to a PNG
#'   file.
#'
#'   This renders the *reconstructed* data (`state$data_survival`), after
#'   `IPDfromKM`'s Kaplan-Meier reconstruction. See
#'   [retro_image_layer_trace()] for the sibling function that renders the
#'   raw digitized trace (`state$data_scaled`) instead, before
#'   reconstruction - comparing the two isolates whether a disagreement
#'   with the source image traces back to retroglyph's own digitization or
#'   to `IPDfromKM`'s reconstruction.
#'
#'   With `censoring = TRUE` the same curves are drawn one pixel wide
#'   instead of dilated to `line_width`, and a short vertical tick marks
#'   every reconstructed censoring time (see
#'   [retro_layer_censoring_data()]).
#'   The thinning is what makes the ticks checkable at all: a tick a couple
#'   of pixels tall vanishes inside a line several pixels thick, so the
#'   dilated rendering cannot show whether the reconstructed censoring
#'   times line up with the ones in the source figure.
#'
#'   Every scalar this function needs is a plain argument — it never reads
#'   `line_width`/`max_y`/`increasing` off a tibble itself. The caller (e.g.
#'   [retro_agent_class]$compare()) pulls each out of whichever tibble
#'   already carries it: `line_width` from `state$data_path`, `max_y`/
#'   `increasing` from `state$data_scaled`.
#' @return `NULL` (invisibly). Called for its side effect of writing
#'   an image file.
#' @param data A tibble with columns `series` (character), `time`, `status` —
#'   reconstructed individual patient data, e.g. `state$data_survival`
#'   (the return value of [retro_data_survival()]).
#' @param output Character scalar, path where the output PNG will be
#'   written.
#' @param layers Character vector specifying the drawing order. Must
#'   contain exactly the unique colors present in `legend$color` (no
#'   duplicates, no extras, no missing). The first element has highest
#'   precedence (drawn last, appears on top).
#' @param background Character scalar, hex color for the background.
#' @param x_axis A tibble with columns `value` (numeric) and `x` (numeric,
#'   pixel column), at least 2 rows, e.g. `state$data_x`.
#' @param y_axis A tibble with columns `value` (numeric) and `y` (numeric,
#'   pixel row), at least 2 rows, e.g. `state$data_y`.
#' @param legend A tibble with columns `series` and `color`, e.g.
#'   `state$data_legend`.
#' @param width,height Integer scalars, canvas size in pixels.
#' @param line_width Integer scalar, line thickness in pixels.
#' @param max_y Numeric scalar, the y-axis scale: `1` for a 0-1
#'   proportion, `100` for a 0-100 percentage.
#' @param increasing Logical scalar, `TRUE` if the plot reads as cumulative
#'   incidence (rising over time) rather than survival (falling over
#'   time).
#' @param censoring Logical scalar. `FALSE` (default) draws the curves
#'   dilated to `line_width` and marks no censoring. `TRUE` draws them one
#'   pixel wide and adds a vertical tick, 2 pixels up and 2 pixels down,
#'   at every reconstructed censoring time.
#' @examples
#'   data <- tibble::tibble(
#'     series = rep("Placebo", 6),
#'     time = c(1, 2, 3, 4, 5, 6),
#'     status = c(1, 0, 1, 0, 1, 0)
#'   )
#'   x_axis <- tibble::tibble(
#'     label = c("A", "B"), value = c(0, 10), x = c(5, 55)
#'   )
#'   y_axis <- tibble::tibble(
#'     label = c("C", "D"), value = c(0, 1), y = c(95, 5)
#'   )
#'   legend <- tibble::tibble(series = "Placebo", color = "#dc3030")
#'   output <- tempfile(fileext = ".png")
#'   retroglyph:::retro_image_layer_survival(
#'     data = data,
#'     output = output,
#'     layers = "#dc3030",
#'     background = "#ffffff",
#'     x_axis = x_axis,
#'     y_axis = y_axis,
#'     legend = legend,
#'     width = 60L,
#'     height = 100L,
#'     line_width = 2L,
#'     max_y = 1,
#'     increasing = FALSE
#'   )
#'   if (interactive()) {
#'     browseURL(output)
#'   }
retro_image_layer_survival <- function(
  data,
  output,
  layers,
  background,
  x_axis,
  y_axis,
  legend,
  width,
  height,
  line_width,
  max_y,
  increasing,
  censoring = FALSE
) {
  stopifnot(
    "data must be a tibble with columns series, time, status" = inherits(
      data,
      "tbl_df"
    ) &&
      all(c("series", "time", "status") %in% names(data)),
    "output must be a single string" = is.character(output) &&
      length(output) == 1L,
    "layers must be a character vector" = is.character(layers) &&
      length(layers) >= 1L,
    "layers must not contain duplicates" = !anyDuplicated(layers),
    "background must be a single string" = is.character(background) &&
      length(background) == 1L,
    "x_axis must be a data frame with columns value and x, >= 2 rows" = is.data.frame(
      x_axis
    ) &&
      all(c("value", "x") %in% names(x_axis)) &&
      nrow(x_axis) >= 2L,
    "y_axis must be a data frame with columns value and y, >= 2 rows" = is.data.frame(
      y_axis
    ) &&
      all(c("value", "y") %in% names(y_axis)) &&
      nrow(y_axis) >= 2L,
    "legend must be a tibble with columns series and color" = inherits(
      legend,
      "tbl_df"
    ) &&
      all(c("series", "color") %in% names(legend)),
    "width must be a single positive number" = is.numeric(width) &&
      length(width) == 1L &&
      width > 0,
    "height must be a single positive number" = is.numeric(height) &&
      length(height) == 1L &&
      height > 0,
    "line_width must be a single positive number" = is.numeric(line_width) &&
      length(line_width) == 1L &&
      line_width > 0,
    "max_y must be a single positive number" = is.numeric(max_y) &&
      length(max_y) == 1L &&
      max_y > 0,
    "increasing must be a single non-missing logical" = is.logical(
      increasing
    ) &&
      length(increasing) == 1L &&
      !is.na(increasing),
    "censoring must be a single non-missing logical" = is.logical(
      censoring
    ) &&
      length(censoring) == 1L &&
      !is.na(censoring)
  )
  legend_colors <- retro_color_rgb(legend$color)
  layers <- retro_color_rgb(layers)
  background <- retro_color_rgb(background)
  missing_colors <- setdiff(legend_colors, layers)
  extra_colors <- setdiff(layers, legend_colors)
  stopifnot(
    "layers must contain all colors in legend" = length(missing_colors) == 0L,
    "layers must not contain colors absent from legend" = length(
      extra_colors
    ) ==
      0L
  )
  width <- as.integer(width)
  height <- as.integer(height)
  # The censoring view draws wire-thin so the ticks stay visible: a tick
  # 2 pixels up and down is swallowed whole by a dilated line.
  half_width <- if (censoring) {
    0L
  } else {
    max(0L, as.integer(floor(line_width / 2)))
  }
  # retro_scale_calibration() normally fits pixel -> data-value; swapping
  # which argument is which fits the inverse, data-value -> pixel, directly.
  x_calibration <- retro_scale_calibration(
    pixel = x_axis$value,
    value = x_axis$x
  )
  y_calibration <- retro_scale_calibration(
    pixel = y_axis$value,
    value = y_axis$y
  )
  pixel_matrix <- matrix(background, nrow = height, ncol = width)
  # Paint from lowest precedence (last) to highest (first).
  # The last color painted wins, so we reverse the layer order.
  for (color in rev(layers)) {
    series_name <- legend$series[match(color, legend_colors)]
    ipd <- data[data$series == series_name, , drop = FALSE]
    pixel_matrix <- retro_layer_survival_draw(
      pixel_matrix = pixel_matrix,
      ipd = ipd,
      color = color,
      x_calibration = x_calibration,
      y_calibration = y_calibration,
      half_width = half_width,
      width = width,
      height = height,
      max_y = max_y,
      increasing = increasing
    )
    # Ticks are painted inside the same layer loop as the curve, so a
    # foreground series' ticks overwrite a background series' curve exactly
    # as its own curve does.
    if (censoring) {
      pixel_matrix <- retro_layer_censoring_draw(
        pixel_matrix = pixel_matrix,
        ipd = ipd,
        color = color,
        x_calibration = x_calibration,
        y_calibration = y_calibration,
        width = width,
        height = height,
        max_y = max_y,
        increasing = increasing
      )
    }
  }
  magick::image_read(pixel_matrix) |>
    magick::image_write(path = output)
  invisible(NULL)
}

#' @title Rebuild one series's Kaplan-Meier step function for plotting
#' @keywords internal
#' @noRd
#' @description Refits a single series' Kaplan-Meier curve from its
#'   reconstructed individual patient data, then expresses it back on the
#'   plot's own scale and orientation (`max_y`/`increasing`) rather than the
#'   canonical 0-1 survival scale `survival::survfit()` returns — the
#'   algebraic inverse of the flip [retro_survival_series()] applies before
#'   reconstruction. Vertices alternate horizontal (constant value, between
#'   jump times) and vertical (a drop or rise at a single jump time), and
#'   the curve extends flat from its last jump out to this series' own last
#'   observed time, event or censoring, which is where
#'   `survival::plot.survfit()` would stop too. That endpoint comes from
#'   `ipd`, so it belongs to this one series and cannot pick up another
#'   series' follow-up: arms whose curves genuinely end early are drawn
#'   ending early. The whole sequence can then be rasterized as
#'   axis-aligned pixel runs by [retro_layer_survival_draw()].
#' @return A data frame with columns `x` (time) and `y` (value on the
#'   plot's own scale), one row per vertex, in drawing order.
#' @param ipd A tibble with columns `time`, `status` — this series' rows
#'   from `data` in [retro_image_layer_survival()].
#' @param max_y Numeric scalar, the y-axis scale: `1` for a 0-1 proportion,
#'   `100` for a 0-100 percentage.
#' @param increasing Logical scalar, `TRUE` if the plot reads as cumulative
#'   incidence (rising over time) rather than survival (falling over
#'   time).
#' @examples
#'   ipd <- tibble::tibble(
#'     time = c(1, 2, 3, 4, 5, 6),
#'     status = c(1, 0, 1, 0, 1, 0)
#'   )
#'   retroglyph:::retro_layer_steps(ipd, max_y = 1, increasing = FALSE)
retro_layer_steps <- function(ipd, max_y, increasing) {
  fit <- survival::survfit(survival::Surv(time, status) ~ 1, data = ipd)
  jumps <- fit$n.event > 0
  jump_times <- fit$time[jumps]
  jump_surv <- fit$surv[jumps]
  rescale <- function(s) {
    if (increasing) max_y - max_y * s else max_y * s
  }
  # fit$time carries censoring times as well as event times, so its maximum
  # is this series' last observation. Deriving the endpoint from ipd rather
  # than from the risk table is what keeps it series-specific: risk tables
  # share their time columns across arms, so the old
  # max(risk_table_series$x) came out the same for every series and padded
  # every curve out to the longest arm's follow-up.
  end_time <- max(fit$time, jump_times, 0)
  prev_time <- 0
  prev_value <- rescale(1)
  vertices <- list(data.frame(x = prev_time, y = prev_value))
  for (index in seq_along(jump_times)) {
    vertices[[length(vertices) + 1L]] <- data.frame(
      x = jump_times[index],
      y = prev_value
    )
    prev_value <- rescale(jump_surv[index])
    vertices[[length(vertices) + 1L]] <- data.frame(
      x = jump_times[index],
      y = prev_value
    )
  }
  vertices[[length(vertices) + 1L]] <- data.frame(x = end_time, y = prev_value)
  do.call(rbind, vertices)
}

#' @title Locate one series's censoring tick marks for plotting
#' @keywords internal
#' @noRd
#' @description Refits a single series' Kaplan-Meier curve from its
#'   reconstructed individual patient data and returns the point on the
#'   curve at every time where at least one patient was censored, expressed
#'   on the plot's own scale and orientation (`max_y`/`increasing`) the same
#'   way [retro_layer_steps()] expresses the curve itself. Each point is the
#'   center of a vertical tick mark that [retro_layer_censoring_draw()]
#'   paints, which is where
#'   `survival::plot.survfit(mark.time = TRUE)` would put one too. Times
#'   where only events occurred are not ticked.
#' @return A data frame with columns `x` (censoring time) and `y` (curve
#'   value on the plot's own scale), one row per censoring time in
#'   ascending order, and zero rows if no patient in `ipd` was censored.
#' @param ipd A tibble with columns `time`, `status` — this series' rows
#'   from `data` in [retro_image_layer_survival()].
#' @param max_y Numeric scalar, the y-axis scale: `1` for a 0-1 proportion,
#'   `100` for a 0-100 percentage.
#' @param increasing Logical scalar, `TRUE` if the plot reads as cumulative
#'   incidence (rising over time) rather than survival (falling over
#'   time).
#' @examples
#'   ipd <- tibble::tibble(
#'     time = c(1, 2, 3, 4, 5, 6),
#'     status = c(1, 0, 1, 0, 1, 0)
#'   )
#'   retroglyph:::retro_layer_censoring_data(
#'     ipd,
#'     max_y = 1,
#'     increasing = FALSE
#'   )
retro_layer_censoring_data <- function(ipd, max_y, increasing) {
  fit <- survival::survfit(survival::Surv(time, status) ~ 1, data = ipd)
  censored <- fit$n.censor > 0
  # fit$surv is the post-jump value at each time, so at a censoring time it
  # is already the height of the flat run the tick sits on.
  value <- fit$surv[censored]
  data.frame(
    x = fit$time[censored],
    y = if (increasing) max_y - max_y * value else max_y * value
  )
}

#' @title Paint one series's Kaplan-Meier step function onto the canvas
#' @keywords internal
#' @noRd
#' @description Takes the vertex sequence [retro_layer_steps()] builds for
#'   one series, maps it from data space into pixel space through the two
#'   per-axis calibrations, and paints every segment between consecutive
#'   vertices with [retro_layer_segment()]. One call paints one layer of
#'   [retro_image_layer_survival()]'s layer loop, which calls this once per
#'   color from lowest precedence to highest, so the returned matrix is the
#'   input matrix with this series' curve overwriting whatever was beneath
#'   it.
#' @return `pixel_matrix` with this series' curve painted in `color`.
#' @param pixel_matrix Character matrix of hex colors, `height` by `width`,
#'   the canvas painted so far.
#' @param ipd A tibble with columns `time`, `status` — this series' rows
#'   from `data` in [retro_image_layer_survival()].
#' @param color Character scalar, hex color to paint this series in.
#' @param x_calibration,y_calibration Lists with numeric `slope` and
#'   `intercept`, the data-value to pixel maps from
#'   [retro_scale_calibration()].
#' @param half_width Integer scalar, pixels to extend on either side of the
#'   curve: `0L` for the wire-thin censoring view, `floor(line_width / 2)`
#'   otherwise.
#' @param width,height Integer scalars, canvas bounds.
#' @param max_y Numeric scalar, the y-axis scale: `1` for a 0-1 proportion,
#'   `100` for a 0-100 percentage.
#' @param increasing Logical scalar, `TRUE` if the plot reads as cumulative
#'   incidence (rising over time) rather than survival (falling over
#'   time).
#' @examples
#'   ipd <- tibble::tibble(time = c(5, 10), status = c(1, 0))
#'   calibration <- list(slope = 1, intercept = 1)
#'   retroglyph:::retro_layer_survival_draw(
#'     pixel_matrix = matrix("#ffffff", nrow = 20L, ncol = 20L),
#'     ipd = ipd,
#'     color = "#dc3030",
#'     x_calibration = calibration,
#'     y_calibration = calibration,
#'     half_width = 0L,
#'     width = 20L,
#'     height = 20L,
#'     max_y = 1,
#'     increasing = FALSE
#'   )
retro_layer_survival_draw <- function(
  pixel_matrix,
  ipd,
  color,
  x_calibration,
  y_calibration,
  half_width,
  width,
  height,
  max_y,
  increasing
) {
  vertices <- retro_layer_steps(ipd, max_y, increasing)
  pixel_x <- as.integer(round(
    x_calibration$slope * vertices$x + x_calibration$intercept
  ))
  pixel_y <- as.integer(round(
    y_calibration$slope * vertices$y + y_calibration$intercept
  ))
  for (index in seq_len(length(pixel_x) - 1L)) {
    segment <- retro_layer_segment(
      pixel_x[index],
      pixel_y[index],
      pixel_x[index + 1L],
      pixel_y[index + 1L],
      half_width,
      width,
      height
    )
    pixel_matrix[cbind(segment$y, segment$x)] <- color
  }
  pixel_matrix
}

#' @title Paint one series's censoring tick marks onto the canvas
#' @keywords internal
#' @noRd
#' @description Takes the censoring points [retro_layer_censoring_data()]
#'   finds for one series, maps them from data space into pixel space
#'   through the two per-axis calibrations, and paints each as a vertical
#'   tick 2 pixels up and 2 pixels down from the curve, 1 pixel wide, with
#'   [retro_layer_segment()]. The sibling of
#'   [retro_layer_survival_draw()], called right after it on the same layer
#'   of [retro_image_layer_survival()]'s layer loop, so a foreground series'
#'   ticks overwrite a background series' curve exactly as its own curve
#'   does. A series with no censored patients leaves the canvas untouched.
#' @return `pixel_matrix` with this series' censoring ticks painted in
#'   `color`.
#' @param pixel_matrix Character matrix of hex colors, `height` by `width`,
#'   the canvas painted so far — including this series' curve.
#' @param ipd A tibble with columns `time`, `status` — this series' rows
#'   from `data` in [retro_image_layer_survival()].
#' @param color Character scalar, hex color to paint this series' ticks in.
#' @param x_calibration,y_calibration Lists with numeric `slope` and
#'   `intercept`, the data-value to pixel maps from
#'   [retro_scale_calibration()].
#' @param width,height Integer scalars, canvas bounds.
#' @param max_y Numeric scalar, the y-axis scale: `1` for a 0-1 proportion,
#'   `100` for a 0-100 percentage.
#' @param increasing Logical scalar, `TRUE` if the plot reads as cumulative
#'   incidence (rising over time) rather than survival (falling over
#'   time).
#' @examples
#'   ipd <- tibble::tibble(time = c(5, 10), status = c(1, 0))
#'   calibration <- list(slope = 1, intercept = 1)
#'   retroglyph:::retro_layer_censoring_draw(
#'     pixel_matrix = matrix("#ffffff", nrow = 20L, ncol = 20L),
#'     ipd = ipd,
#'     color = "#dc3030",
#'     x_calibration = calibration,
#'     y_calibration = calibration,
#'     width = 20L,
#'     height = 20L,
#'     max_y = 1,
#'     increasing = FALSE
#'   )
retro_layer_censoring_draw <- function(
  pixel_matrix,
  ipd,
  color,
  x_calibration,
  y_calibration,
  width,
  height,
  max_y,
  increasing
) {
  ticks <- retro_layer_censoring_data(ipd, max_y, increasing)
  tick_x <- as.integer(round(
    x_calibration$slope * ticks$x + x_calibration$intercept
  ))
  tick_y <- as.integer(round(
    y_calibration$slope * ticks$y + y_calibration$intercept
  ))
  for (index in seq_along(tick_x)) {
    segment <- retro_layer_segment(
      tick_x[index],
      tick_y[index] - 2L,
      tick_x[index],
      tick_y[index] + 2L,
      0L,
      width,
      height
    )
    pixel_matrix[cbind(segment$y, segment$x)] <- color
  }
  pixel_matrix
}

#' @title Pixel coordinates for one axis-aligned curve segment
#' @keywords internal
#' @noRd
#' @description Rasterizes a single horizontal or vertical segment between
#'   two pixel-space points into every pixel it covers, thickened by
#'   `half_width` pixels on either side (perpendicular to the segment's
#'   direction) and clamped to the canvas bounds. Because axis calibration
#'   is an independent affine map per axis, a horizontal or vertical
#'   segment in data space (see [retro_layer_steps()], or the already
#'   axis-aligned trace in [retro_image_layer_trace()]) stays horizontal or
#'   vertical in pixel space too, so no general line-drawing algorithm is
#'   needed. Shared by [retro_layer_survival_draw()],
#'   [retro_layer_censoring_draw()], and [retro_image_layer_trace()].
#' @param x1,y1,x2,y2 Integer scalars, pixel-space segment endpoints.
#'   Exactly one of `x1 == x2` or `y1 == y2` must hold.
#' @param half_width Integer scalar, pixels to extend on either side.
#' @param width,height Integer scalars, canvas bounds.
#' @return A list with integer vectors `x` and `y`, one element per pixel.
#' @examples
#'   retroglyph:::retro_layer_segment(
#'     x1 = 5L, y1 = 10L, x2 = 5L, y2 = 12L,
#'     half_width = 1L, width = 20L, height = 20L
#'   )
retro_layer_segment <- function(x1, y1, x2, y2, half_width, width, height) {
  if (y1 == y2) {
    columns <- seq.int(max(1L, min(x1, x2)), min(width, max(x1, x2)))
    rows <- seq.int(max(1L, y1 - half_width), min(height, y1 + half_width))
  } else {
    rows <- seq.int(max(1L, min(y1, y2)), min(height, max(y1, y2)))
    columns <- seq.int(max(1L, x1 - half_width), min(width, x1 + half_width))
  }
  grid <- expand.grid(x = columns, y = rows)
  list(x = as.integer(grid$x), y = as.integer(grid$y))
}
