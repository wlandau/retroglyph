#' @title Render the digitized survival curve trace with a specified layer
#'   order
#' @keywords internal
#' @noRd
#' @description Take each series' already-digitized pixel trace, express it
#'   back on the plot's own scale and orientation, transform it into pixel
#'   space, and paint it onto a canvas — painting curve layers in a
#'   specified order of precedence. The first color in `layers` is drawn on
#'   top (highest precedence); subsequent colors are layered beneath.
#'   Writes the result to a PNG file.
#'
#'   This renders the raw digitized trace (`state$data_scaled`), *before*
#'   `IPDfromKM`'s Kaplan-Meier reconstruction - the sibling
#'   [retro_image_layer_survival()] renders the *reconstructed* data
#'   (`state$data_survival`) instead, after a fresh Kaplan-Meier refit.
#'   Comparing the two isolates whether a disagreement with the source
#'   image traces back to retroglyph's own digitization or to
#'   `IPDfromKM`'s reconstruction. Unlike
#'   [retro_image_layer_survival()], this function does no refitting: each
#'   series' `x`/`y` rows in `data` are already an axis-aligned,
#'   corner-preserving step-function vertex sequence (from
#'   [retro_data_path()]'s skeletonization and decimation), sorted by `x`
#'   ascending with ties broken by `y` descending, so they are used
#'   directly as the vertices to connect.
#'
#'   Every scalar this function needs is a plain argument — it never reads
#'   `line_width`/`max_y`/`increasing` off a tibble itself. The caller (e.g.
#'   [retro_agent_class]$compare()) pulls each out of whichever tibble
#'   already carries it: `line_width` from `state$data_path`, `max_y`/
#'   `increasing` from `state$data_scaled`.
#' @return `NULL` (invisibly). Called for its side effect of writing
#'   an image file.
#' @param data A tibble with columns `series` (character), `x`, `y` — each
#'   curve's pixel trace scaled into data coordinates, e.g.
#'   `state$data_scaled` (the return value of [retro_data_scale()]), sorted
#'   by series then `x` ascending.
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
#' @examples
#'   data <- tibble::tibble(
#'     series = rep("Placebo", 4),
#'     x = c(0, 5, 5, 10),
#'     y = c(1, 1, 0.5, 0.5)
#'   )
#'   x_axis <- tibble::tibble(
#'     label = c("A", "B"), value = c(0, 10), x = c(5, 55)
#'   )
#'   y_axis <- tibble::tibble(
#'     label = c("C", "D"), value = c(0, 1), y = c(95, 5)
#'   )
#'   legend <- tibble::tibble(series = "Placebo", color = "#dc3030")
#'   output <- tempfile(fileext = ".png")
#'   retroglyph:::retro_image_layer_trace(
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
retro_image_layer_trace <- function(
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
  increasing
) {
  stopifnot(
    "data must be a tibble with columns series, x, y" = inherits(
      data,
      "tbl_df"
    ) &&
      all(c("series", "x", "y") %in% names(data)),
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
      !is.na(increasing)
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
  half_width <- max(0L, as.integer(floor(line_width / 2)))
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
    vertices <- data[data$series == series_name, , drop = FALSE]
    original_y <- if (increasing) {
      max_y - max_y * vertices$y
    } else {
      max_y * vertices$y
    }
    pixel_x <- as.integer(round(
      x_calibration$slope * vertices$x + x_calibration$intercept
    ))
    pixel_y <- as.integer(round(
      y_calibration$slope * original_y + y_calibration$intercept
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
  }
  magick::image_read(pixel_matrix) |>
    magick::image_write(path = output)
  invisible(NULL)
}
