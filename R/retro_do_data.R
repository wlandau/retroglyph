#' @title Read data off a distilled Kaplan-Meier image
#' @keywords internal
#' @noRd
#' @description Turn a distilled image - curves only, on a flat
#'   background - into numbers: recover the pixels each curve lost to
#'   occlusion, read the curves off the axes as step functions, and
#'   reconstruct individual patient survival data from the risk table.
#'   This is the last stage of the reconstruction pipeline.
#' @details Runs in this order, each step an exported function in its own
#'   right:
#'
#'   1. [retro_data_risk()] validates and structures the risk table.
#'   2. [retro_data_events()] validates and structures the optional
#'      per-series total events counts.
#'      Both of these run first, before any image work, so a misread digit
#'      surfaces immediately instead of after path-finding has spent its
#'      time.
#'   3. [retro_data_path()] recovers curve pixels hidden where curves
#'      overlap, via shortest paths over a graph of foreground pixel
#'      adjacencies.
#'   4. [retro_data_scale()] converts each curve's already-thin pixel
#'      trace to data-space coordinates using the axis calibration, and
#'      normalizes the result into a decreasing 0-1 survival curve
#'      starting at `(0, 1)`.
#'   5. [retro_data_survival()] inverts the Kaplan-Meier step function
#'      against the risk table's at-risk counts to recover per-patient
#'      times and censoring status.
#' @return A named list with elements:
#'   * `path` — Tibble with columns `x`, `y`, `color`, `line_width`,
#'     `width`, `height`, one row per curve pixel.
#'   * `risk` — Tibble of the structured risk table.
#'   * `events` — Tibble of per-series total events counts, or `NULL` if no
#'     series reported one.
#'   * `scaled` — Tibble with columns `series`, `x`, `y`, `max_y`,
#'     `increasing`: each curve's pixel trace scaled into data
#'     coordinates and normalized to a decreasing 0-1 survival curve;
#'     `max_y`/`increasing` describe the original plot's scale/orientation,
#'     not `y` itself.
#'   * `survival` — Tibble with columns `series`, `time`, `status`.
#' @param input Character scalar, path to the distilled image file (the
#'   `clean` element of [retro_do_distill()]'s return value).
#' @param data_label A tibble of labeled numbers from
#'   [retro_do_label()], supplying the pixel positions of the
#'   calibration ticks named in `x_axis` and `y_axis`.
#' @param legend A tibble with columns `series`, `color`, and `reference`,
#'   from [retro_do_distill()], mapping each curve color to its series
#'   name. Its `series` values are used to validate and label every
#'   `series` column in the result.
#' @param x_axis A tibble with columns `label` and `value`, at least 2
#'   rows, from [retro_do_distill()].
#' @param y_axis A tibble with columns `label` and `value`, at least 2
#'   rows, from [retro_do_distill()].
#' @param risk_patients Numeric vector of at-risk counts. Required -
#'   every image must have a risk table. See [retro_data_risk()].
#' @param risk_series Character vector of series names, one per
#'   risk table entry. Required.
#' @param risk_x Numeric vector of time coordinates, one per risk table
#'   entry. Required.
#' @param events_total Numeric vector of per-series total events counts, or
#'   `NULL`. Optional, and optional per series. See
#'   [retro_data_events()].
#' @param events_series Character vector of series names, one per
#'   `events_total` entry, or `NULL`. See
#'   [retro_data_events()].
#' @examples
#'   # A step curve on a white background, standing in for a distilled
#'   # Kaplan-Meier image: two flat runs joined by a vertical drop, so the
#'   # whole curve is one connected run of pixels.
#'   pixels <- matrix("#ffffff", nrow = 20L, ncol = 40L)
#'   pixels[5L, 1L:20L] <- "#dc3030"
#'   pixels[5L:10L, 20L] <- "#dc3030"
#'   pixels[10L, 20L:40L] <- "#dc3030"
#'   input <- tempfile(fileext = ".png")
#'   magick::image_write(magick::image_read(pixels), input)
#'   data_label <- tibble::tibble(
#'     label = c("A", "B", "C", "D"),
#'     word = c("0", "10", "0", "1"),
#'     confidence = rep(90, 4L),
#'     x = c(1, 40, 1, 1),
#'     y = c(20, 20, 20, 1),
#'     x1 = c(1L, 39L, 1L, 1L),
#'     x2 = c(2L, 40L, 2L, 2L),
#'     y1 = c(19L, 19L, 19L, 1L),
#'     y2 = c(20L, 20L, 20L, 2L)
#'   )
#'   result <- retroglyph:::retro_do_data(
#'     input = input,
#'     data_label = data_label,
#'     legend = tibble::tibble(series = "Placebo", color = "#dc3030", reference = TRUE),
#'     x_axis = tibble::tibble(label = c("A", "B"), value = c(0, 10)),
#'     y_axis = tibble::tibble(label = c("C", "D"), value = c(0, 1)),
#'     risk_patients = c(100, 60),
#'     risk_series = c("Placebo", "Placebo"),
#'     risk_x = c(0, 10),
#'     events_total = 35,
#'     events_series = "Placebo"
#'   )
#'   print(result$risk)
#'   print(result$events)
#'   print(head(result$scaled))
#'   print(head(result$survival))
retro_do_data <- function(
  input,
  data_label,
  legend,
  x_axis,
  y_axis,
  risk_patients,
  risk_series,
  risk_x,
  events_total = NULL,
  events_series = NULL
) {
  stopifnot(
    "input must be a single string" = is.character(input) &&
      length(input) == 1L,
    "input must exist" = file.exists(input),
    "legend must be a tibble with columns series and color" = inherits(
      legend,
      "tbl_df"
    ) &&
      all(c("series", "color") %in% names(legend))
  )
  # Validate the numbers the model read off the image before path-finding:
  # a misread digit is worth reporting straight away rather than after
  # Dijkstra has run.
  risk <- retro_data_risk(
    risk_patients = risk_patients,
    risk_series = risk_series,
    risk_x = risk_x,
    series_legend = legend$series
  )
  events <- retro_data_events(
    events_total = events_total,
    events_series = events_series,
    series_legend = legend$series
  )
  path <- retro_data_path(input)
  if (nrow(path) == 0L) {
    stop(
      "Path-finding recovered no curve pixels from the distilled image. ",
      "The distill step removed everything: check the series colors ",
      "against the image, since a color that matches no pixel in the ",
      "image leaves nothing behind to trace.",
      call. = FALSE
    )
  }
  scaled <- retro_data_scale(
    data_path = path,
    data_label = data_label,
    x_axis = x_axis,
    y_axis = y_axis,
    legend = legend
  )
  survival <- retro_data_survival(
    scaled = scaled,
    risk_table = risk,
    events_table = events
  )
  list(
    path = path,
    risk = risk,
    events = events,
    scaled = scaled,
    survival = survival
  )
}
