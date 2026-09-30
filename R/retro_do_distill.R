#' @title Distill a Kaplan-Meier image down to its curves
#' @keywords internal
#' @noRd
#' @description Reduce a source image to nothing but its Kaplan-Meier
#'   curves on a flat background: isolate the plotting panel from the axis
#'   calibration ticks, map every pixel to a background-plus-series
#'   palette, and drop the short connected components that palette
#'   mapping cannot catch. This is the whole middle of the reconstruction
#'   pipeline in one call.
#' @details Runs five steps in order, each an exported function in its own
#'   right:
#'
#'   1. [retro_data_calibrate()] validates the two labeled ticks per axis
#'      against `data_label` and attaches their pixel positions.
#'   2. [retro_data_panel()] extrapolates those ticks to predict where the
#'      axes cross and detects the actual axis lines near that point.
#'   3. [retro_image_mask()] masks out the axis lines and everything
#'      outside them, leaving the panel.
#'   4. [retro_image_classify()] maps every panel pixel to the nearest
#'      color in `c(background, series, garbage)`, then flattens the
#'      garbage colors to background.
#'   5. [retro_image_clean()] removes foreground components too
#'      horizontally short to be a curve - legend keys, text fragments,
#'      stray marks - at that function's default span threshold. The
#'      threshold is not exposed here on purpose: it is a hard call to
#'      make from an image, and getting it wrong deletes a curve outright,
#'      which is worse than the debris a permissive threshold leaves for
#'      path-finding to ignore.
#'
#'   The returned legend keeps `series_names`' exact order - it is never
#'   sorted. The `reference` argument identifies which series is the
#'   reference (e.g. a placebo or standard-of-care arm in a clinical
#'   trial, or simply the denominator for hazard ratio calculations): it
#'   need not be first in `series_names`. The `legend` tibble carries a
#'   logical `reference` column (`TRUE` for the reference series, `FALSE`
#'   for others) so downstream code can identify it without relying on
#'   positional conventions. `legend$series` is a plain character vector;
#'   every object built downstream in [retro_do_data()] (`data_risk`,
#'   `data_scaled`, `data_survival`) uses the same series labels.
#'
#'   `counts` is still measured on the *cleaned* image, one entry per
#'   `series`/`series_names` element, but only to report each series'
#'   surviving pixel total - it plays no part in ordering. Counting
#'   earlier would rank a series by debris as much as by curve: a legend
#'   key is a solid block of a series' color and can easily outweigh the
#'   thin curve it stands for. Because cleaning happens inside this
#'   function, there is one count, on the only image where the count
#'   means what it says, and a count of 0 is the clearest sign a series
#'   was classified away or cleaned out entirely.
#' @return A named list with elements:
#'   * `clean` — Character, path to the cleaned image (`output_clean`).
#'   * `panel` — Character, path to the panel image (`output_panel`).
#'   * `classified` — Character, path to the classified image
#'     (`output_classified`).
#'   * `axes` — Single-row tibble of the panel bounding box, from
#'     [retro_data_panel()].
#'   * `x_axis` — Two-row tibble of x-axis calibration ticks, from
#'     [retro_data_calibrate()].
#'   * `y_axis` — Two-row tibble of y-axis calibration ticks.
#'   * `background` — Character, the background color as a 6-character
#'     RGB hex string.
#'   * `legend` — Tibble with one row per series, in the exact order of
#'     `series`/`series_names`, and columns `series` (character),
#'     `color` (character, 6-digit RGB hex), and `reference` (logical,
#'     `TRUE` for the reference series).
#'   * `counts` — Integer vector of surviving pixel counts, one per row of
#'     `legend` and in the same order. A count of 0 means that series was
#'     classified away or cleaned out of the image entirely, which is worth
#'     knowing and hard to see by looking. Does not affect `legend`'s row
#'     order.
#' @param input Character scalar, path to the quantized image file
#'   (output of [retro_image_quantize()]).
#' @param output_panel Character scalar, path where the intermediate
#'   panel image will be written. Nothing
#'   downstream reads it; it exists so a reconstruction that goes wrong
#'   can be traced to the step that broke it.
#' @param output_classified Character scalar, path where the intermediate
#'   classified image will be written, kept for the same reason as
#'   `output_panel`.
#' @param output_clean Character scalar, path where the final cleaned
#'   image will be written. This is the image worth looking at.
#' @param data_label A tibble of labeled numbers from
#'   [retro_do_label()] with columns `label`, `x`, `y`, `x1`, `x2`,
#'   `y1`, `y2`. `x_label` and `y_label` are resolved against its `label`
#'   column.
#' @param x_label Character vector of length 2, the letter labels of the
#'   two x-axis calibration ticks.
#' @param x_value Numeric vector of length 2, the x-axis values printed
#'   at `x_label`.
#' @param y_label Character vector of length 2, the letter labels of the
#'   two y-axis calibration ticks.
#' @param y_value Numeric vector of length 2, the y-axis values printed
#'   at `y_label`.
#' @param background Character scalar, hex color of the image background.
#' @param garbage Character vector of hex colors for non-data elements
#'   (confidence bands, shading, grid lines, annotations). Pixels mapping
#'   to these are set to background.
#' @param series Character vector of hex colors, one per Kaplan-Meier
#'   curve.
#' @param series_names Character vector the same length as `series`,
#'   naming the series each color belongs to, in whatever order
#'   the caller finds natural. The `reference` argument identifies the
#'   reference series separately; `series_names` does not encode that
#'   information positionally.
#' @param reference Character scalar, the name of the reference series.
#'   Must be one of the entries in `series_names`. In a clinical trial
#'   with multiple study arms, this is typically the control arm.
#'   This series is used as the reference/denominator in hazard ratio
#'   calculations.
#' @examples
#'   source <- system.file("simulation.png", package = "retroglyph")
#'   cleaned <- tempfile(fileext = ".png")
#'   # Four labeled ticks, as retro_do_label() would have found them.
#'   data_label <- tibble::tibble(
#'     label = c("F", "M", "A", "E"),
#'     word = c("0", "35", "0.8", "0.2"),
#'     confidence = rep(90, 4L),
#'     x = c(376, 1154.5, 340, 339),
#'     y = c(570, 570, 116, 427),
#'     x1 = c(370L, 1143L, 324L, 324L),
#'     x2 = c(382L, 1166L, 355L, 354L),
#'     y1 = c(561L, 561L, 107L, 418L),
#'     y2 = c(579L, 579L, 126L, 436L)
#'   )
#'   result <- retroglyph:::retro_do_distill(
#'     input = source,
#'     output_clean = cleaned,
#'     data_label = data_label,
#'     x_label = c("F", "M"),
#'     x_value = c(0, 35),
#'     y_label = c("A", "E"),
#'     y_value = c(0.8, 0.2),
#'     background = "#FFFFFF",
#'     series = c("#DC3030", "#3030A0"),
#'     series_names = c("Placebo", "Drug"),
#'     reference = "Placebo"
#'   )
#'   print(result$legend)
#'   if (interactive()) {
#'     browseURL(result$image_clean)
#'   }
retro_do_distill <- function(
  input,
  output_panel = tempfile(fileext = ".png"),
  output_classified = tempfile(fileext = ".png"),
  output_clean = tempfile(fileext = ".png"),
  data_label,
  x_label,
  x_value,
  y_label,
  y_value,
  background,
  garbage = character(0L),
  series,
  series_names,
  reference
) {
  stopifnot(
    "input must be a single string" = is.character(input) &&
      length(input) == 1L,
    "input must exist" = file.exists(input),
    "output_clean must be a single string" = is.character(output_clean) &&
      length(output_clean) == 1L,
    "output_panel must be a single string" = is.character(output_panel) &&
      length(output_panel) == 1L,
    "output_classified must be a single string" = is.character(
      output_classified
    ) &&
      length(output_classified) == 1L,
    "background must be a single hex color" = length(background) == 1L,
    "series must have at least one hex color" = length(series) >= 1L,
    "series and series_names must have the same length" = length(series) ==
      length(series_names),
    "reference must be a single non-empty string" = is.character(reference) &&
      length(reference) == 1L &&
      !is.na(reference) &&
      nzchar(reference),
    "reference must be one of series_names" = reference %in% series_names
  )
  if (is.null(garbage)) {
    garbage <- character(0L)
  }
  stopifnot(
    "background, series, and garbage must be valid hex colors" = all(
      retro_color_valid(c(background, series, garbage))
    )
  )
  calibration <- retro_data_calibrate(
    x_label = x_label,
    x_value = x_value,
    y_label = y_label,
    y_value = y_value,
    data_label = data_label
  )
  panel <- retro_data_panel(
    input = input,
    x_axis = calibration$x_axis,
    y_axis = calibration$y_axis
  )
  retro_image_mask(
    input = input,
    output = output_panel,
    x1 = panel$x1 + panel$pad_x1,
    x2 = panel$x2 + panel$pad_x2,
    y1 = panel$y1 + panel$pad_y1,
    y2 = panel$y2 + panel$pad_y2,
    mask = TRUE,
    initial = FALSE
  )
  retro_image_classify(
    input = output_panel,
    output = output_classified,
    background = background,
    garbage = garbage,
    series = series
  )
  retro_image_clean(
    input = output_classified,
    output = output_clean,
    panel = panel
  )
  colors <- retro_color_rgb(series)
  # Counted on the cleaned image, not the raw one: debris of a series'
  # own color (e.g. a legend key) would otherwise pad its total. This
  # count is reported to the model so it can catch a series classified
  # away or cleaned to 0 pixels; it never affects row order.
  counts <- retro_color_count(output_clean, colors)
  # legend keeps series_names' exact order - no sort of any kind - so
  # every downstream object (data_risk, data_scaled, data_survival) that
  # uses the same series labels groups consistently.
  legend <- tibble::tibble(
    series = as.character(series_names),
    color = colors,
    reference = series_names == reference
  )
  list(
    image_panel = output_panel,
    image_classified = output_classified,
    image_clean = output_clean,
    data_panel = panel,
    x_axis = calibration$x_axis,
    y_axis = calibration$y_axis,
    background = retro_color_rgb(background),
    legend = legend,
    counts = as.integer(counts)
  )
}
