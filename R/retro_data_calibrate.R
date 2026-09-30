#' @title Validate and resolve axis calibration ticks
#' @keywords internal
#' @noRd
#' @description Validate the model's set-of-mark selection of x-axis and
#'   y-axis calibration ticks and resolve them against OCR findings. The
#'   model picks exactly two OCR-labeled ticks per axis, and those two
#'   points fix the affine map from pixel space to data space (and, in
#'   [retro_data_panel()], predict where the axes should cross). Each
#'   axis's two ticks are also checked for being genuinely on that axis:
#'   x-axis ticks must sit side by side, at the same height; y-axis ticks
#'   must sit stacked, at the same left-right position. See
#'   [retro_calibrate_assert_aligned()] for the exact geometric test.
#' @return A named list with elements:
#'   * `x_axis` — A tibble with one row per x-axis calibration tick,
#'     columns `label`, `value`, `x`, `y` (pixel centroid), `x1`, `x2`,
#'     `y1`, `y2` (pixel bounding box), sorted by increasing `value`.
#'     Exactly 2 rows.
#'   * `y_axis` — A tibble with one row per y-axis calibration tick, same
#'     columns as `x_axis`, sorted by increasing `value`. Exactly 2 rows.
#' @param x_label Character vector of length 2, the letter labels from
#'   OCR for the two x-axis calibration ticks.
#' @param x_value Numeric vector of length 2, the x-axis values
#'   corresponding to `x_label`.
#' @param y_label Character vector of length 2, the letter labels from
#'   OCR for the two y-axis calibration ticks.
#' @param y_value Numeric vector of length 2, the y-axis values
#'   corresponding to `y_label`.
#' @param data_label Tibble of OCR findings (from `state$data_label` /
#'   [retro_do_label()]) with columns `label`, `x`, `y`, `x1`, `x2`,
#'   `y1`, `y2`. `x_label`/`y_label` are validated against `data_label$label`
#'   and the matching pixel centroid/bounding box columns are attached to
#'   the returned tibbles. `x_label` and `y_label` must also be disjoint:
#'   the corner tick where the axes meet is often OCR'd as a single "0"
#'   label, and reusing that one label for both axes would calibrate the
#'   y-axis with an x-axis tick's pixel position, silently corrupting the
#'   y-axis map.
#' @examples
#'   data_label <- tibble::tibble(
#'     label = c("A", "B", "C", "D"),
#'     x = c(20, 90, 15, 15), y = c(80, 80, 80, 10),
#'     x1 = c(18, 88, 13, 13), x2 = c(22, 92, 17, 17),
#'     y1 = c(78, 78, 78, 8), y2 = c(82, 82, 82, 12)
#'   )
#'   retroglyph:::retro_data_calibrate(
#'     x_label = c("A", "B"),
#'     x_value = c(0, 12),
#'     y_label = c("C", "D"),
#'     y_value = c(0, 1),
#'     data_label = data_label
#'   )
retro_data_calibrate <- function(
  x_label,
  x_value,
  y_label,
  y_value,
  data_label
) {
  if (length(x_label) != length(x_value) || length(x_label) != 2L) {
    stop(
      "x_label and x_value must both have length exactly 2 - pick two ",
      "OCR-labeled x-axis ticks to calibrate the x-axis."
    )
  }
  if (length(y_label) != length(y_value) || length(y_label) != 2L) {
    stop(
      "y_label and y_value must both have length exactly 2 - pick two ",
      "OCR-labeled y-axis ticks to calibrate the y-axis."
    )
  }
  unknown_labels <- setdiff(c(x_label, y_label), data_label$label)
  if (length(unknown_labels) > 0L) {
    stop(
      "Unknown labels not in OCR data: ",
      paste(unknown_labels, collapse = ", ")
    )
  }
  shared_labels <- intersect(x_label, y_label)
  if (length(shared_labels) > 0L) {
    stop(
      "x_label and y_label share labels: ",
      paste(shared_labels, collapse = ", "),
      " - each axis needs its own two ticks. A label used for both axes ",
      "calibrates the y-axis with an x-axis tick's pixel position (or ",
      "vice versa), which is almost always wrong even when that label's ",
      "value looks correct for both axes."
    )
  }
  x_axis <- retro_calibrate_resolve(x_label, as.numeric(x_value), data_label)
  y_axis <- retro_calibrate_resolve(y_label, as.numeric(y_value), data_label)
  retro_calibrate_assert_aligned(x_axis, "x-axis")
  retro_calibrate_assert_aligned(y_axis, "y-axis")
  list(
    x_axis = x_axis[order(x_axis$value), ],
    y_axis = y_axis[order(y_axis$value), ]
  )
}

#' @title Resolve axis tick labels/values against OCR findings
#' @keywords internal
#' @noRd
#' @description Attach pixel centroid and bounding box columns from OCR
#'   findings to a set of tick labels/values, by matching on `label`.
#' @param label Character vector of length 2, tick labels.
#' @param value Numeric vector of length 2, tick values.
#' @param data_label Tibble of OCR findings with columns `label`, `x`, `y`,
#'   `x1`, `x2`, `y1`, `y2`.
#' @return A tibble with columns `label`, `value`, `x`, `y`, `x1`, `x2`,
#'   `y1`, `y2`.
#' @examples
#'   data_label <- tibble::tibble(
#'     label = c("A", "B", "C"),
#'     x = c(20, 90, 15), y = c(80, 80, 10),
#'     x1 = c(18, 88, 13), x2 = c(22, 92, 17),
#'     y1 = c(78, 78, 8), y2 = c(82, 82, 12)
#'   )
#'   result <- retroglyph:::retro_calibrate_resolve(
#'     label = c("A", "B"),
#'     value = c(0, 12),
#'     data_label = data_label
#'   )
#'   print(result)
retro_calibrate_resolve <- function(label, value, data_label) {
  matched <- match(label, data_label$label)
  tibble::tibble(
    label = label,
    value = value,
    x = data_label$x[matched],
    y = data_label$y[matched],
    x1 = data_label$x1[matched],
    x2 = data_label$x2[matched],
    y1 = data_label$y1[matched],
    y2 = data_label$y2[matched]
  )
}

#' @title Assert that an axis's two calibration ticks are aligned
#' @keywords internal
#' @noRd
#' @description Two ticks calibrating the same axis should sit in a
#'   straight line perpendicular to that axis: two x-axis ticks side by
#'   side, at the same height; two y-axis ticks stacked, at the same
#'   left-right position. This checks that each tick's pixel centroid, on
#'   the dimension perpendicular to the axis, falls inside the other
#'   tick's bounding box on that same dimension. It is a coarse guard
#'   against a tick accidentally picked from the wrong axis, or from
#'   somewhere else in the image entirely - a mistake the affine
#'   calibration in [retro_data_calibrate()] cannot otherwise detect,
#'   because two arbitrary points always fix *some* line.
#' @param axis A tibble with exactly 2 rows, as built by
#'   [retro_calibrate_resolve()]: columns `label`, `x`, `y`, `x1`, `x2`,
#'   `y1`, `y2`.
#' @param name Character scalar, `"x-axis"` or `"y-axis"`, naming which
#'   axis `axis` calibrates. Selects which dimension is checked (`y` for
#'   the x-axis, `x` for the y-axis) and shapes the error message.
#' @return `axis`, invisibly, if its two ticks are aligned. Errors
#'   otherwise.
#' @examples
#'   axis <- tibble::tibble(
#'     label = c("A", "B"), value = c(0, 12),
#'     x = c(20, 90), y = c(80, 80),
#'     x1 = c(18, 88), x2 = c(22, 92),
#'     y1 = c(77, 77), y2 = c(83, 83)
#'   )
#'   retroglyph:::retro_calibrate_assert_aligned(axis, "x-axis")
retro_calibrate_assert_aligned <- function(axis, name) {
  if (identical(name, "x-axis")) {
    centroid <- axis$y
    lower <- axis$y1
    upper <- axis$y2
    arrangement <- "side by side, at the same height"
    dimension <- "height"
  } else {
    centroid <- axis$x
    lower <- axis$x1
    upper <- axis$x2
    arrangement <- "stacked on top of each other, at the same left-right position"
    dimension <- "left-right position"
  }
  aligned <- centroid[1] >= lower[2] &&
    centroid[1] <= upper[2] &&
    centroid[2] >= lower[1] &&
    centroid[2] <= upper[1]
  if (!aligned) {
    stop(
      "The two ",
      name,
      " calibration ticks ('",
      axis$label[1],
      "' and '",
      axis$label[2],
      "') do not appear to be ",
      arrangement,
      " - each tick's pixel centroid should fall within the other tick's ",
      "bounding-box ",
      dimension,
      ". This usually means one of the two picked boxes is not actually a ",
      "tick on the ",
      name,
      ". Pick two boxes that are genuinely both on the ",
      name,
      "."
    )
  }
  invisible(axis)
}
