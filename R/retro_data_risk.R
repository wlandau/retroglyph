#' @title Validate and structure a risk table reading
#' @keywords internal
#' @noRd
#' @description Validate the model's reading of the numbers at risk for a
#'   Kaplan-Meier figure, read off the unmodified source image using the
#'   model's visual and color reasoning (it does not use OCR set-of-mark
#'   labels) or taken from what the user stated,
#'   and return a structured tibble. Only required to be internally
#'   consistent (series names match quantization, every series has at
#'   least one entry) - not required to be consistent with the axis
#'   calibration ticks, since the time points read may fall well
#'   outside the two points chosen for calibration.
#'
#'   This function reads the numbers at risk and nothing else. A series'
#'   total number of events is a separate, optional, per-series quantity
#'   handled by [retro_data_events()], even when the source of that number
#'   is a cumulative events row printed inside the risk table itself.
#'
#'   The entries need not form a complete risk table. Any
#'   `(patients, series, x)` triples will do, and no shape is
#'   special-cased here or downstream, which is what makes the input this
#'   flexible: a full table, one entry per series at `x = 0` (each
#'   series' total number of patients), a start and an end per series, or
#'   differing time points across series are all ordinary readings. The
#'   one entry every series needs is a floor, not a target - more entries
#'   constrain the reconstruction further.
#' @return A tibble with one row per entry, columns `patients`
#'   (integer), `series` (character), and `x`,
#'   sorted by `series_legend` order then increasing `x`. Never returns
#'   `NULL`: some reading is always required, though it may be as small as
#'   one row per series.
#' @param risk_patients Numeric vector, the number of patients at risk
#'   (still under observation) at each entry's time point. Required.
#'   Read off the source image or taken from the user's prompt.
#'   Must have the same length as `risk_series` and `risk_x`.
#' @param risk_series Character vector, the series name for each
#'   entry. Required. Every entry is required (no empty
#'   strings) and must match one of `series_legend`.
#' @param risk_x Numeric vector, the x-axis (time) coordinate each entry
#'   was read at. Required. It need not fall within the range
#'   spanned by the two axis calibration ticks. `0` for a series' total
#'   number of patients.
#' @param series_legend Character vector, valid series
#'   names from the legend. Used to validate that
#'   every entry in `risk_series` names a known series and that every series
#'   has at least one entry.
#'   Also used to sort the series in the output `tibble`.
#' @examples
#'   # A risk table at two time points per series.
#'   retroglyph:::retro_data_risk(
#'     risk_patients = c(100, 60, 95, 45),
#'     risk_series = rep(c("Placebo", "Drug 10mg"), each = 2L),
#'     risk_x = c(0, 12, 0, 12),
#'     series_legend = c("Placebo", "Drug 10mg")
#'   )
#'   # One shape among many: a starting and ending sample size per series,
#'   # with the series reporting different time points and different
#'   # numbers of them. A lone entry per series at x = 0 (each series'
#'   # total number of patients) is equally valid.
#'   retroglyph:::retro_data_risk(
#'     risk_patients = c(482, 300, 210, 479),
#'     risk_series = c("Placebo", "Placebo", "Placebo", "Drug 10mg"),
#'     risk_x = c(0, 6, 12, 0),
#'     series_legend = c("Placebo", "Drug 10mg")
#'   )
retro_data_risk <- function(
  risk_patients,
  risk_series,
  risk_x,
  series_legend
) {
  risk_args <- list(
    risk_patients = risk_patients,
    risk_series = risk_series,
    risk_x = risk_x
  )
  risk_lengths <- lengths(risk_args)
  if (
    length(unique(risk_lengths)) != 1L || risk_lengths[["risk_patients"]] < 1L
  ) {
    stop(
      "risk_patients, risk_series, and risk_x must all have ",
      "the same length, at least 1."
    )
  }
  if (any(!nzchar(risk_series))) {
    stop("risk_series must be non-empty for every risk table entry.")
  }
  series_legend <- unique(as.character(series_legend))
  invalid_series <- setdiff(risk_series, series_legend)
  if (length(invalid_series) > 0L) {
    stop(
      "Invalid series names: ",
      paste(invalid_series, collapse = ", "),
      ". Must be one of: ",
      paste(series_legend, collapse = ", ")
    )
  }
  missing_series <- setdiff(series_legend, risk_series)
  if (length(missing_series) > 0L) {
    stop(
      "Risk table must have entries for every series. Missing: ",
      paste(missing_series, collapse = ", ")
    )
  }
  risk_table <- tibble::tibble(
    patients = as.integer(risk_patients),
    series = as.character(risk_series),
    x = as.numeric(risk_x)
  )
  permutation <- match(risk_table$series, series_legend)
  risk_table[order(permutation, risk_table$x), ]
}
