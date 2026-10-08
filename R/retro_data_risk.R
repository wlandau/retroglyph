#' @title Validate and structure a risk table reading
#' @keywords internal
#' @noRd
#' @description Validate the model's reading of a Kaplan-Meier risk table,
#'   read directly from the unmodified source image using the model's
#'   visual and color reasoning (it does not use OCR set-of-mark labels),
#'   and return a structured tibble. Only required to be internally
#'   consistent (series names match quantization, every series has at
#'   least one entry) - not required to be consistent with the axis
#'   calibration ticks, since the risk table's time points may fall well
#'   outside the two points chosen for calibration. Series may report
#'   different numbers of time points, since some risk tables leave later
#'   columns blank for a series that reached zero at risk first.
#'
#'   This function reads the numbers at risk and nothing else. A series'
#'   total number of events is a separate, optional, per-series quantity
#'   handled by [retro_data_events()], even when the source of that number
#'   is a cumulative events row printed inside the risk table itself.
#'   Series need not report the same number of time points - a risk table
#'   column left blank for one series (commonly the series that reaches
#'   zero at risk first) is a legitimate reading, not a misread.
#'
#'   One time point per series is a legitimate reading too, and it is how a
#'   figure with no risk table at all is reconstructed: a series' total
#'   number of patients is the number at risk at time zero, so a single
#'   entry per series at `x = 0` is a complete, if minimal, risk table.
#'   Nothing here or downstream treats that shape specially.
#' @return A tibble with one row per risk table entry, columns `patients`
#'   (integer), `series` (character), and `x`,
#'   sorted by `series_legend` order then increasing `x`. Never returns
#'   `NULL`: a reading is always required, though for a figure with no
#'   risk table it may be as small as one row per series at `x = 0`
#'   carrying that series' total number of patients.
#' @param risk_patients Numeric vector, the number of patients at risk
#'   (still under observation) for each risk table entry. Required.
#'   Read directly from the source
#'   image. Must have the same length as `risk_series` and `risk_x`.
#'   For a figure with no risk table, this is one entry per series giving
#'   that series' total number of patients - the number at risk at time
#'   zero - paired with `risk_x = 0`.
#' @param risk_series Character vector, the series name for each
#'   risk table entry. Required. Every entry is required (no empty
#'   strings) and must match one of `series_legend`.
#' @param risk_x Numeric vector, the x-axis (time) coordinate of each
#'   risk table entry. Required. Read from the source image (the risk
#'   table's own time-point columns); it need not fall within the range
#'   spanned by the two axis calibration ticks. All zeros when
#'   `risk_patients` carries per-series totals rather than a risk table.
#' @param series_legend Character vector, valid series
#'   names from the legend. Used to validate that
#'   every entry in `risk_series` names a known series and that every series
#'   has at least one entry.
#'   Also used to sort the series in the output `tibble`.
#' @examples
#'   retroglyph:::retro_data_risk(
#'     risk_patients = c(100, 95),
#'     risk_series = c("Placebo", "Drug 10mg"),
#'     risk_x = c(0, 0),
#'     series_legend = c("Placebo", "Drug 10mg")
#'   )
#'   # A figure with no risk table, reconstructed from each series' total
#'   # number of patients: one entry per series, all at time 0.
#'   retroglyph:::retro_data_risk(
#'     risk_patients = c(482, 479),
#'     risk_series = c("Placebo", "Drug 10mg"),
#'     risk_x = c(0, 0),
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
