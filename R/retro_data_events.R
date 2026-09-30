#' @title Validate and structure a total events reading
#' @keywords internal
#' @noRd
#' @description Validate the model's reading of the total number of events
#'   (deaths, progressions, etc.) observed in each series over the
#'   whole follow-up period, and return a structured tibble with one row
#'   per series. Unlike the risk table (see [retro_data_risk()]), this is
#'   optional: total events only sharpens the censoring estimate in the
#'   final interval, so a reconstruction without it still succeeds.
#' @details Total events is a per-series quantity, so a series contributes at
#'   most one entry. That is the one structural rule that separates this
#'   function from [retro_data_risk()], which takes one entry per series *per
#'   time point*: a repeated series here means a whole row of cumulative
#'   counts was supplied where a single total belongs.
#'
#'   Coverage may be partial. Any subset of the series may report a total,
#'   including just one and including none, because `tot.events` is a
#'   per-series input to `IPDfromKM::getIPD()` with no coupling between
#'   series, and published figures routinely state the total for one series
#'   and not the other. This is deliberately unlike [retro_data_risk()],
#'   which requires an entry for every series: the risk table is mandatory,
#'   so demanding full coverage there protects a required input, whereas
#'   demanding it here would only pressure the model into inventing the
#'   missing series' number.
#' @return `NULL` if neither `events_total` nor `events_series` was
#'   supplied. Otherwise a tibble with one row per KM curve
#'   (sorted by the appearance of KM curves in the risk table)
#'   and the following columns:
#'   * `events` — Integer, the series' total events over the whole
#'     follow-up period.
#'   * `series` — Character, the series name.
#' @param events_total Numeric vector, or `NULL`. The total number of
#'   events observed in each series over the whole follow-up period - one
#'   number per series, not a count per time point. `NULL` if
#'   no total is available for any series. Aligned one-to-one with
#'   `events_series` (same length and order).
#' @param events_series Character vector, or `NULL`. The series
#'   name each `events_total` entry belongs to. Every entry is required
#'   (no empty strings), must match one of `series_legend`, and must name a
#'   distinct series. `events_series` also determines the order of the data
#'   series in the returned table.
#' @param series_legend Character vector, valid series
#'   names from the legend. Used to validate that
#'   every entry in `events_series` names a known series.
#'   The order of series names in the legend may not match the desired
#'   order, which is instead in `events_series`.
#' @examples
#'   # Totals for both series.
#'   retroglyph:::retro_data_events(
#'     events_total = c(120, 118),
#'     events_series = c("Placebo", "Drug 10mg"),
#'     series_legend = c("Placebo", "Drug 10mg")
#'   )
#'   # Partial coverage: only one series' total was legible.
#'   retroglyph:::retro_data_events(
#'     events_total = 120,
#'     events_series = "Placebo",
#'     series_legend = c("Placebo", "Drug 10mg")
#'   )
#'   # No total for any series.
#'   retroglyph:::retro_data_events(
#'     series_legend = c("Placebo", "Drug 10mg")
#'   )
retro_data_events <- function(
  events_total = NULL,
  events_series = NULL,
  series_legend
) {
  # Some models return an empty array rather than omitting an optional
  # argument, so zero length means "not supplied" just as NULL does.
  supplied_total <- length(events_total) > 0L
  supplied_series <- length(events_series) > 0L
  if (!supplied_total && !supplied_series) {
    return(NULL)
  }
  if (supplied_total != supplied_series) {
    stop(
      "events_total and events_series must be supplied together, or ",
      "neither. Every total events count needs the series it ",
      "belongs to, and every named series needs a count."
    )
  }
  if (length(events_total) != length(events_series)) {
    stop(
      "events_total and events_series must both have the same length, ",
      "at least 1."
    )
  }
  if (any(!nzchar(events_series))) {
    stop("events_series must be non-empty for every total events entry.")
  }
  series_legend <- unique(as.character(series_legend))
  invalid_series <- setdiff(events_series, series_legend)
  if (length(invalid_series) > 0L) {
    stop(
      "Invalid series names: ",
      paste(invalid_series, collapse = ", "),
      ". Must be one of: ",
      paste(series_legend, collapse = ", ")
    )
  }
  duplicate_series <- unique(events_series[duplicated(events_series)])
  if (length(duplicate_series) > 0L) {
    stop(
      "Total events must have at most one entry per series, but these ",
      "are repeated: ",
      paste(duplicate_series, collapse = ", "),
      ". Total events is a single number per series, covering the whole ",
      "follow-up period. A repeated series usually means a whole row of ",
      "cumulative event counts was supplied instead of one total; if ",
      "the source reports cumulative events per time point, supply only ",
      "that series' last and largest value."
    )
  }
  events_table <- tibble::tibble(
    series = as.character(events_series),
    events = as.integer(events_total)
  )
  events_table[order(match(events_table$series, series_legend)), ]
}
