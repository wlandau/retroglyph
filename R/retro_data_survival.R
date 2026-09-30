#' @title Reconstruct individual patient survival data
#' @keywords internal
#' @noRd
#' @description Reconstruct individual patient data (IPD) from digitized,
#'   scaled curve data and a risk table using the Guyot et al. (2012)
#'   algorithm implemented in the `IPDfromKM` package. For each series, the
#'   scaled data supplies the survival curve and the risk table supplies
#'   the numbers at risk, allowing censoring to be inferred. An arm's total
#'   events, if known, sharpens that inference in the final interval.
#' @return A tibble with columns:
#'   * `series` — Character, treatment series name.
#'   * `time` — Numeric, reconstructed event or censoring time.
#'   * `status` — Integer, 1 = event, 0 = censored.
#'   One row per reconstructed patient, all series stacked.
#' @param scaled A tibble from [retro_data_scale()] with columns `series`,
#'   `x`, and `y`. Already normalized to a 0-1 survival scale: within each
#'   series, `x` starts at `0`, `y` starts at `1`, and `y` never increases
#'   after that.
#' @param risk_table A tibble from [retro_data_risk()] with columns
#'   `patients`, `series`, and `x`. `patients` is the number of patients at
#'   risk (still under observation) at `x`, and must be non-increasing
#'   within each series.
#' @param events_table A tibble from [retro_data_events()] with columns
#'   `events` and `series`, at most one row per series, or `NULL` (the
#'   default) if no arm reported a total. `events` is the arm's total
#'   number of events over the whole follow-up period, and is passed to
#'   `IPDfromKM::getIPD()` as `tot.events` for that series, sharpening the
#'   censoring estimate in the final interval. Optional throughout, and
#'   optional per arm: a series with no row here is reconstructed without
#'   a total, exactly as if `events_table` were `NULL`.
#' @examples
#'   scaled <- tibble::tibble(
#'     series = rep(c("Placebo", "Drug"), each = 11),
#'     x = rep(seq(0, 10, by = 1), 2),
#'     y = c(
#'       c(1.0, 0.95, 0.90, 0.85, 0.80, 0.75, 0.70, 0.65, 0.60, 0.55, 0.50),
#'       c(1.0, 0.90, 0.80, 0.70, 0.60, 0.50, 0.40, 0.35, 0.30, 0.25, 0.20)
#'     )
#'   )
#'   risk_table <- tibble::tibble(
#'     patients = c(100, 80, 60, 100, 70, 40),
#'     series = rep(c("Placebo", "Drug"), each = 3),
#'     x = rep(c(0, 4, 8), 2)
#'   )
#'   events_table <- tibble::tibble(
#'     events = c(20, 30),
#'     series = c("Placebo", "Drug")
#'   )
#'   result <- retroglyph:::retro_data_survival(scaled, risk_table, events_table)
#'   print(head(result))
retro_data_survival <- function(scaled, risk_table, events_table = NULL) {
  retro_survival_validate(scaled, risk_table, events_table)
  if (is.null(events_table)) {
    # An empty events table makes the per-series subsetting below uniform:
    # every arm gets a zero-row slice, which retro_survival_series() turns
    # into tot.events = NULL.
    events_table <- tibble::tibble(
      events = numeric(0L),
      series = character(0L)
    )
  }
  series_names <- unique(risk_table$series)
  reconstructed <- lapply(
    seq_along(series_names),
    function(series_id) {
      name <- series_names[series_id]
      retro_survival_series(
        scaled_series = scaled[scaled$series == name, ],
        risk_table_series = risk_table[risk_table$series == name, ],
        events_table_series = events_table[
          events_table$series == name,
          ,
          drop = FALSE
        ],
        series_id = series_id
      )
    }
  ) |>
    do.call(what = rbind)
  # combined$treat holds the integer series_id assigned above (IPDfromKM
  # names this column "treat"); translate it back to the series name it
  # stands for.
  tibble::tibble(
    series = series_names[reconstructed[, "treat"]],
    time = reconstructed$time,
    status = as.integer(reconstructed$status)
  )
}

#' @title Validate scaled data and risk table inputs before reconstruction
#' @keywords internal
#' @noRd
#' @description Check the structural, monotonicity, and range requirements
#'   [retro_data_survival()] depends on, so its reconstruction logic can
#'   assume clean inputs. Patients can only ever leave a risk set, never
#'   rejoin it, so the numbers at risk must be non-increasing over time
#'   within a series; a violation almost always means a digit from the
#'   wrong row (e.g. events swapped with at-risk) was read into the wrong
#'   place. An arm's total events is bounded above by the number
#'   randomized for the same reason.
#' @param scaled A tibble, see [retro_data_survival()].
#' @param risk_table A tibble, see [retro_data_survival()].
#' @param events_table A tibble or `NULL`, see [retro_data_survival()].
#' @return `NULL`, invisibly. Called for its side effect of raising an
#'   informative error if validation fails.
#' @examples
#'   scaled <- tibble::tibble(
#'     series = "Placebo", x = c(0, 1), y = c(1, 0.9)
#'   )
#'   risk_table <- tibble::tibble(
#'     patients = c(100, 90),
#'     series = c("Placebo", "Placebo"),
#'     x = c(0, 1)
#'   )
#'   events_table <- tibble::tibble(
#'     events = 10,
#'     series = "Placebo"
#'   )
#'   retroglyph:::retro_survival_validate(scaled, risk_table, events_table)
retro_survival_validate <- function(scaled, risk_table, events_table = NULL) {
  stopifnot(
    inherits(scaled, "tbl_df"),
    all(c("x", "y", "series") %in% names(scaled)),
    inherits(risk_table, "tbl_df"),
    all(c("patients", "series", "x") %in% names(risk_table)),
    all(risk_table$patients >= 0)
  )
  risk_series <- unique(risk_table$series)
  missing_series <- setdiff(risk_series, unique(scaled$series))
  if (length(missing_series) > 0L) {
    stop(
      "Risk table contains series not in trace: ",
      paste(missing_series, collapse = ", ")
    )
  }
  if (!is.null(events_table)) {
    stopifnot(
      inherits(events_table, "tbl_df"),
      all(c("events", "series") %in% names(events_table)),
      all(is.finite(events_table$events)),
      all(events_table$events >= 0)
    )
    missing_events_series <- setdiff(
      unique(events_table$series),
      as.character(risk_series)
    )
    if (length(missing_events_series) > 0L) {
      stop(
        "Total events reported for series with no risk table entries: ",
        paste(missing_events_series, collapse = ", "),
        ". Every arm with a total events count must also appear in the ",
        "risk table."
      )
    }
  }
  for (series in risk_series) {
    risk_table_series <- risk_table[risk_table$series == series, , drop = FALSE]
    risk_table_series <- risk_table_series[order(risk_table_series$x), ]
    if (isTRUE(is.unsorted(-risk_table_series$patients, na.rm = TRUE))) {
      stop(
        "Risk table numbers at risk must be non-increasing over time ",
        "for series '",
        series,
        "'. Check for a misread digit or a value ",
        "mistakenly taken from an events/censored row instead of the ",
        "at-risk row."
      )
    }
    total_events <- events_table$events[events_table$series == series]
    # Each patient can have at most one first event, so an arm's total
    # events cannot exceed the number randomized - the at-risk count at
    # time 0. When the risk table starts later, patients have already left
    # the risk set and the table establishes no bound, so skip the check.
    # IPDfromKM::getIPD() would not complain either way: an over-large
    # tot.events drives its censoring count negative, which it silently
    # clamps to zero, so nothing downstream flags a transposed digit.
    randomized <- max(risk_table_series$patients)
    if (
      length(total_events) == 1L &&
        min(risk_table_series$x) == 0 &&
        total_events > randomized
    ) {
      stop(
        "Total events (",
        total_events,
        ") exceeds the number of patients at risk at time 0 (",
        randomized,
        ") for series '",
        series,
        "'. Each patient can have at most one event, so this is ",
        "impossible. Check for a misread digit, an at-risk count read in ",
        "as the total events, or a total taken from a different arm."
      )
    }
  }
  invisible(NULL)
}

#' @title Reconstruct one series's individual patient data
#' @keywords internal
#' @noRd
#' @description Apply the Guyot et al. (2012) algorithm to a single series:
#'   clip the risk table to the scaled data's time range, and hand both to
#'   `IPDfromKM::preprocess()`/`IPDfromKM::getIPD()`, which invert the KM
#'   step function interval by interval to recover per-patient times and
#'   censoring status.
#' @param scaled_series A tibble with columns `x` (time) and `y` (survival,
#'   0-1 scale, starting at 1 at `x = 0` and never increasing) — one
#'   series's rows from `scaled` in [retro_data_survival()].
#' @param risk_table_series A tibble with columns `x` (time) and `patients`
#'   (number at risk), one series's rows from `risk_table` in
#'   [retro_data_survival()].
#' @param events_table_series A one-row tibble with an `events` column
#'   (this series's total events over the whole follow-up period), passed
#'   to `IPDfromKM::getIPD()` as `tot.events` to sharpen the censoring
#'   estimate in the final interval. `NULL` or a zero-row
#'   tibble means no total is known for this series, in which case
#'   `getIPD()` receives `tot.events = NULL`.
#' @param series_id Integer scalar, this series's 1-based position among
#'   all series being reconstructed. Passed to `IPDfromKM::getIPD()` as
#'   `armID`, which stamps it into the output's `treat` column. It must
#'   stay a plain integer: `getIPD()` builds that column with `cbind()`
#'   alongside the numeric `time`/`status` columns, and any non-numeric
#'   value there would coerce the whole matrix to character.
#' @return A data frame with columns `time`, `status`, and `treat` (this
#'   series's `series_id`, repeated), one row per reconstructed patient.
#' @examples
#'   scaled_series <- tibble::tibble(
#'     x = 0:5, y = c(1, 0.9, 0.9, 0.8, 0.7, 0.7)
#'   )
#'   risk_table_series <- tibble::tibble(x = c(0, 5), patients = c(20, 10))
#'   result <- retroglyph:::retro_survival_series(
#'     scaled_series = scaled_series,
#'     risk_table_series = risk_table_series,
#'     events_table_series = tibble::tibble(events = 8, series = "Placebo"),
#'     series_id = 1L
#'   )
#'   print(head(result))
retro_survival_series <- function(
  scaled_series,
  risk_table_series,
  events_table_series = NULL,
  series_id
) {
  dat <- data.frame(time = scaled_series$x, surv = scaled_series$y)
  in_range <- risk_table_series$x <= max(dat$time)
  # IPDfromKM::preprocess() calls dplyr's n() unqualified. retroglyph only
  # ever calls IPDfromKM via `::`, so dplyr - one of IPDfromKM's own
  # Depends - gets loaded but never attached to the search path, and n()
  # cannot resolve (this is why the bug shows up in a fresh
  # library(retroglyph) session, and under R CMD check, but not under
  # devtools::test()/load_all(), which attaches every Import). Attaching
  # dplyr here, only for this call, fixes that without permanently
  # changing the caller's search path the way library(IPDfromKM) would
  # (dplyr masks stats::filter()/lag(), among others).
  dplyr_attached <- "package:dplyr" %in% search()
  if (!dplyr_attached) {
    requireNamespace("dplyr", quietly = TRUE)
    attachNamespace("dplyr")
    on.exit(detach("package:dplyr"), add = TRUE)
  }
  prep <- IPDfromKM::preprocess(
    dat = dat,
    trisk = risk_table_series$x[in_range],
    nrisk = risk_table_series$patients[in_range],
    maxy = 1
  )
  # getIPD() branches on is.null(tot.events), not on its length, so an arm
  # with no total must be normalized to NULL rather than passed through as
  # the numeric(0) that subsetting a zero-row events table leaves behind.
  # numeric(0) takes the non-NULL branch, where `tot.events - <count>`
  # collapses to numeric(0) and the final interval's censoring estimate
  # silently degenerates instead of erroring.
  total_events <- events_table_series$events
  if (length(total_events) != 1L) {
    total_events <- NULL
  }
  IPDfromKM::getIPD(
    prep = prep,
    armID = series_id,
    tot.events = total_events
  )$IPD
}
