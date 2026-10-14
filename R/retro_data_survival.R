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
#'   One row per reconstructed patient, all series stacked. Where `scaled`
#'   carries detected censoring tick marks, the reconstructed censoring
#'   times land on the tick positions rather than at the evenly spaced
#'   guesses `IPDfromKM` would otherwise invent; see
#'   [retro_survival_series()].
#' @param scaled A tibble from [retro_data_scale()] with columns `series`,
#'   `x`, and `y`. Already normalized to a 0-1 survival scale: within each
#'   series, `x` starts at `0`, `y` starts at `1`, and `y` never increases
#'   after that. An optional logical `censored` column from
#'   [retro_data_censor()] marks points sitting under a censoring tick mark,
#'   and is used to position the reconstructed censoring times. It defaults
#'   to all `FALSE` when absent, which reconstructs exactly as if the figure
#'   drew no ticks.
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
  if (!("censored" %in% names(scaled))) {
    # Censoring tick marks are optional input. Defaulting to none here,
    # the same way retro_data_scale() does, means the tick-driven
    # placement in retro_survival_series() simply has nothing to act on,
    # rather than every caller having to supply the column.
    scaled$censored <- rep(FALSE, nrow(scaled))
  }
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
#' @description Apply the Guyot et al. (2012) algorithm to a single series,
#'   twice, so that the reconstructed censoring times land on the series'
#'   digitized censoring tick marks instead of at evenly spaced guesses.
#' @details The Guyot et al. (2012) algorithm recovers *how many* patients
#'   were censored in each risk table interval, by comparing the drop in
#'   survival across the interval against the drop in the number at risk,
#'   but it has no way to know *where* in the interval those censorings
#'   happened. `IPDfromKM::getIPD()` therefore spreads them at uniform
#'   spacing. The figure, though, shows where: a censoring tick mark is
#'   drawn at every censoring time, and [retro_data_censor()] detects them.
#'
#'   `IPDfromKM` accepts no observed censoring times -
#'   `IPDfromKM::preprocess()` takes only the trace and the risk table, and
#'   prunes tied-time rows, so a per-point flag cannot reach `getIPD()` by
#'   that route. What `getIPD()` does respond to is the risk table itself:
#'   an interval holding one censoring and no events leaves the censoring
#'   nowhere to go but that interval's own boundary. Adding a risk table row
#'   at a tick position therefore pins a censoring to that tick, which is
#'   the lever this function pulls:
#'
#'   1. Reconstruct conventionally from the model-read risk table
#'      ([retro_survival_reconstruct()]). The event times and the per-
#'      interval censoring counts are sound; only the censoring positions
#'      are invented.
#'   2. Collapse the flagged trace points into one position per tick mark
#'      ([retro_censor_ticks()]).
#'   3. Count the censorings each risk table interval implies, by
#'      elimination against the first-pass events ([retro_censor_counts()]).
#'   4. Assign those censorings to tick positions within their own interval
#'      ([retro_censor_positions()]).
#'   5. Add a risk table row at each assigned position, with the number at
#'      risk imputed from the first pass ([retro_risk_table_augment()]).
#'   6. Reconstruct again from the *same* trace and the augmented risk
#'      table. This result replaces the first pass entirely.
#'
#'   The risk table alone decides how many patients were censored. Tick
#'   marks only supply the positions those censorings may occupy, so the
#'   number of ticks never enters the arithmetic - a series whose ticks are
#'   partly hidden by overplotting still reconstructs the full count.
#'
#'   A series with no detected ticks augments to the risk table it started
#'   with, so the second pass reproduces the first and the whole mechanism
#'   reduces to the conventional reconstruction.
#'
#'   Two limitations are inherited from tick detection, and this function is
#'   the first consumer to make them visible in output. A figure that draws
#'   no censoring tick marks at all still has its tallest pixel columns
#'   flagged, because [retro_censor_centers()] is stubbed to always report
#'   two groups; and a vertical step drop is flagged at its outer columns.
#'   Either one puts a censoring at a time the figure does not mark. Both
#'   belong to [retro_data_censor()] and are documented there.
#' @param scaled_series A tibble with columns `x` (time) and `y` (survival,
#'   0-1 scale, starting at 1 at `x = 0` and never increasing) — one
#'   series's rows from `scaled` in [retro_data_survival()]. An optional
#'   logical `censored` column marks the points sitting under a censoring
#'   tick mark; absent or all `FALSE`, the reconstruction is the
#'   conventional single pass.
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
  # Pass 1: the conventional reconstruction. Its event times and its
  # per-interval censoring counts are what the risk table supports; only
  # the censoring positions are uniform guesses, and those are what the
  # second pass replaces.
  first_pass <- retro_survival_reconstruct(
    scaled_series = scaled_series,
    risk_table_series = risk_table_series,
    total_events = total_events,
    series_id = series_id
  )
  ticks <- retro_censor_ticks(scaled_series)
  if (!length(ticks)) {
    return(first_pass)
  }
  # Augment: give each censoring the risk table implies a risk table row of
  # its own at a tick position, which is the only lever IPDfromKM offers on
  # where a censoring lands.
  counts <- retro_censor_counts(
    ipd = first_pass,
    risk_table_series = risk_table_series
  )
  positions <- retro_censor_positions(
    ticks = ticks,
    risk_table_series = risk_table_series,
    counts = counts
  )
  if (!length(positions)) {
    return(first_pass)
  }
  augmented <- retro_risk_table_augment(
    risk_table_series = risk_table_series,
    positions = positions,
    ipd = first_pass,
    trace_times = scaled_series$x
  )
  # Pass 2: same trace, augmented risk table. Each added row brackets a
  # tick position, so the censorings have nowhere else to go.
  retro_survival_reconstruct(
    scaled_series = scaled_series,
    risk_table_series = augmented,
    total_events = total_events,
    series_id = series_id
  )
}

#' @title Reconstruct one series conventionally from a risk table
#' @keywords internal
#' @noRd
#' @description Clip a risk table to the scaled data's time range and hand
#'   both to `IPDfromKM::preprocess()`/`IPDfromKM::getIPD()`, which invert
#'   the Kaplan-Meier step function interval by interval to recover
#'   per-patient times and censoring status.
#' @details This is the conventional Guyot et al. (2012) reconstruction, with
#'   no knowledge of censoring tick marks. [retro_survival_series()] calls it
#'   twice: once on the risk table the model read off the figure, and once on
#'   the augmented table built from that first result. It is a separate
#'   function so that both passes are provably the same computation on
#'   different inputs.
#' @return A data frame with columns `time`, `status`, and `treat` (this
#'   series's `series_id`, repeated), one row per reconstructed patient.
#' @param scaled_series A tibble with columns `x` (time) and `y` (survival),
#'   see [retro_survival_series()]. Any `censored` column is ignored here:
#'   tick marks act on the risk table passed in, not on this computation.
#' @param risk_table_series A tibble with columns `x` (time) and `patients`
#'   (number at risk), see [retro_survival_series()]. Either the original
#'   risk table or the augmented one from [retro_risk_table_augment()].
#' @param total_events Numeric scalar, this series's total events over the
#'   whole follow-up period, passed to `IPDfromKM::getIPD()` as
#'   `tot.events`, or `NULL` if no total is known. Already normalized by the
#'   caller: `getIPD()` branches on `is.null()` rather than on length, so a
#'   `numeric(0)` here would silently degenerate the final interval's
#'   censoring estimate.
#' @param series_id Integer scalar, see [retro_survival_series()].
#' @examples
#'   retroglyph:::retro_survival_reconstruct(
#'     scaled_series = tibble::tibble(
#'       x = 0:5, y = c(1, 0.9, 0.9, 0.8, 0.7, 0.7)
#'     ),
#'     risk_table_series = tibble::tibble(x = c(0, 5), patients = c(20, 10)),
#'     total_events = 8,
#'     series_id = 1L
#'   )
retro_survival_reconstruct <- function(
  scaled_series,
  risk_table_series,
  total_events,
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
  IPDfromKM::getIPD(
    prep = prep,
    armID = series_id,
    tot.events = total_events
  )$IPD
}

#' @title Collapse flagged trace points into one position per tick mark
#' @keywords internal
#' @noRd
#' @description Reduce the `censored` flags on one series' scaled trace to
#'   one time per censoring tick mark drawn in the figure.
#' @details [retro_data_censor()] flags per pixel column, and a tick mark is
#'   several pixel columns wide, so one tick arrives here as a run of
#'   consecutive flagged points. Taking every flagged time as its own
#'   censoring would multiply each tick by its own line width. A run of
#'   adjacent flagged points is therefore collapsed to its midpoint - the
#'   tick's center, which is where the figure places the censoring.
#'
#'   Adjacency is judged in the scaled time coordinate rather than in pixel
#'   columns, which this function no longer has: points are consecutive when
#'   the gap between them is no more than the trace's own typical spacing.
#'   Using the median gap as that yardstick keeps the test independent of the
#'   figure's resolution and of the time axis' units.
#' @return Numeric vector of censoring tick positions on the scaled time
#'   axis, sorted increasing, one per tick mark. `numeric(0)` when the series
#'   has no `censored` column or no flagged points.
#' @param scaled_series A tibble with columns `x` (time) and optionally
#'   `censored` (logical), see [retro_survival_series()].
#' @examples
#'   # Three adjacent flagged points are one tick, centered at x = 4.
#'   retroglyph:::retro_censor_ticks(
#'     tibble::tibble(
#'       x = 0:9,
#'       censored = c(rep(FALSE, 3L), rep(TRUE, 3L), rep(FALSE, 4L))
#'     )
#'   )
retro_censor_ticks <- function(scaled_series) {
  if (!("censored" %in% names(scaled_series))) {
    return(numeric(0L))
  }
  flagged <- sort(unique(scaled_series$x[which(scaled_series$censored)]))
  if (!length(flagged)) {
    return(numeric(0L))
  }
  # The trace's own point spacing is the yardstick for "adjacent". A
  # single-point trace has no spacing to measure, in which case every
  # flagged point stands alone.
  spacing <- stats::median(diff(sort(unique(scaled_series$x))))
  if (!length(spacing) || !is.finite(spacing) || spacing <= 0) {
    return(flagged)
  }
  runs <- cumsum(c(1L, diff(flagged) > spacing * 1.5))
  unname(vapply(split(flagged, runs), mean, numeric(1L)))
}

#' @title Count the censorings each risk table interval implies
#' @keywords internal
#' @noRd
#' @description Work out, by elimination, how many patients were censored
#'   within each interval between consecutive risk table times.
#' @details A patient leaves a risk set exactly one way: by having an event,
#'   or by being censored. So across any span of time, the fall in the number
#'   at risk is the number of events plus the number of censorings, and the
#'   censorings are whatever the events do not account for. The first pass
#'   reconstruction supplies the events, the risk table supplies the fall in
#'   the number at risk, and the difference is the censorings.
#'
#'   This count, and not the number of tick marks found, is what decides how
#'   many patients were censored. Ticks can be missed - overplotting erases a
#'   tick's ink outright, and [retro_data_censor()] cannot recover what is
#'   not visible - so counting them would under-report censoring in exactly
#'   the figures that are hardest to read. The risk table does not have that
#'   failure mode.
#'
#'   Two guards keep a misread digit from propagating. The cumulative
#'   censorings are clamped at zero and forced non-decreasing, since a risk
#'   table entry read one digit wrong, or a first-pass event count slightly
#'   over the mark, can otherwise imply that patients rejoined the risk set.
#' @return Integer vector of censoring counts, one per interval between
#'   consecutive risk table times - so one shorter than the risk table, and
#'   `integer(0)` for a risk table with fewer than two rows (a lone number at
#'   risk brackets no interval and implies no censoring anywhere).
#' @param ipd A data frame with columns `time` and `status`, the first pass
#'   reconstruction from [retro_survival_reconstruct()].
#' @param risk_table_series A tibble with columns `x` (time) and `patients`
#'   (number at risk), see [retro_survival_series()].
#' @examples
#'   # 20 at risk falling to 12, with 5 events: 3 patients censored.
#'   retroglyph:::retro_censor_counts(
#'     ipd = data.frame(time = c(1, 2, 3, 4, 5), status = rep(1L, 5L)),
#'     risk_table_series = tibble::tibble(x = c(0, 6), patients = c(20L, 12L))
#'   )
retro_censor_counts <- function(ipd, risk_table_series) {
  risk_table_series <- risk_table_series[order(risk_table_series$x), ]
  times <- risk_table_series$x
  patients <- risk_table_series$patients
  if (length(times) < 2L) {
    return(integer(0L))
  }
  events <- vapply(
    times,
    function(time) sum(ipd$time <= time & ipd$status == 1L),
    numeric(1L)
  )
  # Patients leave a risk set only by event or censoring, so the fall in
  # the number at risk that the events do not explain is censoring.
  censorings <- (patients[1L] - patients) - (events - events[1L])
  censorings <- cummax(pmax(censorings, 0))
  as.integer(diff(censorings))
}

#' @title Assign implied censorings to censoring tick positions
#' @keywords internal
#' @noRd
#' @description Place each censoring a risk table interval implies at one of
#'   the censoring tick positions falling inside that same interval.
#' @details The count comes from the interval and the positions come from the
#'   ticks, and the two need not agree, so the assignment has to handle both
#'   directions of mismatch. Fewer censorings than ticks means only some
#'   ticks receive one: they are taken evenly spread across the interval's
#'   ticks rather than from the front, so the censoring that is placed is
#'   spread over the interval the way the ticks are. More censorings than
#'   ticks means every tick receives at least `n %/% length(ticks)` of them,
#'   with the remainder spread by the same rule. Either way no tick is
#'   favored for being early.
#'
#'   An interval with no ticks is left alone. Its censorings are not dropped -
#'   they are simply not pinned, so the second pass reconstructs them the
#'   conventional way, evenly spaced, which is the best available answer when
#'   the figure shows nothing about where they went.
#' @return Numeric vector of assigned censoring positions on the scaled time
#'   axis, sorted increasing, with repeats where a tick takes more than one
#'   censoring. `numeric(0)` when no censoring can be pinned to a tick.
#' @param ticks Numeric vector of censoring tick positions from
#'   [retro_censor_ticks()].
#' @param risk_table_series A tibble with columns `x` and `patients`, see
#'   [retro_survival_series()].
#' @param counts Integer vector of per-interval censoring counts from
#'   [retro_censor_counts()], one shorter than `risk_table_series`.
#' @examples
#'   # Two censorings over four ticks: the two taken are spread, not the
#'   # first two.
#'   retroglyph:::retro_censor_positions(
#'     ticks = c(1, 2, 3, 4),
#'     risk_table_series = tibble::tibble(x = c(0, 5), patients = c(20L, 12L)),
#'     counts = 2L
#'   )
retro_censor_positions <- function(ticks, risk_table_series, counts) {
  risk_table_series <- risk_table_series[order(risk_table_series$x), ]
  times <- risk_table_series$x
  if (!length(counts) || !length(ticks)) {
    return(numeric(0L))
  }
  positions <- lapply(seq_along(counts), function(interval) {
    n <- counts[interval]
    lower <- times[interval]
    upper <- times[interval + 1L]
    # Half-open on the left: a tick exactly at a risk table time belongs to
    # the interval starting there, so no tick is claimed twice.
    inside <- ticks[ticks > lower & ticks <= upper]
    if (n < 1L || !length(inside)) {
      return(numeric(0L))
    }
    retro_censor_spread(inside, n)
  })
  sort(unlist(positions, use.names = FALSE))
}

#' @title Spread a number of censorings over a set of tick positions
#' @keywords internal
#' @noRd
#' @description Deal `n` censorings over `ticks`, evenly rather than from the
#'   front.
#' @details Every tick first takes the same whole number of censorings,
#'   `n %/% length(ticks)`, which is zero when the censorings are outnumbered.
#'   What is left over, always fewer than there are ticks, goes to a subset
#'   picked at even spacing across the ticks. `seq()` with `length.out` is
#'   what makes the subset even: it lands on the ends and interpolates
#'   between, so the surplus is distributed across the span rather than piled
#'   on the earliest ticks the way a chronological round robin would.
#' @return Numeric vector of length `n`, values drawn from `ticks` with
#'   repeats, sorted increasing.
#' @param ticks Numeric vector of candidate positions, non-empty.
#' @param n Integer scalar, how many censorings to place, at least 1.
#' @examples
#'   retroglyph:::retro_censor_spread(ticks = c(1, 2, 3, 4), n = 2L)
#'   retroglyph:::retro_censor_spread(ticks = c(1, 2, 3), n = 7L)
retro_censor_spread <- function(ticks, n) {
  ticks <- sort(ticks)
  each <- n %/% length(ticks)
  remainder <- n %% length(ticks)
  assigned <- rep(ticks, times = each)
  if (remainder > 0L) {
    spread <- unique(round(
      seq(from = 1L, to = length(ticks), length.out = remainder)
    ))
    assigned <- c(assigned, ticks[spread])
  }
  sort(assigned)
}

#' @title Add a risk table row at each assigned censoring position
#' @keywords internal
#' @noRd
#' @description Build the augmented risk table that pins the second pass'
#'   censorings to the censoring tick positions.
#' @details This is where tick positions become something `IPDfromKM` will
#'   act on. Each assigned position is bracketed by a pair of risk table
#'   rows: one at the trace point just before it, and one at the position
#'   itself. The censorings assigned to that position then fall in an
#'   interval one trace step wide, and `IPDfromKM::getIPD()` distributes an
#'   interval's censorings from its lower endpoint upward, so they land
#'   within one trace step of the tick - as close to it as a reconstruction
#'   built on this trace can place anything. A single row at the position
#'   would not do it: the interval feeding that row still reaches back to
#'   the previous risk table time, and the censorings would spread over all
#'   of it.
#'
#'   The numbers at risk are built up rather than read off the first pass.
#'   Starting from each original row's count, the number at risk at a new
#'   time is that count less the events the trace shows since then, less the
#'   censorings assigned since then:
#'
#'   `patients(t) = patients(start) - events(start, t] - censorings(start, t]`
#'
#'   The events come from the first pass, whose event times are sound; the
#'   censorings are the assignment this function was handed. Because the
#'   assignment totals exactly what [retro_censor_counts()] derived by
#'   elimination, the walk arrives at each original row's own count exactly,
#'   so the added rows interleave with the model-read ones without
#'   contradicting them. Reading the counts off the first pass instead would
#'   import its uniformly spread censoring times - the very thing being
#'   replaced - into the table meant to correct them.
#'
#'   The result is clamped non-increasing as a backstop.
#'   `retro_survival_validate()` requires it, and while the walk cannot
#'   raise the count on its own, a first pass that put an event just outside
#'   the window the trace implies can leave two adjacent rows out of order
#'   by one patient.
#'
#'   A position coinciding with a time already in the table, or with another
#'   position, is merged: a second row at one time cannot narrow the
#'   interval further, so the censorings stack at that one time instead,
#'   which is what repeated assignment to one tick meant.
#' @return A tibble with columns `x` and `patients`, the rows of
#'   `risk_table_series` plus up to two per distinct new position, sorted by
#'   `x`, with `patients` non-increasing and the original rows' values
#'   unchanged. Any other columns of `risk_table_series` are dropped:
#'   [retro_survival_reconstruct()] reads only these two.
#' @param risk_table_series A tibble with columns `x` and `patients`, see
#'   [retro_survival_series()].
#' @param positions Numeric vector of assigned censoring positions from
#'   [retro_censor_positions()], with repeats where a tick takes more than
#'   one censoring.
#' @param ipd A data frame with columns `time` and `status`, the first pass
#'   reconstruction from [retro_survival_reconstruct()]. Its event times
#'   supply the `events` term of the walk described above.
#' @param trace_times Numeric vector of the trace's own time points, used to
#'   place each position's lower bracket at a real data point rather than at
#'   an arbitrary epsilon below it.
#' @examples
#'   retroglyph:::retro_risk_table_augment(
#'     risk_table_series = tibble::tibble(x = c(0, 6), patients = c(20L, 12L)),
#'     positions = c(2, 4),
#'     ipd = data.frame(
#'       time = seq(0.5, 10, by = 0.5),
#'       status = rep(c(1L, 0L), each = 10L)
#'     ),
#'     trace_times = seq(0, 10, by = 0.5)
#'   )
retro_risk_table_augment <- function(
  risk_table_series,
  positions,
  ipd,
  trace_times
) {
  risk_table_series <- risk_table_series[order(risk_table_series$x), ]
  times <- risk_table_series$x
  trace_times <- sort(unique(trace_times))
  event_times <- ipd$time[ipd$status == 1L]
  rows <- lapply(seq_len(length(times) - 1L), function(interval) {
    lower <- times[interval]
    upper <- times[interval + 1L]
    inside <- positions[positions > lower & positions <= upper]
    if (!length(inside)) {
      return(NULL)
    }
    # Each position gets a bracket at the trace point just before it, so the
    # window holding its censorings is one trace step wide. `getIPD()`
    # distributes an interval's censorings from its lower endpoint upward,
    # so a window that narrow lands them within one trace step of the tick -
    # as close as a reconstruction built on this trace can place anything.
    brackets <- vapply(
      unique(inside),
      function(position) {
        earlier <- trace_times[trace_times < position & trace_times > lower]
        if (length(earlier)) max(earlier) else position
      },
      numeric(1L)
    )
    added <- setdiff(unique(c(brackets, inside)), times)
    if (!length(added)) {
      return(NULL)
    }
    added <- sort(added)
    tibble::tibble(
      x = added,
      # Walk down from this interval's own starting count: the events the
      # trace shows since then, plus the censorings assigned since then.
      patients = risk_table_series$patients[interval] -
        vapply(
          added,
          function(time) {
            sum(event_times > lower & event_times <= time) +
              sum(inside <= time)
          },
          numeric(1L)
        ),
      # The window this interval's own endpoints allow. Clamping added rows
      # into it keeps the walk from contradicting a model-read count.
      ceiling_at = risk_table_series$patients[interval],
      floor_at = risk_table_series$patients[interval + 1L]
    )
  })
  rows <- rows[!vapply(rows, is.null, logical(1L))]
  augmented <- tibble::tibble(
    x = as.numeric(times),
    patients = as.numeric(risk_table_series$patients),
    added = FALSE
  )
  if (length(rows)) {
    rows <- do.call(rbind, rows)
    # Clamp only the added rows, never the originals: the counts the model
    # read off the figure are authoritative, and pulling one of them to
    # meet a walked count would discard patients the risk table says were
    # present.
    rows$patients <- pmin(
      pmax(rows$patients, rows$floor_at),
      rows$ceiling_at
    )
    # Added rows sharing an interval are forced non-increasing among
    # themselves, which the clamp alone does not guarantee. Forward, so a
    # row is lowered to match an earlier one rather than an earlier row
    # being dragged down to match a later one.
    rows$patients <- cummin(rows$patients)
    augmented <- rbind(
      augmented,
      tibble::tibble(x = rows$x, patients = rows$patients, added = TRUE)
    )
  }
  augmented <- augmented[order(augmented$x, augmented$added), ]
  augmented$patients <- as.integer(augmented$patients)
  augmented[, c("x", "patients")]
}

