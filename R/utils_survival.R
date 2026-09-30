#' @title Hazard ratios from reconstructed survival data
#' @keywords internal
#' @noRd
#' @description Fit a proportional hazards model to reconstructed individual
#'   patient data and report the hazard ratio of each non-reference series
#'   relative to a chosen reference series.
#' @return A tibble with columns:
#'   * `series` — Character, the comparator arm (all unique values of
#'     `data$series` other than `reference`).
#'   * `estimate` — Numeric, the hazard ratio (`exp(coef)` from
#'     `survival::coxph()`).
#'   * `lower`, `upper` — Numeric, the confidence interval at `confidence`.
#'   * `p_value` — Numeric, the Wald test p-value.
#'   One row per series other than `reference`.
#' @param data A tibble with columns `series` (character), `time`, `status`,
#'   e.g. `state$data_survival` (the return value of [retro_data_survival()]).
#' @param reference Character scalar, the series to treat as the reference
#'   (denominator) level. Must be one of `unique(data$series)`. In a clinical
#'   trial with multiple study arms, this is typically the control arm.
#' @param confidence Numeric scalar strictly between 0 and 1, the confidence
#'   level for `lower`/`upper`.
#' @examples
#'   data <- tibble::tibble(
#'     series = rep(c("Placebo", "Drug"), each = 5),
#'     time = c(1, 2, 3, 4, 5, 2, 4, 6, 8, 10),
#'     status = c(1, 1, 1, 0, 1, 1, 1, 0, 1, 1)
#'   )
#'   retroglyph:::retro_survival_hazard_ratios(data, reference = "Placebo")
retro_survival_hazard_ratios <- function(data, reference, confidence = 0.95) {
  stopifnot(
    "data must be a tibble with columns series, time, status" = inherits(
      data,
      "tbl_df"
    ) &&
      all(c("series", "time", "status") %in% names(data)),
    "data$series must be a character vector" = is.character(data$series),
    "reference must be a single non-missing, non-empty string" = is.character(
      reference
    ) &&
      length(reference) == 1L &&
      !is.na(reference) &&
      nzchar(reference),
    "reference must be one of unique(data$series)" = reference %in%
      unique(data$series),
    "data must have at least one series besides reference" = length(setdiff(
      unique(data$series),
      reference
    )) >=
      1L,
    "confidence must be a single number strictly between 0 and 1" = is.numeric(
      confidence
    ) &&
      length(confidence) == 1L &&
      !is.na(confidence) &&
      confidence > 0 &&
      confidence < 1
  )
  data$series <- factor(
    data$series,
    levels = c(reference, setdiff(unique(data$series), reference))
  )
  fit <- survival::coxph(survival::Surv(time, status) ~ series, data = data)
  # summary.coxph()'s conf.int columns are named for the requested
  # confidence level (e.g. "lower .90" vs "lower .95"), so lower/upper are
  # read by position (3rd/4th column) rather than by name.
  summary_fit <- summary(fit, conf.int = confidence)
  comparators <- levels(data$series)[-1]
  tibble::tibble(
    series = comparators,
    estimate = unname(summary_fit$conf.int[, "exp(coef)"]),
    lower = unname(summary_fit$conf.int[, 3L]),
    upper = unname(summary_fit$conf.int[, 4L]),
    p_value = unname(summary_fit$coefficients[, "Pr(>|z|)"])
  )
}

#' @title Survival quantiles from reconstructed survival data
#' @keywords internal
#' @noRd
#' @description Compute Kaplan-Meier survival quantiles (e.g. median
#'   survival) for each series in reconstructed individual patient data.
#'   Each entry in `probabilities` is a target *survival* probability - the
#'   fraction of patients expected to still be alive at the reported time.
#'   For each one, the earliest time at which the Kaplan-Meier survival
#'   estimate drops to or below it (`NA`, i.e. "not reached", if the
#'   observed survival curve never drops that low - common for a
#'   low-event-rate series, where even a middling requested probability
#'   like `0.5` may never be reached if far fewer than half of patients
#'   ever have the event).
#' @return A tibble with columns:
#'   * `series` — Character, the row group.
#'   * `survival` — Numeric, the distinct values of `probabilities`
#'     (duplicates dropped), sorted in decreasing order - i.e. chronological
#'     order, since higher survival is always reached earlier (or not later)
#'     than lower survival.
#'   * `incidence` — Numeric, `1 - survival` - the same probabilities
#'     viewed as cumulative incidence.
#'   * `time` — Numeric, the survival quantile time, or `NA` if not
#'     reached.
#'   * `time_lower`, `time_upper` — Numeric, the confidence interval at
#'     `confidence` around `time`, or `NA` where not estimable.
#'   One row per series/probability combination, sorted chronologically
#'   (increasing `time`) within each series.
#' @param data A tibble with columns `series` (character), `time`, `status`,
#'   e.g. `state$data_survival` (the return value of [retro_data_survival()]).
#' @param probabilities Numeric vector, target survival probabilities
#'   between 0 and 1 (the fraction of patients expected to still be alive
#'   at the reported time), e.g. `c(0.75, 0.5, 0.25)` for the times by
#'   which survival has dropped to 75%, 50% (median survival), and 25%.
#'   Order does not matter - the output is always sorted chronologically,
#'   with duplicates dropped.
#' @param confidence Numeric scalar strictly between 0 and 1, the confidence
#'   level for `time_lower`/`time_upper`.
#' @examples
#'   data <- tibble::tibble(
#'     series = rep(c("Placebo", "Drug"), each = 5),
#'     time = c(1, 2, 3, 4, 5, 2, 4, 6, 8, 10),
#'     status = c(1, 1, 1, 0, 1, 1, 1, 0, 1, 1)
#'   )
#'   retroglyph:::retro_survival_quantiles(data, probabilities = c(0.75, 0.5, 0.25))
retro_survival_quantiles <- function(data, probabilities, confidence = 0.95) {
  stopifnot(
    "data must be a tibble with columns series, time, status" = inherits(
      data,
      "tbl_df"
    ) &&
      all(c("series", "time", "status") %in% names(data)),
    "data$series must be a character vector" = is.character(data$series),
    "probabilities must be a non-empty numeric vector with values between 0 and 1" = is.numeric(
      probabilities
    ) &&
      length(probabilities) >= 1L &&
      !anyNA(probabilities) &&
      all(probabilities >= 0 & probabilities <= 1),
    "confidence must be a single number strictly between 0 and 1" = is.numeric(
      confidence
    ) &&
      length(confidence) == 1L &&
      !is.na(confidence) &&
      confidence > 0 &&
      confidence < 1
  )
  probabilities <- sort(unique(probabilities), decreasing = TRUE)
  series_levels <- unique(data$series)
  rows <- lapply(series_levels, function(series_name) {
    subset <- data[data$series == series_name, , drop = FALSE]
    fit <- survival::survfit(
      survival::Surv(time, status) ~ 1,
      data = subset,
      conf.int = confidence
    )
    quantiles <- stats::quantile(
      fit,
      probs = 1 - probabilities,
      conf.int = TRUE
    )
    tibble::tibble(
      series = series_name,
      survival = probabilities,
      incidence = 1 - probabilities,
      time = as.numeric(quantiles$quantile),
      time_lower = as.numeric(quantiles$lower),
      time_upper = as.numeric(quantiles$upper)
    )
  })
  do.call(rbind, rows)
}

#' @title Survival probabilities from reconstructed survival data
#' @keywords internal
#' @noRd
#' @description Compute Kaplan-Meier survival probabilities (e.g. survival
#'   at 12 months) for each series in reconstructed individual patient data,
#'   at the times requested. This is the inverse of
#'   [retro_survival_quantiles()]: instead of asking "at what time is a
#'   target survival probability reached", it asks "what is the survival
#'   probability at a target time". A requested time beyond the last
#'   observed event or censoring in a series still returns the
#'   Kaplan-Meier estimate at that time, extended flat from the last
#'   observation, rather than `NA` or a dropped row.
#' @return A tibble with columns:
#'   * `series` — Character, the row group.
#'   * `time` — Numeric, the distinct values of `quantiles` (duplicates
#'     dropped), sorted in increasing order.
#'   * `survival` — Numeric, the Kaplan-Meier survival estimate at `time`.
#'   * `survival_lower`, `survival_upper` — Numeric, the confidence
#'     interval at `confidence` around `survival`.
#'   * `incidence` — Numeric, `1 - survival` - the same estimate viewed as
#'     cumulative incidence.
#'   * `incidence_lower` — Numeric, `1 - survival_lower`.
#'   * `incidence_upper` — Numeric, `1 - survival_upper`.
#'   One row per series/time combination, sorted chronologically
#'   (increasing `time`) within each series.
#' @param data A tibble with columns `series` (character), `time`, `status`,
#'   e.g. `state$data_survival` (the return value of [retro_data_survival()]).
#' @param quantiles Numeric vector, target time points (non-negative), e.g.
#'   `c(6, 12, 24)` for the survival probabilities at 6, 12, and 24 months.
#'   Named `quantiles` (not `time`) to mirror [retro_survival_quantiles()]'s
#'   `probabilities` argument - despite the name, these are time points, not
#'   probabilities. Order does not matter - the output is always sorted
#'   chronologically, with duplicates dropped.
#' @param confidence Numeric scalar strictly between 0 and 1, the confidence
#'   level for `survival_lower`/`survival_upper`.
#' @examples
#'   data <- tibble::tibble(
#'     series = rep(c("Placebo", "Drug"), each = 5),
#'     time = c(1, 2, 3, 4, 5, 2, 4, 6, 8, 10),
#'     status = c(1, 1, 1, 0, 1, 1, 1, 0, 1, 1)
#'   )
#'   retroglyph:::retro_survival_probabilities(data, quantiles = c(2, 4, 6))
retro_survival_probabilities <- function(data, quantiles, confidence = 0.95) {
  stopifnot(
    "data must be a tibble with columns series, time, status" = inherits(
      data,
      "tbl_df"
    ) &&
      all(c("series", "time", "status") %in% names(data)),
    "data$series must be a character vector" = is.character(data$series),
    "quantiles must be a non-empty numeric vector with non-negative values" = is.numeric(
      quantiles
    ) &&
      length(quantiles) >= 1L &&
      !anyNA(quantiles) &&
      all(quantiles >= 0),
    "confidence must be a single number strictly between 0 and 1" = is.numeric(
      confidence
    ) &&
      length(confidence) == 1L &&
      !is.na(confidence) &&
      confidence > 0 &&
      confidence < 1
  )
  times <- sort(unique(quantiles))
  series_levels <- unique(data$series)
  rows <- lapply(series_levels, function(series_name) {
    subset <- data[data$series == series_name, , drop = FALSE]
    fit <- survival::survfit(
      survival::Surv(time, status) ~ 1,
      data = subset,
      conf.int = confidence
    )
    summary_fit <- summary(fit, times = times, extend = TRUE)
    survival <- as.numeric(summary_fit$surv)
    survival_lower <- as.numeric(summary_fit$lower)
    survival_upper <- as.numeric(summary_fit$upper)
    tibble::tibble(
      series = series_name,
      time = times,
      survival = survival,
      survival_lower = survival_lower,
      survival_upper = survival_upper,
      incidence = 1 - survival,
      incidence_lower = 1 - survival_lower,
      incidence_upper = 1 - survival_upper
    )
  })
  do.call(rbind, rows)
}

#' @title Sample size and events from reconstructed survival data
#' @keywords internal
#' @noRd
#' @description Count the number of patients and events per series in the
#'   reconstructed individual patient survival data. One row per series in
#'   `legend`, in the order `legend` lists them, plus a final `"total"` row
#'   summing across all series. Series present in `data` but not in `legend`
#'   are ignored.
#' @return A tibble with columns:
#'   * `series` — Character, the series name. One row per `legend$series`
#'     value, in legend order, plus a final `"total"` row.
#'   * `patients` — Integer, the number of reconstructed patients in that
#'     series (rows in `data` for that series).
#'   * `events` — Integer, the number of reconstructed events
#'     (`status == 1`) in that series.
#' @param data A tibble with columns `series` (character), `time`, `status`,
#'   e.g. `state$data_survival` (the return value of [retro_data_survival()]).
#' @param legend A tibble with column `series` (character), e.g.
#'   `state$data_legend`. The counts have one row per `legend$series` value,
#'   in the same order; series present in `data` but not in `legend` are
#'   ignored.
#' @examples
#'   data <- tibble::tibble(
#'     series = rep(c("Placebo", "Drug"), each = 5),
#'     time = c(1, 2, 3, 4, 5, 2, 4, 6, 8, 10),
#'     status = c(1, 1, 1, 0, 1, 1, 1, 0, 1, 1)
#'   )
#'   legend <- tibble::tibble(
#'     series = c("Placebo", "Drug"),
#'     color = c("#dc3030", "#3030a0"),
#'     reference = c(TRUE, FALSE)
#'   )
#'   retroglyph:::retro_survival_counts(data, legend)
retro_survival_counts <- function(data, legend) {
  stopifnot(
    "data must be a tibble with columns series, time, status" = inherits(
      data,
      "tbl_df"
    ) &&
      all(c("series", "time", "status") %in% names(data)),
    "data$series must be a character vector" = is.character(data$series),
    "legend must be a tibble with column series" = inherits(
      legend,
      "tbl_df"
    ) &&
      "series" %in% names(legend),
    "legend$series must be a character vector" = is.character(legend$series)
  )
  series_names <- legend$series
  patients <- unname(vapply(
    series_names,
    function(series_name) sum(data$series == series_name),
    integer(1L)
  ))
  events <- unname(vapply(
    series_names,
    function(series_name) sum(data$series == series_name & data$status == 1L),
    integer(1L)
  ))
  tibble::tibble(
    series = c(series_names, "total"),
    patients = c(patients, sum(patients)),
    events = c(events, sum(events))
  )
}
