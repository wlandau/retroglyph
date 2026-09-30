#' @title Pivot a long risk table into one row per time point
#' @keywords internal
#' @noRd
#' @description Reshape a long risk table (one row per series/time entry)
#'   into a wide layout: one row per unique `x`, one column per unique
#'   `series` holding `patients`. A series with no entry at a given `x` gets
#'   `NA`. Series columns are ordered by first appearance in `risk_table`,
#'   and rows are sorted by increasing `x` regardless of input order.
#' @param risk_table A tibble with columns `series` (character), `x`
#'   (numeric), and `patients` (numeric), e.g. `state$data_risk` (the return
#'   value of `retro_data_risk()`).
#' @return A tibble with column `x` followed by one column per unique
#'   `series`, one row per unique `x`.
#' @examples
#'   risk_table <- tibble::tibble(
#'     series = c("Placebo", "Placebo", "Drug", "Drug"),
#'     x = c(0, 6, 0, 6),
#'     patients = c(100, 80, 95, 90)
#'   )
#'   retroglyph:::retro_table_wide(risk_table)
retro_table_wide <- function(risk_table) {
  times <- sort(unique(risk_table$x))
  series <- unique(risk_table$series)
  wide_table <- tibble::tibble(x = times)
  for (one_series in series) {
    entries <- risk_table[risk_table$series == one_series, ]
    wide_table[[one_series]] <- entries$patients[match(times, entries$x)]
  }
  wide_table
}

#' @title Transpose a wide risk table into one row per series
#' @keywords internal
#' @noRd
#' @description Transpose the wide layout produced by [retro_table_wide()]
#'   into one row per series and one column per time point, the layout most
#'   published risk tables use alongside their Kaplan-Meier figure. Time
#'   point column names are `as.character()` of `wide_table$x`.
#' @param wide_table A tibble with column `x` followed by one column per
#'   series, e.g. the return value of [retro_table_wide()].
#' @return A tibble with column `series` followed by one column per unique
#'   time point in `wide_table$x`, one row per series.
#' @examples
#'   wide_table <- tibble::tibble(
#'     x = c(0, 6),
#'     Placebo = c(100, 80),
#'     Drug = c(95, 90)
#'   )
#'   retroglyph:::retro_table_transpose(wide_table)
retro_table_transpose <- function(wide_table) {
  times <- wide_table$x
  series <- setdiff(names(wide_table), "x")
  patients <- t(as.matrix(wide_table[, series]))
  colnames(patients) <- as.character(times)
  rownames(patients) <- NULL
  tibble::add_column(
    tibble::as_tibble(patients),
    series = series,
    .before = 1L
  )
}
