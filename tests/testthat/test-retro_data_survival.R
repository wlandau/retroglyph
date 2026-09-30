# --- Test fixtures ---

mock_scaled <- tibble::tibble(
  x = rep(seq(0, 10, by = 1), 2),
  y = c(
    c(1.0, 0.95, 0.90, 0.85, 0.80, 0.75, 0.70, 0.65, 0.60, 0.55, 0.50),
    c(1.0, 0.90, 0.80, 0.70, 0.60, 0.50, 0.40, 0.35, 0.30, 0.25, 0.20)
  ),
  series = rep(c("Placebo", "Drug"), each = 11)
)

mock_risk_table <- tibble::tibble(
  patients = c(100, 80, 60, 100, 70, 40),
  series = rep(c("Placebo", "Drug"), each = 3),
  x = rep(c(0, 4, 8), 2)
)

# --- retro_survival_series() tests ---

test_that("retro_survival_series() returns data frame with time and status", {
  scaled_series <- mock_scaled[mock_scaled$series == "Placebo", ]
  risk_table_series <- mock_risk_table[mock_risk_table$series == "Placebo", ]
  result <- retro_survival_series(
    scaled_series = scaled_series,
    risk_table_series = risk_table_series,
    series_id = 1L
  )
  expect_s3_class(result, "data.frame")
  expect_true("time" %in% names(result))
  expect_true("status" %in% names(result))
  expect_true(nrow(result) > 0L)
  expect_true(all(result$status %in% c(0, 1)))
})

test_that("retro_survival_series() reconstructs correct number of patients", {
  scaled_series <- mock_scaled[mock_scaled$series == "Placebo", ]
  risk_table_series <- mock_risk_table[mock_risk_table$series == "Placebo", ]
  result <- retro_survival_series(
    scaled_series = scaled_series,
    risk_table_series = risk_table_series,
    series_id = 1L
  )
  # Number of reconstructed patients should equal first nrisk
  expect_equal(nrow(result), 100L)
})

# --- retro_data_survival() tests ---

test_that("retro_data_survival() returns tibble with correct columns", {
  result <- retro_data_survival(mock_scaled, mock_risk_table)
  expect_s3_class(result, "tbl_df")
  expect_named(result, c("series", "time", "status"))
  expect_type(result$time, "double")
  expect_type(result$status, "integer")
  expect_type(result$series, "character")
})

test_that("retro_data_survival() status is 0 or 1", {
  result <- retro_data_survival(mock_scaled, mock_risk_table)
  expect_true(all(result$status %in% c(0L, 1L)))
})

test_that("retro_data_survival() output series match input", {
  result <- retro_data_survival(mock_scaled, mock_risk_table)
  expect_true(all(unique(result$series) %in% c("Placebo", "Drug")))
  expect_true("Placebo" %in% result$series)
  expect_true("Drug" %in% result$series)
})

test_that("retro_data_survival() reconstructs correct patient counts", {
  result <- retro_data_survival(mock_scaled, mock_risk_table)
  # Each series starts with 100 at risk
  expect_equal(sum(result$series == "Placebo"), 100L)
  expect_equal(sum(result$series == "Drug"), 100L)
})

test_that("retro_data_survival() errors when risk_table series not in trace", {
  bad_risk <- mock_risk_table
  bad_risk$series[1] <- "Unknown Arm"
  expect_error(
    retro_data_survival(mock_scaled, bad_risk),
    "not in trace"
  )
})

test_that("retro_data_survival() errors on missing columns in trace", {
  bad_trace <- tibble::tibble(x = 1:5, y = 1:5)
  expect_error(retro_data_survival(bad_trace, mock_risk_table))
})

test_that("retro_data_survival() errors on missing columns in risk_table", {
  bad_risk <- tibble::tibble(patients = 100, x = 0)
  expect_error(retro_data_survival(mock_scaled, bad_risk))
})

test_that("retro_data_survival() errors on negative nrisk", {
  bad_risk <- mock_risk_table
  bad_risk$patients[1] <- -5
  expect_error(retro_data_survival(mock_scaled, bad_risk))
})

# --- risk_table monotonicity validation ---

test_that("retro_data_survival() errors when patients increase over time", {
  bad_risk <- mock_risk_table
  bad_risk$patients[bad_risk$series == "Placebo" & bad_risk$x == 4] <- 110
  expect_error(
    retro_data_survival(mock_scaled, bad_risk),
    "non-increasing"
  )
})

# --- events_table validation ---

test_that("retro_data_survival() errors on negative total events", {
  expect_error(
    retro_data_survival(
      mock_scaled,
      mock_risk_table,
      tibble::tibble(events = -1, series = "Placebo")
    ),
    "events >= 0"
  )
})

test_that("retro_data_survival() errors on a total events count for an unknown series", {
  expect_error(
    retro_data_survival(
      mock_scaled,
      mock_risk_table,
      tibble::tibble(events = 20, series = "Unknown Arm")
    ),
    "no risk table entries"
  )
})

test_that("retro_data_survival() rejects total events above the number at risk at time 0", {
  # Each patient can have at most one event, so this is impossible.
  # IPDfromKM silently clamps it, so retroglyph has to catch it.
  expect_error(
    retro_data_survival(
      mock_scaled,
      mock_risk_table,
      tibble::tibble(events = 101, series = "Placebo")
    ),
    "exceeds the number of patients at risk at time 0"
  )
})

test_that("retro_data_survival() skips the total events bound when the risk table starts after time 0", {
  # Patients have already left the risk set before the first column, so
  # the table establishes no upper bound and the check must not fire.
  late_risk <- mock_risk_table
  late_risk$x <- rep(c(4, 6, 8), 2)
  expect_no_error(
    retro_data_survival(
      mock_scaled,
      late_risk,
      tibble::tibble(events = 200, series = "Placebo")
    )
  )
})

# --- events_table -> tot.events routing ---

test_that("retro_survival_series() forwards events_table_series$events verbatim as tot.events", {
  # No max() derivation any more: the total arrives already reduced to one
  # number per arm.
  captured <- "unset"
  local_mocked_bindings(
    getIPD = function(prep, armID = 1, tot.events = NULL) {
      captured <<- tot.events
      list(IPD = data.frame(time = 0, status = 0, treat = armID))
    },
    .package = "IPDfromKM"
  )
  retro_survival_series(
    scaled_series = mock_scaled[mock_scaled$series == "Placebo", ],
    risk_table_series = mock_risk_table[mock_risk_table$series == "Placebo", ],
    events_table_series = tibble::tibble(events = 20, series = "Placebo"),
    series_id = 1L
  )
  expect_equal(captured, 20)
})

test_that("retro_survival_series() passes a NULL tot.events when events_table_series is NULL", {
  captured <- "unset"
  local_mocked_bindings(
    getIPD = function(prep, armID = 1, tot.events = NULL) {
      captured <<- tot.events
      list(IPD = data.frame(time = 0, status = 0, treat = armID))
    },
    .package = "IPDfromKM"
  )
  retro_survival_series(
    scaled_series = mock_scaled[mock_scaled$series == "Placebo", ],
    risk_table_series = mock_risk_table[mock_risk_table$series == "Placebo", ],
    series_id = 1L
  )
  expect_null(captured)
})

test_that("retro_survival_series() passes a NULL tot.events for a zero-row events_table_series", {
  # getIPD() branches on is.null(tot.events), not on its length, so
  # numeric(0) takes the non-NULL branch and silently degenerates the
  # final interval's censoring estimate instead of erroring. expect_null()
  # rather than expect_length(0) is the whole point of this test.
  captured <- "unset"
  local_mocked_bindings(
    getIPD = function(prep, armID = 1, tot.events = NULL) {
      captured <<- tot.events
      list(IPD = data.frame(time = 0, status = 0, treat = armID))
    },
    .package = "IPDfromKM"
  )
  retro_survival_series(
    scaled_series = mock_scaled[mock_scaled$series == "Placebo", ],
    risk_table_series = mock_risk_table[mock_risk_table$series == "Placebo", ],
    events_table_series = tibble::tibble(
      events = numeric(0),
      series = character(0)
    ),
    series_id = 1L
  )
  expect_null(captured)
})

test_that("retro_data_survival() routes each arm's own total, and NULL for arms without one", {
  captured <- list()
  local_mocked_bindings(
    getIPD = function(prep, armID = 1, tot.events = NULL) {
      captured[[length(captured) + 1L]] <<- list(
        arm = armID,
        total = tot.events
      )
      list(IPD = data.frame(time = 0, status = 0, treat = armID))
    },
    .package = "IPDfromKM"
  )
  # mock_risk_table orders Placebo first, so arm 1 is Placebo, arm 2 Drug.
  retro_data_survival(
    mock_scaled,
    mock_risk_table,
    tibble::tibble(events = 30, series = "Drug")
  )
  expect_equal(length(captured), 2L)
  expect_equal(captured[[1L]]$arm, 1L)
  expect_null(captured[[1L]]$total)
  expect_equal(captured[[2L]]$arm, 2L)
  expect_equal(captured[[2L]]$total, 30)
})

test_that("retro_data_survival() treats events_table as optional", {
  expect_no_error(retro_data_survival(mock_scaled, mock_risk_table))
  expect_no_error(retro_data_survival(mock_scaled, mock_risk_table, NULL))
})

test_that("retro_data_survival() total events reaches IPDfromKM and changes the result", {
  # End-to-end proof that tot.events is not silently dropped. A total high
  # enough to bind the final interval's censoring shifts the reconstructed
  # times; the counts are pinned by the first at-risk value either way.
  scaled <- tibble::tibble(
    series = rep("Placebo", 11L),
    x = seq(0, 10, by = 1),
    y = c(1, 0.97, 0.94, 0.92, 0.9, 0.89, 0.88, 0.86, 0.85, 0.84, 0.83)
  )
  risk_table <- tibble::tibble(
    patients = c(100, 50, 30),
    series = rep("Placebo", 3L),
    x = c(0, 4, 8)
  )
  without <- retro_data_survival(scaled, risk_table)
  with <- retro_data_survival(
    scaled,
    risk_table,
    tibble::tibble(events = 29, series = "Placebo")
  )
  expect_false(isTRUE(all.equal(sum(without$time), sum(with$time))))
})
