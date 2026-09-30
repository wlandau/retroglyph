test_that("retro_data_risk() returns correctly-shaped tibble", {
  result <- retro_data_risk(
    risk_patients = c(100, 95),
    risk_series = c("Placebo", "Drug 10mg"),
    risk_x = c(0, 0),
    series_legend = c("Placebo", "Drug 10mg")
  )
  expect_s3_class(result, "tbl_df")
  expect_named(result, c("patients", "series", "x"))
  expect_equal(nrow(result), 2L)
  expect_true(is.character(result$series))
  expect_equal(sort(result$series), c("Drug 10mg", "Placebo"))
})

test_that("retro_data_risk() no longer takes a risk_events argument", {
  # Events moved out of the risk table entirely: they are now one total per
  # arm, handled by retro_data_events(), not a cumulative count per time
  # point riding along with the at-risk numbers.
  expect_false("risk_events" %in% names(formals(retro_data_risk)))
})

test_that("retro_data_risk() requires risk_patients, risk_series, and risk_x", {
  expect_error(
    retro_data_risk(
      risk_series = "Placebo",
      risk_x = 0,
      series_legend = "Placebo"
    ),
    'argument "risk_patients" is missing'
  )
  expect_error(
    retro_data_risk(risk_patients = 100, risk_x = 0, series_legend = "Placebo"),
    'argument "risk_series" is missing'
  )
  expect_error(
    retro_data_risk(
      risk_patients = 100,
      risk_series = "Placebo",
      series_legend = "Placebo"
    ),
    'argument "risk_x" is missing'
  )
})

test_that("retro_data_risk() sorts by series_legend order then increasing x", {
  result <- retro_data_risk(
    risk_patients = c(80, 100, 70, 90),
    risk_series = c("Placebo", "Placebo", "Drug 10mg", "Drug 10mg"),
    risk_x = c(12, 0, 12, 0),
    series_legend = c("Placebo", "Drug 10mg")
  )
  # Placebo appears first in series_legend, so its rows come first, each
  # series then sorted by increasing x.
  expect_equal(
    result$series,
    c("Placebo", "Placebo", "Drug 10mg", "Drug 10mg")
  )
  expect_equal(result$x, c(0, 12, 0, 12))
  expect_equal(result$patients, c(100, 80, 90, 70))
})

test_that("retro_data_risk() series order follows series_legend order", {
  result <- retro_data_risk(
    risk_patients = c(100, 90),
    risk_series = c("Placebo", "Drug 10mg"),
    risk_x = c(0, 0),
    series_legend = c("Drug 10mg", "Placebo")
  )
  expect_true(is.character(result$series))
  expect_equal(result$series, c("Drug 10mg", "Placebo"))
})

test_that("retro_data_risk() errors on mismatched risk lengths", {
  expect_error(
    retro_data_risk(
      risk_patients = 100,
      risk_series = c("Placebo", "Placebo"),
      risk_x = c(0, 12),
      series_legend = "Placebo"
    ),
    "same length"
  )
})

test_that("retro_data_risk() errors on empty risk_series", {
  expect_error(
    retro_data_risk(
      risk_patients = 100,
      risk_series = "",
      risk_x = 0,
      series_legend = "Placebo"
    ),
    "non-empty"
  )
})

test_that("retro_data_risk() errors on invalid series names", {
  expect_error(
    retro_data_risk(
      risk_patients = 100,
      risk_series = "Unknown Arm",
      risk_x = 0,
      series_legend = c("Placebo", "Drug 10mg")
    ),
    "Invalid series names"
  )
})

test_that("retro_data_risk() allows series entry counts to differ", {
  result <- retro_data_risk(
    risk_patients = c(100, 80, 90),
    risk_series = c("Placebo", "Placebo", "Drug 10mg"),
    risk_x = c(0, 6, 0),
    series_legend = c("Placebo", "Drug 10mg")
  )
  expect_equal(sum(result$series == "Placebo"), 2L)
  expect_equal(sum(result$series == "Drug 10mg"), 1L)
})

test_that("retro_data_risk() accepts risk_x outside typical calibration range", {
  result <- retro_data_risk(
    risk_patients = c(100, 95),
    risk_series = c("Placebo", "Drug 10mg"),
    risk_x = c(0, 24),
    series_legend = c("Placebo", "Drug 10mg")
  )
  expect_equal(sort(result$x), c(0, 24))
})

test_that("retro_data_risk() errors when risk table is missing a series", {
  expect_error(
    retro_data_risk(
      risk_patients = 100,
      risk_series = "Placebo",
      risk_x = 0,
      series_legend = c("Placebo", "Drug 10mg")
    ),
    "Missing: Drug 10mg"
  )
})
