test_that("retro_data_events() returns NULL when nothing is supplied", {
  expect_null(
    retro_data_events(series_legend = c("Placebo", "Drug 10mg"))
  )
})

test_that("retro_data_events() treats zero-length vectors as not supplied", {
  # Some models emit an empty array instead of omitting an optional
  # argument, which must mean the same thing as omitting it.
  expect_null(
    retro_data_events(
      events_total = numeric(0),
      events_series = character(0),
      series_legend = c("Placebo", "Drug 10mg")
    )
  )
})

test_that("retro_data_events() returns correctly-shaped tibble", {
  result <- retro_data_events(
    events_total = c(120, 118),
    events_series = c("Placebo", "Drug 10mg"),
    series_legend = c("Placebo", "Drug 10mg")
  )
  expect_s3_class(result, "tbl_df")
  expect_named(result, c("series", "events"))
  expect_equal(nrow(result), 2L)
  expect_true(is.character(result$series))
  expect_type(result$events, "integer")
})

test_that("retro_data_events() accepts partial coverage", {
  # Total events is optional per arm: a publication may state it for one
  # arm and not the other, and guessing the missing one is worse than
  # leaving it out.
  result <- retro_data_events(
    events_total = 120,
    events_series = "Placebo",
    series_legend = c("Placebo", "Drug 10mg")
  )
  expect_equal(nrow(result), 1L)
  expect_equal(result$series, "Placebo")
  expect_equal(result$events, 120)
})

test_that("retro_data_events() sorts by series_legend order", {
  result <- retro_data_events(
    events_total = c(118, 120),
    events_series = c("Drug 10mg", "Placebo"),
    series_legend = c("Placebo", "Drug 10mg")
  )
  expect_equal(result$series, c("Placebo", "Drug 10mg"))
  expect_equal(result$events, c(120, 118))
})

test_that("retro_data_events() series order follows series_legend, not events_series", {
  result <- retro_data_events(
    events_total = c(120, 118),
    events_series = c("Placebo", "Drug 10mg"),
    series_legend = c("Drug 10mg", "Placebo")
  )
  expect_equal(result$series, c("Drug 10mg", "Placebo"))
})

test_that("retro_data_events() errors when only events_total is supplied", {
  expect_error(
    retro_data_events(
      events_total = 120,
      series_legend = c("Placebo", "Drug 10mg")
    ),
    "must be supplied together"
  )
})

test_that("retro_data_events() errors when only events_series is supplied", {
  expect_error(
    retro_data_events(
      events_series = "Placebo",
      series_legend = c("Placebo", "Drug 10mg")
    ),
    "must be supplied together"
  )
})

test_that("retro_data_events() errors on mismatched lengths", {
  expect_error(
    retro_data_events(
      events_total = c(120, 118),
      events_series = "Placebo",
      series_legend = c("Placebo", "Drug 10mg")
    ),
    "same length"
  )
})

test_that("retro_data_events() errors on a repeated series", {
  # The anti-regression test: a repeated arm is what a whole row of
  # cumulative per-time-point counts looks like, which is exactly what
  # this argument replaced.
  expect_error(
    retro_data_events(
      events_total = c(5, 60, 120),
      events_series = rep("Placebo", 3L),
      series_legend = c("Placebo", "Drug 10mg")
    ),
    "at most one entry per series"
  )
})

test_that("retro_data_events() errors on invalid series names", {
  expect_error(
    retro_data_events(
      events_total = 120,
      events_series = "Unknown Arm",
      series_legend = c("Placebo", "Drug 10mg")
    ),
    "Invalid series names"
  )
})

test_that("retro_data_events() errors on empty events_series", {
  expect_error(
    retro_data_events(
      events_total = 120,
      events_series = "",
      series_legend = c("Placebo", "Drug 10mg")
    ),
    "non-empty"
  )
})
