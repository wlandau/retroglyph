data_labels <- tibble::tibble(
  label = c("A", "B", "C", "D"),
  word = c("0", "10", "0", "1"),
  confidence = rep(90, 4L),
  x = c(1, 40, 1, 1),
  y = c(20, 20, 20, 1),
  x1 = c(1L, 39L, 1L, 1L),
  x2 = c(2L, 40L, 2L, 2L),
  y1 = c(19L, 19L, 19L, 1L),
  y2 = c(20L, 20L, 20L, 2L)
)

data_x_axis <- tibble::tibble(label = c("A", "B"), value = c(0, 10))
data_y_axis <- tibble::tibble(label = c("C", "D"), value = c(0, 1))

data_legend <- function() {
  tibble::tibble(series = "Placebo", color = "#dc3030", reference = TRUE)
}

# A descending step curve: two flat runs joined by a vertical drop, so the
# whole thing is one connected run of pixels for path-finding to follow.
data_step_image <- function() {
  pixels <- matrix("#ffffff", nrow = 20L, ncol = 40L)
  pixels[5L, 1L:20L] <- "#dc3030"
  pixels[5L:10L, 20L] <- "#dc3030"
  pixels[10L, 20L:40L] <- "#dc3030"
  retro_test_png(pixels)
}

test_that("retro_do_data() returns every element it documents", {
  result <- retro_do_data(
    input = data_step_image(),
    data_label = data_labels,
    legend = data_legend(),
    x_axis = data_x_axis,
    y_axis = data_y_axis,
    risk_patients = c(100, 60),
    risk_series = c("Placebo", "Placebo"),
    risk_x = c(0, 10)
  )
  expect_named(result, c("path", "risk", "events", "scaled", "survival"))
  expect_s3_class(result$path, "tbl_df")
  expect_s3_class(result$risk, "tbl_df")
  # Total events is optional, and none was supplied here.
  expect_null(result$events)
  expect_s3_class(result$scaled, "tbl_df")
  expect_s3_class(result$survival, "tbl_df")
  expect_equal(
    names(result$scaled),
    c("series", "x", "y", "max_y", "increasing")
  )
  expect_equal(names(result$survival), c("series", "time", "status"))
})

test_that("retro_do_data() requires a risk table", {
  expect_error(
    retro_do_data(
      input = data_step_image(),
      data_label = data_labels,
      legend = data_legend(),
      x_axis = data_x_axis,
      y_axis = data_y_axis
    ),
    'argument "risk_patients" is missing'
  )
})

test_that("retro_do_data() scales a descending curve monotonically", {
  result <- retro_do_data(
    input = data_step_image(),
    data_label = data_labels,
    legend = data_legend(),
    x_axis = data_x_axis,
    y_axis = data_y_axis,
    risk_patients = c(100, 60),
    risk_series = c("Placebo", "Placebo"),
    risk_x = c(0, 10)
  )
  scaled <- result$scaled[order(result$scaled$x), ]
  expect_equal(scaled$x[1L], 0)
  expect_equal(scaled$y[1L], 1)
  expect_true(scaled$y[nrow(scaled)] < scaled$y[1L])
})

test_that("retro_do_data() validates the risk table before path-finding", {
  # A risk table that goes back up is rejected, and because that check runs
  # first the failure does not depend on the image at all.
  expect_error(
    retro_do_data(
      input = data_step_image(),
      data_label = data_labels,
      legend = data_legend(),
      x_axis = data_x_axis,
      y_axis = data_y_axis,
      risk_patients = c(60, 100),
      risk_series = c("Placebo", "Placebo"),
      risk_x = c(0, 10)
    ),
    "non-increasing"
  )
})

test_that("retro_do_data() rejects a risk series not in the legend", {
  expect_error(
    retro_do_data(
      input = data_step_image(),
      data_label = data_labels,
      legend = data_legend(),
      x_axis = data_x_axis,
      y_axis = data_y_axis,
      risk_patients = 100,
      risk_series = "Nonexistent",
      risk_x = 0
    ),
    "Invalid series names"
  )
})

test_that("retro_do_data() records a supplied total events count", {
  result <- retro_do_data(
    input = data_step_image(),
    data_label = data_labels,
    legend = data_legend(),
    x_axis = data_x_axis,
    y_axis = data_y_axis,
    risk_patients = c(100, 60),
    risk_series = c("Placebo", "Placebo"),
    risk_x = c(0, 10),
    events_total = 35,
    events_series = "Placebo"
  )
  expect_s3_class(result$events, "tbl_df")
  expect_named(result$events, c("series", "events"))
  expect_equal(result$events$events, 35)
  expect_equal(result$events$series, "Placebo")
})

test_that("retro_do_data() validates total events before path-finding", {
  # A repeated arm is only ever rejected by retro_data_events(), so this
  # error cannot come from the risk table check that runs alongside it.
  # retro_data_path() is mocked to error, proving the events check fired
  # before path-finding got a chance to run.
  local_mocked_bindings(
    retro_data_path = function(input) stop("path-finding should not run"),
    .package = "retroglyph"
  )
  expect_error(
    retro_do_data(
      input = data_step_image(),
      data_label = data_labels,
      legend = data_legend(),
      x_axis = data_x_axis,
      y_axis = data_y_axis,
      risk_patients = c(100, 60),
      risk_series = c("Placebo", "Placebo"),
      risk_x = c(0, 10),
      events_total = c(5, 35),
      events_series = c("Placebo", "Placebo")
    ),
    "at most one entry per series"
  )
})

test_that("retro_do_data() no longer takes a risk_events argument", {
  expect_false("risk_events" %in% names(formals(retro_do_data)))
})

test_that("retro_do_data() no longer forwards an increasing argument", {
  # increasing is inferred automatically from the scaled data
  # (retro_scale_increasing()), not supplied by the caller, so
  # retro_do_data() has no increasing parameter.
  expect_false("increasing" %in% names(formals(retro_do_data)))
})

test_that("retro_do_data() validates input and legend", {
  expect_error(
    retro_do_data(
      input = tempfile(fileext = ".png"),
      data_label = data_labels,
      legend = data_legend(),
      x_axis = data_x_axis,
      y_axis = data_y_axis
    ),
    "input must exist"
  )
  expect_error(
    retro_do_data(
      input = data_step_image(),
      data_label = data_labels,
      legend = tibble::tibble(series = "Placebo"),
      x_axis = data_x_axis,
      y_axis = data_y_axis
    ),
    "legend must be a tibble with columns series and color"
  )
})

test_that("retro_do_data() reports a path with no pixels in it", {
  # A curve color can survive quantization and still yield no traceable
  # path, which leaves retro_data_path() returning zero rows rather than
  # erroring. Mocked because that state is hard to reach from a real image,
  # and the unhelpful crash it used to cause is what this guards against.
  local_mocked_bindings(
    retro_data_path = function(input) {
      tibble::tibble(
        x = integer(0),
        y = integer(0),
        color = character(0),
        line_width = integer(0),
        width = integer(0),
        height = integer(0)
      )
    },
    .package = "retroglyph"
  )
  expect_error(
    retro_do_data(
      input = data_step_image(),
      data_label = data_labels,
      legend = data_legend(),
      x_axis = data_x_axis,
      y_axis = data_y_axis,
      risk_patients = c(100, 60),
      risk_series = c("Placebo", "Placebo"),
      risk_x = c(0, 10)
    ),
    "recovered no curve pixels"
  )
})
