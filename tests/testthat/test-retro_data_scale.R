test_that("retro_scale_calibration() correct slope/intercept with 4 points", {
  # pixels 2,4,6,8 -> values 0,5,10,15
  # slope = 2.5, intercept = -5
  result <- retro_scale_calibration(
    pixel = c(2, 4, 6, 8),
    value = c(0, 5, 10, 15)
  )
  expect_equal(result$slope, 2.5)
  expect_equal(result$intercept, -5)
})

test_that("retro_scale_calibration() works with exactly 2 points", {
  result <- retro_scale_calibration(pixel = c(2, 8), value = c(0, 15))
  expect_equal(result$slope, 2.5)
  expect_equal(result$intercept, -5)
})

test_that("retro_scale_calibration() errors with fewer than 2 points", {
  expect_error(
    retro_scale_calibration(pixel = 5, value = 10),
    "At least 2"
  )
  expect_error(
    retro_scale_calibration(pixel = numeric(0L), value = numeric(0L)),
    "At least 2"
  )
})

test_that("retro_scale_calibration() errors with zero-variance pixels", {
  expect_error(
    retro_scale_calibration(pixel = c(5, 5, 5), value = c(0, 5, 10)),
    "identical"
  )
})

test_that("retro_scale_max_y() reads 1 for a proportion scale", {
  expect_equal(retro_scale_max_y(c(1, 0.9, 0.5, 0.2)), 1)
})

test_that("retro_scale_max_y() reads 100 for a percentage scale", {
  expect_equal(retro_scale_max_y(c(100, 90, 50, 20)), 100)
})

test_that("retro_scale_max_y() uses the pooled maximum, not any one value", {
  expect_equal(retro_scale_max_y(c(0.1, 0.2, 100, 0.3)), 100)
})

test_that("retro_scale_increasing() is FALSE for a falling (survival) trend", {
  expect_false(retro_scale_increasing(x = 0:4, y = c(1, 0.9, 0.8, 0.7, 0.6)))
})

test_that("retro_scale_increasing() is TRUE for a rising (cumulative incidence) trend", {
  expect_true(retro_scale_increasing(x = 0:4, y = c(0, 0.1, 0.2, 0.3, 0.4)))
})

test_that("retro_scale_increasing() reads a shallow but real rise", {
  expect_true(
    retro_scale_increasing(x = 0:100, y = seq(0, 0.02, length.out = 101L))
  )
})

test_that("retro_scale_increasing() is FALSE when there is no trend to fit", {
  # A single pixel, or every pixel in one column, gives an NA slope. The
  # result feeds an if() in retro_data_scale(), so it must be usable.
  expect_false(retro_scale_increasing(x = 5, y = 5))
  expect_false(retro_scale_increasing(x = c(3, 3, 3), y = c(1, 2, 3)))
})

test_that("retro_scale_increasing() is FALSE for a perfectly flat curve", {
  # The slope is zero only up to rounding, and its sign is then whichever
  # way the last bits fell. The same flat trace has been measured at both
  # +1.4e-17 and -1.4e-17 depending on rounding in the calibrated x.
  expect_false(retro_scale_increasing(x = 0:4, y = rep(0.5, 5L)))
  expect_false(
    retro_scale_increasing(
      x = c(10, -4.440892e-16, 5, 10, -4.440892e-16, 5),
      y = rep(0.5, 6L)
    )
  )
})

test_that("retro_data_scale() errors on unknown path color", {
  data_label <- tibble::tibble(
    label = c("A", "B", "C", "D"),
    word = c("0", "10", "0", "1"),
    confidence = rep(90, 4),
    x = c(2, 8, 1, 1),
    y = c(10, 10, 8, 2),
    x1 = c(1L, 7L, 0L, 0L),
    x2 = c(3L, 9L, 2L, 2L),
    y1 = c(9L, 9L, 7L, 1L),
    y2 = c(11L, 11L, 9L, 3L)
  )
  x_axis <- tibble::tibble(label = c("A", "B"), value = c(0, 10))
  y_axis <- tibble::tibble(label = c("C", "D"), value = c(0, 1))
  data_path <- tibble::new_tibble(
    tibble::tibble(x = 5L, y = 5L, color = "#00ff00"),
    width = 10L,
    height = 12L
  )
  legend <- tibble::tibble(color = "#dc3030", series = "Placebo")
  expect_error(
    retro_data_scale(
      data_path = data_path,
      data_label = data_label,
      x_axis = x_axis,
      y_axis = y_axis,
      legend = legend
    ),
    "not in legend"
  )
})

test_that("retro_data_scale() scales a decreasing curve starting at the origin", {
  # x ticks at pixel 0/100 -> data 0/10 (slope 0.1); y ticks at pixel
  # 100/0 -> data 0/1 (slope -0.01, intercept 1), the usual PNG
  # convention where pixel row increases downward as data value falls.
  data_label <- tibble::tibble(
    label = c("A", "B", "C", "D"),
    word = c("0", "10", "0.0", "1.0"),
    confidence = rep(90, 4),
    x = c(0, 100, 1, 1),
    y = c(50, 50, 100, 0),
    x1 = c(0L, 100L, 0L, 0L),
    x2 = c(0L, 100L, 2L, 2L),
    y1 = c(49L, 49L, 99L, 0L),
    y2 = c(51L, 51L, 101L, 1L)
  )
  x_axis <- tibble::tibble(label = c("A", "B"), value = c(0, 10))
  y_axis <- tibble::tibble(label = c("C", "D"), value = c(0, 1))
  data_path <- tibble::new_tibble(
    tibble::tibble(
      x = seq(10L, 90L, by = 10L),
      y = seq(20L, 100L, by = 10L),
      color = "#dc3030",
      line_width = 1L
    ),
    width = 100L,
    height = 100L
  )
  legend <- tibble::tibble(
    series = "Placebo",
    color = "#dc3030"
  )
  result <- retro_data_scale(
    data_path = data_path,
    data_label = data_label,
    x_axis = x_axis,
    y_axis = y_axis,
    legend = legend
  )
  expect_s3_class(result, "tbl_df")
  expect_named(result, c("series", "x", "y", "max_y", "increasing"))
  expect_equal(result$x, 0:9)
  expect_equal(result$y, c(1, seq(0.8, 0, by = -0.1)))
  expect_true(all(result$max_y == 1))
  expect_false(any(result$increasing))
  expect_true(all(diff(result$y) <= 0))
})

test_that("retro_data_scale() complements a rising trace into decreasing survival scale", {
  # Same axes as above, but the curve's pixel row falls as x increases -
  # a cumulative-incidence-style rise in raw data terms.
  data_label <- tibble::tibble(
    label = c("A", "B", "C", "D"),
    word = c("0", "10", "0.0", "1.0"),
    confidence = rep(90, 4),
    x = c(0, 100, 1, 1),
    y = c(50, 50, 100, 0),
    x1 = c(0L, 100L, 0L, 0L),
    x2 = c(0L, 100L, 2L, 2L),
    y1 = c(49L, 49L, 99L, 0L),
    y2 = c(51L, 51L, 101L, 1L)
  )
  x_axis <- tibble::tibble(label = c("A", "B"), value = c(0, 10))
  y_axis <- tibble::tibble(label = c("C", "D"), value = c(0, 1))
  data_path <- tibble::new_tibble(
    tibble::tibble(
      x = seq(10L, 90L, by = 10L),
      y = seq(100L, 20L, by = -10L),
      color = "#dc3030",
      line_width = 1L
    ),
    width = 100L,
    height = 100L
  )
  legend <- tibble::tibble(
    series = "Placebo",
    color = "#dc3030"
  )
  result <- retro_data_scale(
    data_path = data_path,
    data_label = data_label,
    x_axis = x_axis,
    y_axis = y_axis,
    legend = legend
  )
  expect_true(all(result$increasing))
  # increasing describes the ORIGINAL plot only - y itself still comes out
  # as a decreasing 0-1 survival curve starting at 1.
  expect_equal(result$x[1L], 0)
  expect_equal(result$y[1L], 1)
  expect_true(all(diff(result$y) <= 0))
  expect_equal(result$y, c(1, seq(1, 0.2, by = -0.1)))
})

test_that("retro_data_scale() normalizes a percentage-scale y-axis to 0-1", {
  data_label <- tibble::tibble(
    label = c("A", "B", "C", "D"),
    word = c("0", "10", "0", "100"),
    confidence = rep(90, 4),
    x = c(0, 100, 1, 1),
    y = c(50, 50, 100, 0),
    x1 = c(0L, 100L, 0L, 0L),
    x2 = c(0L, 100L, 2L, 2L),
    y1 = c(49L, 49L, 99L, 0L),
    y2 = c(51L, 51L, 101L, 1L)
  )
  x_axis <- tibble::tibble(label = c("A", "B"), value = c(0, 10))
  y_axis <- tibble::tibble(label = c("C", "D"), value = c(0, 100))
  data_path <- tibble::new_tibble(
    tibble::tibble(
      x = seq(10L, 90L, by = 10L),
      y = seq(20L, 100L, by = 10L),
      color = "#dc3030",
      line_width = 1L
    ),
    width = 100L,
    height = 100L
  )
  legend <- tibble::tibble(
    series = "Placebo",
    color = "#dc3030"
  )
  result <- retro_data_scale(
    data_path = data_path,
    data_label = data_label,
    x_axis = x_axis,
    y_axis = y_axis,
    legend = legend
  )
  expect_true(all(result$max_y == 100))
  # max_y describes the ORIGINAL plot only - y itself is already 0-1.
  expect_equal(result$y, c(1, seq(0.8, 0, by = -0.1)))
})

test_that("retro_data_scale() keeps the legend's series order in the series column", {
  data_label <- tibble::tibble(
    label = c("A", "B", "C", "D"),
    word = c("0", "10", "0", "1"),
    confidence = rep(90, 4),
    x = c(2, 8, 1, 1),
    y = c(10, 10, 8, 2),
    x1 = c(1L, 7L, 0L, 0L),
    x2 = c(3L, 9L, 2L, 2L),
    y1 = c(9L, 9L, 7L, 1L),
    y2 = c(11L, 11L, 9L, 3L)
  )
  x_axis <- tibble::tibble(label = c("A", "B"), value = c(0, 10))
  y_axis <- tibble::tibble(label = c("C", "D"), value = c(0, 1))
  levels_in_order <- c("Placebo", "Drug 10mg")
  data_path <- tibble::new_tibble(
    tibble::tibble(
      x = c(4L, 6L, 8L, 4L, 6L, 8L),
      y = c(9L, 7L, 5L, 9L, 7L, 5L),
      color = c(rep("#3030a0", 3L), rep("#dc3030", 3L)),
      line_width = 1L
    ),
    width = 10L,
    height = 12L
  )
  legend <- tibble::tibble(
    color = c("#dc3030", "#3030a0"),
    series = c("Placebo", "Drug 10mg")
  )
  result <- retro_data_scale(
    data_path = data_path,
    data_label = data_label,
    x_axis = x_axis,
    y_axis = y_axis,
    legend = legend
  )
  expect_true(is.character(result$series))
  expect_equal(sort(unique(result$series)), sort(levels_in_order))
  # Sorted alphabetically by series.
  expect_equal(result$series[1L], "Drug 10mg")
  # Each series gets its own origin, independent of the other's pixels.
  drug <- result[result$series == "Drug 10mg", ]
  placebo <- result[result$series == "Placebo", ]
  expect_equal(drug$x[1L], 0)
  expect_equal(placebo$x[1L], 0)
  expect_equal(drug$y[1L], 1)
  expect_equal(placebo$y[1L], 1)
  expect_true(all(diff(drug$x) >= 0))
  expect_true(all(diff(placebo$x) >= 0))
})

test_that("retro_data_scale() output drives retro_data_survival(), preserving a vertical drop", {
  # A thin, already-skeletonized step path: flat at pixel row 20 for
  # columns 1-30, a vertical drop spanning rows 20:60 at column 30 (many
  # pixels sharing one column - the shape a skeletonized vertical drop
  # actually produces), then flat at row 60 for columns 30-60.
  before_x <- 1:30L
  before_y <- rep(20L, 30L)
  drop_x <- rep(30L, 41L)
  drop_y <- 20:60L
  after_x <- 30:60L
  after_y <- rep(60L, 31L)
  data_path <- tibble::new_tibble(
    tibble::tibble(
      x = c(before_x, drop_x, after_x),
      y = c(before_y, drop_y, after_y),
      color = "#dc3030",
      line_width = 1L
    ),
    width = 61L,
    height = 101L
  )
  data_label <- tibble::tibble(
    label = c("A", "B", "C", "D"),
    word = c("0", "24", "0.0", "1.0"),
    confidence = rep(90, 4),
    x = c(0, 60, 1, 1),
    y = c(100, 100, 100, 0),
    x1 = c(0L, 59L, 0L, 0L),
    x2 = c(1L, 60L, 2L, 2L),
    y1 = c(99L, 99L, 99L, 0L),
    y2 = c(101L, 101L, 101L, 1L)
  )
  legend <- tibble::tibble(
    series = "Placebo",
    color = "#dc3030"
  )
  risk_table <- retro_data_risk(
    risk_patients = c(100, 70, 30),
    risk_series = rep("Placebo", 3L),
    risk_x = c(0, 12, 24),
    series_legend = legend$series
  )
  scaled <- retro_data_scale(
    data_path = data_path,
    data_label = data_label,
    x_axis = tibble::tibble(label = c("A", "B"), value = c(0, 24)),
    y_axis = tibble::tibble(label = c("C", "D"), value = c(0, 1)),
    legend = legend
  )
  expect_false(any(scaled$increasing))
  expect_equal(scaled$x[1L], 0)
  expect_equal(scaled$y[1L], 1)
  # The drop's many skeleton pixels (same x, distinct y) all survive into
  # the scaled output - not collapsed to one value.
  drop_rows <- scaled[abs(scaled$x - 12) < 1e-6, ]
  expect_true(nrow(drop_rows) > 5L)
  # Within the drop, order is x ascending then y descending - top of the
  # drop first, then down.
  expect_true(all(diff(drop_rows$y) <= 0))
  survival <- retro_data_survival(scaled = scaled, risk_table = risk_table)
  expect_s3_class(survival, "tbl_df")
  expect_true(all(c("series", "time", "status") %in% names(survival)))
  expect_equal(nrow(survival), 100L)
  expect_true(all(survival$time >= 0))
})

test_that("retro_data_scale() errors on fewer than 2 x-axis rows", {
  data_label <- tibble::tibble(
    label = c("A", "C", "D"),
    word = c("0", "0", "1"),
    confidence = rep(90, 3),
    x = c(2, 1, 1),
    y = c(10, 8, 2),
    x1 = c(1L, 0L, 0L),
    x2 = c(3L, 2L, 2L),
    y1 = c(9L, 7L, 1L),
    y2 = c(11L, 9L, 3L)
  )
  x_axis <- tibble::tibble(label = "A", value = 0)
  y_axis <- tibble::tibble(label = c("C", "D"), value = c(0, 1))
  data_path <- tibble::new_tibble(
    tibble::tibble(x = 5L, y = 5L, color = "#dc3030"),
    width = 10L,
    height = 12L
  )
  legend <- tibble::tibble(color = "#dc3030", series = "Placebo")
  expect_error(
    retro_data_scale(
      data_path = data_path,
      data_label = data_label,
      x_axis = x_axis,
      y_axis = y_axis,
      legend = legend
    )
  )
})

test_that("retro_data_scale() errors on fewer than 2 y-axis rows", {
  data_label <- tibble::tibble(
    label = c("A", "B", "C"),
    word = c("0", "10", "0"),
    confidence = rep(90, 3),
    x = c(2, 8, 1),
    y = c(10, 10, 8),
    x1 = c(1L, 7L, 0L),
    x2 = c(3L, 9L, 2L),
    y1 = c(9L, 9L, 7L),
    y2 = c(11L, 11L, 9L)
  )
  x_axis <- tibble::tibble(label = c("A", "B"), value = c(0, 10))
  y_axis <- tibble::tibble(label = "C", value = 0)
  data_path <- tibble::new_tibble(
    tibble::tibble(x = 5L, y = 5L, color = "#dc3030"),
    width = 10L,
    height = 12L
  )
  legend <- tibble::tibble(color = "#dc3030", series = "Placebo")
  expect_error(
    retro_data_scale(
      data_path = data_path,
      data_label = data_label,
      x_axis = x_axis,
      y_axis = y_axis,
      legend = legend
    )
  )
})

test_that("retro_data_scale() errors on labels not in data_label", {
  data_label <- tibble::tibble(
    label = c("A", "B", "C", "D"),
    word = c("0", "10", "0", "1"),
    confidence = rep(90, 4),
    x = c(2, 8, 1, 1),
    y = c(10, 10, 8, 2),
    x1 = c(1L, 7L, 0L, 0L),
    x2 = c(3L, 9L, 2L, 2L),
    y1 = c(9L, 9L, 7L, 1L),
    y2 = c(11L, 11L, 9L, 3L)
  )
  x_axis <- tibble::tibble(label = c("A", "Z"), value = c(0, 10))
  y_axis <- tibble::tibble(label = c("C", "D"), value = c(0, 1))
  data_path <- tibble::new_tibble(
    tibble::tibble(x = 5L, y = 5L, color = "#dc3030"),
    width = 10L,
    height = 12L
  )
  legend <- tibble::tibble(color = "#dc3030", series = "Placebo")
  expect_error(
    retro_data_scale(
      data_path = data_path,
      data_label = data_label,
      x_axis = x_axis,
      y_axis = y_axis,
      legend = legend
    )
  )
})
