# A and B (x-axis ticks) sit side by side at the same height (y = 50).
# C and D (y-axis ticks) sit stacked at the same left-right position (x = 5).
# E is unused and left off both axes.
data_label <- tibble::tibble(
  label = c("A", "B", "C", "D", "E"),
  x = c(10, 20, 5, 5, 50),
  y = c(50, 50, 15, 25, 55),
  x1 = c(8, 18, 3, 3, 48),
  x2 = c(12, 22, 7, 7, 52),
  y1 = c(48, 48, 13, 23, 53),
  y2 = c(52, 52, 17, 27, 57)
)

test_that("retro_data_calibrate() returns correctly-shaped tibbles", {
  result <- retro_data_calibrate(
    x_label = c("A", "B"),
    x_value = c(0, 12),
    y_label = c("C", "D"),
    y_value = c(0, 1),
    data_label = data_label
  )
  expect_type(result, "list")
  expect_named(result, c("x_axis", "y_axis"))
  expect_s3_class(result$x_axis, "tbl_df")
  expect_true(all(
    c("label", "value", "x", "y", "x1", "x2", "y1", "y2") %in%
      names(result$x_axis)
  ))
  expect_equal(result$x_axis$label, c("A", "B"))
  expect_equal(result$x_axis$value, c(0, 12))
  expect_s3_class(result$y_axis, "tbl_df")
  expect_equal(result$y_axis$label, c("C", "D"))
  expect_equal(result$y_axis$value, c(0, 1))
})

test_that("retro_data_calibrate() resolves pixel positions from data_label", {
  result <- retro_data_calibrate(
    x_label = c("A", "B"),
    x_value = c(0, 12),
    y_label = c("C", "D"),
    y_value = c(0, 1),
    data_label = data_label
  )
  expect_equal(result$x_axis$x, c(10, 20))
  expect_equal(result$x_axis$y1, c(48, 48))
  expect_equal(result$y_axis$y, c(15, 25))
})

test_that("retro_data_calibrate() sorts x_axis by increasing value", {
  result <- retro_data_calibrate(
    x_label = c("A", "B"),
    x_value = c(12, 0),
    y_label = c("C", "D"),
    y_value = c(0, 1),
    data_label = data_label
  )
  expect_equal(result$x_axis$label, c("B", "A"))
  expect_equal(result$x_axis$value, c(0, 12))
})

test_that("retro_data_calibrate() sorts y_axis by increasing value", {
  result <- retro_data_calibrate(
    x_label = c("A", "B"),
    x_value = c(0, 1),
    y_label = c("C", "D"),
    y_value = c(1, 0),
    data_label = data_label
  )
  expect_equal(result$y_axis$label, c("D", "C"))
  expect_equal(result$y_axis$value, c(0, 1))
})

test_that("retro_data_calibrate() coerces values to numeric", {
  result <- retro_data_calibrate(
    x_label = c("A", "B"),
    x_value = c("0", "12"),
    y_label = c("C", "D"),
    y_value = c("0", "1"),
    data_label = data_label
  )
  expect_type(result$x_axis$value, "double")
  expect_type(result$y_axis$value, "double")
})

test_that("retro_data_calibrate() errors on mismatched x lengths", {
  expect_error(
    retro_data_calibrate(
      x_label = c("A", "B"),
      x_value = c(0),
      y_label = c("C", "D"),
      y_value = c(0, 1),
      data_label = data_label
    ),
    "x_label and x_value"
  )
})

test_that("retro_data_calibrate() errors when x length is not 2", {
  expect_error(
    retro_data_calibrate(
      x_label = "A",
      x_value = 0,
      y_label = c("C", "D"),
      y_value = c(0, 1),
      data_label = data_label
    ),
    "x_label and x_value"
  )
  expect_error(
    retro_data_calibrate(
      x_label = c("A", "B", "C"),
      x_value = c(0, 6, 12),
      y_label = c("D", "E"),
      y_value = c(0, 1),
      data_label = data_label
    ),
    "x_label and x_value"
  )
})

test_that("retro_data_calibrate() errors on mismatched y lengths", {
  expect_error(
    retro_data_calibrate(
      x_label = c("A", "B"),
      x_value = c(0, 12),
      y_label = c("C", "D"),
      y_value = c(0),
      data_label = data_label
    ),
    "y_label and y_value"
  )
})

test_that("retro_data_calibrate() errors on unknown labels", {
  expect_error(
    retro_data_calibrate(
      x_label = c("A", "Z"),
      x_value = c(0, 12),
      y_label = c("C", "D"),
      y_value = c(0, 1),
      data_label = data_label
    ),
    "Unknown labels"
  )
})

test_that("retro_data_calibrate() errors when x_label and y_label share a label", {
  expect_error(
    retro_data_calibrate(
      x_label = c("A", "B"),
      x_value = c(0, 12),
      y_label = c("A", "D"),
      y_value = c(0, 1),
      data_label = data_label
    ),
    "share labels"
  )
})

test_that("retro_data_calibrate() errors when x-axis ticks are not side by side", {
  # E (x = 50, y = 55) is not at the same height as A (y = 50, height 48-52).
  expect_error(
    retro_data_calibrate(
      x_label = c("A", "E"),
      x_value = c(0, 12),
      y_label = c("C", "D"),
      y_value = c(0, 1),
      data_label = data_label
    ),
    "x-axis.*side by side"
  )
})

test_that("retro_data_calibrate() errors when y-axis ticks are not stacked", {
  # E (x = 50, width 48-52) is not at the same left-right position as
  # C (x = 5, width 3-7).
  expect_error(
    retro_data_calibrate(
      x_label = c("A", "B"),
      x_value = c(0, 12),
      y_label = c("C", "E"),
      y_value = c(0, 1),
      data_label = data_label
    ),
    "y-axis.*stacked"
  )
})
