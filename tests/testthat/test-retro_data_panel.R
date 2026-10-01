# Shared synthetic fixture: 100x100 white image with an L-shaped axis -
# y-axis at column 20 (rows 10-80), x-axis at row 80 (columns 20-90).
retro_panel_synthetic_image <- function() {
  input <- tempfile(fileext = ".png")
  matrix <- matrix("#ffffffff", nrow = 100, ncol = 100)
  matrix[10:80, 20] <- "#000000ff"
  matrix[80, 20:90] <- "#000000ff"
  magick::image_write(retro_test_image(matrix), input)
  input
}

# Same L-shaped axis as retro_panel_synthetic_image(), plus a top border
# (row 5, columns 25-95) and a right border (column 95, rows 15-75) - both
# a few pixels beyond the axis lines' own extent (row 10 / column 90), so
# detecting them exercises genuine independent-line detection rather than
# a coincidental overlap with the axis lines' own pixels. Deliberately
# avoids column 20 (the y-axis's own column) and row 80 (the x-axis's
# own row), so the axis lines' own detected extent is unaffected.
retro_panel_synthetic_boxed_image <- function() {
  input <- tempfile(fileext = ".png")
  matrix <- matrix("#ffffffff", nrow = 100, ncol = 100)
  matrix[10:80, 20] <- "#000000ff"
  matrix[80, 20:90] <- "#000000ff"
  matrix[5, 25:95] <- "#000000ff"
  matrix[15:75, 95] <- "#000000ff"
  magick::image_write(retro_test_image(matrix), input)
  input
}

# Tick labels sit adjacent to their axis line: x-axis labels below the
# x-axis (at row 80), y-axis labels to the left of the y-axis (at column
# 20). The anchor-based search finds axes by looking outward from these
# label edges.
retro_panel_synthetic_tick <- function(label, value, x, y, height = 6) {
  tibble::tibble(
    label = label,
    value = value,
    x = x,
    y = y,
    x1 = x - 2,
    x2 = x + 2,
    y1 = y - height / 2,
    y2 = y + height / 2
  )
}

retro_panel_synthetic_ticks <- function() {
  list(
    x_axis = rbind(
      retro_panel_synthetic_tick("A", 0, x = 20, y = 86),
      retro_panel_synthetic_tick("B", 12, x = 90, y = 86)
    ),
    y_axis = rbind(
      retro_panel_synthetic_tick("C", 0, x = 15, y = 80),
      retro_panel_synthetic_tick("D", 1, x = 15, y = 10)
    )
  )
}

test_that("retro_data_panel() returns panel coordinates", {
  input <- retro_panel_synthetic_image()
  on.exit(unlink(input))
  ticks <- retro_panel_synthetic_ticks()
  result <- retro_data_panel(input, ticks$x_axis, ticks$y_axis)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1L)
  expect_named(
    result,
    c("x1", "x2", "y1", "y2", "pad_x1", "pad_x2", "pad_y1", "pad_y2")
  )
  expect_true(all(vapply(result, is.numeric, logical(1L))))
  expect_true(result$x1 < result$x2)
  expect_true(result$y1 < result$y2)
  # The required y-axis/x-axis lines always pad inward.
  expect_true(result$pad_x1 > 0L)
  expect_true(result$pad_y2 < 0L)
})

test_that("retro_data_panel() detects axes on synthetic image", {
  input <- retro_panel_synthetic_image()
  on.exit(unlink(input))
  ticks <- retro_panel_synthetic_ticks()
  result <- retro_data_panel(
    input = input,
    x_axis = ticks$x_axis,
    y_axis = ticks$y_axis
  )
  expect_true(result$x1 <= 20L)
  expect_true(result$x2 >= 90L)
  expect_true(result$y1 <= 10L)
  expect_true(result$y2 >= 80L)
})

test_that("retro_data_panel() pads outward when no top or right border is drawn", {
  input <- retro_panel_synthetic_image()
  on.exit(unlink(input))
  ticks <- retro_panel_synthetic_ticks()
  result <- retro_data_panel(input, ticks$x_axis, ticks$y_axis)
  expect_equal(result$y1, 10L)
  expect_equal(result$x2, 90L)
  expect_true(result$pad_y1 < 0L)
  expect_true(result$pad_x2 > 0L)
})

test_that("retro_data_panel() pads inward when a top and right border are detected", {
  input <- retro_panel_synthetic_boxed_image()
  on.exit(unlink(input))
  ticks <- retro_panel_synthetic_ticks()
  result <- retro_data_panel(input, ticks$x_axis, ticks$y_axis)
  expect_equal(result$y1, 5L)
  expect_equal(result$x2, 95L)
  expect_true(result$pad_y1 > 0L)
  expect_true(result$pad_x2 < 0L)
})

test_that("retro_data_panel() errors when a too-small window misses the axis", {
  input <- retro_panel_synthetic_image()
  on.exit(unlink(input))
  ticks <- retro_panel_synthetic_ticks()
  # Place y-axis tick labels far to the left (x2 = 5), so the tiny band

  # [5, 5+1] does not reach the true y-axis line at column 20.
  ticks$y_axis <- rbind(
    retro_panel_synthetic_tick("C", 0, x = 3, y = 80),
    retro_panel_synthetic_tick("D", 1, x = 3, y = 10)
  )
  expect_error(
    retro_data_panel(input, ticks$x_axis, ticks$y_axis, window = 0.1),
    "Could not find the y-axis"
  )
})

test_that("retro_data_panel() a wider window recovers from distant tick labels", {
  input <- retro_panel_synthetic_image()
  on.exit(unlink(input))
  ticks <- retro_panel_synthetic_ticks()
  ticks$y_axis <- rbind(
    retro_panel_synthetic_tick("C", 0, x = 3, y = 80),
    retro_panel_synthetic_tick("D", 1, x = 3, y = 10)
  )
  result <- retro_data_panel(
    input,
    ticks$x_axis,
    ticks$y_axis,
    window = 10
  )
  expect_true(result$x1 <= 20L)
})

test_that("retro_data_panel() errors when the y-axis tick-label edge is off-canvas", {
  input <- retro_panel_synthetic_image()
  on.exit(unlink(input))
  ticks <- retro_panel_synthetic_ticks()
  # y-axis tick labels with x2 = 202, outside the 100-wide image.
  ticks$y_axis <- rbind(
    retro_panel_synthetic_tick("C", 0, x = 200, y = 80),
    retro_panel_synthetic_tick("D", 1, x = 200, y = 10)
  )
  expect_error(
    retro_data_panel(input, ticks$x_axis, ticks$y_axis, window = 0.1),
    "Could not find the y-axis. The tick-label edge"
  )
})

test_that("retro_data_panel() errors when the x-axis tick-label edge is off-canvas", {
  input <- retro_panel_synthetic_image()
  on.exit(unlink(input))
  ticks <- retro_panel_synthetic_ticks()
  # x-axis tick labels with y1 = -3, outside the image (row < 1).
  ticks$x_axis <- rbind(
    retro_panel_synthetic_tick("A", 0, x = 20, y = 0, height = 6),
    retro_panel_synthetic_tick("B", 12, x = 90, y = 0, height = 6)
  )
  expect_error(
    retro_data_panel(input, ticks$x_axis, ticks$y_axis),
    "Could not find the x-axis. The tick-label edge"
  )
})

test_that("retro_data_panel() requires exactly 2 rows per axis", {
  input <- retro_panel_synthetic_image()
  on.exit(unlink(input))
  ticks <- retro_panel_synthetic_ticks()
  expect_error(
    retro_data_panel(input, ticks$x_axis[1L, ], ticks$y_axis),
    "x_axis must have exactly 2 rows"
  )
  expect_error(
    retro_data_panel(input, ticks$x_axis, ticks$y_axis[1L, ]),
    "y_axis must have exactly 2 rows"
  )
})

test_that("retro_data_panel() requires the two tick values to differ", {
  input <- retro_panel_synthetic_image()
  on.exit(unlink(input))
  ticks <- retro_panel_synthetic_ticks()
  ticks$x_axis$value <- c(0, 0)
  expect_error(
    retro_data_panel(input, ticks$x_axis, ticks$y_axis),
    "x_axis\\$value"
  )
})

test_that("retro_panel_anchor() returns tick-label edges", {
  x_axis <- data.frame(y1 = c(83L, 83L), y2 = c(89L, 89L))
  y_axis <- data.frame(x2 = c(17L, 17L))
  anchor <- retro_panel_anchor(x_axis, y_axis)
  expect_equal(anchor$y_axis_column, 17L)
  expect_equal(anchor$x_axis_row, 83L)
})

test_that("retro_panel_anchor() picks the extreme edges across both ticks", {
  x_axis <- data.frame(y1 = c(83L, 85L), y2 = c(89L, 91L))
  y_axis <- data.frame(x2 = c(15L, 17L))
  anchor <- retro_panel_anchor(x_axis, y_axis)
  expect_equal(anchor$y_axis_column, 17L)
  expect_equal(anchor$x_axis_row, 83L)
})

test_that("retro_panel_band() scales with median tick height and window", {
  x_axis <- data.frame(y1 = c(77, 77), y2 = c(83, 83))
  y_axis <- data.frame(y1 = c(77, 7), y2 = c(83, 13))
  expect_equal(retro_panel_band(x_axis, y_axis, window = 4), 24L)
  expect_equal(retro_panel_band(x_axis, y_axis, window = 0.1), 1L)
})

test_that("retro_panel_band() is at least 1", {
  x_axis <- data.frame(y1 = c(0, 0), y2 = c(0, 0))
  y_axis <- data.frame(y1 = c(0, 0), y2 = c(0, 0))
  expect_equal(retro_panel_band(x_axis, y_axis, window = 0.0001), 1L)
})

test_that("retro_panel_find_axis() finds a vertical line", {
  foreground <- matrix(FALSE, nrow = 100, ncol = 100)
  foreground[10:80, 20] <- TRUE
  result <- retro_panel_find_axis(foreground, columns = 1:30, axis = "y")
  expect_equal(result$x, c(20L, 20L))
  expect_equal(result$y, c(10L, 80L))
})

test_that("retro_panel_find_axis() picks the longest run over a shorter one", {
  foreground <- matrix(FALSE, nrow = 100, ncol = 100)
  foreground[10:80, 15] <- TRUE
  foreground[40:50, 25] <- TRUE
  result <- retro_panel_find_axis(foreground, columns = 1:30, axis = "y")
  expect_equal(result$x[1L], 15L)
})

test_that("retro_panel_find_axis() picks the center of a thick axis line", {
  foreground <- matrix(FALSE, nrow = 100, ncol = 100)
  # A 5-pixel-thick stroke (columns 18-22), as a real axis line would be
  # after anti-aliasing, rather than a single-pixel-wide line.
  foreground[10:80, 18:22] <- TRUE
  result <- retro_panel_find_axis(foreground, columns = 1:30, axis = "y")
  expect_equal(result$x[1L], 20L)
  expect_equal(result$y, c(10L, 80L))
})

test_that("retro_panel_find_axis() centers an even-thickness line", {
  foreground <- matrix(FALSE, nrow = 100, ncol = 100)
  foreground[10:80, 18:21] <- TRUE
  result <- retro_panel_find_axis(foreground, columns = 1:30, axis = "y")
  expect_true(result$x[1L] %in% c(19L, 20L))
})

test_that("retro_panel_find_axis() errors when no axis found", {
  foreground <- matrix(FALSE, nrow = 100, ncol = 100)
  foreground[10:20, 50] <- TRUE
  expect_error(
    retro_panel_find_axis(foreground, columns = 1:30, axis = "y"),
    "Could not find the y-axis"
  )
  expect_error(
    retro_panel_find_axis(foreground, columns = 60:80, axis = "x"),
    "Could not find the x-axis"
  )
})

test_that("retro_panel_find_line() finds a vertical line", {
  foreground <- matrix(FALSE, nrow = 100, ncol = 100)
  foreground[10:80, 20] <- TRUE
  result <- retro_panel_find_line(foreground, columns = 1:30)
  expect_equal(result$x, c(20L, 20L))
  expect_equal(result$y, c(10L, 80L))
})

test_that("retro_panel_find_line() returns NULL below the noise floor", {
  foreground <- matrix(FALSE, nrow = 100, ncol = 100)
  foreground[10:20, 50] <- TRUE
  expect_null(retro_panel_find_line(foreground, columns = 1:30))
  expect_null(retro_panel_find_line(foreground, columns = 60:80))
})

test_that("retro_panel_find_line() rejects a run below a third of the image dimension", {
  # A run of 20 clears the old fixed floor of 3, but not a third of this
  # 100-row image (34) - e.g. a Kaplan-Meier curve's own long flat
  # plateau, which should never be mistaken for an axis or bounding line.
  foreground <- matrix(FALSE, nrow = 100, ncol = 100)
  foreground[1:20, 50] <- TRUE
  expect_null(retro_panel_find_line(foreground, columns = 1:100))
})

test_that("retro_panel_find_line() accepts a run at least a third of the image dimension", {
  foreground <- matrix(FALSE, nrow = 100, ncol = 100)
  foreground[1:40, 50] <- TRUE
  result <- retro_panel_find_line(foreground, columns = 1:100)
  expect_equal(result$x, c(50L, 50L))
})

test_that("retro_panel_find_line() keeps a floor of 3 pixels for small images", {
  long_enough <- matrix(FALSE, nrow = 5, ncol = 3)
  long_enough[1:3, 1] <- TRUE
  expect_false(is.null(retro_panel_find_line(long_enough, columns = 1:3)))
  too_short <- matrix(FALSE, nrow = 5, ncol = 3)
  too_short[1:2, 1] <- TRUE
  expect_null(retro_panel_find_line(too_short, columns = 1:3))
})

test_that("retro_panel_line_pad() measures a vertical line's thickness", {
  pixel_matrix <- matrix("#ffffffff", nrow = 100, ncol = 100)
  pixel_matrix[10:80, 20] <- "#000000ff"
  input <- tempfile(fileext = ".png")
  on.exit(unlink(input))
  magick::image_write(retro_test_image(pixel_matrix), input)
  endpoints <- data.frame(x = c(20L, 20L), y = c(10L, 80L))
  expect_equal(retro_panel_line_pad(input, endpoints, median_height = 6), 3L)
})

test_that("retro_panel_line_pad() measures a horizontal line's thickness", {
  pixel_matrix <- matrix("#ffffffff", nrow = 100, ncol = 100)
  pixel_matrix[80, 20:90] <- "#000000ff"
  input <- tempfile(fileext = ".png")
  on.exit(unlink(input))
  magick::image_write(retro_test_image(pixel_matrix), input)
  endpoints <- data.frame(x = c(20L, 90L), y = c(80L, 80L))
  expect_equal(retro_panel_line_pad(input, endpoints, median_height = 6), 3L)
})

test_that("retro_panel_position_y_axis() returns endpoints and its own pad", {
  input <- retro_panel_synthetic_image()
  on.exit(unlink(input))
  foreground <- retro_components_foreground_mask(magick::image_read(input))
  result <- retro_panel_position_y_axis(
    foreground = foreground,
    anchor = list(y_axis_column = 17L, x_axis_row = 83L),
    band_width = 24L,
    input = input,
    median_height = 6
  )
  expect_equal(result$endpoints$x, c(20L, 20L))
  expect_equal(result$endpoints$y, c(10L, 80L))
  expect_equal(result$pad, 3L)
})

test_that("retro_panel_position_x_axis() returns endpoints and its own pad", {
  input <- retro_panel_synthetic_image()
  on.exit(unlink(input))
  foreground <- retro_components_foreground_mask(magick::image_read(input))
  result <- retro_panel_position_x_axis(
    foreground = foreground,
    anchor = list(y_axis_column = 17L, x_axis_row = 83L),
    band_width = 24L,
    input = input,
    median_height = 6
  )
  expect_equal(result$endpoints$x, c(20L, 90L))
  expect_equal(result$endpoints$y, c(80L, 80L))
  expect_equal(result$pad, 3L)
})

test_that("retro_panel_position_top() pads outward when no top line exists", {
  input <- retro_panel_synthetic_image()
  on.exit(unlink(input))
  foreground <- retro_components_foreground_mask(magick::image_read(input))
  result <- retro_panel_position_top(
    foreground = foreground,
    x1 = 20L,
    x2 = 90L,
    y1 = 10L,
    band_width = 24L,
    input = input,
    median_height = 6,
    outward_margin = 3L
  )
  expect_equal(result$y1, 10L)
  expect_equal(result$pad_y1, -3L)
})

test_that("retro_panel_position_top() finds a top line and pads inward", {
  input <- retro_panel_synthetic_boxed_image()
  on.exit(unlink(input))
  foreground <- retro_components_foreground_mask(magick::image_read(input))
  result <- retro_panel_position_top(
    foreground = foreground,
    x1 = 20L,
    x2 = 90L,
    y1 = 10L,
    band_width = 24L,
    input = input,
    median_height = 6,
    outward_margin = 3L
  )
  expect_equal(result$y1, 5L)
  expect_true(result$pad_y1 > 0L)
})

test_that("retro_panel_position_top() clips the outward margin to available room", {
  foreground <- matrix(FALSE, nrow = 100, ncol = 100)
  foreground[2:80, 20] <- TRUE
  foreground[80, 20:90] <- TRUE
  pixel_matrix <- matrix("#ffffffff", nrow = 100, ncol = 100)
  pixel_matrix[foreground] <- "#000000ff"
  input <- tempfile(fileext = ".png")
  on.exit(unlink(input))
  magick::image_write(retro_test_image(pixel_matrix), input)
  result <- retro_panel_position_top(
    foreground = foreground,
    x1 = 20L,
    x2 = 90L,
    y1 = 2L,
    band_width = 24L,
    input = input,
    median_height = 6,
    outward_margin = 20L
  )
  expect_equal(result$y1, 2L)
  expect_equal(result$pad_y1, -1L)
})

test_that("retro_panel_position_right() pads outward when no right line exists", {
  input <- retro_panel_synthetic_image()
  on.exit(unlink(input))
  foreground <- retro_components_foreground_mask(magick::image_read(input))
  result <- retro_panel_position_right(
    foreground = foreground,
    x2 = 90L,
    y1 = 10L,
    y2 = 80L,
    band_width = 24L,
    input = input,
    median_height = 6,
    outward_margin = 3L
  )
  expect_equal(result$x2, 90L)
  expect_equal(result$pad_x2, 3L)
})

test_that("retro_panel_position_right() finds a right line and pads inward", {
  input <- retro_panel_synthetic_boxed_image()
  on.exit(unlink(input))
  foreground <- retro_components_foreground_mask(magick::image_read(input))
  result <- retro_panel_position_right(
    foreground = foreground,
    x2 = 90L,
    y1 = 10L,
    y2 = 80L,
    band_width = 24L,
    input = input,
    median_height = 6,
    outward_margin = 3L
  )
  expect_equal(result$x2, 95L)
  expect_true(result$pad_x2 < 0L)
})

test_that("retro_panel_position_right() clips the outward margin to available room", {
  foreground <- matrix(FALSE, nrow = 100, ncol = 100)
  foreground[10:80, 20] <- TRUE
  foreground[80, 20:98] <- TRUE
  pixel_matrix <- matrix("#ffffffff", nrow = 100, ncol = 100)
  pixel_matrix[foreground] <- "#000000ff"
  input <- tempfile(fileext = ".png")
  on.exit(unlink(input))
  magick::image_write(retro_test_image(pixel_matrix), input)
  result <- retro_panel_position_right(
    foreground = foreground,
    x2 = 98L,
    y1 = 10L,
    y2 = 80L,
    band_width = 24L,
    input = input,
    median_height = 6,
    outward_margin = 20L
  )
  expect_equal(result$x2, 98L)
  expect_equal(result$pad_x2, 2L)
})

test_that("retro_panel_max_run() handles edge cases", {
  expect_equal(retro_panel_max_run(logical(0L)), 0L)
  expect_equal(retro_panel_max_run(rep(FALSE, 5L)), 0L)
  expect_equal(retro_panel_max_run(rep(TRUE, 5L)), 5L)
  expect_equal(
    retro_panel_max_run(c(TRUE, TRUE, FALSE, TRUE, TRUE, TRUE)),
    3L
  )
})
