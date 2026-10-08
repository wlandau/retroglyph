# A valid baseline argument list for retro_image_layer_survival(): a single
# series, "Placebo", with 2 reconstructed patients (one event at time 5, one
# censored at time 10) and axis calibration chosen so every pixel position
# below is an exact integer:
#   x: value 0 -> pixel 1, value 10 -> pixel 19 (slope 1.8, intercept 1)
#   y: value 1 -> pixel 1, value 0 -> pixel 21 (slope -20, intercept 21)
# The refit KM curve is flat at 1 from t=0 to t=5, drops to 0.5 at t=5,
# flat at 0.5 from t=5 to its last observation, the censoring at t=10. In
# pixel space that's: row 1 from column 1 to 10, column 10 from row 1 to
# 11, row 11 from column 10 to 19.
layer_args <- function() {
  list(
    data = tibble::tibble(
      series = factor(rep("Placebo", 2), levels = "Placebo"),
      time = c(5, 10),
      status = c(1, 0)
    ),
    output = tempfile(fileext = ".png"),
    layers = "#dc3030",
    background = "#ffffff",
    x_axis = tibble::tibble(
      label = c("A", "B"),
      value = c(0, 10),
      x = c(1, 19)
    ),
    y_axis = tibble::tibble(label = c("C", "D"), value = c(0, 1), y = c(21, 1)),
    legend = tibble::tibble(series = factor("Placebo"), color = "#dc3030"),
    width = 20L,
    height = 21L,
    line_width = 1L,
    max_y = 1,
    increasing = FALSE
  )
}

# --- Argument validation -----------------------------------------------------

test_that("retro_image_layer_survival() errors if data is missing required columns", {
  args <- layer_args()
  on.exit(unlink(args$output))
  args$data <- tibble::tibble(x = 1)
  expect_error(
    do.call(retro_image_layer_survival, args),
    "data must be a tibble with columns series, time, status"
  )
})

test_that("retro_image_layer_survival() errors if output is not a single string", {
  args <- layer_args()
  on.exit(unlink(args$output))
  args$output <- c("a.png", "b.png")
  expect_error(
    do.call(retro_image_layer_survival, args),
    "output must be a single string"
  )
  args$output <- 123
  expect_error(
    do.call(retro_image_layer_survival, args),
    "output must be a single string"
  )
})

test_that("retro_image_layer_survival() errors if layers is not character", {
  args <- layer_args()
  on.exit(unlink(args$output))
  args$layers <- 1L
  expect_error(
    do.call(retro_image_layer_survival, args),
    "layers must be a character vector"
  )
})

test_that("retro_image_layer_survival() errors if layers has duplicates", {
  args <- layer_args()
  on.exit(unlink(args$output))
  args$layers <- c("#dc3030", "#dc3030")
  expect_error(
    do.call(retro_image_layer_survival, args),
    "layers must not contain duplicates"
  )
})

test_that("retro_image_layer_survival() errors if x_axis is malformed", {
  args <- layer_args()
  on.exit(unlink(args$output))
  args$x_axis <- tibble::tibble(label = "A", value = 0, x = 1)
  expect_error(
    do.call(retro_image_layer_survival, args),
    "x_axis must be a data frame with columns value and x"
  )
})

test_that("retro_image_layer_survival() errors if y_axis is malformed", {
  args <- layer_args()
  on.exit(unlink(args$output))
  args$y_axis <- tibble::tibble(label = "C", value = 0)
  expect_error(
    do.call(retro_image_layer_survival, args),
    "y_axis must be a data frame with columns value and y"
  )
})

test_that("retro_image_layer_survival() errors if legend is missing required columns", {
  args <- layer_args()
  on.exit(unlink(args$output))
  args$legend <- tibble::tibble(series = "Placebo")
  expect_error(
    do.call(retro_image_layer_survival, args),
    "legend must be a tibble with columns series and color"
  )
})

test_that("retro_image_layer_survival() errors on non-positive width/height", {
  args <- layer_args()
  on.exit(unlink(args$output))
  args$width <- 0L
  expect_error(
    do.call(retro_image_layer_survival, args),
    "width must be a single positive number"
  )
  args <- layer_args()
  args$height <- -1L
  expect_error(
    do.call(retro_image_layer_survival, args),
    "height must be a single positive number"
  )
})

test_that("retro_image_layer_survival() errors on non-positive line_width", {
  args <- layer_args()
  on.exit(unlink(args$output))
  args$line_width <- 0L
  expect_error(
    do.call(retro_image_layer_survival, args),
    "line_width must be a single positive number"
  )
})

test_that("retro_image_layer_survival() errors on non-positive max_y", {
  args <- layer_args()
  on.exit(unlink(args$output))
  args$max_y <- 0
  expect_error(
    do.call(retro_image_layer_survival, args),
    "max_y must be a single positive number"
  )
})

test_that("retro_image_layer_survival() errors on a non-scalar-logical increasing", {
  args <- layer_args()
  on.exit(unlink(args$output))
  args$increasing <- "yes"
  expect_error(
    do.call(retro_image_layer_survival, args),
    "increasing must be a single non-missing logical"
  )
  args$increasing <- NA
  expect_error(
    do.call(retro_image_layer_survival, args),
    "increasing must be a single non-missing logical"
  )
})

test_that("retro_image_layer_survival() errors on a non-scalar-logical censoring", {
  args <- layer_args()
  on.exit(unlink(args$output))
  args$censoring <- "yes"
  expect_error(
    do.call(retro_image_layer_survival, args),
    "censoring must be a single non-missing logical"
  )
  args$censoring <- NA
  expect_error(
    do.call(retro_image_layer_survival, args),
    "censoring must be a single non-missing logical"
  )
})

test_that("retro_image_layer_survival() errors if layers is missing a legend color", {
  args <- layer_args()
  on.exit(unlink(args$output))
  args$legend <- tibble::tibble(
    series = factor(c("Placebo", "Drug"), levels = c("Placebo", "Drug")),
    color = c("#dc3030", "#3030a0")
  )
  expect_error(
    do.call(retro_image_layer_survival, args),
    "layers must contain all colors in legend"
  )
})

test_that("retro_image_layer_survival() errors if layers has extra colors", {
  args <- layer_args()
  on.exit(unlink(args$output))
  args$layers <- c("#dc3030", "#00ff00")
  expect_error(
    do.call(retro_image_layer_survival, args),
    "layers must not contain colors absent from legend"
  )
})

# --- Output correctness ------------------------------------------------------

test_that("retro_image_layer_survival() writes a PNG with correct dimensions", {
  args <- layer_args()
  on.exit(unlink(args$output))
  do.call(retro_image_layer_survival, args)
  expect_true(file.exists(args$output))
  info <- magick::image_info(magick::image_read(args$output))
  expect_equal(info$width, args$width)
  expect_equal(info$height, args$height)
})

test_that("retro_image_layer_survival() returns invisible NULL", {
  args <- layer_args()
  on.exit(unlink(args$output))
  result <- do.call(retro_image_layer_survival, args)
  expect_null(result)
})

test_that("retro_image_layer_survival() paints the refit curve at the calibrated pixel positions", {
  args <- layer_args()
  on.exit(unlink(args$output))
  do.call(retro_image_layer_survival, args)
  raster <- magick::image_read(args$output) |>
    magick::image_raster(tidy = FALSE) |>
    as.matrix()
  # Flat segment at survival = 1, row 1, columns 1-10
  expect_equal(retro_color_rgb(raster[1, 1]), "#dc3030")
  expect_equal(retro_color_rgb(raster[1, 10]), "#dc3030")
  # Vertical drop at time = 5 (column 10), rows 1-11
  expect_equal(retro_color_rgb(raster[5, 10]), "#dc3030")
  expect_equal(retro_color_rgb(raster[11, 10]), "#dc3030")
  # Flat segment at survival = 0.5, row 11, columns 10-19
  expect_equal(retro_color_rgb(raster[11, 19]), "#dc3030")
  # Untouched corner stays background
  expect_equal(retro_color_rgb(raster[21, 20]), "#ffffff")
})

test_that("retro_image_layer_survival() uses the background argument correctly", {
  args <- layer_args()
  args$background <- "#cccccc"
  on.exit(unlink(args$output))
  do.call(retro_image_layer_survival, args)
  raster <- magick::image_read(args$output) |>
    magick::image_raster(tidy = FALSE) |>
    as.matrix()
  expect_equal(retro_color_rgb(raster[21, 20]), "#cccccc")
  expect_equal(retro_color_rgb(raster[1, 1]), "#dc3030")
})

test_that("retro_image_layer_survival() thickens the curve by line_width", {
  args <- layer_args()
  args$line_width <- 3L
  on.exit(unlink(args$output))
  do.call(retro_image_layer_survival, args)
  raster <- magick::image_read(args$output) |>
    magick::image_raster(tidy = FALSE) |>
    as.matrix()
  # The row-1 flat segment should now also paint rows 2 (half_width = 1
  # on either side of row 1, clamped at the top edge).
  expect_equal(retro_color_rgb(raster[2, 5]), "#dc3030")
})

test_that("retro_image_layer_survival() first layer wins where two series' curves overlap", {
  # Both series have identical IPD and calibration, so their refit curves
  # land on exactly the same pixels everywhere.
  data <- tibble::tibble(
    series = factor(rep(c("A", "B"), each = 2), levels = c("A", "B")),
    time = rep(c(5, 10), 2),
    status = rep(c(1, 0), 2)
  )
  legend <- tibble::tibble(
    series = factor(c("A", "B"), levels = c("A", "B")),
    color = c("#ff0000", "#0000ff")
  )
  args <- layer_args()
  args$data <- data
  args$legend <- legend
  output_red_top <- tempfile(fileext = ".png")
  output_blue_top <- tempfile(fileext = ".png")
  on.exit(unlink(c(output_red_top, output_blue_top, args$output)))
  args$output <- output_red_top
  args$layers <- c("#ff0000", "#0000ff")
  do.call(retro_image_layer_survival, args)
  args$output <- output_blue_top
  args$layers <- c("#0000ff", "#ff0000")
  do.call(retro_image_layer_survival, args)
  raster_red <- magick::image_read(output_red_top) |>
    magick::image_raster(tidy = FALSE) |>
    as.matrix()
  raster_blue <- magick::image_read(output_blue_top) |>
    magick::image_raster(tidy = FALSE) |>
    as.matrix()
  expect_equal(retro_color_rgb(raster_red[1, 1]), "#ff0000")
  expect_equal(retro_color_rgb(raster_red[11, 19]), "#ff0000")
  expect_equal(retro_color_rgb(raster_blue[1, 1]), "#0000ff")
  expect_equal(retro_color_rgb(raster_blue[11, 19]), "#0000ff")
})

test_that("retro_image_layer_survival() ends each series at its own last observation", {
  # Series A has 4 patients: one event at t=5 (survival 1 -> 0.75) and three
  # censored at t=10. Series B has 2: one event at t=5 (survival 1 -> 0.5)
  # and one censored at t=6. The two tails therefore sit on different rows,
  # so each is visible on its own. Under layer_args() calibration
  # (pixel_x = 1.8 * time + 1, pixel_y = 21 - 20 * survival):
  #   A's tail is row 6, columns 10 to 19.
  #   B's tail is row 11, columns 10 to 12 (t=6 rounds to column 12).
  # Nothing at all is drawn on row 11 past column 12. Before the endpoint
  # became series-specific, B was padded out to the shared risk-table time
  # and ran all the way to column 19 alongside A.
  data <- tibble::tibble(
    series = factor(
      c(rep("A", 4), rep("B", 2)),
      levels = c("A", "B")
    ),
    time = c(5, 10, 10, 10, 5, 6),
    status = c(1, 0, 0, 0, 1, 0)
  )
  legend <- tibble::tibble(
    series = factor(c("A", "B"), levels = c("A", "B")),
    color = c("#ff0000", "#0000ff")
  )
  args <- layer_args()
  on.exit(unlink(args$output))
  args$data <- data
  args$legend <- legend
  args$layers <- c("#ff0000", "#0000ff")
  do.call(retro_image_layer_survival, args)
  raster <- magick::image_read(args$output) |>
    magick::image_raster(tidy = FALSE) |>
    as.matrix()
  # The longer series reaches its own endpoint.
  expect_equal(retro_color_rgb(raster[6, 19]), "#ff0000")
  # The shorter series reaches its own endpoint and stops there.
  expect_equal(retro_color_rgb(raster[11, 12]), "#0000ff")
  expect_equal(retro_color_rgb(raster[11, 13]), "#ffffff")
  expect_equal(retro_color_rgb(raster[11, 19]), "#ffffff")
})

# --- censoring = TRUE --------------------------------------------------------

test_that("retro_image_layer_survival(censoring = TRUE) draws the curve wire-thin", {
  # The censoring = FALSE counterpart of this test expects row 2 painted
  # (half_width = 1 around the row-1 flat segment). Thinning is what makes a
  # 2-pixel tick visible at all, so line_width must stop mattering here.
  args <- layer_args()
  args$line_width <- 3L
  args$censoring <- TRUE
  on.exit(unlink(args$output))
  do.call(retro_image_layer_survival, args)
  raster <- magick::image_read(args$output) |>
    magick::image_raster(tidy = FALSE) |>
    as.matrix()
  expect_equal(retro_color_rgb(raster[1, 5]), "#dc3030")
  expect_equal(retro_color_rgb(raster[2, 5]), "#ffffff")
})

test_that("retro_image_layer_survival(censoring = TRUE) ticks the censoring time", {
  # The censoring at t=10 lands on column 19, on the survival = 0.5 flat run
  # at row 11, so the tick spans rows 9 to 13 and stops there.
  args <- layer_args()
  args$censoring <- TRUE
  on.exit(unlink(args$output))
  do.call(retro_image_layer_survival, args)
  raster <- magick::image_read(args$output) |>
    magick::image_raster(tidy = FALSE) |>
    as.matrix()
  expect_equal(retro_color_rgb(raster[9, 19]), "#dc3030")
  expect_equal(retro_color_rgb(raster[13, 19]), "#dc3030")
  expect_equal(retro_color_rgb(raster[14, 19]), "#ffffff")
  expect_equal(retro_color_rgb(raster[8, 19]), "#ffffff")
  # The event at t=5 (column 10) is not a censoring time, so the vertical
  # drop there is not extended past the curve.
  expect_equal(retro_color_rgb(raster[13, 10]), "#ffffff")
})

test_that("retro_image_layer_survival(censoring = TRUE) ticks nothing when no patient is censored", {
  # Both patients now have events, so t=10 drops the curve to 0 instead of
  # ending it with a censoring. Only the rows above the drop distinguish a
  # tick from the drop itself: rows 11 to 21 of column 19 are the drop.
  args <- layer_args()
  args$data$status <- c(1, 1)
  args$censoring <- TRUE
  on.exit(unlink(args$output))
  do.call(retro_image_layer_survival, args)
  raster <- magick::image_read(args$output) |>
    magick::image_raster(tidy = FALSE) |>
    as.matrix()
  expect_equal(retro_color_rgb(raster[1, 1]), "#dc3030")
  expect_equal(retro_color_rgb(raster[11, 19]), "#dc3030")
  expect_equal(retro_color_rgb(raster[9, 19]), "#ffffff")
  expect_equal(retro_color_rgb(raster[10, 19]), "#ffffff")
})

# --- retro_layer_steps() ------------------------------------------------

test_that("retro_layer_steps() builds a flat-drop-flat vertex sequence", {
  ipd <- tibble::tibble(time = c(5, 10), status = c(1, 0))
  result <- retro_layer_steps(ipd, max_y = 1, increasing = FALSE)
  expect_equal(result$x, c(0, 5, 5, 10))
  expect_equal(result$y, c(1, 1, 0.5, 0.5))
})

test_that("retro_layer_steps() rescales to a 0-100 percentage", {
  ipd <- tibble::tibble(time = c(5, 10), status = c(1, 0))
  result <- retro_layer_steps(ipd, max_y = 100, increasing = FALSE)
  expect_equal(result$y, c(100, 100, 50, 50))
})

test_that("retro_layer_steps() flips for cumulative incidence", {
  ipd <- tibble::tibble(time = c(5, 10), status = c(1, 0))
  result <- retro_layer_steps(ipd, max_y = 1, increasing = TRUE)
  expect_equal(result$y, c(0, 0, 0.5, 0.5))
})

test_that("retro_layer_steps() stops at the last observed time", {
  # The last observation is the censoring at t=10, not the last event at
  # t=5, and nothing extends the curve past it.
  ipd <- tibble::tibble(time = c(5, 10), status = c(1, 0))
  result <- retro_layer_steps(ipd, max_y = 1, increasing = FALSE)
  expect_equal(result$x[length(result$x)], 10)
  expect_equal(result$y[length(result$y)], 0.5)
})

test_that("retro_layer_steps() endpoints come from each series alone", {
  # Same events, different follow-up. Nothing about one call can reach the
  # other, so the endpoints must differ.
  short <- tibble::tibble(time = c(5, 6), status = c(1, 0))
  long <- tibble::tibble(time = c(5, 40), status = c(1, 0))
  result_short <- retro_layer_steps(short, max_y = 1, increasing = FALSE)
  result_long <- retro_layer_steps(long, max_y = 1, increasing = FALSE)
  expect_equal(result_short$x[length(result_short$x)], 6)
  expect_equal(result_long$x[length(result_long$x)], 40)
})

test_that("retro_layer_steps() is a flat line when no events occur", {
  ipd <- tibble::tibble(time = c(5, 10), status = c(0, 0))
  result <- retro_layer_steps(ipd, max_y = 1, increasing = FALSE)
  expect_true(all(result$y == 1))
  expect_equal(result$x[length(result$x)], 10)
})

# --- retro_layer_censoring_data() --------------------------------------------

test_that("retro_layer_censoring_data() returns the censoring times and curve heights", {
  ipd <- tibble::tibble(time = c(5, 10), status = c(1, 0))
  result <- retro_layer_censoring_data(ipd, max_y = 1, increasing = FALSE)
  expect_equal(result$x, 10)
  expect_equal(result$y, 0.5)
})

test_that("retro_layer_censoring_data() ignores times with events only", {
  # Events at t=2 and t=6, censoring at t=4 and t=8.
  ipd <- tibble::tibble(time = c(2, 4, 6, 8), status = c(1, 0, 1, 0))
  result <- retro_layer_censoring_data(ipd, max_y = 1, increasing = FALSE)
  expect_equal(result$x, c(4, 8))
})

test_that("retro_layer_censoring_data() rescales to a 0-100 percentage", {
  ipd <- tibble::tibble(time = c(5, 10), status = c(1, 0))
  result <- retro_layer_censoring_data(ipd, max_y = 100, increasing = FALSE)
  expect_equal(result$y, 50)
})

test_that("retro_layer_censoring_data() flips for cumulative incidence", {
  # One event at t=5 out of 4 patients, so survival is 0.75 at the t=10
  # censoring and incidence is 0.25 - a value the flip cannot land on by
  # symmetry.
  ipd <- tibble::tibble(time = c(5, 10, 10, 10), status = c(1, 0, 0, 0))
  result <- retro_layer_censoring_data(ipd, max_y = 1, increasing = TRUE)
  expect_equal(result$y, 0.25)
})

test_that("retro_layer_censoring_data() returns zero rows when nothing is censored", {
  ipd <- tibble::tibble(time = c(5, 10), status = c(1, 1))
  result <- retro_layer_censoring_data(ipd, max_y = 1, increasing = FALSE)
  expect_equal(nrow(result), 0L)
  expect_equal(names(result), c("x", "y"))
})

# --- retro_layer_survival_draw() ----------------------------------------

test_that("retro_layer_survival_draw() paints the step function and returns the matrix", {
  # Identity-ish calibration (pixel = value + 1) on a 1-to-1 grid, so the
  # flat-drop-flat curve for one event at t=5 and one censoring at t=10 is
  # row 2 (survival 1) from column 1 to 6, column 6 from row 2 to 2 (the
  # drop to 0.5 rounds to the same row), and row 2 from column 6 to 11.
  ipd <- tibble::tibble(time = c(5, 10), status = c(1, 0))
  calibration <- list(slope = 1, intercept = 1)
  result <- retro_layer_survival_draw(
    pixel_matrix = matrix("#ffffff", nrow = 20L, ncol = 20L),
    ipd = ipd,
    color = "#dc3030",
    x_calibration = calibration,
    y_calibration = list(slope = -10, intercept = 15),
    half_width = 0L,
    width = 20L,
    height = 20L,
    max_y = 1,
    increasing = FALSE
  )
  expect_true(is.matrix(result))
  expect_equal(dim(result), c(20L, 20L))
  # Flat at survival = 1 -> row 5; flat at 0.5 -> row 10.
  expect_equal(result[5L, 1L], "#dc3030")
  expect_equal(result[5L, 6L], "#dc3030")
  expect_equal(result[10L, 11L], "#dc3030")
  expect_equal(result[20L, 20L], "#ffffff")
})

test_that("retro_layer_survival_draw() paints over what is already on the canvas", {
  ipd <- tibble::tibble(time = c(5, 10), status = c(1, 0))
  calibration <- list(slope = 1, intercept = 1)
  result <- retro_layer_survival_draw(
    pixel_matrix = matrix("#0000ff", nrow = 20L, ncol = 20L),
    ipd = ipd,
    color = "#dc3030",
    x_calibration = calibration,
    y_calibration = list(slope = -10, intercept = 15),
    half_width = 0L,
    width = 20L,
    height = 20L,
    max_y = 1,
    increasing = FALSE
  )
  expect_equal(result[5L, 1L], "#dc3030")
  expect_equal(result[20L, 20L], "#0000ff")
})

# --- retro_layer_censoring_draw() ---------------------------------------

test_that("retro_layer_censoring_draw() paints a 5-pixel tick at each censoring time", {
  # The censoring at t=10 maps to column 11 and row 10, so the tick spans
  # rows 8 to 12 and stops there.
  ipd <- tibble::tibble(time = c(5, 10), status = c(1, 0))
  result <- retro_layer_censoring_draw(
    pixel_matrix = matrix("#ffffff", nrow = 20L, ncol = 20L),
    ipd = ipd,
    color = "#dc3030",
    x_calibration = list(slope = 1, intercept = 1),
    y_calibration = list(slope = -10, intercept = 15),
    width = 20L,
    height = 20L,
    max_y = 1,
    increasing = FALSE
  )
  expect_equal(result[8L:12L, 11L], rep("#dc3030", 5L))
  expect_equal(result[7L, 11L], "#ffffff")
  expect_equal(result[13L, 11L], "#ffffff")
  # The event at t=5 (column 6) gets no tick.
  expect_equal(result[8L, 6L], "#ffffff")
})

test_that("retro_layer_censoring_draw() leaves the canvas untouched with no censoring", {
  ipd <- tibble::tibble(time = c(5, 10), status = c(1, 1))
  before <- matrix("#ffffff", nrow = 20L, ncol = 20L)
  result <- retro_layer_censoring_draw(
    pixel_matrix = before,
    ipd = ipd,
    color = "#dc3030",
    x_calibration = list(slope = 1, intercept = 1),
    y_calibration = list(slope = -10, intercept = 15),
    width = 20L,
    height = 20L,
    max_y = 1,
    increasing = FALSE
  )
  expect_equal(result, before)
})

# --- retro_layer_segment() -----------------------------------------------

test_that("retro_layer_segment() returns a horizontal run thickened by half_width", {
  result <- retro_layer_segment(
    x1 = 2L,
    y1 = 5L,
    x2 = 6L,
    y2 = 5L,
    half_width = 1L,
    width = 20L,
    height = 20L
  )
  expect_true(all(result$x %in% 2:6))
  expect_true(all(result$y %in% 4:6))
  expect_equal(length(result$x), 5L * 3L)
})

test_that("retro_layer_segment() returns a vertical run thickened by half_width", {
  result <- retro_layer_segment(
    x1 = 5L,
    y1 = 2L,
    x2 = 5L,
    y2 = 6L,
    half_width = 1L,
    width = 20L,
    height = 20L
  )
  expect_true(all(result$x %in% 4:6))
  expect_true(all(result$y %in% 2:6))
  expect_equal(length(result$x), 5L * 3L)
})

test_that("retro_layer_segment() clamps to canvas bounds", {
  result <- retro_layer_segment(
    x1 = 1L,
    y1 = 1L,
    x2 = 1L,
    y2 = 1L,
    half_width = 3L,
    width = 5L,
    height = 5L
  )
  expect_true(all(result$x >= 1L & result$x <= 5L))
  expect_true(all(result$y >= 1L & result$y <= 5L))
})
