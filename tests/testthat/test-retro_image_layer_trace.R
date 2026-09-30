# A valid baseline argument list for retro_image_layer_trace(): a single
# series, "Placebo", whose already-digitized trace is the same flat-drop-flat
# step shape as test-retro_image_layer_survival.R's layer_args() fixture (so
# the calibration below paints the identical pixels), but supplied directly
# as x/y vertices rather than as time/status individual patient data:
#   x: value 0 -> pixel 1, value 10 -> pixel 19 (slope 1.8, intercept 1)
#   y: value 1 -> pixel 1, value 0 -> pixel 21 (slope -20, intercept 21)
# Flat at 1 from x=0 to x=5, drops to 0.5 at x=5, flat at 0.5 from x=5 to
# x=10. In pixel space that's: row 1 from column 1 to 10, column 10 from
# row 1 to 11, row 11 from column 10 to 19.
trace_args <- function() {
  list(
    data = tibble::tibble(
      series = factor(rep("Placebo", 4), levels = "Placebo"),
      x = c(0, 5, 5, 10),
      y = c(1, 1, 0.5, 0.5)
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

test_that("retro_image_layer_trace() errors if data is missing required columns", {
  args <- trace_args()
  on.exit(unlink(args$output))
  args$data <- tibble::tibble(x = 1)
  expect_error(
    do.call(retro_image_layer_trace, args),
    "data must be a tibble with columns series, x, y"
  )
})

test_that("retro_image_layer_trace() errors if output is not a single string", {
  args <- trace_args()
  on.exit(unlink(args$output))
  args$output <- c("a.png", "b.png")
  expect_error(
    do.call(retro_image_layer_trace, args),
    "output must be a single string"
  )
  args$output <- 123
  expect_error(
    do.call(retro_image_layer_trace, args),
    "output must be a single string"
  )
})

test_that("retro_image_layer_trace() errors if layers is not character", {
  args <- trace_args()
  on.exit(unlink(args$output))
  args$layers <- 1L
  expect_error(
    do.call(retro_image_layer_trace, args),
    "layers must be a character vector"
  )
})

test_that("retro_image_layer_trace() errors if layers has duplicates", {
  args <- trace_args()
  on.exit(unlink(args$output))
  args$layers <- c("#dc3030", "#dc3030")
  expect_error(
    do.call(retro_image_layer_trace, args),
    "layers must not contain duplicates"
  )
})

test_that("retro_image_layer_trace() errors if x_axis is malformed", {
  args <- trace_args()
  on.exit(unlink(args$output))
  args$x_axis <- tibble::tibble(label = "A", value = 0, x = 1)
  expect_error(
    do.call(retro_image_layer_trace, args),
    "x_axis must be a data frame with columns value and x"
  )
})

test_that("retro_image_layer_trace() errors if y_axis is malformed", {
  args <- trace_args()
  on.exit(unlink(args$output))
  args$y_axis <- tibble::tibble(label = "C", value = 0)
  expect_error(
    do.call(retro_image_layer_trace, args),
    "y_axis must be a data frame with columns value and y"
  )
})

test_that("retro_image_layer_trace() errors if legend is missing required columns", {
  args <- trace_args()
  on.exit(unlink(args$output))
  args$legend <- tibble::tibble(series = "Placebo")
  expect_error(
    do.call(retro_image_layer_trace, args),
    "legend must be a tibble with columns series and color"
  )
})

test_that("retro_image_layer_trace() errors on non-positive width/height", {
  args <- trace_args()
  on.exit(unlink(args$output))
  args$width <- 0L
  expect_error(
    do.call(retro_image_layer_trace, args),
    "width must be a single positive number"
  )
  args <- trace_args()
  args$height <- -1L
  expect_error(
    do.call(retro_image_layer_trace, args),
    "height must be a single positive number"
  )
})

test_that("retro_image_layer_trace() errors on non-positive line_width", {
  args <- trace_args()
  on.exit(unlink(args$output))
  args$line_width <- 0L
  expect_error(
    do.call(retro_image_layer_trace, args),
    "line_width must be a single positive number"
  )
})

test_that("retro_image_layer_trace() errors on non-positive max_y", {
  args <- trace_args()
  on.exit(unlink(args$output))
  args$max_y <- 0
  expect_error(
    do.call(retro_image_layer_trace, args),
    "max_y must be a single positive number"
  )
})

test_that("retro_image_layer_trace() errors on a non-scalar-logical increasing", {
  args <- trace_args()
  on.exit(unlink(args$output))
  args$increasing <- "yes"
  expect_error(
    do.call(retro_image_layer_trace, args),
    "increasing must be a single non-missing logical"
  )
  args$increasing <- NA
  expect_error(
    do.call(retro_image_layer_trace, args),
    "increasing must be a single non-missing logical"
  )
})

test_that("retro_image_layer_trace() errors if layers is missing a legend color", {
  args <- trace_args()
  on.exit(unlink(args$output))
  args$legend <- tibble::tibble(
    series = factor(c("Placebo", "Drug"), levels = c("Placebo", "Drug")),
    color = c("#dc3030", "#3030a0")
  )
  expect_error(
    do.call(retro_image_layer_trace, args),
    "layers must contain all colors in legend"
  )
})

test_that("retro_image_layer_trace() errors if layers has extra colors", {
  args <- trace_args()
  on.exit(unlink(args$output))
  args$layers <- c("#dc3030", "#00ff00")
  expect_error(
    do.call(retro_image_layer_trace, args),
    "layers must not contain colors absent from legend"
  )
})

# --- Output correctness ------------------------------------------------------

test_that("retro_image_layer_trace() writes a PNG with correct dimensions", {
  args <- trace_args()
  on.exit(unlink(args$output))
  do.call(retro_image_layer_trace, args)
  expect_true(file.exists(args$output))
  info <- magick::image_info(magick::image_read(args$output))
  expect_equal(info$width, args$width)
  expect_equal(info$height, args$height)
})

test_that("retro_image_layer_trace() returns invisible NULL", {
  args <- trace_args()
  on.exit(unlink(args$output))
  result <- do.call(retro_image_layer_trace, args)
  expect_null(result)
})

test_that("retro_image_layer_trace() paints the trace at the calibrated pixel positions", {
  args <- trace_args()
  on.exit(unlink(args$output))
  do.call(retro_image_layer_trace, args)
  raster <- magick::image_read(args$output) |>
    magick::image_raster(tidy = FALSE) |>
    as.matrix()
  # Flat segment at survival = 1, row 1, columns 1-10
  expect_equal(retro_color_rgb(raster[1, 1]), "#dc3030")
  expect_equal(retro_color_rgb(raster[1, 10]), "#dc3030")
  # Vertical drop at x = 5 (column 10), rows 1-11
  expect_equal(retro_color_rgb(raster[5, 10]), "#dc3030")
  expect_equal(retro_color_rgb(raster[11, 10]), "#dc3030")
  # Flat segment at survival = 0.5, row 11, columns 10-19
  expect_equal(retro_color_rgb(raster[11, 19]), "#dc3030")
  # Untouched corner stays background
  expect_equal(retro_color_rgb(raster[21, 20]), "#ffffff")
})

test_that("retro_image_layer_trace() uses the background argument correctly", {
  args <- trace_args()
  args$background <- "#cccccc"
  on.exit(unlink(args$output))
  do.call(retro_image_layer_trace, args)
  raster <- magick::image_read(args$output) |>
    magick::image_raster(tidy = FALSE) |>
    as.matrix()
  expect_equal(retro_color_rgb(raster[21, 20]), "#cccccc")
  expect_equal(retro_color_rgb(raster[1, 1]), "#dc3030")
})

test_that("retro_image_layer_trace() thickens the curve by line_width", {
  args <- trace_args()
  args$line_width <- 3L
  on.exit(unlink(args$output))
  do.call(retro_image_layer_trace, args)
  raster <- magick::image_read(args$output) |>
    magick::image_raster(tidy = FALSE) |>
    as.matrix()
  # The row-1 flat segment should now also paint row 2 (half_width = 1 on
  # either side of row 1, clamped at the top edge).
  expect_equal(retro_color_rgb(raster[2, 5]), "#dc3030")
})

test_that("retro_image_layer_trace() rescales for a 0-100 percentage and cumulative incidence", {
  args <- trace_args()
  args$max_y <- 100
  args$increasing <- TRUE
  # A 0-100 cumulative-incidence axis is calibrated on that scale, not the
  # canonical 0-1 survival scale, so the y-axis ticks change too: value 0
  # and 100 still land on pixel rows 21 and 1 respectively (slope -0.2,
  # intercept 21).
  args$y_axis <- tibble::tibble(
    label = c("C", "D"),
    value = c(0, 100),
    y = c(21, 1)
  )
  on.exit(unlink(args$output))
  do.call(retro_image_layer_trace, args)
  raster <- magick::image_read(args$output) |>
    magick::image_raster(tidy = FALSE) |>
    as.matrix()
  # y = 1 (canonical survival scale) complements to 0 on the cumulative
  # incidence scale, which is row 21 (pixel_y = -0.2 * 0 + 21).
  expect_equal(retro_color_rgb(raster[21, 1]), "#dc3030")
  # y = 0.5 complements to 50, which is row 11 (pixel_y = -0.2 * 50 + 21).
  expect_equal(retro_color_rgb(raster[11, 10]), "#dc3030")
})

test_that("retro_image_layer_trace() first layer wins where two series' traces overlap", {
  # Both series have identical trace points and calibration, so they land on
  # exactly the same pixels everywhere.
  data <- tibble::tibble(
    series = factor(rep(c("A", "B"), each = 4), levels = c("A", "B")),
    x = rep(c(0, 5, 5, 10), 2),
    y = rep(c(1, 1, 0.5, 0.5), 2)
  )
  legend <- tibble::tibble(
    series = factor(c("A", "B"), levels = c("A", "B")),
    color = c("#ff0000", "#0000ff")
  )
  args <- trace_args()
  args$data <- data
  args$legend <- legend
  output_red_top <- tempfile(fileext = ".png")
  output_blue_top <- tempfile(fileext = ".png")
  on.exit(unlink(c(output_red_top, output_blue_top, args$output)))
  args$output <- output_red_top
  args$layers <- c("#ff0000", "#0000ff")
  do.call(retro_image_layer_trace, args)
  args$output <- output_blue_top
  args$layers <- c("#0000ff", "#ff0000")
  do.call(retro_image_layer_trace, args)
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
