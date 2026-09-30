ruler_data_panel <- tibble::tibble(
  x1 = 20L,
  x2 = 180L,
  y1 = 10L,
  y2 = 220L,
  pad_x1 = 3L,
  pad_x2 = -3L,
  pad_y1 = 3L,
  pad_y2 = -3L
)

ruler_data_label <- tibble::tibble(
  label = c("A", "B", "C", "D"),
  word = c("0", "10", "0", "1"),
  confidence = c(90, 90, 90, 90),
  x = c(50, 120, 10, 10),
  y = c(230, 230, 150, 50),
  x1 = c(40L, 110L, 0L, 0L),
  x2 = c(60L, 130L, 15L, 15L),
  y1 = c(222L, 222L, 140L, 40L),
  y2 = c(238L, 238L, 160L, 60L)
)

ruler_x_axis <- tibble::tibble(label = c("A", "B"), value = c(0, 10))
ruler_y_axis <- tibble::tibble(label = c("C", "D"), value = c(0, 1))

ruler_blank_input <- function() {
  input <- tempfile(fileext = ".png")
  magick::image_blank(200, 250, color = "#ffffffff") |>
    magick::image_write(input)
  input
}

test_that("retro_image_ruler() writes an output file", {
  input <- ruler_blank_input()
  output <- tempfile(fileext = ".png")
  on.exit(unlink(c(input, output)))
  retro_image_ruler(
    input = input,
    output = output,
    data_panel = ruler_data_panel,
    data_label = ruler_data_label,
    x_axis = ruler_x_axis,
    y_axis = ruler_y_axis
  )
  expect_true(file.exists(output))
})

test_that("retro_image_ruler() returns NULL invisibly", {
  input <- ruler_blank_input()
  output <- tempfile(fileext = ".png")
  on.exit(unlink(c(input, output)))
  result <- withVisible(
    retro_image_ruler(
      input = input,
      output = output,
      data_panel = ruler_data_panel,
      data_label = ruler_data_label,
      x_axis = ruler_x_axis,
      y_axis = ruler_y_axis
    )
  )
  expect_null(result$value)
  expect_false(result$visible)
})

test_that("retro_image_ruler() does not modify the input file", {
  input <- ruler_blank_input()
  output <- tempfile(fileext = ".png")
  on.exit(unlink(c(input, output)))
  md5_before <- tools::md5sum(input)
  retro_image_ruler(
    input = input,
    output = output,
    data_panel = ruler_data_panel,
    data_label = ruler_data_label,
    x_axis = ruler_x_axis,
    y_axis = ruler_y_axis
  )
  expect_equal(tools::md5sum(input), md5_before)
})

test_that("retro_image_ruler() preserves image dimensions", {
  input <- ruler_blank_input()
  output <- tempfile(fileext = ".png")
  on.exit(unlink(c(input, output)))
  retro_image_ruler(
    input = input,
    output = output,
    data_panel = ruler_data_panel,
    data_label = ruler_data_label,
    x_axis = ruler_x_axis,
    y_axis = ruler_y_axis
  )
  input_info <- magick::image_info(magick::image_read(input))
  output_info <- magick::image_info(magick::image_read(output))
  expect_equal(output_info$width, input_info$width)
  expect_equal(output_info$height, input_info$height)
})

test_that("retro_image_ruler() draws the x-axis and y-axis lines at the exact detected pixels", {
  input <- ruler_blank_input()
  output <- tempfile(fileext = ".png")
  on.exit(unlink(c(input, output)))
  retro_image_ruler(
    input = input,
    output = output,
    data_panel = ruler_data_panel,
    data_label = ruler_data_label,
    x_axis = ruler_x_axis,
    y_axis = ruler_y_axis
  )
  raster <- magick::image_raster(magick::image_read(output), tidy = TRUE)
  pixel <- function(x, y) raster$col[raster$x == x & raster$y == y]
  # x-axis: exactly row 220 (ruler_data_panel$y2), one pixel wide - no
  # anti-aliasing blur into the neighboring rows, unlike the old
  # graphics::segments()-based drawing this replaces.
  expect_equal(pixel(100L, 220L), "#000000ff")
  expect_equal(pixel(100L, 219L), "#ffffffff")
  expect_equal(pixel(100L, 221L), "#ffffffff")
  # y-axis: exactly column 20 (ruler_data_panel$x1).
  expect_equal(pixel(20L, 100L), "#000000ff")
  expect_equal(pixel(19L, 100L), "#ffffffff")
  expect_equal(pixel(21L, 100L), "#ffffffff")
})

test_that("retro_image_ruler() draws non-background pixels for tick labels", {
  input <- ruler_blank_input()
  output <- tempfile(fileext = ".png")
  on.exit(unlink(c(input, output)))
  retro_image_ruler(
    input = input,
    output = output,
    data_panel = ruler_data_panel,
    data_label = ruler_data_label,
    x_axis = ruler_x_axis,
    y_axis = ruler_y_axis
  )
  input_raster <- magick::image_raster(magick::image_read(input), tidy = TRUE)
  output_raster <- magick::image_raster(magick::image_read(output), tidy = TRUE)
  expect_true(any(output_raster$col != input_raster$col))
})

test_that("retro_ruler_tick_length() doubles the max of the y-axis's and x-axis's own pads", {
  expect_equal(
    retro_ruler_tick_length(tibble::tibble(pad_x1 = 5L, pad_y2 = -3L)),
    10L
  )
  expect_equal(
    retro_ruler_tick_length(tibble::tibble(pad_x1 = 2L, pad_y2 = -7L)),
    14L
  )
})

test_that("retro_ruler_axes() writes the x-axis and y-axis lines into the raster", {
  edges <- list(x1 = 5L, x2 = 35L, y1 = 3L, y2 = 17L)
  raster <- matrix("#ffffffff", nrow = 20L, ncol = 40L)
  raster <- retro_ruler_axes(raster, edges, color = "#000000")
  expect_true(all(raster[17L, 5L:35L] == "#000000ff"))
  expect_true(all(raster[3L:17L, 5L] == "#000000ff"))
  expect_equal(raster[10L, 10L], "#ffffffff")
})

test_that("retro_ruler_axes() draws in the color it is given", {
  edges <- list(x1 = 5L, x2 = 35L, y1 = 3L, y2 = 17L)
  raster <- matrix("#111111ff", nrow = 20L, ncol = 40L)
  raster <- retro_ruler_axes(raster, edges, color = "#ffffff")
  expect_true(all(raster[17L, 5L:35L] == "#ffffffff"))
  expect_true(all(raster[3L:17L, 5L] == "#ffffffff"))
})

test_that("retro_ruler_tick_lines() writes a vertical run for top ticks", {
  edges <- list(x1 = 5L, x2 = 35L, y1 = 3L, y2 = 25L)
  data_label <- tibble::tibble(
    label = c("A", "B"),
    x = c(10, 30),
    y = c(27, 27)
  )
  axis <- tibble::tibble(label = c("A", "B"), value = c(0, 10))
  raster <- matrix("#ffffffff", nrow = 30L, ncol = 40L)
  raster <- retro_ruler_tick_lines(
    raster = raster,
    edges = edges,
    data_label = data_label,
    axis = axis,
    tick_position = "top",
    tick_length = 4L,
    color = "#000000"
  )
  expect_true(all(raster[25L:29L, 10L] == "#000000ff"))
  expect_true(all(raster[25L:29L, 30L] == "#000000ff"))
  expect_equal(raster[25L, 20L], "#ffffffff")
})

test_that("retro_ruler_tick_lines() writes a horizontal run for right ticks, clipped to the raster", {
  edges <- list(x1 = 20L, x2 = 35L, y1 = 3L, y2 = 25L)
  data_label <- tibble::tibble(
    label = c("C", "D"),
    x = c(10, 10),
    y = c(8, 15)
  )
  axis <- tibble::tibble(label = c("C", "D"), value = c(0, 1))
  raster <- matrix("#ffffffff", nrow = 30L, ncol = 40L)
  # tick_length (30) overshoots axis_edge (20), so the run clips to column 1
  # rather than going negative.
  raster <- retro_ruler_tick_lines(
    raster = raster,
    edges = edges,
    data_label = data_label,
    axis = axis,
    tick_position = "right",
    tick_length = 30L,
    color = "#000000"
  )
  expect_true(all(raster[8L, 1L:20L] == "#000000ff"))
  expect_true(all(raster[15L, 1L:20L] == "#000000ff"))
})

test_that("retro_ruler_color() picks white for a dark background", {
  raster <- matrix("#111111ff", nrow = 4L, ncol = 4L)
  expect_equal(retro_ruler_color(raster), "#ffffff")
})

test_that("retro_ruler_color() picks black for a light background", {
  raster <- matrix("#eeeeeeff", nrow = 4L, ncol = 4L)
  expect_equal(retro_ruler_color(raster), "#000000")
})

test_that("retro_image_ruler() draws the ruler in white against a dark background", {
  input <- tempfile(fileext = ".png")
  on.exit(unlink(input))
  magick::image_blank(200, 250, color = "#000000ff") |>
    magick::image_write(input)
  output <- tempfile(fileext = ".png")
  on.exit(unlink(output), add = TRUE)
  retro_image_ruler(
    input = input,
    output = output,
    data_panel = ruler_data_panel,
    data_label = ruler_data_label,
    x_axis = ruler_x_axis,
    y_axis = ruler_y_axis
  )
  raster <- magick::image_raster(magick::image_read(output), tidy = TRUE)
  pixel <- function(x, y) raster$col[raster$x == x & raster$y == y]
  expect_equal(pixel(100L, 220L), "#ffffffff")
})

test_that("retro_ruler_cex() scales up for a larger bounding box", {
  raster <- matrix("#ffffffff", nrow = 50L, ncol = 100L)
  image <- magick::image_read(raster)
  image <- magick::image_draw(image)
  on.exit(dev.off())
  small <- retro_ruler_cex("0.2", box_width = 20, box_height = 10)
  large <- retro_ruler_cex("0.2", box_width = 40, box_height = 20)
  expect_true(large > small)
})

test_that("retro_ruler_cex() is independent for very different box sizes on x vs y", {
  # Reproduces the reported bug: x-axis and y-axis labels can be set in
  # very different, unpredictable font sizes. The chosen cex should track
  # each label's own box rather than a single fixed pixel-per-cex ratio.
  raster <- matrix("#ffffffff", nrow = 50L, ncol = 100L)
  image <- magick::image_read(raster)
  image <- magick::image_draw(image)
  on.exit(dev.off())
  x_cex <- retro_ruler_cex("35", box_width = 60, box_height = 60)
  y_cex <- retro_ruler_cex("0.8", box_width = 15, box_height = 15)
  expect_true(x_cex > y_cex)
})

test_that("retro_ruler_cex() shrinks to fit a narrow box for a long label", {
  raster <- matrix("#ffffffff", nrow = 50L, ncol = 100L)
  image <- magick::image_read(raster)
  image <- magick::image_draw(image)
  on.exit(dev.off())
  wide_label_cex <- retro_ruler_cex("100.25", box_width = 20, box_height = 20)
  narrow_label_cex <- retro_ruler_cex("1", box_width = 20, box_height = 20)
  expect_true(wide_label_cex < narrow_label_cex)
})

test_that("retro_image_ruler() errors when input does not exist", {
  output <- tempfile(fileext = ".png")
  expect_error(
    retro_image_ruler(
      input = "/nonexistent/path.png",
      output = output,
      data_panel = ruler_data_panel,
      data_label = ruler_data_label,
      x_axis = ruler_x_axis,
      y_axis = ruler_y_axis
    ),
    "input must exist"
  )
})

test_that("retro_image_ruler() errors when x_axis has fewer than 2 rows", {
  input <- ruler_blank_input()
  output <- tempfile(fileext = ".png")
  on.exit(unlink(input))
  expect_error(
    retro_image_ruler(
      input = input,
      output = output,
      data_panel = ruler_data_panel,
      data_label = ruler_data_label,
      x_axis = ruler_x_axis[1L, ],
      y_axis = ruler_y_axis
    ),
    "x_axis must be a data frame"
  )
})

test_that("retro_image_ruler() errors when y_axis has fewer than 2 rows", {
  input <- ruler_blank_input()
  output <- tempfile(fileext = ".png")
  on.exit(unlink(input))
  expect_error(
    retro_image_ruler(
      input = input,
      output = output,
      data_panel = ruler_data_panel,
      data_label = ruler_data_label,
      x_axis = ruler_x_axis,
      y_axis = ruler_y_axis[1L, ]
    ),
    "y_axis must be a data frame"
  )
})

test_that("retro_image_ruler() errors when data_panel is missing columns", {
  input <- ruler_blank_input()
  output <- tempfile(fileext = ".png")
  on.exit(unlink(input))
  bad_axes <- ruler_data_panel[, c("x1", "x2", "y1")]
  expect_error(
    retro_image_ruler(
      input = input,
      output = output,
      data_panel = bad_axes,
      data_label = ruler_data_label,
      x_axis = ruler_x_axis,
      y_axis = ruler_y_axis
    ),
    "data_panel must be a data frame"
  )
})

test_that("retro_image_ruler() errors when data_panel is missing pad columns", {
  input <- ruler_blank_input()
  output <- tempfile(fileext = ".png")
  on.exit(unlink(input))
  bad_panel <- ruler_data_panel[, c("x1", "x2", "y1", "y2")]
  expect_error(
    retro_image_ruler(
      input = input,
      output = output,
      data_panel = bad_panel,
      data_label = ruler_data_label,
      x_axis = ruler_x_axis,
      y_axis = ruler_y_axis
    ),
    "data_panel must be a data frame"
  )
})

test_that("retro_image_ruler() errors when data_label is missing columns", {
  input <- ruler_blank_input()
  output <- tempfile(fileext = ".png")
  on.exit(unlink(input))
  bad_label <- ruler_data_label[, setdiff(names(ruler_data_label), "x2")]
  expect_error(
    retro_image_ruler(
      input = input,
      output = output,
      data_panel = ruler_data_panel,
      data_label = bad_label,
      x_axis = ruler_x_axis,
      y_axis = ruler_y_axis
    ),
    "data_label must be a data frame"
  )
})
