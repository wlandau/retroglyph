test_that("retro_image_classify() reduces colors", {
  raster <- matrix("#ffffffff", nrow = 60, ncol = 60)
  raster[20, 5:55] <- "#ff2222ff"
  raster[40, 5:55] <- "#2222ffff"
  raster[10:15, 10:15] <- "#33cc33ff"
  input <- retro_test_png(raster)
  output <- tempfile(fileext = ".png")
  on.exit(unlink(output))
  count_colors <- function(file) {
    raster <- magick::image_read(file) |>
      magick::image_raster(tidy = TRUE)
    length(unique(raster$col))
  }
  expect_gt(count_colors(input), 2L)
  result <- retro_image_classify(
    input,
    output,
    background = "#FFFFFF",
    series = c("#FF0000")
  )
  expect_null(result)
  expect_true(file.exists(output))
  expect_lte(count_colors(output), 2L)
})

test_that("retro_image_classify() writes correct colors to output", {
  raster <- matrix("#ffffffff", nrow = 60, ncol = 60)
  raster[20, 5:55] <- "#ff2222ff"
  raster[40, 5:55] <- "#2222ffff"
  raster[10:15, 10:15] <- "#33cc33ff"
  input <- retro_test_png(raster)
  output <- tempfile(fileext = ".png")
  on.exit(unlink(output))
  retro_image_classify(
    input,
    output,
    background = "#FFFFFF",
    series = c("#FF0000", "#0000FF", "#00FF00")
  )
  raster <- magick::image_read(output) |>
    magick::image_raster(tidy = TRUE)
  output_colors <- unique(raster$col)
  # Should have at most 4 colors (background + 3 series)
  expect_lte(length(output_colors), 4L)
})

test_that("retro_image_classify() does not modify input", {
  raster <- matrix("#ffffffff", nrow = 30, ncol = 30)
  raster[15, 5:25] <- "#ff0000ff"
  input <- retro_test_png(raster)
  output <- tempfile(fileext = ".png")
  on.exit(unlink(c(input, output)))
  md5_before <- tools::md5sum(input)
  retro_image_classify(
    input,
    output,
    background = "#FFFFFF",
    series = c("#FF0000")
  )
  expect_equal(tools::md5sum(input), md5_before)
})

test_that("retro_image_classify() validates input", {
  output <- tempfile(fileext = ".png")
  expect_error(
    retro_image_classify(123, output, background = "#FFF", series = "#000"),
    "single string"
  )
  expect_error(
    retro_image_classify(
      "nonexistent.png",
      output,
      background = "#FFF",
      series = "#000"
    ),
    "must exist"
  )
})

test_that("retro_image_classify() validates output", {
  input <- retro_test_png(matrix("#ffffffff", nrow = 10L, ncol = 10L))
  expect_error(
    retro_image_classify(input, 123, background = "#FFF", series = "#000"),
    "single string"
  )
})

test_that("retro_image_classify() validates background", {
  input <- retro_test_png(matrix("#ffffffff", nrow = 10L, ncol = 10L))
  output <- tempfile(fileext = ".png")
  expect_error(
    retro_image_classify(input, output, background = 123, series = "#000"),
    "single string"
  )
  expect_error(
    retro_image_classify(
      input,
      output,
      background = c("#FFF", "#000"),
      series = "#111"
    ),
    "single string"
  )
})

test_that("retro_image_classify() validates series", {
  input <- retro_test_png(matrix("#ffffffff", nrow = 10L, ncol = 10L))
  output <- tempfile(fileext = ".png")
  expect_error(
    retro_image_classify(input, output, background = "#FFF", series = 123),
    "character vector"
  )
  expect_error(
    retro_image_classify(
      input,
      output,
      background = "#FFF",
      series = character(0L)
    ),
    "character vector"
  )
})

test_that("retro_image_classify() requires unique colors", {
  input <- retro_test_png(matrix("#ffffffff", nrow = 10L, ncol = 10L))
  output <- tempfile(fileext = ".png")
  expect_error(
    retro_image_classify(
      input,
      output,
      background = "#FFFFFF",
      series = c("#FFFFFF", "#000000")
    ),
    "unique"
  )
  expect_error(
    retro_image_classify(
      input,
      output,
      background = "#FFFFFF",
      series = c("#FF0000", "#FF0000")
    ),
    "unique"
  )
})

test_that("retro_image_classify() replaces garbage colors with background", {
  output <- tempfile(fileext = ".png")
  on.exit(unlink(output))
  # White background, red curve, light blue confidence band.
  raster <- matrix("#ffffffff", nrow = 100, ncol = 100)
  raster[50, 10:90] <- "#ff0000ff"
  raster[45:55, 10:90] <- "#aaddffff"
  raster[50, 10:90] <- "#ff0000ff"
  input <- retro_test_png(raster)
  on.exit(unlink(input), add = TRUE)
  retro_image_classify(
    input,
    output,
    background = "#FFFFFF",
    series = "#FF0000",
    garbage = "#AADDFF"
  )
  tidy <- magick::image_raster(magick::image_read(output), tidy = TRUE)
  # No garbage color should remain in the output.
  expect_false(any(grepl("^#aaddff", tidy$col)))
  # The curve color should still be present.
  expect_true(any(grepl("^#ff0000", tidy$col)))
})

test_that("retro_image_classify() works with empty garbage vector", {
  output <- tempfile(fileext = ".png")
  on.exit(unlink(output))
  raster <- matrix("#ffffffff", nrow = 50, ncol = 50)
  raster[25, 10:40] <- "#ff0000ff"
  input <- retro_test_png(raster)
  on.exit(unlink(input), add = TRUE)
  retro_image_classify(
    input,
    output,
    background = "#FFFFFF",
    series = "#FF0000",
    garbage = character(0L)
  )
  expect_true(file.exists(output))
})
