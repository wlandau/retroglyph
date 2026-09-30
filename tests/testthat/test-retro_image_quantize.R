test_that("retro_image_quantize() reduces colors", {
  raster <- matrix("#ffffffff", nrow = 30, ncol = 30)
  raster[10, ] <- "#ff0000ff"
  raster[20, ] <- "#00ff00ff"
  raster[5, ] <- "#0000ffff"
  raster[25, ] <- "#ffff00ff"
  raster[15, ] <- "#ff00ffff"
  input <- retro_test_png(raster)
  output <- tempfile(fileext = ".png")
  on.exit(unlink(output))
  count_colors <- function(file) {
    raster <- magick::image_read(file) |>
      magick::image_raster(tidy = FALSE)
    length(unique(as.vector(as.matrix(raster))))
  }
  result <- retro_image_quantize(input, output, n_colors = 3L)
  expect_null(result)
  expect_true(file.exists(output))
  expect_lte(count_colors(output), 3L)
})

test_that("retro_image_quantize() keeps colors when already below n_colors", {
  raster <- matrix("#ffffffff", nrow = 20, ncol = 20)
  raster[10, ] <- "#ff0000ff"
  input <- retro_test_png(raster)
  output <- tempfile(fileext = ".png")
  on.exit(unlink(output))
  retro_image_quantize(input, output, n_colors = 64L)
  result_raster <- magick::image_read(output) |>
    magick::image_raster(tidy = FALSE)
  colors <- unique(as.vector(as.matrix(result_raster)))
  expect_lte(length(colors), 64L)
})

test_that("retro_image_quantize() validates arguments", {
  raster <- matrix("#ffffffff", nrow = 10, ncol = 10)
  input <- retro_test_png(raster)
  output <- tempfile(fileext = ".png")
  expect_error(
    retro_image_quantize(123, output),
    "Source image is missing"
  )
  expect_error(
    retro_image_quantize("nonexistent.png", output),
    "input must exist"
  )

  expect_error(
    retro_image_quantize(input, 123),
    "output must be a single string"
  )
  expect_error(
    retro_image_quantize(input, output, n_colors = 0),
    "n_colors must be a single positive integer"
  )
  expect_error(
    retro_image_quantize(input, output, n_colors = -1),
    "n_colors must be a single positive integer"
  )
})
