test_that("retro_image_grayscale() validates inputs", {
  input <- retro_test_png(matrix("#ffffffff", nrow = 20L, ncol = 20L))
  output <- tempfile(fileext = ".png")
  expect_error(
    retro_image_grayscale("/nonexistent.png", output),
    "input must exist"
  )
  expect_error(
    retro_image_grayscale(123, output),
    "input must be a single string"
  )
  expect_error(
    retro_image_grayscale(input, 123),
    "output must be a single string"
  )
})

test_that("retro_image_grayscale() preserves distinct anti-aliased gray shades", {
  raster <- matrix("#ffffff", nrow = 10L, ncol = 10L)
  raster[5L, 5L] <- "#808080"
  raster[3L, 3L] <- "#404040"
  input <- retro_test_png(raster)
  gray <- tempfile(fileext = ".png")
  on.exit(unlink(gray))
  retro_image_grayscale(input, gray)
  tidy <- magick::image_raster(magick::image_read(gray), tidy = TRUE)
  # Two distinct midtone shades should survive as distinct shades, not
  # collapse into a single foreground color the way strict quantization
  # would (see retro_ocr_quantize() in test-retro_data_ocr.R).
  expect_equal(length(unique(tidy$col)), 3L)
})

test_that("retro_image_grayscale() preserves source image dimensions", {
  input <- retro_test_png(matrix("#ffffffff", nrow = 15L, ncol = 25L))
  output <- tempfile(fileext = ".png")
  on.exit(unlink(output))
  retro_image_grayscale(input, output)
  expect_true(file.exists(output))
  source_info <- magick::image_info(magick::image_read(input))
  gray_info <- magick::image_info(magick::image_read(output))
  expect_equal(
    c(source_info$width, source_info$height),
    c(gray_info$width, gray_info$height)
  )
})
