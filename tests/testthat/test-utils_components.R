test_that("retro_components_foreground_mask() returns a logical matrix", {
  matrix <- matrix("#ffffffff", nrow = 50, ncol = 50)
  matrix[10:40, 15] <- "#000000ff"
  image <- retro_test_image(matrix)
  result <- retro_components_foreground_mask(image)
  expect_true(is.logical(result))
  expect_equal(dim(result), c(50L, 50L))
  expect_true(all(result[10:40, 15]))
  expect_false(any(result[1:9, 15]))
})

test_that("retro_components_foreground_mask() works with dark background", {
  matrix <- matrix("#000000ff", nrow = 50, ncol = 50)
  matrix[10:40, 15] <- "#ffffffff"
  image <- retro_test_image(matrix)
  result <- retro_components_foreground_mask(image)
  expect_true(is.logical(result))
  expect_true(all(result[10:40, 15]))
  expect_false(result[1, 1])
})

test_that("retro_components_foreground_mask() survives an alpha channel", {
  # The regression this guards: image_background(flatten = TRUE) makes
  # every pixel opaque but leaves the alpha channel in place, and some
  # ImageMagick builds then return a single "transparent" color from
  # image_quantize() rather than the 2 colors requested. Every pixel then
  # equals the detected background and the mask is all FALSE. Reported as
  # n_colors=1 colors=transparent on r-universe Linux while Windows and
  # macOS returned the correct 2 colors from the same source. This fixture
  # keeps the alpha channel deliberately, so it must not go through
  # retro_test_image(), which now drops it.
  matrix <- matrix("#ffffffff", nrow = 50, ncol = 50)
  matrix[10:40, 15] <- "#000000ff"
  image <- magick::image_read(matrix)
  expect_true(magick::image_info(image)$matte)
  result <- retro_components_foreground_mask(image)
  expect_equal(sum(result), 31L)
  expect_true(all(result[10:40, 15]))
})

test_that("retro_components_quantize() drops the alpha channel", {
  matrix <- matrix("#ffffffff", nrow = 10, ncol = 10)
  matrix[3:8, 5] <- "#000000ff"
  image <- magick::image_read(matrix)
  expect_true(magick::image_info(image)$matte)
  quantized <- retro_components_quantize(image)
  expect_false(magick::image_info(quantized)$matte)
  colors <- unique(as.vector(magick::image_raster(quantized, tidy = FALSE)))
  expect_lte(length(colors), 2L)
  expect_false(any(colors == "transparent"))
})

test_that("retro_components_quantize() leaves opaque colors unchanged", {
  # Dropping the alpha channel is only safe if it is color-neutral on the
  # already-flattened images the pipeline actually produces.
  matrix <- matrix(
    c("#ff0000ff", "#ffffffff", "#000000ff", "#0000ffff"),
    nrow = 2
  )
  image <- retro_test_image(matrix)
  before <- as.vector(magick::image_raster(image, tidy = FALSE))
  after <- as.vector(
    magick::image_raster(retro_color_opaque(image), tidy = FALSE)
  )
  expect_equal(before, after)
})
