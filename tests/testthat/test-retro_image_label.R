test_that("retro_image_label() validates inputs", {
  input <- retro_test_png(matrix("#ffffffff", nrow = 20L, ncol = 20L))
  output <- tempfile(fileext = ".png")
  findings <- tibble::tibble(label = "A", x1 = 1L, x2 = 5L, y1 = 1L, y2 = 5L)
  expect_error(
    retro_image_label("/nonexistent.png", output, findings),
    "input must exist"
  )
  expect_error(
    retro_image_label(123, output, findings),
    "Source image is missing"
  )
  expect_error(
    retro_image_label(input, 123, findings),
    "output must be a single string"
  )
  expect_error(
    retro_image_label(input, output, list(label = "A")),
    "findings must be a data frame"
  )
})

test_that("retro_image_label() returns NULL invisibly and writes output", {
  input <- retro_test_png(matrix("#ffffffff", nrow = 50L, ncol = 100L))
  output <- tempfile(fileext = ".png")
  on.exit(unlink(output))
  findings <- tibble::tibble(
    label = c("A", "B"),
    x1 = c(10L, 60L),
    x2 = c(20L, 70L),
    y1 = c(10L, 20L),
    y2 = c(20L, 30L)
  )
  result <- withVisible(retro_image_label(input, output, findings))
  expect_null(result$value)
  expect_false(result$visible)
  expect_true(file.exists(output))
})

test_that("retro_image_label() draws annotations for every finding", {
  raster <- matrix("#ffffff", nrow = 50L, ncol = 100L)
  input <- retro_test_png(raster)
  output <- tempfile(fileext = ".png")
  on.exit(unlink(output))
  findings <- tibble::tibble(
    label = c("A", "B"),
    x1 = c(10L, 60L),
    x2 = c(20L, 70L),
    y1 = c(10L, 20L),
    y2 = c(20L, 30L)
  )
  retro_image_label(input, output, findings)
  tidy <- magick::image_raster(magick::image_read(output), tidy = TRUE)
  expect_true(sum(tidy$col == "#ff0000ff") > 0L)
})

test_that("retro_image_label() draws nothing when findings is empty", {
  raster <- matrix("#ffffff", nrow = 50L, ncol = 100L)
  input <- retro_test_png(raster)
  output <- tempfile(fileext = ".png")
  on.exit(unlink(output))
  findings <- tibble::tibble(
    label = character(0L),
    x1 = integer(0L),
    x2 = integer(0L),
    y1 = integer(0L),
    y2 = integer(0L)
  )
  retro_image_label(input, output, findings)
  tidy <- magick::image_raster(magick::image_read(output), tidy = TRUE)
  expect_true(all(tidy$col == "#ffffffff"))
})

test_that("retro_label_draw_annotation() places badge to the right when box is at left edge", {
  # Create a small test image and open a drawing device
  raster <- matrix("#ffffffff", nrow = 50L, ncol = 100L)
  image <- magick::image_read(raster)
  image <- magick::image_draw(image)
  base_char_height <- abs(strheight("M", units = "user"))
  # x1 = 1 means the badge would go off the left edge

  expect_no_error(
    retro_label_draw_annotation(
      label = "A",
      x1 = 1L,
      x2 = 10L,
      y1 = 20L,
      y2 = 35L,
      base_char_height = base_char_height,
      median_height = 16
    )
  )
  dev.off()
})
