test_that("retro_image_mask() does not modify input", {
  input <- tempfile(fileext = ".png")
  output <- tempfile(fileext = ".png")
  on.exit(unlink(c(input, output)))
  magick::image_blank(120, 80, color = "blue") |>
    magick::image_write(input)
  md5_before <- tools::md5sum(input)
  retro_image_mask(
    input = input,
    output = output,
    x1 = 1L,
    x2 = 60L,
    y1 = 41L,
    y2 = 80L,
    mask = FALSE,
    initial = TRUE,
    background = "#ffffffff"
  )
  expect_equal(tools::md5sum(input), md5_before)
})

# --- initial = TRUE: start with everything, remove a region ----------------

test_that("retro_image_mask() initial = TRUE, remove left", {
  input <- tempfile(fileext = ".png")
  output <- tempfile(fileext = ".png")
  on.exit(unlink(c(input, output)))
  magick::image_blank(120, 80, color = "#ff0000ff") |>
    magick::image_write(input)
  retro_image_mask(
    input = input,
    output = output,
    x1 = 1L,
    x2 = 60L,
    y1 = 1L,
    y2 = 80L,
    mask = FALSE,
    initial = TRUE,
    background = "#ffffffff"
  )
  raster <- magick::image_raster(magick::image_read(output))
  expect_true(all(raster$col[raster$x <= 60] == "#ffffffff"))
  expect_true(all(raster$col[raster$x > 60] == "#ff0000ff"))
})

test_that("retro_image_mask() initial = TRUE, remove right", {
  input <- tempfile(fileext = ".png")
  output <- tempfile(fileext = ".png")
  on.exit(unlink(c(input, output)))
  magick::image_blank(120, 80, color = "#ff0000ff") |>
    magick::image_write(input)
  retro_image_mask(
    input = input,
    output = output,
    x1 = 61L,
    x2 = 120L,
    y1 = 1L,
    y2 = 80L,
    mask = FALSE,
    initial = TRUE,
    background = "#ffffffff"
  )
  raster <- magick::image_raster(magick::image_read(output))
  expect_true(all(raster$col[raster$x < 61] == "#ff0000ff"))
  expect_true(all(raster$col[raster$x >= 61] == "#ffffffff"))
})

test_that("retro_image_mask() initial = TRUE, remove top", {
  input <- tempfile(fileext = ".png")
  output <- tempfile(fileext = ".png")
  on.exit(unlink(c(input, output)))
  magick::image_blank(120, 80, color = "#ff0000ff") |>
    magick::image_write(input)
  retro_image_mask(
    input = input,
    output = output,
    x1 = 1L,
    x2 = 120L,
    y1 = 1L,
    y2 = 40L,
    mask = FALSE,
    initial = TRUE,
    background = "#ffffffff"
  )
  raster <- magick::image_raster(magick::image_read(output))
  # Top half (rows 1-40) removed
  expect_true(all(raster$col[raster$y <= 40] == "#ffffffff"))
  expect_true(all(raster$col[raster$y > 40] == "#ff0000ff"))
})

test_that("retro_image_mask() initial = TRUE, remove bottom", {
  input <- tempfile(fileext = ".png")
  output <- tempfile(fileext = ".png")
  on.exit(unlink(c(input, output)))
  magick::image_blank(120, 80, color = "#ff0000ff") |>
    magick::image_write(input)
  retro_image_mask(
    input = input,
    output = output,
    x1 = 1L,
    x2 = 120L,
    y1 = 41L,
    y2 = 80L,
    mask = FALSE,
    initial = TRUE,
    background = "#ffffffff"
  )
  raster <- magick::image_raster(magick::image_read(output))
  # Bottom half (rows 41-80) removed
  expect_true(all(raster$col[raster$y < 41] == "#ff0000ff"))
  expect_true(all(raster$col[raster$y >= 41] == "#ffffffff"))
})

# --- initial = TRUE: remove a region in the middle -------------------------

test_that("retro_image_mask() initial = TRUE, remove middle region", {
  input <- tempfile(fileext = ".png")
  output <- tempfile(fileext = ".png")
  on.exit(unlink(c(input, output)))
  magick::image_blank(120, 80, color = "#ff0000ff") |>
    magick::image_write(input)
  # Pixel region: columns 25-84, rows 33-72
  retro_image_mask(
    input = input,
    output = output,
    x1 = 25L,
    x2 = 84L,
    y1 = 33L,
    y2 = 72L,
    mask = FALSE,
    initial = TRUE,
    background = "#ffffffff"
  )
  raster <- magick::image_raster(magick::image_read(output))
  inside <- raster$x >= 25L &
    raster$x <= 84L &
    raster$y >= 33L &
    raster$y <= 72L
  outside <- !inside
  expect_true(all(raster$col[inside] == "#ffffffff"))
  expect_true(all(raster$col[outside] == "#ff0000ff"))
})

# --- initial = FALSE: add a region in the middle ---------------------------

test_that("retro_image_mask() initial = FALSE, add middle region", {
  input <- tempfile(fileext = ".png")
  output <- tempfile(fileext = ".png")
  on.exit(unlink(c(input, output)))
  magick::image_blank(120, 80, color = "#ff0000ff") |>
    magick::image_write(input)
  retro_image_mask(
    input = input,
    output = output,
    x1 = 25L,
    x2 = 84L,
    y1 = 33L,
    y2 = 72L,
    mask = TRUE,
    initial = FALSE,
    background = "#ffffffff"
  )
  raster <- magick::image_raster(magick::image_read(output))
  inside <- raster$x >= 25L &
    raster$x <= 84L &
    raster$y >= 33L &
    raster$y <= 72L
  outside <- !inside
  expect_true(all(raster$col[inside] == "#ff0000ff"))
  expect_true(all(raster$col[outside] == "#ffffffff"))
})

# --- initial = FALSE: start with nothing, add back a region ----------------

test_that("retro_image_mask() initial = FALSE, add left", {
  input <- tempfile(fileext = ".png")
  output <- tempfile(fileext = ".png")
  on.exit(unlink(c(input, output)))
  magick::image_blank(120, 80, color = "#ff0000ff") |>
    magick::image_write(input)
  retro_image_mask(
    input = input,
    output = output,
    x1 = 1L,
    x2 = 60L,
    y1 = 1L,
    y2 = 80L,
    mask = TRUE,
    initial = FALSE,
    background = "#ffffffff"
  )
  raster <- magick::image_raster(magick::image_read(output))
  expect_true(all(raster$col[raster$x <= 60] == "#ff0000ff"))
  expect_true(all(raster$col[raster$x > 60] == "#ffffffff"))
})

test_that("retro_image_mask() initial = FALSE, add right", {
  input <- tempfile(fileext = ".png")
  output <- tempfile(fileext = ".png")
  on.exit(unlink(c(input, output)))
  magick::image_blank(120, 80, color = "#ff0000ff") |>
    magick::image_write(input)
  retro_image_mask(
    input = input,
    output = output,
    x1 = 61L,
    x2 = 120L,
    y1 = 1L,
    y2 = 80L,
    mask = TRUE,
    initial = FALSE,
    background = "#ffffffff"
  )
  raster <- magick::image_raster(magick::image_read(output))
  expect_true(all(raster$col[raster$x < 61] == "#ffffffff"))
  expect_true(all(raster$col[raster$x >= 61] == "#ff0000ff"))
})

test_that("retro_image_mask() initial = FALSE, add top", {
  input <- tempfile(fileext = ".png")
  output <- tempfile(fileext = ".png")
  on.exit(unlink(c(input, output)))
  magick::image_blank(120, 80, color = "#ff0000ff") |>
    magick::image_write(input)
  retro_image_mask(
    input = input,
    output = output,
    x1 = 1L,
    x2 = 120L,
    y1 = 1L,
    y2 = 40L,
    mask = TRUE,
    initial = FALSE,
    background = "#ffffffff"
  )
  raster <- magick::image_raster(magick::image_read(output))
  # Top half (rows 1-40) kept
  expect_true(all(raster$col[raster$y <= 40] == "#ff0000ff"))
  expect_true(all(raster$col[raster$y > 40] == "#ffffffff"))
})

test_that("retro_image_mask() initial = FALSE, add bottom", {
  input <- tempfile(fileext = ".png")
  output <- tempfile(fileext = ".png")
  on.exit(unlink(c(input, output)))
  magick::image_blank(120, 80, color = "#ff0000ff") |>
    magick::image_write(input)
  retro_image_mask(
    input = input,
    output = output,
    x1 = 1L,
    x2 = 120L,
    y1 = 41L,
    y2 = 80L,
    mask = TRUE,
    initial = FALSE,
    background = "#ffffffff"
  )
  raster <- magick::image_raster(magick::image_read(output))
  # Bottom half (rows 41-80) kept
  expect_true(all(raster$col[raster$y < 41] == "#ffffffff"))
  expect_true(all(raster$col[raster$y >= 41] == "#ff0000ff"))
})

# --- Multiple masks applied sequentially ------------------------------------

test_that("retro_image_mask() applies multiple overlapping interior masks", {
  input <- tempfile(fileext = ".png")
  output <- tempfile(fileext = ".png")
  on.exit(unlink(c(input, output)))
  magick::image_blank(120, 80, color = "#ff0000ff") |>
    magick::image_write(input)
  # Three overlapping interior operations applied sequentially:
  # 1. Remove a large region: mask = FALSE
  # 2. Add back a chunk in the middle (overlaps region 1): mask = TRUE
  # 3. Remove a piece (overlaps region 2): mask = FALSE
  x1 <- c(7L, 28L, 53L)
  x2 <- c(70L, 92L, 112L)
  y1 <- c(37L, 16L, 4L)
  y2 <- c(75L, 65L, 47L)

  retro_image_mask(
    input = input,
    output = output,
    x1 = x1,
    x2 = x2,
    y1 = y1,
    y2 = y2,
    mask = c(FALSE, TRUE, FALSE),
    initial = TRUE,
    background = "#ffffffff"
  )
  raster <- magick::image_raster(magick::image_read(output))
  op1 <- raster$x >= x1[1] &
    raster$x <= x2[1] &
    raster$y >= y1[1] &
    raster$y <= y2[1]
  op2 <- raster$x >= x1[2] &
    raster$x <= x2[2] &
    raster$y >= y1[2] &
    raster$y <= y2[2]
  op3 <- raster$x >= x1[3] &
    raster$x <= x2[3] &
    raster$y >= y1[3] &
    raster$y <= y2[3]
  # Simulate sequential mask
  keep <- rep(TRUE, nrow(raster))
  keep[op1] <- FALSE
  keep[op2] <- TRUE
  keep[op3] <- FALSE
  expect_true(all(raster$col[keep] == "#ff0000ff"))
  expect_true(all(raster$col[!keep] == "#ffffffff"))
  # Sanity: confirm all three regions overlap
  expect_true(any(op1 & op2))
  expect_true(any(op2 & op3))
})

# --- Validation tests -------------------------------------------------------

test_that("retro_image_mask() validates input path", {
  output <- tempfile(fileext = ".png")
  expect_error(
    retro_image_mask(123, output, 1L, 10L, 1L, 10L, TRUE),
    "single string"
  )
  expect_error(
    retro_image_mask("nonexistent.png", output, 1L, 10L, 1L, 10L, TRUE),
    "must exist"
  )
})

test_that("retro_image_mask() validates output path", {
  input <- system.file("simulation.png", package = "retroglyph")
  expect_error(
    retro_image_mask(input, 123, 1L, 10L, 1L, 10L, TRUE),
    "single string"
  )
})

test_that("retro_image_mask() validates initial", {
  input <- system.file("simulation.png", package = "retroglyph")
  output <- tempfile(fileext = ".png")
  expect_error(
    retro_image_mask(input, output, 1L, 10L, 1L, 10L, TRUE, initial = "yes"),
    "TRUE or FALSE"
  )
})

test_that("retro_image_mask() validates vector types", {
  input <- system.file("simulation.png", package = "retroglyph")
  output <- tempfile(fileext = ".png")
  expect_error(
    retro_image_mask(input, output, 0.5, 10L, 1L, 10L, TRUE),
    "integerish"
  )
  expect_error(
    retro_image_mask(input, output, 1L, 10L, 1L, 10L, "yes"),
    "mask must be logical"
  )
})

test_that("retro_image_mask() validates vector lengths", {
  input <- system.file("simulation.png", package = "retroglyph")
  output <- tempfile(fileext = ".png")
  expect_error(
    retro_image_mask(input, output, c(1L, 5L), 10L, 1L, 10L, TRUE),
    "same length"
  )
})

test_that("retro_image_mask() validates no missing values", {
  input <- system.file("simulation.png", package = "retroglyph")
  output <- tempfile(fileext = ".png")
  expect_error(
    retro_image_mask(input, output, NA_integer_, 10L, 1L, 10L, TRUE),
    "must not contain missing"
  )
  expect_error(
    retro_image_mask(input, output, 1L, 10L, 1L, 10L, NA),
    "must not contain missing"
  )
})

test_that("retro_image_mask() validates coordinate bounds", {
  input <- tempfile(fileext = ".png")
  output <- tempfile(fileext = ".png")
  on.exit(unlink(c(input, output)))
  magick::image_blank(120, 80, color = "blue") |>
    magick::image_write(input)
  expect_error(
    retro_image_mask(input, output, 0L, 10L, 1L, 10L, TRUE),
    ">= 1"
  )
  expect_error(
    retro_image_mask(input, output, 1L, 121L, 1L, 10L, TRUE),
    "image width"
  )
  expect_error(
    retro_image_mask(input, output, 1L, 10L, 0L, 10L, TRUE),
    ">= 1"
  )
  expect_error(
    retro_image_mask(input, output, 1L, 10L, 1L, 81L, TRUE),
    "image height"
  )
})

test_that("retro_image_mask() validates x1 <= x2 and y1 <= y2", {
  input <- tempfile(fileext = ".png")
  output <- tempfile(fileext = ".png")
  on.exit(unlink(c(input, output)))
  magick::image_blank(120, 80, color = "blue") |>
    magick::image_write(input)
  expect_error(
    retro_image_mask(input, output, 80L, 20L, 1L, 10L, TRUE),
    "x1 must be <= x2"
  )
  expect_error(
    retro_image_mask(input, output, 1L, 10L, 60L, 20L, TRUE),
    "y1 must be <= y2"
  )
})

# --- retro_background_color() -------------------------------------------------

test_that("retro_background_color() needs quantize to avoid a fragmented background", {
  # Background split across 3 near-white shades (24, 24, 22 px) so no single
  # raw shade beats the 30 px solid dark foreground - the raw mode
  # misidentifies the foreground as background. Quantizing to 2 colors first
  # collapses the 3 near-white shades into one cluster (70 px), which
  # correctly outweighs the dark cluster (30 px).
  values <- c(
    rep("#fefefeff", 24L),
    rep("#fdfdfdff", 24L),
    rep("#fcfcfcff", 22L),
    rep("#101010ff", 30L)
  )
  input <- retro_test_png(matrix(values, nrow = 10L, ncol = 10L))
  raster <- magick::image_raster(magick::image_read(input), tidy = FALSE)
  expect_equal(retro_background_color(raster, quantize = FALSE), "#101010")
  quantized <- retro_background_color(raster, quantize = TRUE)
  expect_false(identical(quantized, "#101010"))
  expect_gt(mean(grDevices::col2rgb(quantized)), 200)
})
