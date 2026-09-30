test_that("retro_image_clean() removes short-span components", {
  # 100x100 image: a long horizontal line (cols 5-95) and a short legend
  # fragment (cols 80-90, row 10) that doesn't span enough
  raster <- matrix("#ffffff", nrow = 100, ncol = 100)
  raster[50, 5:95] <- "#ff0000"
  raster[10, 80:90] <- "#000000"
  input <- retro_test_png(raster)
  output <- tempfile(fileext = ".png")
  panel <- data.frame(x1 = 1L, x2 = 100L, y1 = 1L, y2 = 100L)
  on.exit(unlink(c(input, output)))
  retro_image_clean(input, output, panel, span_threshold = 0.5)
  expect_true(file.exists(output))
  tidy <- magick::image_raster(magick::image_read(output), tidy = TRUE)
  # The long line should be preserved (diagonal =~ 91, well above the
  # panel-diagonal threshold of 0.5 * 100 * sqrt(2) =~ 70.7)
  data_area <- tidy$y == 50 & tidy$x >= 5 & tidy$x <= 95
  expect_true(all(tidy$col[data_area] == "#ff0000ff"))
  # The short fragment should be removed (diagonal =~ 11, well below 70.7)
  legend_area <- tidy$y == 10 & tidy$x >= 80 & tidy$x <= 90
  expect_true(all(tidy$col[legend_area] == "#ffffffff"))
})

test_that("retro_image_clean() keeps multiple long-span components", {
  # Two separate curves that both span wide
  raster <- matrix("#ffffff", nrow = 50, ncol = 100)
  raster[20, 5:80] <- "#ff0000"
  raster[40, 10:90] <- "#0000ff"
  input <- retro_test_png(raster)
  output <- tempfile(fileext = ".png")
  panel <- data.frame(x1 = 1L, x2 = 100L, y1 = 1L, y2 = 50L)
  on.exit(unlink(c(input, output)))
  retro_image_clean(input, output, panel, span_threshold = 0.5)
  tidy <- magick::image_raster(magick::image_read(output), tidy = TRUE)
  # Both lines should be retained
  red_area <- tidy$y == 20 & tidy$x >= 5 & tidy$x <= 80
  expect_true(all(tidy$col[red_area] == "#ff0000ff"))
  blue_area <- tidy$y == 40 & tidy$x >= 10 & tidy$x <= 90
  expect_true(all(tidy$col[blue_area] == "#0000ffff"))
})

test_that("retro_image_clean() keeps a component a third of the panel diagonal", {
  # 35 of 100 columns: kept at the 0.3 default, dropped at 0.5. A curve
  # truncated at an occlusion, or one whose arm ran out of patients early,
  # looks like this, and cleaning it away cannot be undone downstream.
  raster <- matrix("#ffffff", nrow = 50, ncol = 100)
  raster[25, 5:39] <- "#ff0000"
  input <- retro_test_png(raster)
  output <- tempfile(fileext = ".png")
  panel <- data.frame(x1 = 1L, x2 = 100L, y1 = 1L, y2 = 50L)
  on.exit(unlink(c(input, output)))
  retro_image_clean(input, output, panel)
  tidy <- magick::image_raster(magick::image_read(output), tidy = TRUE)
  short_curve <- tidy$y == 25 & tidy$x >= 5 & tidy$x <= 39
  expect_true(all(tidy$col[short_curve] == "#ff0000ff"))
})

test_that("retro_image_clean() returns invisible NULL", {
  raster <- matrix("#ffffff", nrow = 50, ncol = 100)
  raster[25, 5:95] <- "#ff0000"
  input <- retro_test_png(raster)
  output <- tempfile(fileext = ".png")
  panel <- data.frame(x1 = 1L, x2 = 100L, y1 = 1L, y2 = 50L)
  on.exit(unlink(c(input, output)))
  result <- retro_image_clean(input, output, panel)
  expect_null(result)
})

test_that("retro_image_clean() does not modify input", {
  raster <- matrix("#ffffff", nrow = 50, ncol = 100)
  raster[25, 5:95] <- "#ff0000"
  raster[5, 90:95] <- "#000000"
  input <- retro_test_png(raster)
  output <- tempfile(fileext = ".png")
  panel <- data.frame(x1 = 1L, x2 = 100L, y1 = 1L, y2 = 50L)
  on.exit(unlink(c(input, output)))
  md5_before <- tools::md5sum(input)
  retro_image_clean(input, output, panel)
  expect_equal(tools::md5sum(input), md5_before)
})

test_that("retro_image_clean() validates input path", {
  output <- tempfile(fileext = ".png")
  panel <- data.frame(x1 = 1L, x2 = 10L, y1 = 1L, y2 = 10L)
  expect_error(
    retro_image_clean("/nonexistent.png", output, panel),
    "input must exist"
  )
  expect_error(
    retro_image_clean(123, output, panel),
    "input must be a single string"
  )
})

test_that("retro_image_clean() validates panel", {
  raster <- matrix("#ffffff", nrow = 10, ncol = 10)
  input <- retro_test_png(raster)
  output <- tempfile(fileext = ".png")
  on.exit(unlink(input))
  expect_error(
    retro_image_clean(
      input,
      output,
      panel = list(x1 = 1L, x2 = 10L, y1 = 1L, y2 = 10L)
    ),
    "panel must be a data frame with one row"
  )
  expect_error(
    retro_image_clean(input, output, panel = data.frame(x1 = 1L, x2 = 10L)),
    "panel must have numeric x1, x2, y1, y2 columns"
  )
})

test_that("retro_image_clean() validates span_threshold", {
  raster <- matrix("#ffffff", nrow = 10, ncol = 10)
  input <- retro_test_png(raster)
  output <- tempfile(fileext = ".png")
  panel <- data.frame(x1 = 1L, x2 = 10L, y1 = 1L, y2 = 10L)
  on.exit(unlink(input))
  expect_error(
    retro_image_clean(input, output, panel, span_threshold = 0),
    "span_threshold must be in"
  )
  expect_error(
    retro_image_clean(input, output, panel, span_threshold = 1.5),
    "span_threshold must be in"
  )
  expect_error(
    retro_image_clean(input, output, panel, span_threshold = "half"),
    "span_threshold must be a single number"
  )
})

test_that("retro_image_clean() preserves image dimensions", {
  raster <- matrix("#ffffff", nrow = 80, ncol = 120)
  raster[40, 10:110] <- "#ff0000"
  input <- retro_test_png(raster)
  output <- tempfile(fileext = ".png")
  panel <- data.frame(x1 = 1L, x2 = 120L, y1 = 1L, y2 = 80L)
  on.exit(unlink(c(input, output)))
  retro_image_clean(input, output, panel)
  info_in <- magick::image_info(magick::image_read(input))
  info_out <- magick::image_info(magick::image_read(output))
  expect_equal(info_out$width, info_in$width)
  expect_equal(info_out$height, info_in$height)
})

test_that("retro_image_clean() handles image with no foreground", {
  raster <- matrix("#ffffff", nrow = 20, ncol = 20)
  input <- retro_test_png(raster)
  output <- tempfile(fileext = ".png")
  panel <- data.frame(x1 = 1L, x2 = 20L, y1 = 1L, y2 = 20L)
  on.exit(unlink(c(input, output)))
  retro_image_clean(input, output, panel)
  expect_true(file.exists(output))
})

test_that("retro_image_clean() measures span against the panel, not the full canvas", {
  # A component that legitimately spans most of a small panel would have
  # been wrongly discarded if measured against the full (much larger)
  # canvas instead of the panel's own diagonal. Panel diagonal here is
  # sqrt(101^2 + 61^2) =~ 118.0, so the 0.3 default requires a diagonal of
  # only =~ 35.4; the component's diagonal (col span 81) is =~ 81.0, well
  # above that, but well below what 0.3 of the 300-wide canvas would have
  # required (=~ 94.9 under the old, image-width-based rule).
  raster <- matrix("#ffffff", nrow = 100, ncol = 300)
  raster[50, 110:190] <- "#ff0000"
  input <- retro_test_png(raster)
  output <- tempfile(fileext = ".png")
  panel <- data.frame(x1 = 100L, x2 = 200L, y1 = 20L, y2 = 80L)
  on.exit(unlink(c(input, output)))
  retro_image_clean(input, output, panel, span_threshold = 0.3)
  tidy <- magick::image_raster(magick::image_read(output), tidy = TRUE)
  curve_area <- tidy$y == 50 & tidy$x >= 110 & tidy$x <= 190
  expect_true(all(tidy$col[curve_area] == "#ff0000ff"))
})

test_that("retro_image_clean() retains a steep component via diagonal span, not column span alone", {
  # A near-vertical run has almost no column span (1 pixel), so a
  # width-only rule would discard it as a stray mark. Its row span (81)
  # makes its diagonal extent (=~ 81.0) well above the panel's 0.3 * 100 *
  # sqrt(2) =~ 42.4 threshold - resembling a real, steeply dropping curve.
  raster <- matrix("#ffffff", nrow = 100, ncol = 100)
  raster[10:90, 50] <- "#ff0000"
  input <- retro_test_png(raster)
  output <- tempfile(fileext = ".png")
  panel <- data.frame(x1 = 1L, x2 = 100L, y1 = 1L, y2 = 100L)
  on.exit(unlink(c(input, output)))
  retro_image_clean(input, output, panel, span_threshold = 0.3)
  tidy <- magick::image_raster(magick::image_read(output), tidy = TRUE)
  curve_area <- tidy$x == 50 & tidy$y >= 10 & tidy$y <= 90
  expect_true(all(tidy$col[curve_area] == "#ff0000ff"))
})

# --- retro_clean_filter_components -------------------------------------------

test_that("retro_clean_filter_components() keeps long, removes short", {
  foreground_mask <- matrix(FALSE, nrow = 5, ncol = 20)
  foreground_mask[3, 1:18] <- TRUE
  foreground_mask[1, 19:20] <- TRUE
  kept <- retro_clean_filter_components(foreground_mask, min_span = 10)
  # Long component retained
  expect_true(all(kept[3, 1:18]))
  # Short component removed
  expect_false(any(kept[1, 19:20]))
})

test_that("retro_clean_filter_components() returns unchanged mask when empty", {
  foreground_mask <- matrix(FALSE, nrow = 5, ncol = 10)
  result <- retro_clean_filter_components(foreground_mask, min_span = 5)
  expect_equal(result, foreground_mask)
})

# --- retro_clean_span_components ----------------------------------------------

test_that("retro_clean_span_components() keeps components whose diagonal meets the threshold", {
  # Component 1: row span 1, column span 3 (diagonal sqrt(1^2+3^2) =~ 3.16)
  # Component 2: row span 5, column span 2 (diagonal sqrt(5^2+2^2) =~ 5.39)
  membership <- c(1L, 1L, 1L, 2L, 2L)
  rows <- c(1L, 1L, 1L, 1L, 5L)
  cols <- c(1L, 2L, 3L, 7L, 8L)
  retained <- retro_clean_span_components(membership, rows, cols, min_span = 4)
  expect_equal(retained, 2L)
})

test_that("retro_clean_span_components() retains a component that fails a width-only check", {
  # This component has a column span of only 1 (it doesn't move
  # horizontally at all) but a row span of 20 - a width-only rule would
  # discard it, but its diagonal extent clears the threshold.
  membership <- rep(1L, 20)
  rows <- 1:20
  cols <- rep(5L, 20)
  retained <- retro_clean_span_components(membership, rows, cols, min_span = 15)
  expect_equal(retained, 1L)
})
