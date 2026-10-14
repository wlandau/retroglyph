# --- retro_data_censor_column ----------------------------------------------

test_that("retro_data_censor_column() measures ink height beyond the path", {
  # Ink spans rows 5-25 (21 rows), the path spans rows 14-16 (3 rows): 18
  # pixels of ink height the path does not account for.
  expect_equal(retro_data_censor_column(ink_y = 5L:25L, path_y = 14L:16L), 18)
})

test_that("retro_data_censor_column() counts both overhangs, not just one", {
  # A tick clipped at the panel edge keeps only the overhang it has: 10
  # pixels above and 1 below, against 18 for the full tick above. Lower than
  # an unclipped tick, but still well above the line width an ordinary
  # column yields.
  expect_equal(retro_data_censor_column(ink_y = 5L:17L, path_y = 15L:16L), 11)
})

test_that("retro_data_censor_column() nets out a vertical step drop", {
  # Where the skeleton traverses a step drop it spans nearly as many rows as
  # the ink does, so the two spans cancel. Compare against a tick with the
  # same ink span but a thin path through it.
  expect_equal(retro_data_censor_column(ink_y = 14L:40L, path_y = 16L:38L), 4)
  expect_equal(retro_data_censor_column(ink_y = 14L:40L, path_y = 26L:28L), 24)
})

test_that("retro_data_censor_column() clamps when the path out-spans the ink", {
  # A reconstructed path bridging an occlusion gap can span many rows above
  # just a stray pixel of this series' own color. Without the clamp the
  # difference would be large and negative, and an unsigned distance would
  # report it as a tall tick.
  expect_equal(retro_data_censor_column(ink_y = 15L, path_y = 15L:39L), 0)
})

test_that("retro_data_censor_column() is 0 when the path covers the ink", {
  expect_equal(retro_data_censor_column(ink_y = 14L:16L, path_y = 14L:16L), 0)
})

test_that("retro_data_censor_column() is 0 for a column with no ink", {
  # Occluded by another series, or a reconstructed path pixel in a gap:
  # nothing visible to evidence a tick.
  expect_equal(retro_data_censor_column(ink_y = NULL, path_y = 14L:16L), 0)
  expect_equal(
    retro_data_censor_column(ink_y = integer(0L), path_y = 14L:16L),
    0
  )
})

# --- retro_censor_centers --------------------------------------------------

test_that("retro_censor_centers() is a stub that always reports 2", {
  # Replacing this stub with real one-vs-two group detection is the
  # outstanding work; see the function's own documentation.
  expect_equal(retro_censor_centers(c(1, 1, 2, 1, 9, 10)), 2L)
  expect_equal(retro_censor_centers(rep(1, 10)), 2L)
})

# --- retro_data_censor_color -----------------------------------------------

test_that("retro_data_censor_color() flags the tallest column", {
  # A path along row 15 from columns 1-20, with ink extending far above and
  # below the path at column 10 only.
  series <- data.frame(x = 1L:20L, y = rep(15L, 20L))
  ink <- rbind(
    data.frame(x = rep(1L:20L, each = 3L), y = rep(14L:16L, times = 20L)),
    data.frame(x = rep(10L, 21L), y = 5L:25L)
  )
  result <- retro_data_censor_color(ink, series)
  expect_equal(result$x[result$censored], 10L)
})

test_that("retro_data_censor_color() flags every path pixel in a tick column", {
  # The excess height is a property of the column, so a column with several
  # path pixels (a vertical drop) has all of them labeled together.
  series <- data.frame(x = c(1L, 2L, 2L, 2L, 3L), y = c(5L, 5L, 6L, 7L, 7L))
  ink <- rbind(
    data.frame(x = c(1L, 3L), y = c(5L, 7L)),
    data.frame(x = rep(2L, 21L), y = 1L:21L)
  )
  result <- retro_data_censor_color(ink, series)
  expect_equal(result$censored, c(FALSE, TRUE, TRUE, TRUE, FALSE))
})

test_that("retro_data_censor_color() labels nothing when columns are uniform", {
  # Every column's ink lines up with the path exactly, so there is no split
  # to make and the length(unique(excess_heights)) < 2 guard fires.
  series <- data.frame(x = 1L:10L, y = rep(5L, 10L))
  ink <- data.frame(x = 1L:10L, y = rep(5L, 10L))
  result <- retro_data_censor_color(ink, series)
  expect_false(any(result$censored))
})

test_that("retro_data_censor_color() keeps the columns it was given", {
  series <- data.frame(
    x = 1L:5L,
    y = rep(5L, 5L),
    color = "#dc3030",
    line_width = 1L
  )
  ink <- data.frame(x = 1L:5L, y = rep(5L, 5L))
  result <- retro_data_censor_color(ink, series)
  expect_equal(
    names(result),
    c("x", "y", "color", "line_width", "censored")
  )
  expect_type(result$censored, "logical")
})

# --- retro_data_censor -----------------------------------------------------

test_that("retro_data_censor() detects a tick mark on a flat curve", {
  # A flat line four pixels thick with a tall tick at column 10.
  pixel_matrix <- matrix("#ffffff", nrow = 30L, ncol = 20L)
  pixel_matrix[14L:17L, 2L:19L] <- "#dc3030"
  pixel_matrix[6L:25L, 10L] <- "#dc3030"
  input <- retro_test_png(pixel_matrix)
  path <- retro_data_path(input)
  censoring <- retro_data_censor(input, path)
  expect_true(any(censoring$censored))
  expect_equal(sort(unique(censoring$x[censoring$censored])), 10L)
})

test_that("retro_data_censor() returns path data plus one column", {
  pixel_matrix <- matrix("#ffffff", nrow = 30L, ncol = 20L)
  pixel_matrix[14L:17L, 2L:19L] <- "#dc3030"
  pixel_matrix[6L:25L, 10L] <- "#dc3030"
  input <- retro_test_png(pixel_matrix)
  path <- retro_data_path(input)
  censoring <- retro_data_censor(input, path)
  # Same rows, same order, same columns, plus `censored`.
  expect_equal(names(censoring), c(names(path), "censored"))
  expect_equal(nrow(censoring), nrow(path))
  expect_equal(censoring[, names(path)], path)
  expect_type(censoring$censored, "logical")
  expect_false(anyNA(censoring$censored))
})

test_that("retro_data_censor() handles a curve with no tick marks", {
  # A plain thick line. With retro_censor_centers() stubbed to always
  # report 2, the k-means still splits whatever variation the line's own
  # anti-aliasing and skeleton leave behind, so only the structural
  # contract is asserted here. The value assertion - that nothing at all is
  # flagged - arrives with the real retro_censor_centers().
  pixel_matrix <- matrix("#ffffff", nrow = 30L, ncol = 40L)
  pixel_matrix[14L:17L, 2L:39L] <- "#dc3030"
  input <- retro_test_png(pixel_matrix)
  path <- retro_data_path(input)
  censoring <- retro_data_censor(input, path)
  expect_equal(nrow(censoring), nrow(path))
  expect_type(censoring$censored, "logical")
  expect_false(anyNA(censoring$censored))
})

test_that("retro_data_censor() handles each series separately", {
  # Two curves that never touch, in separate connected components. Only the
  # red one carries a tick mark.
  pixel_matrix <- matrix("#ffffff", nrow = 60L, ncol = 40L)
  pixel_matrix[14L:17L, 2L:39L] <- "#dc3030"
  pixel_matrix[6L:25L, 20L] <- "#dc3030"
  pixel_matrix[44L:47L, 2L:39L] <- "#3030a0"
  input <- retro_test_png(pixel_matrix)
  path <- retro_data_path(input)
  censoring <- retro_data_censor(input, path)
  expect_setequal(unique(censoring$color), c("#dc3030", "#3030a0"))
  red <- censoring[censoring$color == "#dc3030", ]
  expect_equal(sort(unique(red$x[red$censored])), 20L)
  # Every pixel of the path is accounted for, in both series.
  expect_equal(nrow(censoring), nrow(path))
  expect_equal(
    sort(table(censoring$color)),
    sort(table(path$color))
  )
})

test_that("retro_data_censor() reads the same pixels retro_data_path() did", {
  # Excess heights are measured against ink matched by color string, so
  # every color the path recorded has to exist in the pixel matrix the
  # censor step reads. retro_color_pixels() is shared between the two to
  # guarantee it; this asserts the guarantee holds.
  pixel_matrix <- matrix("#ffffff", nrow = 60L, ncol = 40L)
  pixel_matrix[14L:17L, 2L:39L] <- "#dc3030"
  pixel_matrix[44L:47L, 2L:39L] <- "#3030a0"
  input <- retro_test_png(pixel_matrix)
  path <- retro_data_path(input)
  pixels <- retro_color_pixels(magick::image_read(input))
  expect_true(all(
    unique(path$color) %in% unique(as.vector(pixels$pixel_matrix))
  ))
})

test_that("retro_data_censor() validates its arguments", {
  pixel_matrix <- matrix("#ffffff", nrow = 30L, ncol = 20L)
  pixel_matrix[14L:17L, 2L:19L] <- "#dc3030"
  input <- retro_test_png(pixel_matrix)
  path <- retro_data_path(input)
  expect_error(
    retro_data_censor(c(input, input), path),
    class = "simpleError"
  )
  expect_error(
    retro_data_censor(tempfile(fileext = ".png"), path),
    class = "simpleError"
  )
  expect_error(
    retro_data_censor(input, path[, c("x", "y")]),
    class = "simpleError"
  )
})
