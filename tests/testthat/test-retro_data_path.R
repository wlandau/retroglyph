# Helper: create a small quantized PNG from a character matrix of hex colors.
write_pixel_matrix <- function(pixel_matrix, path) {
  magick::image_read(pixel_matrix) |>
    magick::image_write(path = path)
}

# --- retro_path_endpoints ---------------------------------------------------

test_that("retro_path_endpoints() returns start and end for simple case", {
  # Target pixels on row 2, columns 3-8
  target_mask <- matrix(FALSE, nrow = 3, ncol = 10)
  target_mask[2, 3:8] <- TRUE
  target_positions <- which(target_mask, arr.ind = TRUE)
  foreground_mask <- target_mask
  endpoints <- retro_path_endpoints(
    target_positions,
    foreground_mask,
    start_x = 5L,
    line_width = 1L
  )
  expect_type(endpoints, "list")
  expect_equal(endpoints$start_x, 3L)
  expect_equal(endpoints$start_y, 2L)
  expect_equal(endpoints$end_x, 8L)
  expect_equal(endpoints$end_y, 2L)
})

test_that("retro_path_endpoints() uses synthetic start when target is right of start_x", {
  # Target on row 2, columns 5-8. Foreground also on row 2, column 1.
  target_mask <- matrix(FALSE, nrow = 3, ncol = 10)
  target_mask[2, 5:8] <- TRUE
  target_positions <- which(target_mask, arr.ind = TRUE)
  foreground_mask <- target_mask
  foreground_mask[2, 1] <- TRUE
  endpoints <- retro_path_endpoints(
    target_positions,
    foreground_mask,
    start_x = 1L,
    line_width = 1L
  )
  expect_equal(endpoints$start_x, 1L)
  expect_equal(endpoints$start_y, 2L)
  expect_equal(endpoints$end_x, 8L)
})

test_that("retro_path_endpoints() returns NULL when no foreground at start_x", {
  target_mask <- matrix(FALSE, nrow = 3, ncol = 10)
  target_mask[2, 5:8] <- TRUE
  target_positions <- which(target_mask, arr.ind = TRUE)
  foreground_mask <- target_mask
  # No foreground at column 1

  result <- retro_path_endpoints(
    target_positions,
    foreground_mask,
    start_x = 1L,
    line_width = 1L
  )
  expect_null(result)
})

test_that("retro_path_endpoints() breaks a rightmost tie toward the bottommost pixel for a downward curve", {
  # Row 2 (start) runs columns 3-8; row 5 joins in at column 6, so rows 2
  # and 5 tie for the rightmost column (8). The curve trends downward from
  # the start, so the tie should go to the bottommost pixel (row 5).
  target_mask <- matrix(FALSE, nrow = 8, ncol = 10)
  target_mask[2, 3:8] <- TRUE
  target_mask[5, 6:8] <- TRUE
  target_positions <- which(target_mask, arr.ind = TRUE)
  foreground_mask <- target_mask
  endpoints <- retro_path_endpoints(
    target_positions,
    foreground_mask,
    start_x = 5L,
    line_width = 1L
  )
  expect_equal(endpoints$start_x, 3L)
  expect_equal(endpoints$start_y, 2L)
  expect_equal(endpoints$end_x, 8L)
  expect_equal(endpoints$end_y, 5L)
})

test_that("retro_path_endpoints() breaks a rightmost tie toward the topmost pixel for an upward curve", {
  # Row 5 (start) runs columns 3-8; row 2 joins in at column 6, so rows 5
  # and 2 tie for the rightmost column (8). The curve trends upward from
  # the start, so the tie should go to the topmost pixel (row 2).
  target_mask <- matrix(FALSE, nrow = 8, ncol = 10)
  target_mask[5, 3:8] <- TRUE
  target_mask[2, 6:8] <- TRUE
  target_positions <- which(target_mask, arr.ind = TRUE)
  foreground_mask <- target_mask
  endpoints <- retro_path_endpoints(
    target_positions,
    foreground_mask,
    start_x = 5L,
    line_width = 1L
  )
  expect_equal(endpoints$start_x, 3L)
  expect_equal(endpoints$start_y, 5L)
  expect_equal(endpoints$end_x, 8L)
  expect_equal(endpoints$end_y, 2L)
})

test_that("retro_path_endpoints() drops a disconnected debris speck when picking the end point", {
  # A real 10-pixel curve on row 3, columns 1-10, plus a disconnected
  # 2-pixel speck of the same color further right at row 1, columns 18-19.
  # With line_width = 1 and the default component_multiplier = 8, the
  # speck's component (2 pixels) falls under the 8-pixel threshold and is
  # dropped, so the curve's own rightmost pixel (column 10) is chosen.
  target_mask <- matrix(FALSE, nrow = 5, ncol = 20)
  target_mask[3, 1:10] <- TRUE
  target_mask[1, 18:19] <- TRUE
  target_positions <- which(target_mask, arr.ind = TRUE)
  foreground_mask <- target_mask
  endpoints <- retro_path_endpoints(
    target_positions,
    foreground_mask,
    start_x = 1L,
    line_width = 1L
  )
  expect_equal(endpoints$end_x, 10L)
  expect_equal(endpoints$end_y, 3L)
})

# --- retro_path_endpoints_start ---------------------------------------------

test_that("retro_path_endpoints_start() returns the leftmost pixel when it is at or before start_x", {
  target_mask <- matrix(FALSE, nrow = 3, ncol = 10)
  target_mask[2, 3:8] <- TRUE
  target_positions <- which(target_mask, arr.ind = TRUE)
  foreground_mask <- target_mask
  result <- retro_path_endpoints_start(
    target_positions,
    foreground_mask,
    start_x = 5L
  )
  expect_equal(result$start_x, 3L)
  expect_equal(result$start_y, 2L)
})

test_that("retro_path_endpoints_start() returns a synthetic start when target is right of start_x", {
  target_mask <- matrix(FALSE, nrow = 3, ncol = 10)
  target_mask[2, 5:8] <- TRUE
  target_positions <- which(target_mask, arr.ind = TRUE)
  foreground_mask <- target_mask
  foreground_mask[2, 1] <- TRUE
  result <- retro_path_endpoints_start(
    target_positions,
    foreground_mask,
    start_x = 1L
  )
  expect_equal(result$start_x, 1L)
  expect_equal(result$start_y, 2L)
})

test_that("retro_path_endpoints_start() returns NULL when no foreground at start_x", {
  target_mask <- matrix(FALSE, nrow = 3, ncol = 10)
  target_mask[2, 5:8] <- TRUE
  target_positions <- which(target_mask, arr.ind = TRUE)
  foreground_mask <- target_mask
  result <- retro_path_endpoints_start(
    target_positions,
    foreground_mask,
    start_x = 1L
  )
  expect_null(result)
})

# --- retro_path_endpoints_end -----------------------------------------------

test_that("retro_path_endpoints_end() breaks a rightmost tie toward the farthest pixel", {
  target_mask <- matrix(FALSE, nrow = 8, ncol = 10)
  target_mask[2, 3:8] <- TRUE
  target_mask[5, 6:8] <- TRUE
  target_positions <- which(target_mask, arr.ind = TRUE)
  foreground_mask <- target_mask
  result <- retro_path_endpoints_end(
    target_positions,
    foreground_mask,
    start_y = 2L,
    line_width = 1L,
    component_multiplier = 8
  )
  expect_equal(result$end_x, 8L)
  expect_equal(result$end_y, 5L)
})

test_that("retro_path_endpoints_end() drops a disconnected debris speck before picking the rightmost pixel", {
  target_mask <- matrix(FALSE, nrow = 5, ncol = 20)
  target_mask[3, 1:10] <- TRUE
  target_mask[1, 18:19] <- TRUE
  target_positions <- which(target_mask, arr.ind = TRUE)
  foreground_mask <- target_mask
  result <- retro_path_endpoints_end(
    target_positions,
    foreground_mask,
    start_y = 3L,
    line_width = 1L,
    component_multiplier = 8
  )
  expect_equal(result$end_x, 10L)
  expect_equal(result$end_y, 3L)
})

# --- retro_path_endpoints_filter_components ---------------------------------

test_that("retro_path_endpoints_filter_components() drops a small disconnected component", {
  target_mask <- matrix(FALSE, nrow = 5, ncol = 20)
  target_mask[3, 1:10] <- TRUE
  target_mask[1, 18:19] <- TRUE
  target_positions <- which(target_mask, arr.ind = TRUE)
  result <- retro_path_endpoints_filter_components(
    target_positions,
    height = 5L,
    width = 20L,
    line_width = 1L,
    component_multiplier = 8
  )
  expect_true(max(result[, "col"]) <= 10L)
  expect_false(any(result[, "col"] >= 18L))
})

test_that("retro_path_endpoints_filter_components() falls back to unfiltered positions when everything is too small", {
  target_mask <- matrix(FALSE, nrow = 3, ncol = 10)
  target_mask[2, 3:8] <- TRUE
  target_mask[1, 1] <- TRUE
  target_positions <- which(target_mask, arr.ind = TRUE)
  result <- retro_path_endpoints_filter_components(
    target_positions,
    height = 3L,
    width = 10L,
    line_width = 10L,
    component_multiplier = 8
  )
  expect_equal(nrow(result), nrow(target_positions))
})

test_that("retro_path_endpoints_filter_components() respects a non-default component_multiplier", {
  target_mask <- matrix(FALSE, nrow = 5, ncol = 20)
  target_mask[3, 1:10] <- TRUE
  target_mask[1, 15:18] <- TRUE # a 4-pixel component
  target_positions <- which(target_mask, arr.ind = TRUE)
  # component_multiplier = 1, line_width = 1: threshold is 1 pixel, so both
  # components (10 pixels and 4 pixels) survive.
  lenient <- retro_path_endpoints_filter_components(
    target_positions,
    height = 5L,
    width = 20L,
    line_width = 1L,
    component_multiplier = 1
  )
  expect_true(any(lenient[, "col"] >= 15L))
  # component_multiplier = 8, line_width = 1: threshold is 8 pixels, so the
  # 4-pixel component is dropped.
  strict <- retro_path_endpoints_filter_components(
    target_positions,
    height = 5L,
    width = 20L,
    line_width = 1L,
    component_multiplier = 8
  )
  expect_false(any(strict[, "col"] >= 15L))
})

# --- retro_path_shortest --------------------------------------------------------

test_that("retro_path_shortest() finds a path through foreground", {
  # Horizontal strip of foreground on row 3, columns 1-10
  target_mask <- matrix(FALSE, nrow = 5, ncol = 10)
  target_mask[3, c(1:3, 8:10)] <- TRUE
  foreground_mask <- matrix(FALSE, nrow = 5, ncol = 10)
  foreground_mask[3, 1:10] <- TRUE
  path <- retro_path_shortest(
    target_mask,
    foreground_mask,
    start_x = 1L,
    start_y = 3L,
    end_x = 10L,
    end_y = 3L
  )
  expect_s3_class(path, "tbl_df")
  expect_named(path, c("x", "y"))
  expect_true(nrow(path) > 0L)
  # Path must include start and end
  expect_true(1L %in% path$x)
  expect_true(10L %in% path$x)
  # All path pixels are on the foreground strip (row 3)
  expect_true(all(path$y == 3L))
})

# --- retro_path_series (edge cases) ----------------------------------------

test_that("retro_path_series() returns empty for absent target color", {
  pixel_matrix <- matrix("#ffffff", nrow = 5, ncol = 10)
  pixel_matrix[3, 1:10] <- "#0000ff"
  foreground_mask <- pixel_matrix != "#ffffff"
  result <- retro_path_series(
    pixel_matrix = pixel_matrix,
    target_color = "#ff0000",
    background = "#ffffff",
    foreground_mask = foreground_mask,
    width = 10L,
    height = 5L,
    start_x = 1L,
    line_width = 1L
  )
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0L)
})

test_that("retro_path_series() returns empty when endpoints is NULL", {
  # Target starts at column 5, but no foreground at start_x = 1
  pixel_matrix <- matrix("#ffffff", nrow = 3, ncol = 10)
  pixel_matrix[2, 5:8] <- "#ff0000"
  foreground_mask <- pixel_matrix != "#ffffff"
  result <- retro_path_series(
    pixel_matrix = pixel_matrix,
    target_color = "#ff0000",
    background = "#ffffff",
    foreground_mask = foreground_mask,
    width = 10L,
    height = 3L,
    start_x = 1L,
    line_width = 1L
  )
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0L)
})

test_that("retro_path_series() returns single pixel for degenerate case", {
  # Only one target pixel — start == end
  pixel_matrix <- matrix("#ffffff", nrow = 3, ncol = 5)
  pixel_matrix[2, 3] <- "#ff0000"
  foreground_mask <- pixel_matrix != "#ffffff"
  result <- retro_path_series(
    pixel_matrix = pixel_matrix,
    target_color = "#ff0000",
    background = "#ffffff",
    foreground_mask = foreground_mask,
    width = 5L,
    height = 3L,
    start_x = 3L,
    line_width = 1L
  )
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1L)
  expect_equal(result$x, 3L)
  expect_equal(result$y, 2L)
  expect_equal(result$color, "#ff0000")
})

# --- retro_path_conductance_graph ------------------------------------------------

test_that("retro_path_conductance_graph() returns a list with an igraph", {
  target_mask <- matrix(FALSE, nrow = 3, ncol = 3)
  target_mask[2, 1] <- TRUE
  foreground_mask <- target_mask
  foreground_mask[2, 3] <- TRUE
  result <- retro_path_conductance_graph(target_mask, foreground_mask)
  expect_type(result, "list")
  expect_true(igraph::is_igraph(result$graph))
  expect_true(is.integer(result$vertex_lookup))
  expect_equal(igraph::vcount(result$graph), 2L)
})

test_that("retro_path_conductance_graph() gives lower cost to target edges", {
  # 1x3 strip: cells 1-2 are target, cell 3 is other foreground
  target_mask <- matrix(FALSE, nrow = 1, ncol = 3)
  target_mask[1, 1:2] <- TRUE
  foreground_mask <- matrix(TRUE, nrow = 1, ncol = 3)
  result <- retro_path_conductance_graph(target_mask, foreground_mask)
  weights <- igraph::E(result$graph)$weight
  # Edge between target-target (cost = 1/1 = 1) should be cheaper than

  # edge between target-other (cost = 1/mean(1, 0.1) = 1/0.55 ≈ 1.82)
  expect_equal(length(weights), 2L)
  expect_lt(min(weights), max(weights))
})

test_that("retro_path_conductance_graph() includes vertical (down) edges", {
  # 3x1 column: all foreground, target on rows 1-2

  target_mask <- matrix(FALSE, nrow = 3, ncol = 1)
  target_mask[1:2, 1] <- TRUE
  foreground_mask <- matrix(TRUE, nrow = 3, ncol = 1)
  result <- retro_path_conductance_graph(target_mask, foreground_mask)
  expect_equal(igraph::vcount(result$graph), 3L)
  expect_equal(igraph::ecount(result$graph), 2L)
})

# --- retro_path_claim_pixels ------------------------------------------------------

test_that("retro_path_claim_pixels() returns foreground pixels along path", {
  foreground_mask <- matrix(FALSE, nrow = 5, ncol = 10)
  foreground_mask[3, 2:8] <- TRUE
  recovered <- tibble::tibble(x = 5L, y = 3L)
  result <- retro_path_claim_pixels(
    recovered,
    width = 10,
    height = 5,
    foreground_mask = foreground_mask,
    line_width = 3L,
    target_color = "#ff0000"
  )
  expect_s3_class(result, "tbl_df")
  expect_named(result, c("x", "y", "color"))
  expect_true(all(result$color == "#ff0000"))
  expect_true(nrow(result) > 0L)
})

test_that("retro_path_claim_pixels() respects foreground mask", {
  foreground_mask <- matrix(FALSE, nrow = 5, ncol = 10)
  foreground_mask[3, 3:7] <- TRUE
  recovered <- tibble::tibble(x = 5L, y = 3L)
  result <- retro_path_claim_pixels(
    recovered,
    width = 10,
    height = 5,
    foreground_mask = foreground_mask,
    line_width = 3L,
    target_color = "#ff0000"
  )
  # All claimed pixels must be within the foreground
  if (nrow(result) > 0L) {
    for (i in seq_len(nrow(result))) {
      expect_true(foreground_mask[result$y[i], result$x[i]])
    }
  }
})

test_that("retro_path_claim_pixels() returns a thin skeleton, not the full foreground band", {
  # A 7-row-tall foreground blob - wide enough that a skeleton is a real
  # thinning, not just the same shape the 1-row-tall tests above degenerate
  # to.
  foreground_mask <- matrix(FALSE, nrow = 9, ncol = 10)
  foreground_mask[2:8, 2:9] <- TRUE
  recovered <- tibble::tibble(x = 2:9, y = rep(5L, 8L))
  result <- retro_path_claim_pixels(
    recovered,
    width = 10L,
    height = 9L,
    foreground_mask = foreground_mask,
    line_width = 3L,
    target_color = "#ff0000"
  )
  expect_true(nrow(result) > 0L)
  expect_true(nrow(result) < sum(foreground_mask))
})

# --- retro_path_line_width --------------------------------------------------------

test_that("retro_path_line_width() falls back to 2 with no foreground", {
  pixel_matrix <- matrix("#ffffff", nrow = 3, ncol = 3)
  result <- retro_path_line_width(pixel_matrix, background = "#ffffff")
  expect_equal(result, 2L)
})

test_that("retro_path_line_width() estimates width for a single series", {
  # A 3-pixel-thick horizontal red line
  pixel_matrix <- matrix("#ffffff", nrow = 10, ncol = 20)
  pixel_matrix[4:6, 3:18] <- "#ff0000"
  result <- retro_path_line_width(pixel_matrix, background = "#ffffff")
  expect_true(is.integer(result))
  expect_true(result >= 2L && result <= 5L)
})

test_that("retro_path_line_width() aggregates across every series", {
  pixel_matrix <- matrix("#ffffff", nrow = 20, ncol = 20)
  pixel_matrix[4:6, 3:18] <- "#ff0000"
  pixel_matrix[10:16, 3:18] <- "#0000ff"
  red_width <- retro_path_color_width(pixel_matrix, "#ff0000")
  blue_width <- retro_path_color_width(pixel_matrix, "#0000ff")
  result <- retro_path_line_width(pixel_matrix, background = "#ffffff")
  expect_true(is.integer(result))
  expect_true(red_width != blue_width)
  expect_equal(result, as.integer(ceiling(max(c(red_width, blue_width)))))
})

test_that("retro_path_color_width() measures one color's thickness", {
  pixel_matrix <- matrix("#ffffff", nrow = 10, ncol = 20)
  pixel_matrix[4:6, 3:18] <- "#ff0000"
  result <- retro_path_color_width(pixel_matrix, "#ff0000")
  expect_true(is.numeric(result))
  expect_true(result >= 2 && result <= 5)
})

# --- retro_data_path -------------------------------------------------------

test_that("retro_data_path() returns correct class and structure", {
  pixel_matrix <- matrix("#ffffff", nrow = 5, ncol = 20)
  pixel_matrix[3, 1:10] <- "#ff0000"
  pixel_matrix[3, 8:20] <- "#0000ff"
  input <- tempfile(fileext = ".png")
  on.exit(unlink(input))
  write_pixel_matrix(pixel_matrix, input)
  result <- retro_data_path(input)
  expect_s3_class(result, "tbl_df")
  expect_equal(
    names(result),
    c("x", "y", "color", "line_width", "width", "height")
  )
  expect_true(all(result$width == 20L))
  expect_true(all(result$height == 5L))
  expect_true(is.integer(result$x))
  expect_true(is.integer(result$y))
  expect_true(is.integer(result$line_width))
  expect_true(all(result$line_width == result$line_width[1L]))
})

test_that("retro_data_path() finds path through occluded region", {
  pixel_matrix <- matrix("#ffffff", nrow = 5, ncol = 20)
  pixel_matrix[3, 1:7] <- "#ff0000"
  pixel_matrix[3, 13:20] <- "#ff0000"
  pixel_matrix[3, 8:12] <- "#0000ff"
  input <- tempfile(fileext = ".png")
  on.exit(unlink(input))
  write_pixel_matrix(pixel_matrix, input)
  result <- retro_data_path(input)
  # Red series should have pixels spanning the occluded region. The
  # reconstructed trace is decimated, so a collinear pixel in the middle
  # of the (now-filled-in) straight run is not guaranteed to survive -
  # check that the endpoints bracket the occlusion instead.
  red_pixels <- result[result$color == "#ff0000", ]
  expect_true(nrow(red_pixels) > 0L)
  expect_true(min(red_pixels$x) <= 7L)
  expect_true(max(red_pixels$x) >= 13L)
})

test_that("retro_data_path() handles single-color image", {
  pixel_matrix <- matrix("#ffffff", nrow = 3, ncol = 10)
  pixel_matrix[2, 2:9] <- "#ff0000"
  input <- tempfile(fileext = ".png")
  on.exit(unlink(input))
  write_pixel_matrix(pixel_matrix, input)
  result <- retro_data_path(input)
  expect_s3_class(result, "tbl_df")
  expect_true(nrow(result) > 0L)
})

test_that("retro_data_path() errors on non-existent input", {
  expect_error(
    retro_data_path("/nonexistent/path.png"),
    "input must exist"
  )
})

test_that("retro_data_path() errors on non-string input", {
  expect_error(retro_data_path(123), "input must be a single string")
})

test_that("retro_data_path() errors on image with only background", {
  pixel_matrix <- matrix("#ffffff", nrow = 3, ncol = 3)
  input <- tempfile(fileext = ".png")
  on.exit(unlink(input))
  write_pixel_matrix(pixel_matrix, input)
  expect_error(
    suppressWarnings(retro_data_path(input)),
    "No Kaplan-Meier curves detected"
  )
})

test_that("retro_data_path() recovers both series when curves never touch", {
  pixel_matrix <- matrix("#ffffff", nrow = 5, ncol = 20)
  pixel_matrix[3, 1:5] <- "#ff0000"
  pixel_matrix[3, 12:20] <- "#0000ff"
  input <- tempfile(fileext = ".png")
  on.exit(unlink(input))
  write_pixel_matrix(pixel_matrix, input)
  result <- retro_data_path(input)
  expect_true(any(result$color == "#ff0000"))
  expect_true(any(result$color == "#0000ff"))
})

# --- retro_path_component_mask ----------------------------------------------

test_that("retro_path_component_mask() keeps the majority component and drops the minority one", {
  # Two disconnected red blobs: rows 1-2 (4 pixels) and row 5 (2 pixels).
  # The row 1-2 blob has more red pixels, so it should win.
  pixel_matrix <- matrix("#ffffff", nrow = 5, ncol = 5)
  pixel_matrix[1:2, 1:2] <- "#ff0000"
  pixel_matrix[5, 4:5] <- "#ff0000"
  foreground_mask <- pixel_matrix != "#ffffff"
  labeled <- retro_components_label(foreground_mask)
  result <- retro_path_component_mask(pixel_matrix, "#ff0000", labeled)
  expect_true(all(result[1:2, 1:2]))
  expect_false(any(result[5, 4:5]))
})

test_that("retro_data_path() does not modify input file", {
  pixel_matrix <- matrix("#ffffff", nrow = 5, ncol = 20)
  pixel_matrix[3, 1:7] <- "#ff0000"
  pixel_matrix[3, 8:20] <- "#0000ff"
  input <- tempfile(fileext = ".png")
  on.exit(unlink(input))
  write_pixel_matrix(pixel_matrix, input)
  md5_before <- tools::md5sum(input)
  retro_data_path(input)
  expect_equal(tools::md5sum(input), md5_before)
})
