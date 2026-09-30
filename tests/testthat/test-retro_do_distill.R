# Labeled ticks whose bounding box edges sit adjacent to the synthetic
# image's L-shaped axis lines: x-axis labels below row 80, y-axis labels
# to the left of column 20.
distill_labels <- tibble::tibble(
  label = c("A", "B", "C", "D"),
  word = c("0", "12", "0", "1"),
  confidence = rep(90, 4L),
  x = c(20, 90, 15, 15),
  y = c(86, 86, 80, 10),
  x1 = c(18, 88, 13, 13),
  x2 = c(22, 92, 17, 17),
  y1 = c(83, 83, 77, 7),
  y2 = c(89, 89, 83, 13)
)

# White background, y-axis at column 20 (rows 10-80), x-axis at row 80
# (columns 20-90). The detected panel is x in [23, 87], y in [13, 77].
distill_raster <- function() {
  raster <- matrix("#ffffffff", nrow = 100, ncol = 100)
  raster[10:80, 20] <- "#000000ff"
  raster[80, 20:90] <- "#000000ff"
  raster
}

distill_run <- function(raster, ...) {
  # Inputs and outputs land in tempdir(), which R clears on exit.
  input <- retro_test_png(raster)
  retro_do_distill(
    input = input,
    output_clean = tempfile(fileext = ".png"),
    data_label = distill_labels,
    x_label = c("A", "B"),
    x_value = c(0, 12),
    y_label = c("C", "D"),
    y_value = c(0, 1),
    ...
  )
}

test_that("retro_do_distill() returns every element it documents", {
  raster <- distill_raster()
  raster[40:44, 25:85] <- "#ff0000ff"
  result <- distill_run(
    raster,
    background = "#FFFFFF",
    series = "#FF0000",
    series_names = "Arm A",
    reference = "Arm A"
  )
  expect_named(
    result,
    c(
      "image_panel",
      "image_classified",
      "image_clean",
      "data_panel",
      "x_axis",
      "y_axis",
      "background",
      "legend",
      "counts"
    )
  )
  expect_true(all(file.exists(
    result$image_clean,
    result$image_panel,
    result$image_classified
  )))
  expect_equal(result$background, "#ffffff")
  expect_equal(nrow(result$x_axis), 2L)
  expect_equal(nrow(result$y_axis), 2L)
  expect_equal(nrow(result$data_panel), 1L)
})

test_that("retro_do_distill() crops to the detected panel", {
  result <- distill_run(
    distill_raster(),
    background = "#FFFFFF",
    series = "#FF0000",
    series_names = "Arm A",
    reference = "Arm A"
  )
  # panel$x1/y2 land exactly on the detected axis lines at column 20 and
  # row 80; retro_do_distill() pads past them only when masking, not in
  # the panel tibble it returns.
  expect_equal(result$data_panel$x1, 20L)
  expect_equal(result$data_panel$y2, 80L)
})

test_that("retro_do_distill() clears a closed bounding box", {
  raster <- distill_raster()
  # Close the box: top row 10 and right column 90.
  raster[10, 20:90] <- "#000000ff"
  raster[10:80, 90] <- "#000000ff"
  result <- distill_run(
    raster,
    background = "#FFFFFF",
    series = "#FF0000",
    series_names = "Arm A",
    reference = "Arm A"
  )
  tidy <- magick::image_raster(
    magick::image_read(result$image_clean),
    tidy = TRUE
  )
  expect_true(all(tidy$col[tidy$x == 20] == "#ffffffff"))
  expect_true(all(tidy$col[tidy$x == 90] == "#ffffffff"))
  expect_true(all(tidy$col[tidy$y == 10] == "#ffffffff"))
  expect_true(all(tidy$col[tidy$y == 80] == "#ffffffff"))
})

test_that("retro_do_distill() counts pixels on the cleaned image", {
  raster <- distill_raster()
  # 5 rows x 61 columns of red curve survive cleaning.
  raster[40:44, 25:85] <- "#ff0000ff"
  result <- distill_run(
    raster,
    background = "#FFFFFF",
    series = "#FF0000",
    series_names = "Arm A",
    reference = "Arm A"
  )
  expect_equal(result$counts, 5L * 61L)
})

test_that("retro_do_distill() keeps a curve a third of the width", {
  raster <- distill_raster()
  # 35 of 100 columns, well inside the panel. Survives because cleaning
  # runs at retro_image_clean()'s permissive default, which is not
  # exposed here for the model to get wrong.
  raster[40:44, 25:59] <- "#ff0000ff"
  result <- distill_run(
    raster,
    background = "#FFFFFF",
    series = "#FF0000",
    series_names = "Arm A",
    reference = "Arm A"
  )
  expect_equal(result$counts, 5L * 35L)
})

test_that("retro_do_distill() reports 0 for a series cleaned away", {
  raster <- distill_raster()
  raster[40:44, 25:85] <- "#ff0000ff"
  # Blue appears only as a short legend key, so cleaning removes it all.
  raster[30:34, 70:78] <- "#0000ffff"
  result <- distill_run(
    raster,
    background = "#FFFFFF",
    series = c("#FF0000", "#0000FF"),
    series_names = c("Arm A", "Arm B"),
    reference = "Arm A"
  )
  blue <- result$legend$color == "#0000ff"
  expect_equal(result$counts[blue], 0L)
  expect_true(result$counts[!blue] > 0L)
})

test_that("retro_do_distill() keeps legend and counts in the given order", {
  raster <- distill_raster()
  # Blue's curve (5 x 61) has more surviving pixels than Red's (5 x 30),
  # but series_names lists Red first - the legend must preserve that
  # order rather than sorting by which curve has more pixels.
  raster[40:44, 25:54] <- "#ff0000ff"
  raster[50:54, 25:85] <- "#0000ffff"
  result <- distill_run(
    raster,
    background = "#FFFFFF",
    series = c("#FF0000", "#0000FF"),
    series_names = c("Red", "Blue"),
    reference = "Red"
  )
  expect_equal(result$legend$series, c("Red", "Blue"))
  # counts stay aligned with legend rows, in input order, even though
  # Blue's count (5 x 61) is larger than Red's (5 x 30).
  expect_equal(result$counts, c(5L * 30L, 5L * 61L))
})

test_that("retro_do_distill() marks the reference series in the legend", {
  raster <- distill_raster()
  raster[40:44, 25:85] <- "#ff0000ff"
  raster[50:54, 25:85] <- "#0000ffff"
  result <- distill_run(
    raster,
    background = "#FFFFFF",
    series = c("#FF0000", "#0000FF"),
    series_names = c("Placebo", "Drug"),
    reference = "Placebo"
  )
  expect_true(is.character(result$legend$series))
  expect_equal(result$legend$series, c("Placebo", "Drug"))
  expect_true(is.logical(result$legend$reference))
  expect_equal(result$legend$reference, c(TRUE, FALSE))
})

test_that("retro_do_distill() flattens garbage colors to background", {
  raster <- distill_raster()
  raster[40:44, 25:85] <- "#ff0000ff"
  # A long green band, wide enough to survive cleaning on its own.
  raster[60:64, 25:85] <- "#00ff00ff"
  result <- distill_run(
    raster,
    background = "#FFFFFF",
    series = "#FF0000",
    series_names = "Arm A",
    reference = "Arm A",
    garbage = "#00FF00"
  )
  tidy <- magick::image_raster(
    magick::image_read(result$image_clean),
    tidy = TRUE
  )
  band <- tidy$y >= 60 & tidy$y <= 64 & tidy$x >= 25 & tidy$x <= 85
  expect_true(all(tidy$col[band] == "#ffffffff"))
})

test_that("retro_do_distill() writes intermediates where told", {
  raster <- distill_raster()
  raster[40:44, 25:85] <- "#ff0000ff"
  panel <- tempfile(fileext = ".png")
  classified <- tempfile(fileext = ".png")
  on.exit(unlink(c(panel, classified)))
  result <- distill_run(
    raster,
    output_panel = panel,
    output_classified = classified,
    background = "#FFFFFF",
    series = "#FF0000",
    series_names = "Arm A",
    reference = "Arm A"
  )
  expect_equal(result$image_panel, panel)
  expect_equal(result$image_classified, classified)
  expect_true(all(file.exists(panel, classified)))
})

test_that("retro_do_distill() validates its arguments", {
  raster <- distill_raster()
  expect_error(
    distill_run(
      raster,
      background = "#FFFFFF",
      series = c("#FF0000", "#0000FF"),
      series_names = "Arm A",
      reference = "Arm A"
    ),
    "series and series_names must have the same length"
  )
  expect_error(
    distill_run(
      raster,
      background = "#FFFFFF",
      series = "chartreuse",
      series_names = "Arm A",
      reference = "Arm A"
    ),
    "must be valid hex colors"
  )
  expect_error(
    distill_run(
      raster,
      background = "#FFFFFF",
      series = character(0L),
      series_names = character(0L),
      reference = "Arm A"
    ),
    "series must have at least one hex color"
  )
})

test_that("retro_do_distill() rejects a missing input", {
  expect_error(
    retro_do_distill(
      input = tempfile(fileext = ".png"),
      output_clean = tempfile(fileext = ".png"),
      data_label = distill_labels,
      x_label = c("A", "B"),
      x_value = c(0, 12),
      y_label = c("C", "D"),
      y_value = c(0, 1),
      background = "#FFFFFF",
      series = "#FF0000",
      series_names = "Arm A",
      reference = "Arm A"
    ),
    "input must exist"
  )
})
