# Shared test fixtures. testthat sources every helper-*.R file before
# running any test, so these are available everywhere with no explicit
# source() call.

# Read a hex-color character matrix into an opaque magick image.
#
# Mirrors retro_image_png(), the pipeline's entry point: composite
# transparency onto white, then drop the alpha channel. Tests build
# images straight from a matrix and bypass that entry point, so they have
# to do both steps themselves. retro_color_opaque() documents why the
# second step is not redundant.
retro_test_image <- function(matrix) {
  magick::image_read(matrix) |>
    magick::image_background("white", flatten = TRUE) |>
    retro_color_opaque()
}

# Write a hex-color character matrix to a temp PNG and return its path.
retro_test_png <- function(matrix) {
  path <- tempfile(fileext = ".png")
  retro_test_image(matrix) |> magick::image_write(path)
  path
}

# Build a retro_data_path tibble directly from a pixel matrix, by
# diffing against a background color. Avoids running the full path-finding
# algorithm when a test only needs a path-shaped result to act on.
retro_test_path <- function(
  pixel_matrix,
  background = "#ffffff",
  line_width = 1L
) {
  positions <- which(pixel_matrix != background, arr.ind = TRUE)
  tibble::tibble(
    x = as.integer(positions[, "col"]),
    y = as.integer(positions[, "row"]),
    color = pixel_matrix[positions],
    line_width = line_width,
    width = ncol(pixel_matrix),
    height = nrow(pixel_matrix)
  )
}

# A synthetic chart-free canvas with scattered numeric labels, for tests
# that exercise the real OCR pipeline end to end. Unlike a real chart, it
# has no thin ruler ticks or curve strokes, so Tesseract's "single text
# line" segmentation mode never mistakes chart artifacts for slivers of a
# line and emits its own "too small to scale" diagnostics.
retro_test_ocr_png <- function() {
  image <- magick::image_blank(800, 400, color = "white")
  labels <- list(
    list(txt = "0", loc = "+50+40"),
    list(txt = "6", loc = "+300+40"),
    list(txt = "12", loc = "+550+40"),
    list(txt = "18", loc = "+50+220"),
    list(txt = "24", loc = "+300+220"),
    list(txt = "30", loc = "+550+220")
  )
  for (label in labels) {
    image <- magick::image_annotate(
      image,
      label$txt,
      size = 60,
      color = "black",
      location = label$loc
    )
  }
  path <- tempfile(fileext = ".png")
  magick::image_write(image, path)
  path
}

# Build a single-row OCR findings tibble shaped like retro_ocr_detect()'s
# output, for tests of retro_ocr_pool_findings() that only need bounding
# boxes and confidence to compare, not a real Tesseract call.
retro_test_finding <- function(word, confidence, x1, y1, x2, y2) {
  tibble::tibble(
    word = word,
    confidence = confidence,
    x = (x1 + x2) / 2,
    x1 = x1,
    x2 = x2,
    y = (y1 + y2) / 2,
    y1 = y1,
    y2 = y2
  )
}
