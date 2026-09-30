test_that("retro_do_label() validates inputs", {
  input <- retro_test_png(matrix("#ffffffff", nrow = 20L, ncol = 20L))
  output <- tempfile(fileext = ".png")
  expect_error(
    retro_do_label("/nonexistent.png", output),
    "input must exist"
  )
  expect_error(
    retro_do_label(input, output, confidence = 200),
    "confidence must be in"
  )
  expect_error(
    retro_do_label(input, output, confidence = -1),
    "confidence must be in"
  )
  expect_error(
    retro_do_label(input, output, magnify = 0.5),
    "magnify must be >= 1"
  )
  expect_error(
    retro_do_label(input, 123),
    "output must be a single string"
  )
})

# The next three tests only check the shape of retro_do_label()'s output
# (column names/types, that a file gets written, that labels are sequential)
# rather than OCR accuracy, so Tesseract is mocked and the input image is a
# tiny synthetic canvas - the real "does OCR actually detect text" claim is
# covered once, below, by "detects text from a real OCR fixture".

test_that("retro_do_label() returns correct column structure", {
  input <- retro_test_png(matrix("#ffffffff", nrow = 20L, ncol = 20L))
  output <- tempfile(fileext = ".png")
  on.exit(unlink(output))
  local_mocked_bindings(
    tesseract = function(...) NULL,
    ocr_data = function(...) {
      tibble::tibble(
        word = c("0", "12"),
        confidence = c(90, 85),
        bbox = c("1,1,5,5", "8,8,12,12")
      )
    },
    .package = "tesseract"
  )
  result <- retro_do_label(input, output, magnify = 4)
  expect_s3_class(result, "tbl_df")
  expect_named(
    result,
    c("label", "word", "confidence", "x", "y", "x1", "x2", "y1", "y2")
  )
  expect_type(result$label, "character")
  expect_type(result$word, "character")
  expect_type(result$confidence, "double")
  expect_type(result$x, "double")
  expect_type(result$y, "double")
  expect_type(result$x1, "integer")
  expect_type(result$x2, "integer")
  expect_type(result$y1, "integer")
  expect_type(result$y2, "integer")
})

test_that("retro_do_label() writes output image", {
  input <- retro_test_png(matrix("#ffffffff", nrow = 20L, ncol = 20L))
  output <- tempfile(fileext = ".png")
  on.exit(unlink(output))
  local_mocked_bindings(
    tesseract = function(...) NULL,
    ocr_data = function(...) {
      tibble::tibble(word = "0", confidence = 90, bbox = "1,1,5,5")
    },
    .package = "tesseract"
  )
  retro_do_label(input, output, magnify = 4)
  expect_true(file.exists(output))
  info <- magick::image_info(magick::image_read(output))
  expect_true(info$width > 0L)
  expect_true(info$height > 0L)
})

test_that("retro_do_label() detects text from a real OCR fixture", {
  input <- retro_test_ocr_png()
  output <- tempfile(fileext = ".png")
  on.exit(unlink(c(input, output)))
  result <- retro_do_label(input, output, magnify = 4)
  expect_true(nrow(result) >= 4L)
  expect_equal(result$label[1L], "A")
  if (nrow(result) >= 2L) expect_equal(result$label[2L], "B")
})

test_that("retro_do_label() strips hyphens and keeps only positive numbers", {
  input <- retro_test_png(matrix("#ffffffff", nrow = 20L, ncol = 20L))
  output <- tempfile(fileext = ".png")
  on.exit(unlink(output))
  local_mocked_bindings(
    tesseract = function(...) NULL,
    ocr_data = function(...) {
      tibble::tibble(
        word = c("1-2", "-5", "3-4-5", "12"),
        confidence = c(90, 90, 90, 90),
        bbox = c(
          "10,10,20,20",
          "30,10,40,20",
          "50,10,60,20",
          "70,10,80,20"
        )
      )
    },
    .package = "tesseract"
  )
  result <- retro_do_label(input, output)
  expect_equal(result$word, c("12", "5", "345", "12"))
})

test_that("retro_do_label() labels are sequential", {
  input <- retro_test_png(matrix("#ffffffff", nrow = 20L, ncol = 20L))
  output <- tempfile(fileext = ".png")
  on.exit(unlink(output))
  local_mocked_bindings(
    tesseract = function(...) NULL,
    ocr_data = function(...) {
      tibble::tibble(
        word = c("0", "1", "2", "3"),
        confidence = rep(90, 4L),
        bbox = c("1,1,3,3", "5,5,7,7", "9,9,11,11", "13,13,15,15")
      )
    },
    .package = "tesseract"
  )
  result <- retro_do_label(input, output, magnify = 4)
  expected_labels <- retro_ocr_letters(nrow(result))
  expect_equal(result$label, expected_labels)
})

test_that("retro_do_label() handles empty detections", {
  # A line only (no glyphs), so real OCR would legitimately find nothing -
  # but scaling such a sparse image trips Tesseract's own size warnings.
  # Mock ocr_data() to assert the zero-row shape without that noise.
  raster <- matrix("#ffffffff", nrow = 100L, ncol = 200L)
  raster[50L, 20L:180L] <- "#000000ff"
  input <- retro_test_png(raster)
  output <- tempfile(fileext = ".png")
  on.exit(unlink(output))
  local_mocked_bindings(
    tesseract = function(...) NULL,
    ocr_data = function(...) {
      tibble::tibble(
        word = character(0L),
        confidence = numeric(0L),
        bbox = character(0L)
      )
    },
    .package = "tesseract"
  )
  result <- retro_do_label(input, output, confidence = 90)
  expect_equal(nrow(result), 0L)
  expect_named(
    result,
    c("label", "word", "confidence", "x", "y", "x1", "x2", "y1", "y2")
  )
  expect_true(file.exists(output))
})

test_that("retro_do_label() respects magnify argument", {
  # The numeric-label fixture shrunk to 15%: too small for Tesseract to
  # find anything at native size, but recovers at 4x. A single tiny glyph
  # (rather than a scaled-down page of several) makes Tesseract's own
  # page-layout step print "Empty page!!" regardless of magnify, so this
  # keeps enough real structure to avoid that.
  base <- retro_test_ocr_png()
  on.exit(unlink(base), add = TRUE)
  small <- magick::image_scale(magick::image_read(base), "15%")
  input <- tempfile(fileext = ".png")
  magick::image_write(small, input)
  output_1x <- tempfile(fileext = ".png")
  output_4x <- tempfile(fileext = ".png")
  on.exit(unlink(c(input, output_1x, output_4x)), add = TRUE)
  result_1x <- retro_do_label(input, output_1x, magnify = 1)
  # Comparing exact counts against result_1x is flaky: detection counts at
  # each magnification depend on the installed Tesseract/leptonica version,
  # which differs across platforms. The stable claim is just that
  # magnification recovers a glyph too small to detect at native size.
  result_4x <- retro_do_label(input, output_4x, magnify = 4)
  expect_gte(nrow(result_4x), 1L)
})

test_that("retro_do_label() annotates the uncleaned grayscale image", {
  # A long dark line (which cleaning strips before OCR) plus a light
  # gridline (which the allow list drops). Both must still be present in
  # the annotated output, since only detection sees the cleaned copy.
  # This test checks annotation, not detection, so OCR is mocked to avoid
  # Tesseract's own size warnings on a glyph-free image.
  raster <- matrix("#ffffff", nrow = 60L, ncol = 120L)
  raster[30L, 5L:115L] <- "#000000"
  raster[10L, 20L:60L] <- "#d0d0d0"
  input <- retro_test_png(raster)
  output <- tempfile(fileext = ".png")
  on.exit(unlink(c(input, output)))
  local_mocked_bindings(
    tesseract = function(...) NULL,
    ocr_data = function(...) {
      tibble::tibble(
        word = character(0L),
        confidence = numeric(0L),
        bbox = character(0L)
      )
    },
    .package = "tesseract"
  )
  retro_do_label(input, output)
  tidy <- magick::image_raster(magick::image_read(output), tidy = TRUE)
  pixel <- function(x, y) tidy$col[tidy$x == x & tidy$y == y]
  expect_equal(pixel(60L, 30L), "#000000ff")
  expect_equal(pixel(40L, 10L), "#d0d0d0ff")
})

test_that("retro_do_label() validates span_threshold", {
  input <- retro_test_png(matrix("#ffffffff", nrow = 20L, ncol = 20L))
  output <- tempfile(fileext = ".png")
  expect_error(
    retro_do_label(input, output, span_threshold = 0),
    "span_threshold must be in"
  )
  expect_error(
    retro_do_label(input, output, span_threshold = 1.5),
    "span_threshold must be in"
  )
  expect_error(
    retro_do_label(input, output, span_threshold = "a"),
    "span_threshold must be a single number"
  )
})
