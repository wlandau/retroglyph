test_that("retro_data_ocr() validates inputs", {
  input <- retro_test_png(matrix("#ffffffff", nrow = 20L, ncol = 20L))
  expect_error(
    retro_data_ocr("/nonexistent.png"),
    "input must exist"
  )
  expect_error(
    retro_data_ocr(input, confidence = 200),
    "confidence must be in"
  )
  expect_error(
    retro_data_ocr(input, confidence = -1),
    "confidence must be in"
  )
  expect_error(
    retro_data_ocr(input, magnify = 0.5),
    "magnify must be >= 1"
  )
  expect_error(
    retro_data_ocr(input, span_threshold = 0),
    "span_threshold must be in"
  )
  expect_error(
    retro_data_ocr(input, span_threshold = 1.5),
    "span_threshold must be in"
  )
  expect_error(
    retro_data_ocr(input, span_threshold = "a"),
    "span_threshold must be a single number"
  )
})

test_that("retro_data_ocr() returns findings with labels already assigned", {
  input <- retro_test_png(matrix("#ffffffff", nrow = 20L, ncol = 20L))
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
  result <- retro_data_ocr(input, magnify = 4)
  expect_s3_class(result, "tbl_df")
  expect_named(
    result,
    c("label", "word", "confidence", "x", "y", "x1", "x2", "y1", "y2")
  )
  expect_equal(result$label, c("A", "B"))
})

test_that("retro_data_ocr() detects text from a real OCR fixture", {
  source <- retro_test_ocr_png()
  gray_input <- tempfile(fileext = ".png")
  on.exit(unlink(c(source, gray_input)))
  retro_image_grayscale(source, gray_input)
  result <- retro_data_ocr(gray_input, magnify = 4)
  expect_true(nrow(result) >= 4L)
  expect_equal(result$label[1L], "A")
})

# --- retro_ocr_quantize -----------------------------------------------------

test_that("retro_ocr_quantize() collapses to exactly 2 colors", {
  raster <- matrix("#ffffff", nrow = 10L, ncol = 10L)
  raster[5L, 5L] <- "#808080"
  raster[3L, 3L] <- "#101010"
  input <- retro_test_png(raster)
  binary <- retro_ocr_quantize(input)
  on.exit(unlink(binary))
  tidy <- magick::image_raster(magick::image_read(binary), tidy = TRUE)
  expect_equal(length(unique(tidy$col)), 2L)
})

# --- retro_ocr_clean_image / retro_ocr_oversized_components -----------------

test_that("retro_ocr_clean_image() erases a component spanning too much width", {
  # 20 rows x 100 cols: threshold is 0.15 * 20 = 3 rows or 0.15 * 100 = 15
  # cols. A "line" spanning cols 5-95 (91 px) trips the width threshold.
  # A small 2x2 "digit" block (row span 2, col span 2) trips neither.
  raster <- matrix("#ffffff", nrow = 20L, ncol = 100L)
  raster[10L, 5L:95L] <- "#000000"
  raster[15L:16L, 60L:61L] <- "#000000"
  input <- retro_test_png(raster)
  cleaned <- retro_ocr_clean_image(
    input,
    input,
    span_threshold = 0.15,
    dilate = 0
  )
  on.exit(unlink(cleaned))
  tidy <- magick::image_raster(magick::image_read(cleaned), tidy = TRUE)
  pixel <- function(x, y) tidy$col[tidy$x == x & tidy$y == y]
  expect_equal(pixel(50L, 10L), "#ffffffff")
  expect_equal(pixel(60L, 15L), "#000000ff")
})

test_that("retro_ocr_clean_image() erases a component spanning too much height", {
  # 100 rows x 20 cols: threshold is 0.15 * 100 = 15 rows or 0.15 * 20 = 3
  # cols. A vertical "line" spanning rows 5-95 (91 px) trips the height
  # threshold. A small 2x2 "digit" block (row span 2, col span 2) trips
  # neither.
  raster <- matrix("#ffffff", nrow = 100L, ncol = 20L)
  raster[5L:95L, 10L] <- "#000000"
  raster[60L:61L, 15L:16L] <- "#000000"
  input <- retro_test_png(raster)
  cleaned <- retro_ocr_clean_image(
    input,
    input,
    span_threshold = 0.15,
    dilate = 0
  )
  on.exit(unlink(cleaned))
  tidy <- magick::image_raster(magick::image_read(cleaned), tidy = TRUE)
  pixel <- function(x, y) tidy$col[tidy$x == x & tidy$y == y]
  expect_equal(pixel(10L, 50L), "#ffffffff")
  expect_equal(pixel(15L, 60L), "#000000ff")
})

test_that("retro_ocr_clean_image() is a no-op on an image with no foreground", {
  raster <- matrix("#ffffff", nrow = 20L, ncol = 20L)
  input <- retro_test_png(raster)
  cleaned <- retro_ocr_clean_image(
    input,
    input,
    span_threshold = 0.15,
    dilate = 0
  )
  on.exit(unlink(cleaned))
  tidy_in <- magick::image_raster(magick::image_read(input), tidy = TRUE)
  tidy_out <- magick::image_raster(magick::image_read(cleaned), tidy = TRUE)
  expect_equal(tidy_out$col, tidy_in$col)
})

test_that("retro_ocr_clean_image() dilation reaches dark fringe near an oversized line", {
  # A dark line spanning most of the width (oversized), plus a dark pixel
  # two rows above it. That pixel quantizes to foreground and is its own
  # small component, so the allow list and the span filter both keep it -
  # only dilating the line's mask reaches far enough to erase it.
  raster <- matrix("#ffffff", nrow = 20L, ncol = 100L)
  raster[10L, 5L:95L] <- "#000000"
  raster[8L, 50L] <- "#000000"
  input <- retro_test_png(raster)
  gray <- tempfile(fileext = ".png")
  retro_image_grayscale(input, gray)
  binary <- retro_ocr_quantize(input)
  on.exit(unlink(c(gray, binary)))
  fringe <- function(path) {
    tidy <- magick::image_raster(magick::image_read(path), tidy = TRUE)
    tidy$col[tidy$x == 50L & tidy$y == 8L]
  }
  undilated <- retro_ocr_clean_image(gray, binary, 0.15, dilate = 0)
  dilated <- retro_ocr_clean_image(gray, binary, 0.15, dilate = 2)
  on.exit(unlink(c(undilated, dilated)), add = TRUE)
  expect_equal(fringe(undilated), "#000000ff")
  expect_equal(fringe(dilated), "#ffffffff")
})

test_that("retro_ocr_clean_image() drops light gridlines that quantize to background", {
  # A short, light gridline: too small to trip span_threshold, so the
  # component filter leaves it alone, but light enough to quantize to
  # background. The allow list is what removes it. A dark digit-sized block
  # quantizes to foreground and must survive.
  raster <- matrix("#ffffff", nrow = 40L, ncol = 40L)
  raster[10L, 12L:16L] <- "#d0d0d0"
  raster[20L:23L, 20L:23L] <- "#000000"
  input <- retro_test_png(raster)
  gray <- tempfile(fileext = ".png")
  retro_image_grayscale(input, gray)
  binary <- retro_ocr_quantize(input)
  on.exit(unlink(c(gray, binary)))
  # Confirm the premise: the gridline is not part of the quantized
  # foreground, so only an allow list can remove it.
  quantized_mask <- retro_components_foreground_mask(
    magick::image_read(binary)
  )
  expect_false(quantized_mask[10L, 14L])
  expect_true(quantized_mask[21L, 21L])
  cleaned <- retro_ocr_clean_image(gray, binary, 0.15, dilate = 0)
  on.exit(unlink(cleaned), add = TRUE)
  tidy <- magick::image_raster(magick::image_read(cleaned), tidy = TRUE)
  pixel <- function(x, y) tidy$col[tidy$x == x & tidy$y == y]
  expect_equal(pixel(14L, 10L), "#ffffffff")
  expect_equal(pixel(21L, 21L), "#000000ff")
})

# --- retro_ocr_dilate --------------------------------------------------------

test_that("retro_ocr_dilate() grows a mask and respects radius 0", {
  mask <- matrix(FALSE, nrow = 9L, ncol = 9L)
  mask[5L, 5L] <- TRUE
  # Dark foreground on light background: the polarity of a real plot, where
  # ImageMagick's "Erode" is what grows the masked region.
  grown <- retro_ocr_dilate(
    mask,
    radius = 2L,
    foreground = "#000000",
    background = "#ffffff"
  )
  expect_equal(sum(grown), 25L)
  expect_equal(range(which(apply(grown, 1L, any))), c(3L, 7L))
  expect_equal(range(which(apply(grown, 2L, any))), c(3L, 7L))
  expect_equal(retro_ocr_dilate(mask, 0L, "#000000", "#ffffff"), mask)
})

test_that("retro_ocr_dilate() grows a mask for light-on-dark polarity too", {
  mask <- matrix(FALSE, nrow = 9L, ncol = 9L)
  mask[5L, 5L] <- TRUE
  grown <- retro_ocr_dilate(
    mask,
    radius = 1L,
    foreground = "#ffffff",
    background = "#000000"
  )
  expect_equal(sum(grown), 9L)
})

test_that("retro_ocr_oversized_components() flags components by row or column span", {
  # Component 1: rows 1-2, cols 1-1 (row span 2). Component 2: rows 5-5,
  # cols 5-14 (col span 10).
  membership <- c(1L, 1L, 2L, 2L)
  rows <- c(1L, 2L, 5L, 5L)
  cols <- c(1L, 1L, 5L, 14L)
  result <- retro_ocr_oversized_components(
    membership,
    rows,
    cols,
    max_height = 5L,
    max_width = 5L
  )
  expect_equal(result, 2L)
})

# --- retro_ocr_is_positive_numeric_word --------------------------------------

test_that("retro_ocr_is_positive_numeric_word() flags positive numbers only", {
  word <- c("12", "3.5", "n=10", "-4", "")
  expect_equal(
    retro_ocr_is_positive_numeric_word(word),
    c(TRUE, TRUE, FALSE, FALSE, FALSE)
  )
})

# --- retro_ocr_pool_findings --------------------------------------------------

test_that("retro_ocr_pool_findings() keeps findings with non-overlapping boxes", {
  mode_a <- retro_test_finding("12", 80, 1L, 1L, 5L, 5L)
  mode_b <- retro_test_finding("50", 70, 20L, 20L, 25L, 25L)
  pooled <- retro_ocr_pool_findings(list(mode_a, mode_b))
  expect_equal(nrow(pooled), 2L)
  expect_setequal(pooled$word, c("12", "50"))
})

test_that("retro_ocr_pool_findings() keeps the higher-confidence overlapping finding", {
  mode_a <- retro_test_finding("12", 80, 1L, 1L, 5L, 5L)
  mode_b <- retro_test_finding("12", 90, 2L, 2L, 6L, 6L)
  pooled <- retro_ocr_pool_findings(list(mode_a, mode_b))
  expect_equal(nrow(pooled), 1L)
  expect_equal(pooled$confidence, 90)
})

test_that("retro_ocr_pool_findings() does not treat edge-touching boxes as overlapping", {
  # Boxes that share only a boundary pixel (corner-touching, zero-area
  # overlap) are distinct findings, not two passes' readings of the same
  # word - this is the shape rounding noise from magnify descaling takes.
  mode_a <- retro_test_finding("0", 90, 0L, 0L, 2L, 2L)
  mode_b <- retro_test_finding("12", 85, 2L, 2L, 3L, 3L)
  pooled <- retro_ocr_pool_findings(list(mode_a, mode_b))
  expect_equal(nrow(pooled), 2L)
  expect_setequal(pooled$word, c("0", "12"))
})

test_that("retro_ocr_pool_findings() breaks confidence ties by list order", {
  mode_a <- retro_test_finding("12", 80, 1L, 1L, 5L, 5L)
  mode_b <- retro_test_finding("21", 80, 2L, 2L, 6L, 6L)
  pooled <- retro_ocr_pool_findings(list(mode_a, mode_b))
  expect_equal(nrow(pooled), 1L)
  expect_equal(pooled$word, "12")
})

test_that("retro_ocr_letters() generates correct sequences", {
  expect_equal(retro_ocr_letters(0L), character(0L))
  expect_equal(retro_ocr_letters(3L), c("A", "B", "C"))
  expect_equal(retro_ocr_letters(25L)[25L], "Z")
  expect_equal(retro_ocr_letters(26L)[26L], "AA")
  expect_equal(retro_ocr_letters(27L)[27L], "AB")
  expect_equal(retro_ocr_letters(50L)[50L], "AZ")
  expect_equal(retro_ocr_letters(51L)[51L], "BA")
})

test_that("retro_ocr_letters() never emits Q", {
  expect_false(any(grepl("Q", retro_ocr_letters(200L), fixed = TRUE)))
})
