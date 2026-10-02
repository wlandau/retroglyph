# TEMPORARY DIAGNOSTIC SCAFFOLDING.
#
# These two tests fail on Linux and pass on Windows/macOS. The failing
# assertion in both is all(result[10:40, 15]), which reports only FALSE
# and so cannot say which of several defects produced it.
#
# stop(deparse(result)) and stop(deparse1(result)) both lose the answer:
# stop() truncates its message at 8190 characters, and deparsing a
# 2500-element logical matrix takes about 17500, so over half is
# discarded and the output cuts off mid-FALSE before reaching anything
# informative. retro_test_mask_summary() run-length encodes the mask
# instead, which fits in tens of characters and survives stop() intact.
#
# Reading the output:
#   n_colors=1          quantization collapsed to one color, so every
#                       pixel matched the detected background and the
#                       mask is all FALSE.
#   n_true=0            same conclusion from the mask side.
#   n_true=31 with runs of single TRUE separated by 49 FALSE
#                       the mask is correct but transposed. (Ruled out
#                       on Windows, which showed one contiguous run of
#                       31, but worth keeping since Linux is untested.)
#   n_true=31 contiguous, runs=FALSE x709,TRUE x31,FALSE x1760
#                       the mask is correct and the fault is elsewhere.
#   n_na>0              some comparison yielded NA rather than FALSE.
#
# Delete this helper and the two stop() calls once Linux has reported.
retro_test_mask_summary <- function(mask, image) {
  runs <- rle(as.vector(mask))
  quantized <- magick::image_quantize(image, max = 2L, dither = FALSE)
  colors <- unique(as.vector(magick::image_raster(quantized, tidy = FALSE)))
  paste0(
    "dim=", paste(dim(mask), collapse = "x"),
    " class=", class(mask)[1L],
    " type=", typeof(mask),
    " n_true=", sum(mask, na.rm = TRUE),
    " n_na=", sum(is.na(mask)),
    " n_colors=", length(colors),
    " colors=", paste(sort(colors), collapse = "/"),
    " runs=", paste0(runs$values, "x", runs$lengths, collapse = ",")
  )
}

test_that("retro_components_foreground_mask() returns a logical matrix", {
  matrix <- matrix("#ffffffff", nrow = 50, ncol = 50)
  matrix[10:40, 15] <- "#000000ff"
  image <- retro_test_image(matrix)
  result <- retro_components_foreground_mask(image)
  stop(retro_test_mask_summary(result, image))
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
  stop(retro_test_mask_summary(result, image))
  expect_true(is.logical(result))
  expect_true(all(result[10:40, 15]))
  expect_false(result[1, 1])
})
