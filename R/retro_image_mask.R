#' @title Mask regions of an image
#' @keywords internal
#' @noRd
#' @description Apply a sequence of rectangular masks to keep or remove
#'   pixel regions, setting masked-out pixels to the background color.
#'   The background color is detected automatically as the most frequent
#'   color in the input image.
#' @details Coordinates use the PNG pixel coordinate system:
#'   the origin is the top-left corner, x increases left to right
#'   (columns), and y increases top to bottom (rows).
#'   Mask operations are applied sequentially in the order given.
#' @return `NULL`, invisibly.
#' @param input Character scalar, path to the input image file
#'   (typically a quantized image). Not modified.
#' @param output Character scalar, path where the masked image
#'   will be written.
#' @param x1 Integer vector, left column of each rectangle
#'   (1 to image width).
#' @param x2 Integer vector, right column of each rectangle
#'   (1 to image width). Must be >= `x1` element-wise.
#' @param y1 Integer vector, top row of each rectangle
#'   (1 to image height).
#' @param y2 Integer vector, bottom row of each rectangle
#'   (1 to image height). Must be >= `y1` element-wise.
#' @param mask Logical vector, same length as `x1`.
#'   `TRUE` means keep pixels in the region.
#'   `FALSE` means remove pixels in the region (set to background).
#' @param initial Logical scalar. The starting state of all pixels.
#'   `TRUE` (default) starts with all pixels kept, then `mask = FALSE`
#'   entries carve away regions. `FALSE` starts with all pixels as
#'   background, then `mask = TRUE` entries add regions back.
#' @param background Character scalar hex color to use as the background,
#'   or `NULL` (default) to detect it automatically as the most frequent
#'   color in the input image. Mainly useful for testing.
#' @examples
#'   input <- system.file("simulation.png", package = "retroglyph")
#'   output <- tempfile(fileext = ".png")
#'   retroglyph:::retro_image_mask(
#'     input = input,
#'     output = output,
#'     x1 = 1L,
#'     x2 = 600L,
#'     y1 = 13L,
#'     y2 = 120L,
#'     mask = TRUE,
#'     initial = FALSE
#'   )
retro_image_mask <- function(
  input,
  output,
  x1,
  x2,
  y1,
  y2,
  mask,
  initial = TRUE,
  background = NULL
) {
  stopifnot(
    "input must be a single string" = is.character(input) &&
      length(input) == 1L,
    "input must exist" = file.exists(input),
    "output must be a single string" = is.character(output) &&
      length(output) == 1L,
    "initial must be TRUE or FALSE" = isTRUE(initial) || isFALSE(initial),
    "x1 must be numeric" = is.numeric(x1),
    "x2 must be numeric" = is.numeric(x2),
    "y1 must be numeric" = is.numeric(y1),
    "y2 must be numeric" = is.numeric(y2),
    "mask must be logical" = is.logical(mask),
    "x1, x2, y1, y2, and mask must be the same length" = length(x1) ==
      length(x2) &&
      length(x1) == length(y1) &&
      length(x1) == length(y2) &&
      length(x1) == length(mask),
    "x1 must not contain missing values" = !anyNA(x1),
    "x2 must not contain missing values" = !anyNA(x2),
    "y1 must not contain missing values" = !anyNA(y1),
    "y2 must not contain missing values" = !anyNA(y2),
    "mask must not contain missing values" = !anyNA(mask),
    "x1 must be integerish" = all(x1 == as.integer(x1)),
    "x2 must be integerish" = all(x2 == as.integer(x2)),
    "y1 must be integerish" = all(y1 == as.integer(y1)),
    "y2 must be integerish" = all(y2 == as.integer(y2))
  )
  x1 <- as.integer(x1)
  x2 <- as.integer(x2)
  y1 <- as.integer(y1)
  y2 <- as.integer(y2)
  image <- magick::image_read(input)
  info <- magick::image_info(image)
  width <- info$width
  height <- info$height
  stopifnot(
    "x1 values must be >= 1" = all(x1 >= 1L),
    "x2 values must be <= image width" = all(x2 <= width),
    "y1 values must be >= 1" = all(y1 >= 1L),
    "y2 values must be <= image height" = all(y2 <= height),
    "x1 must be <= x2" = all(x1 <= x2),
    "y1 must be <= y2" = all(y1 <= y2)
  )
  raster <- magick::image_raster(image, tidy = FALSE)
  background <- background %||% retro_background_color(raster, quantize = TRUE)
  # Convert to RGBA for raster assignment (magick rasters use 8-char hex)
  background_rgba <- retro_color_rgba(background)
  # raster is a character matrix: rows = pixel rows (top to bottom),
  # cols = pixel columns (left to right).
  # x1/x2 are columns, y1/y2 are rows (top-left origin).
  keep <- matrix(initial, nrow = height, ncol = width)
  for (i in seq_along(mask)) {
    rows <- seq.int(from = y1[i], to = y2[i], by = 1L)
    cols <- seq.int(from = x1[i], to = x2[i], by = 1L)
    keep[rows, cols] <- mask[i]
  }
  raster[!keep] <- background_rgba
  magick::image_read(raster) |>
    magick::image_write(path = output)
  invisible(NULL)
}

#' @title Detect the background color of an image
#' @keywords internal
#' @noRd
#' @description Quantizes to 2 colors (no dithering) to collapse shading
#'   variation, then returns the most frequent of the two as the background.
#'   This avoids misidentifying the background when it has many similar shades
#'   that individually are less frequent than a uniform foreground color.
#' @param raster A native raster matrix from
#'   `magick::image_raster(tidy = FALSE)`.
#' @param quantize TRUE to perform 2-color quantization before selecting
#'   background, `FALSE` to omit quantization.
#' @return A hex color string.
#' @examples
#'   pixels <- matrix("#ffffff", nrow = 4, ncol = 4)
#'   pixels[2, 2] <- "#ff0000"
#'   image <- magick::image_read(pixels)
#'   raster <- magick::image_raster(image, tidy = FALSE)
#'   retroglyph:::retro_background_color(raster, quantize = TRUE)
retro_background_color <- function(raster, quantize = FALSE) {
  if (quantize) {
    raster <- magick::image_read(raster) |>
      magick::image_quantize(max = 2L, dither = FALSE) |>
      magick::image_raster(tidy = FALSE)
  }
  colors <- sort(table(raster), decreasing = TRUE)
  retro_color_rgb(names(colors)[1L])
}
