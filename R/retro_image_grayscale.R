#' @title Convert image to grayscale
#' @keywords internal
#' @noRd
#' @description Convert the image to grayscale, preserving anti-aliasing
#'   and gray gradients for legibility. The result has a whitish background
#'   and dark text without the harshness of strict 2-color quantization.
#'   This is what OCR runs on and what the annotated output is drawn on in
#'   [retro_do_label()] - never the strictly quantized copy built internally
#'   by [retro_data_ocr()], which exists only to build the component mask.
#' @param input Character scalar, path to an image file.
#' @param output Character scalar, path where the grayscale image will be
#'   written.
#' @return `NULL` (invisibly). Called for its side effect of writing an
#'   image file.
#' @examples
#'   input <- system.file("simulation.png", package = "retroglyph")
#'   output <- tempfile(fileext = ".png")
#'   retroglyph:::retro_image_grayscale(input, output)
#'   source_info <- magick::image_info(magick::image_read(input))
#'   gray_info <- magick::image_info(magick::image_read(output))
#'   print(gray_info$colorspace)
#'   # Same pixel dimensions as the source - only the colorspace changes.
#'   print(identical(
#'     c(source_info$width, source_info$height),
#'     c(gray_info$width, gray_info$height)
#'   ))
retro_image_grayscale <- function(input, output) {
  stopifnot(
    "input must be a single string" = is.character(input) &&
      length(input) == 1L,
    "input must exist" = file.exists(input),
    "output must be a single string" = is.character(output) &&
      length(output) == 1L
  )
  image <- magick::image_read(input)
  image <- magick::image_convert(image, colorspace = "Gray")
  magick::image_write(image, path = output)
  invisible(NULL)
}
