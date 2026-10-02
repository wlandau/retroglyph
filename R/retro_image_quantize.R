#' @title Quantize an image to a reduced color palette
#' @keywords internal
#' @noRd
#' @description Reduce the number of distinct colors in an image to at most
#'   `n_colors` using `magick::image_quantize()` with dithering disabled.
#'   The output is a faithful rendering of the input with fewer, flatter
#'   colors, which makes it easier for the model to read colors by eye than
#'   the full-color original.
#' @details Drops any alpha channel first, via [retro_color_opaque()],
#'   which explains why `magick::image_quantize()` needs an opaque input
#'   to behave the same way on every `ImageMagick` build. This previously
#'   relied on `magick::image_convert(type = "TrueColor")`, which also
#'   removes the channel but as a side effect of forcing the colorspace;
#'   naming the intent keeps every quantization in the package using one
#'   mechanism.
#' @return `NULL` (invisibly). Called for its side effect of writing
#'   an image file.
#' @param input Character scalar, path to the source image file.
#' @param output Character scalar, path where the quantized image
#'   will be written.
#' @param n_colors Integer scalar, maximum number of distinct colors to
#'   retain.
#' @examples
#'   input <- system.file("simulation.png", package = "retroglyph")
#'   output <- tempfile(fileext = ".png")
#'   retroglyph:::retro_image_quantize(input = input, output = output)
#'   if (interactive()) {
#'     browseURL(output)
#'   }
retro_image_quantize <- function(input, output, n_colors = 256L) {
  stopifnot(
    "Source image is missing. Please provide a source image." = is.character(
      input
    ) &&
      length(input) == 1L,
    "input must exist" = file.exists(input),
    "output must be a single string" = is.character(output) &&
      length(output) == 1L,
    "n_colors must be a single positive integer" = is.numeric(n_colors) &&
      length(n_colors) == 1L &&
      !is.na(n_colors) &&
      n_colors >= 1L
  )
  quantized <- magick::image_read(input) |>
    retro_color_opaque() |>
    magick::image_quantize(max = as.integer(n_colors), dither = FALSE)
  magick::image_write(quantized, path = output)
  invisible(NULL)
}
