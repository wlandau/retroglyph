#' @title Convert an image to PNG
#' @keywords internal
#' @noRd
#' @description Read an image in any format that `magick` supports (such as
#'   SVG or JPEG) and write it out as an opaque PNG raster. Pixels and
#'   colors pass through unchanged, so an opaque background is never
#'   altered. Transparency is the one exception: it is flattened onto
#'   white.
#' @details Downstream image functions in this package operate on PNG
#'   rasters. This helper normalizes an arbitrary source image to PNG at
#'   the entry point of the pipeline. For vector inputs (such as SVG), the
#'   `density` argument controls the rasterization resolution; it has no
#'   effect on inputs that are already rasters.
#'
#'   Flattening is what makes the rest of the pipeline safe to write in
#'   terms of hex colors. `magick::image_raster()` reports a fully
#'   transparent pixel as the color name `"transparent"` rather than as
#'   hex, and which images carry an alpha channel varies by `ImageMagick`
#'   version. Removing the alpha channel once, here, means no downstream
#'   step ever sees a non-hex pixel value. Converting the colorspace does
#'   not help; only dropping the alpha channel does. Flattening also gives
#'   a transparent pixel the same value as the white background it is
#'   drawn over, so background detection counts it as background rather
#'   than as a distinct curve color.
#' @return `NULL` (invisibly). Called for its side effect of writing
#'   an image file.
#' @param input Character scalar, path to the source image file.
#' @param output Character scalar, path where the PNG image will be written.
#' @param density Numeric scalar, resolution in dots per inch used to
#'   rasterize vector inputs such as SVG. Ignored for raster inputs.
#' @examples
#'   input <- system.file("simulation.png", package = "retroglyph")
#'   output <- tempfile(fileext = ".png")
#'   retroglyph:::retro_image_png(input = input, output = output)
retro_image_png <- function(input, output, density = 300) {
  stopifnot(
    "input must be a single string" = is.character(input) &&
      length(input) == 1L,
    "input must exist" = file.exists(input),
    "output must be a single string" = is.character(output) &&
      length(output) == 1L
  )
  magick::image_read(input, density = density) |>
    magick::image_background("white", flatten = TRUE) |>
    magick::image_write(path = output, format = "png")
  invisible(NULL)
}
