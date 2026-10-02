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
#'   Flattening is what makes a partially transparent source image
#'   reconstructable. A curve drawn at partial opacity - common where
#'   confidence bands or overplotted series are involved - is a blend of
#'   the curve color and whatever sits behind it, and the blend is the
#'   color a reader actually sees. Compositing onto white records that
#'   blend. Merely dropping the alpha channel instead would record the
#'   undiluted curve color, which never appeared in the figure, and the
#'   color-keyed series matching in [retro_do_distill()] would key off a
#'   value that is not there. Flattening also gives a fully transparent
#'   pixel the same value as the white background it is drawn over, so
#'   background detection counts it as background rather than as a
#'   distinct curve color.
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
