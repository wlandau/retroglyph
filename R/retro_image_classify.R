#' @title Classify an image
#' @keywords internal
#' @noRd
#' @description Map every pixel in an image to the nearest color in a
#'   supplied palette of background plus series colors. Optionally, pixels
#'   that map to garbage colors (e.g. confidence bands, annotations) are
#'   set to background in the final output.
#' @return `NULL` (invisibly). Called for its side effect of writing
#'   an image file.
#' @param input Character scalar, path to the source image file.
#' @param output Character scalar, path where the classified image
#'   will be written.
#' @param background Character scalar, hex color for the background.
#' @param garbage Character vector of hex color values for non-data
#'   elements such as confidence bands or annotations. Pixels that map
#'   to these colors are set to background in the final image. Sits next to
#'   `background` because it ends up as background.
#' @param series Character vector of hex color values for the foreground
#'   curves, as identified by the visual language model. Must not contain
#'   `background`.
#' @examples
#'   input <- system.file("simulation.png", package = "retroglyph")
#'   output <- tempfile(fileext = ".png")
#'   retroglyph:::retro_image_classify(
#'     input = input,
#'     output = output,
#'     background = "#FFFFFF",
#'     series = c("#DC3030", "#3030A0")
#'   )
#'   if (interactive()) {
#'     browseURL(output)
#'   }
retro_image_classify <- function(
  input,
  output,
  background,
  garbage = character(0L),
  series
) {
  stopifnot(
    "input must be a single string" = is.character(input) &&
      length(input) == 1L,
    "input must exist" = file.exists(input),
    "output must be a single string" = is.character(output) &&
      length(output) == 1L,
    "background must be a single string" = is.character(background) &&
      length(background) == 1L,
    "series must be a character vector" = is.character(series) &&
      length(series) >= 1L,
    "garbage must be a character vector" = is.character(garbage),
    "series, garbage, and background must all be unique" = !anyDuplicated(c(
      background,
      series,
      garbage
    ))
  )
  # Build full palette: background + series colors + garbage colors
  palette <- c(background, series, garbage)
  # Create a reference image from the palette for image_map
  palette_image <- magick::image_read(
    matrix(palette, nrow = 1L)
  )
  # Map every pixel to the nearest palette color
  original <- magick::image_read(input) |>
    magick::image_convert(type = "TrueColor")
  mapped <- magick::image_map(original, map = palette_image, dither = FALSE)
  # Replace garbage colors with background
  if (length(garbage) > 0L) {
    raster <- magick::image_raster(mapped, tidy = FALSE)
    matrix <- as.matrix(raster)
    garbage_rgba <- retro_color_rgba(garbage)
    background_rgba <- retro_color_rgba(background)
    for (color in garbage_rgba) {
      matrix[matrix == color] <- background_rgba
    }
    mapped <- magick::image_read(matrix)
  }
  magick::image_write(mapped, path = output)
  invisible(NULL)
}
