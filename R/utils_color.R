#' @title Convert colors to 6-character RGB format
#' @keywords internal
#' @noRd
#' @description Strips the transparency component from 8-character RGBA hex
#'   strings (e.g. `"#dc3030ff"` → `"#dc3030"`). Also lowercases the
#'   result for consistency. Input may be 6-character (returned as-is
#'   after lowercasing) or 8-character.
#' @details Parses each color and re-formats it rather than slicing the
#'   string, because the input is not guaranteed to be hex.
#'   `magick::image_raster()` returns color *specifications*, and for a
#'   fully transparent pixel it returns the name `"transparent"`. Slicing
#'   that to 7 characters produced `"transpa"`, an invalid color name that
#'   stopped the pipeline downstream. Parsing accepts anything
#'   `grDevices::col2rgb()` understands and always yields valid hex.
#' @param x Character vector of colors, in any specification
#'   `grDevices::col2rgb()` accepts (hex or a `grDevices::colors()` name).
#' @return Character vector of 7-character hex strings (`#rrggbb`),
#'   lowercased.
#' @examples
#'   retro_color_rgb(c("#DC3030FF", "#3030a0ff", "#FFFFFF"))
retro_color_rgb <- function(x) {
  retro_color_format(x, alpha = FALSE)
}

#' @title Convert colors to 8-character RGBA format
#' @keywords internal
#' @noRd
#' @description Appends `"ff"` (full opacity) to 6-character RGB hex
#'   strings (e.g. `"#dc3030"` → `"#dc3030ff"`). Also lowercases the
#'   result for consistency. Input may be 8-character (returned as-is
#'   after lowercasing) or 6-character.
#' @details Parses rather than concatenates, for the reason given in
#'   [retro_color_rgb()]: the input may be a color name, and appending
#'   `"ff"` to `"transparent"` produced the invalid `"transpaff"`. An
#'   alpha channel already present is preserved.
#' @param x Character vector of colors, in any specification
#'   `grDevices::col2rgb()` accepts (hex or a `grDevices::colors()` name).
#' @return Character vector of 9-character hex strings (`#rrggbbaa`),
#'   lowercased.
#' @examples
#'   retro_color_rgba(c("#DC3030", "#3030a0", "#ffffffff"))
retro_color_rgba <- function(x) {
  retro_color_format(x, alpha = TRUE)
}

#' @title Parse colors and re-format them as hex
#' @keywords internal
#' @noRd
#' @description Shared implementation of [retro_color_rgb()] and
#'   [retro_color_rgba()]. Converts each color to channel values with
#'   `grDevices::col2rgb()` and back to hex with `grDevices::rgb()`.
#' @details `grDevices::col2rgb()` returns a matrix with one column per
#'   color and one row per channel, so `grDevices::rgb()` reassembles the
#'   colors by taking those rows. The two cases call `grDevices::rgb()`
#'   separately rather than passing a conditional `alpha` argument: the
#'   way to ask `grDevices::rgb()` for an opaque color is to omit `alpha`
#'   altogether, and spelling both calls out states that plainly instead
#'   of relying on `NULL` to mean "omitted".
#'
#'   `maxColorValue = 255` matches `grDevices::col2rgb()`, which always
#'   returns integers on 0-255 whatever the width of the input
#'   specification. It is the scale of the values coming in, not a limit on
#'   the precision going out.
#' @param x Character vector or matrix of colors.
#' @param alpha `TRUE` to keep the transparency channel, `FALSE` to drop it.
#' @return Character vector of hex colors, lowercased, the same length as
#'   `x`. Always a plain vector, even when `x` is a matrix: the callers
#'   that pass a pixel matrix assign the result back with `matrix[] <- `,
#'   which fills the matrix in place and keeps its dimensions.
#' @examples
#'   retroglyph:::retro_color_format("transparent", alpha = FALSE)
retro_color_format <- function(x, alpha) {
  stopifnot("alpha must be TRUE or FALSE" = isTRUE(alpha) || isFALSE(alpha))
  channels <- grDevices::col2rgb(x, alpha = alpha)
  hex <- if (alpha) {
    grDevices::rgb(
      red = channels["red", ],
      green = channels["green", ],
      blue = channels["blue", ],
      alpha = channels["alpha", ],
      maxColorValue = 255L
    )
  } else {
    grDevices::rgb(
      red = channels["red", ],
      green = channels["green", ],
      blue = channels["blue", ],
      maxColorValue = 255L
    )
  }
  tolower(hex)
}

#' @title Test whether strings are valid hex colors
#' @keywords internal
#' @noRd
#' @description Test whether each element is a valid RGB or RGBA hex color:
#'   a `"#"` followed by exactly 6 or 8 hexadecimal digits.
#' @param x Character vector of candidate hex color strings.
#' @return Logical vector, `TRUE` for each valid hex color.
#' @examples
#'   retro_color_valid(c("#DC3030", "#3030a0ff", "red", "#12"))
retro_color_valid <- function(x) {
  grepl("^#([0-9a-fA-F]{6}|[0-9a-fA-F]{8})$", x)
}

#' @title Count image pixels matching each of a set of colors
#' @keywords internal
#' @noRd
#' @description Tally every pixel of an image and report how many match
#'   each color in a supplied vector.
#' @param image Character vector of length 1, path to a PNG image file.
#' @param colors Character vector of hex color strings to count.
#' @return Integer vector, the same length and order as `colors`, giving
#'   the number of pixels in `image` that match each color. Colors with
#'   no matching pixels count as `0`.
#' @examples
#'   image <- system.file("simulation.png", package = "retroglyph")
#'   retro_color_count(image, c("#ffffff", "#000000"))
retro_color_count <- function(image, colors) {
  counts <- magick::image_read(image) |>
    magick::image_raster(tidy = FALSE) |>
    retro_color_rgb() |>
    table()
  colors <- retro_color_rgb(colors)
  result <- unname(as.integer(counts[colors]))
  result[is.na(result)] <- 0L
  result
}

#' @title Find the nearest built-in R color name for a hex color
#' @keywords internal
#' @noRd
#' @description Map each hex color to the closest named color in
#'   `grDevices::colors()`, by Euclidean distance in RGB space.
#' @param x Character vector of hex color strings (6- or 8-character;
#'   any alpha channel is ignored).
#' @return Character vector, the same length as `x`, giving the name of
#'   the nearest `grDevices::colors()` entry for each input.
#' @examples
#'   retro_color_name(c("#dc3030", "#3030a0"))
retro_color_name <- function(x) {
  target <- grDevices::col2rgb(retro_color_rgb(x))
  palette <- grDevices::col2rgb(grDevices::colors())
  vapply(
    seq_len(ncol(target)),
    function(i) {
      distances <- colSums((palette - target[, i])^2)
      grDevices::colors()[which.min(distances)]
    },
    character(1L)
  )
}
