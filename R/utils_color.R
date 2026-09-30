#' @title Convert hex colors to 6-character RGB format
#' @keywords internal
#' @noRd
#' @description Strips the transparency component from 8-character RGBA hex
#'   strings (e.g. `"#dc3030ff"` → `"#dc3030"`). Also lowercases the
#'   result for consistency. Input may be 6-character (returned as-is
#'   after lowercasing) or 8-character.
#' @param x Character vector of hex color strings.
#' @return Character vector of 7-character hex strings (`#rrggbb`),
#'   lowercased.
#' @examples
#'   retro_color_rgb(c("#DC3030FF", "#3030a0ff", "#FFFFFF"))
retro_color_rgb <- function(x) {
  tolower(substr(x, 1L, 7L))
}

#' @title Convert hex colors to 8-character RGBA format
#' @keywords internal
#' @noRd
#' @description Appends `"ff"` (full opacity) to 6-character RGB hex
#'   strings (e.g. `"#dc3030"` → `"#dc3030ff"`). Also lowercases the
#'   result for consistency. Input may be 8-character (returned as-is
#'   after lowercasing) or 6-character.
#' @param x Character vector of hex color strings.
#' @return Character vector of 9-character hex strings (`#rrggbbaa`),
#'   lowercased.
#' @examples
#'   retro_color_rgba(c("#DC3030", "#3030a0", "#ffffffff"))
retro_color_rgba <- function(x) {
  x <- tolower(x)
  needs_alpha <- nchar(x) == 7L
  x[needs_alpha] <- paste0(x[needs_alpha], "ff")
  x
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
