#' @title Turn a string into a filename-friendly slug
#' @keywords internal
#' @noRd
#' @description Lowercase a string and collapse every run of
#'   non-alphanumeric characters into a single underscore, trimming any
#'   leading or trailing underscore.
#' @param x Character vector.
#' @return Character vector, the same length as `x`.
#' @examples
#'   retro_slug(c("Drug 10mg", "Arm A"))
retro_slug <- function(x) {
  x <- gsub("[^a-z0-9]+", "_", tolower(x))
  gsub("^_+|_+$", "", x)
}
