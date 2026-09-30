#' @title Clean non-data artifacts from an image
#' @keywords internal
#' @noRd
#' @description Remove legend fragments, text residue, and other non-curve
#'   artifacts from a panel image using connected-component analysis.
#'   Only foreground components whose diagonal extent exceeds a fraction
#'   of the plotting panel's diagonal are retained — these are the real
#'   Kaplan-Meier curves. Everything else (legend keys, stray marks, text
#'   fragments) is set to the background color.
#' @details
#'   The algorithm:
#'
#'   1. Quantizes the image to two colors (no dithering) to produce a
#'      clean foreground/background separation. Colors that are close
#'      to the background (e.g. light grays, faint grid lines) collapse
#'      into the background, while all distinct curve colors collapse
#'      into the foreground.
#'   2. Labels 4-connected components in the foreground.
#'   3. Computes the diagonal extent of each component's pixel bounding
#'      box (the Euclidean length of its row span and column span taken
#'      together).
#'   4. Keeps only components whose diagonal extent is at least
#'      `span_threshold` times the plotting panel's own diagonal
#'      (from `panel`, as detected by [retro_data_panel()]).
#'   5. Sets all other foreground pixels to background in the
#'      **original** (non-quantized) image.
#'
#'   This exploits the structural property that Kaplan-Meier curves
#'   traverse most of the plotting panel (they represent survival, or
#'   cumulative incidence, over time), while legend keys, text, and
#'   artifacts cover only a small corner of it. Measuring the diagonal
#'   rather than only the horizontal span also catches curves that drop
#'   or rise steeply enough to trace a genuinely diagonal path across the
#'   panel; a bounding-box diagonal is direction-agnostic, so a decreasing
#'   survival curve and an increasing cumulative-incidence curve are
#'   measured the same way, with no special-casing.
#' @return `NULL` (invisibly). Called for its side effect of writing
#'   an image file.
#' @param input Character scalar, path to the panel image file.
#' @param output Character scalar, path where the cleaned image
#'   will be written.
#' @param panel Single-row data frame with numeric columns `x1`, `x2`,
#'   `y1`, `y2`, the plotting panel's bounding box as returned by
#'   [retro_data_panel()]. The unpadded edges are used directly - the
#'   `pad_x1`/`pad_x2`/`pad_y1`/`pad_y2` columns, if present, are ignored.
#' @param span_threshold Numeric scalar between 0 and 1, the minimum
#'   diagonal extent (as a fraction of the panel's own diagonal) for a
#'   connected component to be retained. The threshold is
#'   deliberately permissive: a curve truncated at an occlusion, or one
#'   belonging to an arm that ran out of patients early, can legitimately
#'   cover well under half the panel, and losing a real curve costs more
#'   than keeping a wide artifact. Path-finding tolerates leftover debris;
#'   it cannot recover a curve that cleaning deleted.
#' @examples
#'   source <- system.file(
#'     "simulation",
#'     "simulation.png",
#'     package = "retroglyph"
#'   )
#'   panel_image <- tempfile(fileext = ".png")
#'   classified <- tempfile(fileext = ".png")
#'   cleaned <- tempfile(fileext = ".png")
#'   x_axis <- tibble::tibble(
#'     label = c("F", "M"), value = c(0, 35),
#'     x = c(376, 1154.5), y = c(570, 570),
#'     x1 = c(370L, 1143L), x2 = c(382L, 1166L),
#'     y1 = c(561L, 561L), y2 = c(579L, 579L)
#'   )
#'   y_axis <- tibble::tibble(
#'     label = c("A", "E"), value = c(0.8, 0.2),
#'     x = c(340, 339), y = c(116, 427),
#'     x1 = c(324L, 324L), x2 = c(355L, 354L),
#'     y1 = c(107L, 418L), y2 = c(126L, 436L)
#'   )
#'   axes <- retroglyph:::retro_data_panel(
#'     input = source,
#'     x_axis = x_axis,
#'     y_axis = y_axis
#'   )
#'   retroglyph:::retro_image_mask(
#'     input = source,
#'     output = panel_image,
#'     x1 = axes$x1,
#'     x2 = axes$x2,
#'     y1 = axes$y1,
#'     y2 = axes$y2,
#'     mask = TRUE,
#'     initial = FALSE
#'   )
#'   retroglyph:::retro_image_classify(
#'     input = panel_image,
#'     output = classified,
#'     background = "#FFFFFF",
#'     series = c("#DC3030", "#3030A0")
#'   )
#'   retroglyph:::retro_image_clean(
#'     input = classified,
#'     output = cleaned,
#'     panel = axes
#'   )
#'   if (interactive()) {
#'     browseURL(cleaned)
#'   }
retro_image_clean <- function(
  input,
  output,
  panel,
  span_threshold = 0.3
) {
  stopifnot(
    "input must be a single string" = is.character(input) &&
      length(input) == 1L,
    "input must exist" = file.exists(input),
    "output must be a single string" = is.character(output) &&
      length(output) == 1L,
    "panel must be a data frame with one row" = is.data.frame(panel) &&
      nrow(panel) == 1L,
    "panel must have numeric x1, x2, y1, y2 columns" = all(
      c("x1", "x2", "y1", "y2") %in% names(panel)
    ) &&
      is.numeric(panel$x1) &&
      is.numeric(panel$x2) &&
      is.numeric(panel$y1) &&
      is.numeric(panel$y2),
    "span_threshold must be a single number" = is.numeric(span_threshold) &&
      length(span_threshold) == 1L,
    "span_threshold must be in (0, 1]" = span_threshold > 0 &&
      span_threshold <= 1
  )
  image <- magick::image_read(input)
  original_pixels <- retro_components_pixel_matrix(image)
  foreground_mask <- retro_components_foreground_mask(image)
  panel_width <- panel$x2 - panel$x1 + 1
  panel_height <- panel$y2 - panel$y1 + 1
  panel_diagonal <- sqrt(panel_width^2 + panel_height^2)
  min_span <- span_threshold * panel_diagonal
  kept_mask <- retro_clean_filter_components(foreground_mask, min_span)
  background <- retro_components_background_color(
    foreground_mask,
    original_pixels
  )
  original_pixels[foreground_mask & !kept_mask] <- background
  output_image <- magick::image_read(original_pixels)
  magick::image_write(output_image, path = output)
  invisible(NULL)
}

#' @title Filter foreground components by diagonal span
#' @keywords internal
#' @noRd
#' @description Labels 4-connected components in a foreground mask and
#'   returns a mask retaining only those whose diagonal span meets
#'   the minimum threshold.
#' @param foreground_mask Logical matrix (height x width).
#' @param min_span Numeric scalar, minimum diagonal span in pixels.
#' @return Logical matrix (same dimensions), `TRUE` for pixels in
#'   retained components.
#' @examples
#'   # A 5x10 mask: a "long" component spanning columns 1-9 on row 3
#'   # (standing in for a real Kaplan-Meier curve), and a "short"
#'   # component spanning only columns 5-6 on row 1 (a stray artifact).
#'   foreground_mask <- matrix(FALSE, nrow = 5, ncol = 10)
#'   foreground_mask[3, 1:9] <- TRUE
#'   foreground_mask[1, 5:6] <- TRUE
#'   result <- retroglyph:::retro_clean_filter_components(
#'     foreground_mask, min_span = 8
#'   )
#'   print(result)
#'   # Only the long component (row 3, diagonal span 9) meets the
#'   # min_span = 8 threshold; the short component (row 1, diagonal
#'   # span 2) is dropped.
retro_clean_filter_components <- function(foreground_mask, min_span) {
  labeled <- retro_components_label(foreground_mask)
  if (length(labeled$foreground_indices) == 0L) {
    return(foreground_mask)
  }
  height <- nrow(foreground_mask)
  width <- ncol(foreground_mask)
  rows <- ((labeled$foreground_indices - 1L) %% height) + 1L
  cols <- ((labeled$foreground_indices - 1L) %/% height) + 1L
  retained_ids <- retro_clean_span_components(
    labeled$membership,
    rows,
    cols,
    min_span
  )
  kept_vertices <- labeled$membership %in% retained_ids
  result <- matrix(FALSE, nrow = height, ncol = width)
  result[labeled$foreground_indices[kept_vertices]] <- TRUE
  result
}

#' @title Identify components with sufficient diagonal span
#' @keywords internal
#' @noRd
#' @description Given component membership and pixel positions, returns
#'   the IDs of components whose diagonal extent meets the threshold. The
#'   diagonal extent of a component is the Euclidean length of its pixel
#'   bounding box: the row span and column span combined, rather than
#'   either alone. This is direction-agnostic, so a component that runs
#'   mostly horizontally, mostly vertically, or diagonally between the
#'   two, is measured the same way.
#' @param membership Integer vector of component IDs (one per vertex).
#' @param rows Integer vector of row positions (one per vertex).
#' @param cols Integer vector of column positions (one per vertex).
#' @param min_span Numeric scalar, minimum diagonal span in pixels.
#' @return Integer vector of component IDs to retain.
#' @examples
#'   # Component 1 spans columns 1-3 and row 1 only (row span 1, column
#'   # span 3, diagonal sqrt(1^2 + 3^2) =~ 3.16); component 2 spans
#'   # columns 7-8 and rows 1-5 (row span 5, column span 2, diagonal
#'   # sqrt(5^2 + 2^2) =~ 5.39).
#'   membership <- c(1L, 1L, 1L, 2L, 2L)
#'   rows <- c(1L, 1L, 1L, 1L, 5L)
#'   cols <- c(1L, 2L, 3L, 7L, 8L)
#'   retroglyph:::retro_clean_span_components(membership, rows, cols, min_span = 4)
#'   # Only component 2 meets the threshold; returns 2.
retro_clean_span_components <- function(membership, rows, cols, min_span) {
  component_ids <- unique(membership)
  is_long <- vapply(
    component_ids,
    function(component_id) {
      in_component <- membership == component_id
      row_span <- max(rows[in_component]) - min(rows[in_component]) + 1L
      col_span <- max(cols[in_component]) - min(cols[in_component]) + 1L
      diagonal <- sqrt(row_span^2 + col_span^2)
      diagonal >= min_span
    },
    logical(1L)
  )
  component_ids[is_long]
}
