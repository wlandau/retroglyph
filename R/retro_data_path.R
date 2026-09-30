#' @title Find paths through occlusion
#' @keywords internal
#' @noRd
#' @description Find the path for each series through overlapping regions of a
#'   classified Kaplan-Meier image. Where multiple curves occlude each other,
#'   a highest-conductance shortest path reconstructs the hidden portions of
#'   each curve. Returns a tibble of pixel coordinates for every curve,
#'   including both original and reconstructed pixels.
#' @details
#'   This function requires that all curves in the input image use solid
#'   line styles. Dashed or dotted lines produce many small pixel gaps
#'   that are indistinguishable from genuine occlusion gaps, and the
#'   path-finding algorithm would misinterpret them.
#'
#'   Graph traversal is restricted to the pixels that actually
#'   became vertices, so the path can never cut across empty space or skip
#'   ahead of the pixels it is tracing, and it can never enter the
#'   background at all, since background pixels are excluded from the
#'   graph entirely (no vertices, no edges).
#'
#'   Before any of that, each series is scoped to its own 4-connected
#'   component of the foreground mask (see [retro_path_component_mask()]):
#'   the component holding the most pixels of that series' color. Curves
#'   that never touch or overlap anywhere in the image land in separate
#'   components, so this keeps every series' start and end vertices
#'   mutually reachable by construction, with no need for a start point
#'   shared across curves that don't actually share one.
#'
#'   The algorithm then processes each series independently:
#'
#'   1. **Start and end detection.** The start point is the leftmost
#'      column containing any foreground pixel within the series' own
#'      component (the time-axis origin). If the target color does not have
#'      a pixel at that column, the nearest foreground pixel there is used
#'      as a synthetic start. The end point is the rightmost pixel of the
#'      target color, after first discarding small connected components of
#'      that color that are disconnected from the real curve - anti-aliasing
#'      debris that could otherwise be mistaken for the curve's true end
#'      (see [retro_path_endpoints_filter_components()]); ties (a vertical
#'      run at the rightmost column, as from a steep step drop) are broken
#'      by picking whichever tied pixel is farthest from the start point in
#'      the vertical direction.
#'
#'   2. **Conductance surface.** A conductance matrix the size of the
#'      image is built: target-color pixels receive conductance 1
#'      (highest), other foreground (non-background) pixels receive
#'      conductance 0.1, and background pixels are NA.
#'      Because background is NA, those cells are excluded entirely
#'      from the transition graph — no nodes, no edges. The path can
#'      only traverse foreground.
#'
#'   3. **Graph construction.** The conductance surface is converted to a
#'      sparse igraph with 4-directional connectivity (cardinal moves),
#'      matching the horizontal and vertical geometry of Kaplan-Meier step
#'      functions. Only foreground pixels become vertices; background
#'      pixels are excluded entirely. Edge weights are the reciprocal of
#'      mean conductance between adjacent cells.
#'
#'   4. **Single-pass Dijkstra.** `igraph::shortest_paths()` finds the
#'      lowest-cost route from start to end in one pass.
#'      The path preferentially follows target pixels and crosses through
#'      other curves' pixels where occlusion occurs.
#'
#'   5. **Pixel claiming.** Each pixel on the path is over-dilated by the
#'      line width, cropped to the foreground, then skeletonized down to a
#'      thin, centered medial-axis trace (see [retro_path_claim_pixels()])
#'      - the skeleton is returned, not re-dilated back out to the line
#'      width. A vertical step-drop's skeleton spans multiple rows at the
#'      same column, so the drop's full extent is preserved as
#'      reconstructed pixels for this series.
#'
#'   The results from all series are combined into a single
#'   tibble. Each pixel belongs to exactly one curve — the one whose
#'   Dijkstra path claimed it.
#' @return A tibble with columns:
#'   - `x`: integer, pixel x-coordinate (column, 1-based).
#'   - `y`: integer, pixel y-coordinate (row, 1-based, top-down).
#'   - `color`: character, hex color (e.g. `"#dc3030"`).
#'   - `line_width`: integer, the plot's line thickness in pixels, averaged
#'     across every series (see [retro_path_line_width()]); the same value
#'     on every row.
#'   - `width`, `height`: integer, the original image dimensions in pixels;
#'     the same values on every row. Columns rather than attributes, so
#'     they survive `rbind()`/subsetting like any other column.
#'
#'   Each curve's rows are a thin, centered medial-axis trace rather than
#'   a full line-width band, so a vertical step-drop still contributes its
#'   top and bottom pixel, preserving the drop's shape.
#' @param input Character scalar, path to a classified image file
#'   (typically the output of [retro_image_classify()]).
#' @examples
#'   source <- system.file("simulation.png", package = "retroglyph")
#'   panel <- tempfile(fileext = ".png")
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
#'     output = panel,
#'     x1 = axes$x1,
#'     x2 = axes$x2,
#'     y1 = axes$y1,
#'     y2 = axes$y2,
#'     mask = TRUE,
#'     initial = FALSE
#'   )
#'   retroglyph:::retro_image_classify(
#'     input = panel,
#'     output = classified,
#'     background = "#FFFFFF",
#'     series = c("#DC3030", "#3030A0")
#'   )
#'   retroglyph:::retro_image_clean(
#'     input = classified,
#'     output = cleaned,
#'     panel = axes,
#'     span_threshold = 0.4
#'   )
#'   path <- retroglyph:::retro_data_path(cleaned)
#'   print(path)
retro_data_path <- function(input) {
  stopifnot(
    "input must be a single string" = is.character(input) &&
      length(input) == 1L,
    "input must exist" = file.exists(input)
  )
  image <- magick::image_read(input)
  info <- magick::image_info(image)
  width <- info$width
  height <- info$height
  raster <- magick::image_raster(image, tidy = FALSE)
  pixel_matrix <- as.matrix(raster)
  pixel_matrix[] <- retro_color_rgb(pixel_matrix)
  background <- retro_background_color(raster, quantize = FALSE)
  foreground_mask <- pixel_matrix != background
  labeled <- retro_components_label(foreground_mask)
  curve_colors <- setdiff(unique(as.vector(pixel_matrix)), background)
  if (length(curve_colors) == 0L) {
    stop("No Kaplan-Meier curves detected.")
  }
  line_width <- retro_path_line_width(pixel_matrix, background)
  series_results <- lapply(curve_colors, function(target_color) {
    component_mask <- retro_path_component_mask(
      pixel_matrix,
      target_color,
      labeled
    )
    local_pixel_matrix <- pixel_matrix
    local_pixel_matrix[!component_mask] <- background
    retro_path_series(
      pixel_matrix = local_pixel_matrix,
      target_color = target_color,
      background = background,
      foreground_mask = component_mask,
      width = width,
      height = height,
      start_x = min(col(pixel_matrix)[component_mask]),
      line_width = line_width
    )
  })
  result <- do.call(rbind, series_results)
  result$line_width <- line_width
  result$width <- width
  result$height <- height
  tibble::new_tibble(result)
}

#' @title Restrict the foreground mask to the component that owns a color
#' @keywords internal
#' @noRd
#' @description Finds the 4-connected component holding the most pixels of
#'   `target_color` and returns a foreground mask restricted to just that
#'   component. Curves that never touch or overlap land in separate
#'   components; scoping each series to its own component keeps Dijkstra's
#'   start and end vertices always mutually reachable, with no need for a
#'   shared start point across curves that don't share one. If the color
#'   appears in more than one component, the one with the most pixels of
#'   that color wins; the rest are treated as noise for this series. A
#'   tie is broken deterministically (whichever component id sorts
#'   first), not randomly, so this stays a pure function of its input.
#' @param pixel_matrix Character matrix (height x width) of hex colors.
#' @param target_color Character scalar, the hex color to find the
#'   component for.
#' @param labeled A list as returned by [retro_components_label()]:
#'   `foreground_indices` and `membership`.
#' @return Logical matrix (height x width), `TRUE` for pixels in the
#'   component with the most `target_color` pixels.
#' @examples
#'   pixel_matrix <- matrix("#ffffff", nrow = 3, ncol = 10)
#'   pixel_matrix[2, 1:3] <- "#ff0000"
#'   pixel_matrix[2, 7:9] <- "#0000ff"
#'   foreground_mask <- pixel_matrix != "#ffffff"
#'   labeled <- retroglyph:::retro_components_label(foreground_mask)
#'   retroglyph:::retro_path_component_mask(pixel_matrix, "#ff0000", labeled)
retro_path_component_mask <- function(pixel_matrix, target_color, labeled) {
  target_indices <- which(pixel_matrix == target_color)
  target_membership <- labeled$membership[
    match(target_indices, labeled$foreground_indices)
  ]
  owning_component <- as.integer(names(which.max(table(target_membership))))
  kept <- labeled$foreground_indices[labeled$membership == owning_component]
  component_mask <- matrix(FALSE, nrow(pixel_matrix), ncol(pixel_matrix))
  component_mask[kept] <- TRUE
  component_mask
}

#' @title Find the path for a single series
#' @keywords internal
#' @noRd
#' @description Orchestrates the full path-finding pipeline for one series:
#'   detects start/end points, runs Dijkstra path-finding on the
#'   conductance surface, and skeletonizes the resulting path to a thin,
#'   centered medial-axis trace. Returns the claimed pixels as a tibble.
#' @param pixel_matrix Character matrix (height x width) of hex colors.
#' @param target_color Character scalar, the hex color to follow.
#' @param background Character scalar, the background hex color.
#' @param foreground_mask Logical matrix (height x width), `TRUE` for
#'   foreground pixels in this series' own connected component (see
#'   [retro_path_component_mask()]) - not necessarily the whole image.
#' @param width Integer, image width in pixels.
#' @param height Integer, image height in pixels.
#' @param start_x Integer, the leftmost foreground column within this
#'   series' own connected component (its time-axis origin).
#' @param line_width Integer, the measured line width in pixels.
#' @return A tibble with columns `x`, `y`, `color`. Empty (0 rows) if
#'   the target color has no pixels or no valid start point exists.
#' @examples
#'   # A tiny 5x10 image: red from columns 1-4 and 8-10,
#'   # blue occluding columns 5-7 on the same row.
#'   pixel_matrix <- matrix("#ffffff", nrow = 5, ncol = 10)
#'   pixel_matrix[3, 1:4] <- "#ff0000"
#'   pixel_matrix[3, 5:7] <- "#0000ff"
#'   pixel_matrix[3, 8:10] <- "#ff0000"
#'   foreground_mask <- pixel_matrix != "#ffffff"
#'   start_x <- min(which(foreground_mask, arr.ind = TRUE)[, "col"])
#'   result <- retroglyph:::retro_path_series(
#'     pixel_matrix = pixel_matrix,
#'     target_color = "#ff0000",
#'     background = "#ffffff",
#'     foreground_mask = foreground_mask,
#'     width = 10L,
#'     height = 5L,
#'     start_x = start_x,
#'     line_width = 1L
#'   )
#'   print(result)
retro_path_series <- function(
  pixel_matrix,
  target_color,
  background,
  foreground_mask,
  width,
  height,
  start_x,
  line_width
) {
  target_mask <- pixel_matrix == target_color
  target_positions <- which(target_mask, arr.ind = TRUE)
  if (nrow(target_positions) == 0L) {
    return(
      tibble::tibble(x = integer(0L), y = integer(0L), color = character(0L))
    )
  }
  endpoints <- retro_path_endpoints(
    target_positions,
    foreground_mask,
    start_x,
    line_width
  )
  if (is.null(endpoints)) {
    return(
      tibble::tibble(x = integer(0L), y = integer(0L), color = character(0L))
    )
  }
  if (
    endpoints$start_x == endpoints$end_x &&
      endpoints$start_y == endpoints$end_y
  ) {
    return(tibble::tibble(
      x = endpoints$start_x,
      y = endpoints$start_y,
      color = target_color
    ))
  }
  path <- retro_path_shortest(
    target_mask,
    foreground_mask,
    endpoints$start_x,
    endpoints$start_y,
    endpoints$end_x,
    endpoints$end_y
  )
  retro_path_claim_pixels(
    path,
    width,
    height,
    foreground_mask,
    line_width,
    target_color
  )
}

#' @title Determine start and end pixel coordinates for a single series
#' @keywords internal
#' @noRd
#' @description Finds the start and end points for Dijkstra path-finding by
#'   composing [retro_path_endpoints_start()] and
#'   [retro_path_endpoints_end()]. The start point is either the leftmost
#'   target pixel (if it is at or before `start_x`) or a synthetic point:
#'   the nearest foreground pixel at `start_x`. The end point is the
#'   rightmost target pixel after [retro_path_endpoints_end()] drops small
#'   disconnected components of the target color - anti-aliasing debris,
#'   see [retro_path_endpoints_filter_components()] - that would otherwise
#'   be eligible; if several remaining pixels tie for the rightmost column,
#'   the tie goes to whichever one is farthest from the start point in the
#'   vertical direction (the bottommost pixel for a curve trending
#'   downward, the topmost for a curve trending upward).
#' @param target_positions Integer matrix with columns `"row"` and `"col"`
#'   from `which(target_mask, arr.ind = TRUE)`.
#' @param foreground_mask Logical matrix (height x width), scoped by the
#'   caller to one series' own connected component.
#' @param start_x Integer, the leftmost foreground column within that
#'   component.
#' @param line_width Integer, the measured line width in pixels, used to
#'   scale the debris-filtering threshold.
#' @param component_multiplier Numeric scalar, the minimum size (as a
#'   multiple of `line_width`) a connected component of the target color
#'   must have to be eligible as the end point.
#' @return A list with `start_x`, `start_y`, `end_x`, `end_y`, or `NULL`
#'   if no valid start point can be found.
#' @examples
#'   # Target pixels span columns 3-8 on row 2 of a 3x10 image.
#'   target_mask <- matrix(FALSE, nrow = 3, ncol = 10)
#'   target_mask[2, 3:8] <- TRUE
#'   target_positions <- which(target_mask, arr.ind = TRUE)
#'   foreground_mask <- target_mask
#'
#'   # Case 1: target starts at or before start_x.
#'   endpoints <- retroglyph:::retro_path_endpoints(
#'     target_positions, foreground_mask, start_x = 5L, line_width = 1L
#'   )
#'   print(endpoints)
#'   # start_x = 3, start_y = 2, end_x = 8, end_y = 2
#'
#'   # Case 2: target starts after start_x (synthetic start).
#'   foreground_mask[2, 1] <- TRUE
#'   endpoints <- retroglyph:::retro_path_endpoints(
#'     target_positions, foreground_mask, start_x = 1L, line_width = 1L
#'   )
#'   print(endpoints)
#'   # start_x = 1, start_y = 2, end_x = 8, end_y = 2
#'
#'   # Case 3: a vertical run ties for the rightmost column (rows 2 and 5
#'   # both reach column 8). The start is on row 2, so the tie goes to the
#'   # pixel farthest away vertically: row 5.
#'   target_mask[5, 6:8] <- TRUE
#'   target_positions <- which(target_mask, arr.ind = TRUE)
#'   foreground_mask <- target_mask
#'   endpoints <- retroglyph:::retro_path_endpoints(
#'     target_positions, foreground_mask, start_x = 5L, line_width = 1L
#'   )
#'   print(endpoints)
#'   # start_x = 3, start_y = 2, end_x = 8, end_y = 5
#'
#'   # Case 4: a disconnected 2-pixel speck of the target color (anti-
#'   # aliasing debris) sits to the right of the real 10-pixel curve. With
#'   # the default component_multiplier = 8 and line_width = 1, the speck's
#'   # component (2 pixels) falls under the threshold (8 pixels) and is
#'   # dropped, so the real curve's rightmost pixel is chosen instead.
#'   target_mask <- matrix(FALSE, nrow = 5, ncol = 20)
#'   target_mask[3, 1:10] <- TRUE
#'   target_mask[1, 18:19] <- TRUE
#'   target_positions <- which(target_mask, arr.ind = TRUE)
#'   foreground_mask <- target_mask
#'   endpoints <- retroglyph:::retro_path_endpoints(
#'     target_positions, foreground_mask, start_x = 1L, line_width = 1L
#'   )
#'   print(endpoints)
#'   # start_x = 1, start_y = 3, end_x = 10, end_y = 3
retro_path_endpoints <- function(
  target_positions,
  foreground_mask,
  start_x,
  line_width,
  component_multiplier = 8
) {
  start <- retro_path_endpoints_start(
    target_positions,
    foreground_mask,
    start_x
  )
  if (is.null(start)) {
    return(NULL)
  }
  end <- retro_path_endpoints_end(
    target_positions,
    foreground_mask,
    start$start_y,
    line_width,
    component_multiplier
  )
  list(
    start_x = start$start_x,
    start_y = start$start_y,
    end_x = end$end_x,
    end_y = end$end_y
  )
}

#' @title Find the leftmost point of a series' target color
#' @keywords internal
#' @noRd
#' @description Finds the start point for Dijkstra path-finding: the
#'   leftmost target-color pixel, or - if the target color starts to the
#'   right of the series' own time-axis origin - a synthetic start at the
#'   nearest foreground pixel at that origin column.
#' @param target_positions Integer matrix with columns `"row"` and `"col"`
#'   from `which(target_mask, arr.ind = TRUE)`.
#' @param foreground_mask Logical matrix (height x width), scoped by the
#'   caller to one series' own connected component.
#' @param start_x Integer, the leftmost foreground column within that
#'   component (the time-axis origin).
#' @return A list with `start_x` and `start_y`, or `NULL` if no foreground
#'   pixel exists at `start_x` for a synthetic start.
#' @examples
#'   # Target starts after start_x, so the start point is synthetic: the
#'   # nearest foreground pixel at column 1.
#'   target_mask <- matrix(FALSE, nrow = 3, ncol = 10)
#'   target_mask[2, 5:8] <- TRUE
#'   target_positions <- which(target_mask, arr.ind = TRUE)
#'   foreground_mask <- target_mask
#'   foreground_mask[2, 1] <- TRUE
#'   retroglyph:::retro_path_endpoints_start(
#'     target_positions, foreground_mask, start_x = 1L
#'   )
#'   # start_x = 1, start_y = 2
retro_path_endpoints_start <- function(
  target_positions,
  foreground_mask,
  start_x
) {
  target_x <- as.integer(target_positions[, "col"])
  target_y <- as.integer(target_positions[, "row"])
  leftmost_x <- min(target_x)
  if (leftmost_x <= start_x) {
    return(list(
      start_x = leftmost_x,
      start_y = target_y[target_x == leftmost_x][1L]
    ))
  }
  start_candidates <- which(foreground_mask[, start_x])
  if (length(start_candidates) == 0L) {
    return(NULL)
  }
  leftmost_y <- target_y[target_x == leftmost_x][1L]
  list(
    start_x = start_x,
    start_y = start_candidates[which.min(abs(start_candidates - leftmost_y))]
  )
}

#' @title Find the rightmost point of a series' target color, after
#'   dropping debris
#' @keywords internal
#' @noRd
#' @description Finds the end point for Dijkstra path-finding: the
#'   rightmost pixel of the target color, after first calling
#'   [retro_path_endpoints_filter_components()] to drop small disconnected
#'   components of the target color (anti-aliasing debris) that would
#'   otherwise be eligible. Ties for the rightmost column (a vertical run,
#'   as from a steep step drop) are broken by picking whichever tied pixel
#'   is farthest from the start point in the vertical direction.
#' @param target_positions Integer matrix with columns `"row"` and `"col"`
#'   from `which(target_mask, arr.ind = TRUE)`.
#' @param foreground_mask Logical matrix (height x width), scoped by the
#'   caller to one series' own connected component; only its dimensions
#'   are used here.
#' @param start_y Integer, the start point's row, used to break rightmost
#'   ties.
#' @param line_width Integer, the measured line width in pixels.
#' @param component_multiplier Numeric scalar, passed through to
#'   [retro_path_endpoints_filter_components()].
#' @return A list with `end_x` and `end_y`.
#' @examples
#'   # Rows 2 and 5 tie for the rightmost column (8). The start is on row
#'   # 2, so the tie goes to the pixel farthest away vertically: row 5.
#'   target_mask <- matrix(FALSE, nrow = 8, ncol = 10)
#'   target_mask[2, 3:8] <- TRUE
#'   target_mask[5, 6:8] <- TRUE
#'   target_positions <- which(target_mask, arr.ind = TRUE)
#'   foreground_mask <- target_mask
#'   retroglyph:::retro_path_endpoints_end(
#'     target_positions, foreground_mask,
#'     start_y = 2L, line_width = 1L, component_multiplier = 8
#'   )
#'   # end_x = 8, end_y = 5
retro_path_endpoints_end <- function(
  target_positions,
  foreground_mask,
  start_y,
  line_width,
  component_multiplier
) {
  filtered_positions <- retro_path_endpoints_filter_components(
    target_positions,
    height = nrow(foreground_mask),
    width = ncol(foreground_mask),
    line_width = line_width,
    component_multiplier = component_multiplier
  )
  target_x <- as.integer(filtered_positions[, "col"])
  target_y <- as.integer(filtered_positions[, "row"])
  end_x <- max(target_x)
  end_y_candidates <- target_y[target_x == end_x]
  end_y <- end_y_candidates[which.max(abs(end_y_candidates - start_y))]
  list(end_x = end_x, end_y = end_y)
}

#' @title Drop small disconnected components of the target color
#' @keywords internal
#' @noRd
#' @description Anti-aliasing can leave a stray pixel of the exact target
#'   color disconnected from the real curve; if that speck lands further
#'   right than the real curve, [retro_path_endpoints_end()] would
#'   otherwise pick it as the end point. This labels the 4-connected
#'   components of the target color's own pixels only - not the whole
#'   foreground - and drops any component with fewer than
#'   `component_multiplier * line_width` pixels. If every component falls
#'   under the threshold (a genuinely short curve, or the tiny synthetic
#'   curves used in tests), returns `target_positions` unfiltered rather
#'   than leaving no candidate pixels at all.
#' @details This function exists because anti-aliasing debris can throw off
#'   the entire direction of Dijkstra's shortest path.
#'   It makes the bet that Kaplan-Maier curves tend to overlap less
#'   on the right side than on the left side.
#'   This bet appears to be correct in the vast majority of cases.
#' @param target_positions Integer matrix with columns `"row"` and `"col"`
#'   from `which(target_mask, arr.ind = TRUE)`.
#' @param height Integer, image height in pixels.
#' @param width Integer, image width in pixels.
#' @param line_width Integer, the measured line width in pixels.
#' @param component_multiplier Numeric scalar, the minimum component size
#'   as a multiple of `line_width`; components smaller than
#'   `component_multiplier * line_width` pixels are dropped.
#' @return An integer matrix shaped like `target_positions` (columns
#'   `"row"` and `"col"`), restricted to pixels in components that met the
#'   threshold.
#' @examples
#'   # A real curve (10 pixels) plus a disconnected 2-pixel speck.
#'   target_mask <- matrix(FALSE, nrow = 5, ncol = 20)
#'   target_mask[3, 1:10] <- TRUE
#'   target_mask[1, 18:19] <- TRUE
#'   target_positions <- which(target_mask, arr.ind = TRUE)
#'   result <- retroglyph:::retro_path_endpoints_filter_components(
#'     target_positions,
#'     height = 5L,
#'     width = 20L,
#'     line_width = 1L,
#'     component_multiplier = 8
#'   )
#'   print(max(result[, "col"]))
#'   # 10 - the speck at columns 18-19 is dropped
retro_path_endpoints_filter_components <- function(
  target_positions,
  height,
  width,
  line_width,
  component_multiplier
) {
  target_mask <- matrix(FALSE, nrow = height, ncol = width)
  target_mask[
    cbind(target_positions[, "row"], target_positions[, "col"])
  ] <- TRUE
  labeled <- retro_components_label(target_mask)
  min_pixels <- component_multiplier * line_width
  component_sizes <- table(labeled$membership)
  kept_components <- as.integer(
    names(component_sizes)[component_sizes >= min_pixels]
  )
  kept_indices <- labeled$foreground_indices[
    labeled$membership %in% kept_components
  ]
  if (length(kept_indices) == 0L) {
    return(target_positions)
  }
  kept_mask <- matrix(FALSE, nrow = height, ncol = width)
  kept_mask[kept_indices] <- TRUE
  which(kept_mask, arr.ind = TRUE)
}

#' @title Find the shortest path between two pixels on the conductance surface
#' @keywords internal
#' @noRd
#' @description Builds a sparse igraph from the foreground pixels, assigns
#'   edge costs as the reciprocal of mean conductance, and runs Dijkstra's
#'   algorithm via `igraph::shortest_paths()`. Returns pixel coordinates
#'   directly — no spatial coordinate conversion needed.
#' @param target_mask Logical matrix (height x width).
#' @param foreground_mask Logical matrix (height x width).
#' @param start_x Integer, start pixel x-coordinate.
#' @param start_y Integer, start pixel y-coordinate.
#' @param end_x Integer, end pixel x-coordinate.
#' @param end_y Integer, end pixel y-coordinate.
#' @return A tibble with columns `x` and `y` (integer pixel coordinates
#'   along the shortest path from start to end).
#' @examples
#'   # A 5x10 foreground strip on row 3. Target is columns 1-3 and 8-10,
#'   # with a "bridge" of other-foreground in columns 4-7.
#'   target_mask <- matrix(FALSE, nrow = 5, ncol = 10)
#'   target_mask[3, c(1:3, 8:10)] <- TRUE
#'   foreground_mask <- matrix(FALSE, nrow = 5, ncol = 10)
#'   foreground_mask[3, 1:10] <- TRUE
#'   path <- retroglyph:::retro_path_shortest(
#'     target_mask, foreground_mask,
#'     start_x = 1L, start_y = 3L,
#'     end_x = 10L, end_y = 3L
#'   )
#'   print(path)
retro_path_shortest <- function(
  target_mask,
  foreground_mask,
  start_x,
  start_y,
  end_x,
  end_y
) {
  graph_result <- retro_path_conductance_graph(target_mask, foreground_mask)
  height <- nrow(foreground_mask)
  start_vertex <- graph_result$vertex_lookup[
    (start_x - 1L) * height + start_y
  ]
  end_vertex <- graph_result$vertex_lookup[
    (end_x - 1L) * height + end_y
  ]
  path_vertices <- igraph::shortest_paths(
    graph_result$graph,
    from = start_vertex,
    to = end_vertex
  )$vpath[[1L]]
  foreground_indices <- which(foreground_mask)
  path_linear <- foreground_indices[as.integer(path_vertices)]
  path_y <- as.integer(((path_linear - 1L) %% height) + 1L)
  path_x <- as.integer(((path_linear - 1L) %/% height) + 1L)
  tibble::tibble(x = path_x, y = path_y)
}

#' @title Build a conductance graph from the foreground masks
#' @keywords internal
#' @noRd
#' @description Constructs a sparse igraph where only foreground pixels are
#'   vertices. Edges connect 4-adjacent foreground pixels. Edge weights are
#'   the cost (reciprocal of mean conductance): target-color pixels have
#'   conductance 1, other foreground pixels have conductance 0.1. Background
#'   pixels are excluded entirely — no vertices, no edges.
#' @param target_mask Logical matrix (height x width).
#' @param foreground_mask Logical matrix (height x width).
#' @return A list with elements:
#'   - `graph`: an igraph object with weighted edges (cost = 1/conductance).
#'   - `vertex_lookup`: integer vector of length `height * width` mapping
#'     each linear pixel index to its igraph vertex ID (0 if not foreground).
#' @examples
#'   # 3x5 grid: target on row 2 columns 1-2, other foreground columns 3-5.
#'   target_mask <- matrix(FALSE, nrow = 3, ncol = 5)
#'   target_mask[2, 1:2] <- TRUE
#'   foreground_mask <- matrix(FALSE, nrow = 3, ncol = 5)
#'   foreground_mask[2, 1:5] <- TRUE
#'   result <- retroglyph:::retro_path_conductance_graph(target_mask, foreground_mask)
#'   print(igraph::vcount(result$graph))
#'   print(igraph::ecount(result$graph))
#'   print(igraph::E(result$graph)$weight)
retro_path_conductance_graph <- function(target_mask, foreground_mask) {
  foreground_indices <- which(foreground_mask)
  vertex_count <- length(foreground_indices)
  vertex_lookup <- integer(length(foreground_mask))
  vertex_lookup[foreground_indices] <- seq_len(vertex_count)
  conductance <- ifelse(target_mask[foreground_indices], 1.0, 0.1)
  height <- nrow(foreground_mask)
  width <- ncol(foreground_mask)
  rows <- ((foreground_indices - 1L) %% height) + 1L
  cols <- ((foreground_indices - 1L) %/% height) + 1L
  edges_from <- integer(0L)
  edges_to <- integer(0L)
  edge_costs <- numeric(0L)
  # Right neighbors (same row, column + 1)
  right_col <- cols + 1L
  right_valid <- right_col <= width
  right_linear <- (right_col - 1L) * height + rows
  right_has_neighbor <- right_valid &
    foreground_mask[ifelse(right_valid, right_linear, 1L)]
  if (any(right_has_neighbor)) {
    from_idx <- seq_len(vertex_count)[right_has_neighbor]
    to_idx <- vertex_lookup[right_linear[right_has_neighbor]]
    cost <- 1.0 / ((conductance[from_idx] + conductance[to_idx]) / 2.0)
    edges_from <- c(edges_from, from_idx)
    edges_to <- c(edges_to, to_idx)
    edge_costs <- c(edge_costs, cost)
  }
  # Down neighbors (row + 1, same column)
  down_row <- rows + 1L
  down_valid <- down_row <= height
  down_linear <- (cols - 1L) * height + down_row
  down_has_neighbor <- down_valid &
    foreground_mask[ifelse(down_valid, down_linear, 1L)]
  if (any(down_has_neighbor)) {
    from_idx <- seq_len(vertex_count)[down_has_neighbor]
    to_idx <- vertex_lookup[down_linear[down_has_neighbor]]
    cost <- 1.0 / ((conductance[from_idx] + conductance[to_idx]) / 2.0)
    edges_from <- c(edges_from, from_idx)
    edges_to <- c(edges_to, to_idx)
    edge_costs <- c(edge_costs, cost)
  }
  edge_matrix <- rbind(edges_from, edges_to)
  graph <- igraph::make_empty_graph(n = vertex_count, directed = FALSE)
  graph <- igraph::add_edges(graph, as.vector(edge_matrix))
  igraph::E(graph)$weight <- edge_costs
  list(graph = graph, vertex_lookup = vertex_lookup)
}

#' @title Skeletonize the path to a thin medial-axis trace within the
#'   foreground mask
#' @keywords internal
#' @noRd
#' @description Produces a thin medial-axis trace for a curve
#'   from a 1-pixel Dijkstra path, constrained to the foreground. Uses a
#'   three-stage pipeline:
#'
#'   1. **Over-dilate** the path by the line width (disk kernel). This
#'      spreads the path wide enough to cover the full foreground region
#'      even when the Dijkstra path hugs one edge.
#'   2. **Crop to foreground** by AND-ing with the foreground mask. This
#'      removes any dilation that bled into background.
#'   3. **Skeletonize** the cropped result to find the centered medial axis.
#'      Because the over-dilated shape was constrained to foreground, its
#'      skeleton lies in the interior — effectively shifting the path away
#'      from edges. A vertical step-drop's skeleton spans multiple rows at
#'      the same column, so the drop's full extent is preserved rather than
#'      needing to be reconstructed later from a wider band.
#'
#' @param recovered Tibble with columns `x` and `y` (integer path pixels).
#' @param width Integer, image width.
#' @param height Integer, image height.
#' @param foreground_mask Logical matrix (height x width).
#' @param line_width Integer, the measured line width in pixels, used only
#'   for the over-dilation radius in stage 1.
#' @param target_color Character scalar, hex color assigned to these pixels.
#' @return A tibble with columns `x`, `y`, `color`: the curve's thin
#'   medial-axis skeleton pixels.
#' @examples
#'   # A 5x10 image with a horizontal foreground strip on row 3.
#'   # The path is a single pixel at (5, 3); dilation spreads it across the
#'   # strip before skeletonizing back down to a thin trace.
#'   foreground_mask <- matrix(FALSE, nrow = 5, ncol = 10)
#'   foreground_mask[3, 2:9] <- TRUE
#'   recovered <- tibble::tibble(x = 5L, y = 3L)
#'   result <- retroglyph:::retro_path_claim_pixels(
#'     recovered,
#'     width = 10L,
#'     height = 5L,
#'     foreground_mask = foreground_mask,
#'     line_width = 2L,
#'     target_color = "#ff0000"
#'   )
#'   print(result)
retro_path_claim_pixels <- function(
  recovered,
  width,
  height,
  foreground_mask,
  line_width,
  target_color
) {
  # Render path as a binary image. ImageMagick morphology treats
  # white as foreground, so path = white, background = black.
  path_matrix <- matrix("#000000", nrow = height, ncol = width)
  path_set <- unique(cbind(x = recovered$x, y = recovered$y))
  path_matrix[cbind(path_set[, "y"], path_set[, "x"])] <- "#ffffff"
  path_image <- magick::image_read(path_matrix)
  # Over-dilate by the line width so the blob covers the full foreground
  # region even when the Dijkstra path runs along one edge
  over_radius <- max(1L, as.integer(line_width))
  dilated_wide <- magick::image_morphology(
    path_image,
    "Dilate",
    paste0("Disk:", over_radius)
  )
  # Crop to foreground — removes any bleed into background
  wide_raster <- as.matrix(magick::image_raster(dilated_wide, tidy = FALSE))
  wide_mask <- (wide_raster != "#000000ff") & foreground_mask
  # Skeletonize to find the centered medial axis within foreground.
  # Because the over-dilated shape was clipped to foreground, its skeleton
  # is shifted inward from the original Dijkstra path. This thin skeleton
  # is the curve's claimed pixel set - it is not dilated back out.
  centered_matrix <- matrix("#000000", nrow = height, ncol = width)
  centered_matrix[wide_mask] <- "#ffffff"
  centered_image <- magick::image_read(centered_matrix)
  skeleton_image <- magick::image_morphology(
    centered_image,
    "Thinning",
    "Skeleton"
  )
  skeleton_raster <- as.matrix(
    magick::image_raster(skeleton_image, tidy = FALSE)
  )
  claimed_positions <- which(skeleton_raster != "#000000ff", arr.ind = TRUE)
  tibble::tibble(
    x = as.integer(claimed_positions[, "col"]),
    y = as.integer(claimed_positions[, "row"]),
    color = target_color
  )
}

#' @title Measure the global line width, averaged across every series
#' @keywords internal
#' @noRd
#' @description Computes line width as total pixels / skeleton pixels for
#'   each series independently (see [retro_path_color_width()]), then
#'   averages across all of them. The skeleton is the 1-pixel-wide medial
#'   axis obtained via morphological thinning; dividing total area by
#'   skeleton length gives the average line thickness, which is resilient
#'   to line geometry (works for horizontal, vertical, and diagonal
#'   segments alike).
#'
#'   This is computed once globally because all curves in a Kaplan-Meier
#'   image share the same line width. Averaging across every series is more
#'   robust than reading it off a single one, since a curve that happens to
#'   be short or heavily occluded no longer determines the whole estimate
#'   by itself.
#' @param pixel_matrix Character matrix (height x width) of hex colors.
#' @param background Character scalar, the background hex color.
#' @return Integer scalar, the estimated line width in pixels, averaged
#'   across every series. Falls back to 2 if no series yields a measurable
#'   skeleton.
#' @examples
#'   # A 10x20 image with a 3-pixel-thick horizontal red line.
#'   pixel_matrix <- matrix("#ffffff", nrow = 10, ncol = 20)
#'   pixel_matrix[4:6, 3:18] <- "#ff0000"
#'   retroglyph:::retro_path_line_width(pixel_matrix, background = "#ffffff")
#'   # Returns approximately 3
#'
#'   # No foreground at all — falls back to 2.
#'   empty <- matrix("#ffffff", nrow = 3, ncol = 3)
#'   retroglyph:::retro_path_line_width(empty, background = "#ffffff")
retro_path_line_width <- function(pixel_matrix, background) {
  colors <- setdiff(unique(as.vector(pixel_matrix)), background)
  if (length(colors) == 0L) {
    return(2L)
  }
  widths <- vapply(
    colors,
    function(color) retro_path_color_width(pixel_matrix, color),
    numeric(1L)
  )
  widths <- widths[!is.na(widths)]
  if (length(widths) == 0L) {
    return(2L) # nocov
  }
  # Skeletonization is not a perfect wireframe, and it doesn't
  # always remove all the thickness.
  # And curve overplotting can artificially narrow the width
  # of occluded curves in places.
  # These factors cause us to underestimate line width,
  # so we take the largest and round up.
  as.integer(ceiling(max(widths)))
}

#' @title Measure one color's line width via area over skeleton length
#' @keywords internal
#' @noRd
#' @description Skeletonizes one color's pixels (morphological thinning) to
#'   find its 1-pixel-wide medial axis, then divides total pixel area by
#'   skeleton length to get its average thickness. Used by
#'   [retro_path_line_width()] to measure every series before averaging.
#' @param pixel_matrix Character matrix (height x width) of hex colors.
#' @param color Character scalar, the hex color to measure.
#' @return Numeric scalar, the estimated line width in pixels, or `NA` if
#'   the color's skeleton has no pixels.
#' @examples
#'   pixel_matrix <- matrix("#ffffff", nrow = 10, ncol = 20)
#'   pixel_matrix[4:6, 3:18] <- "#ff0000"
#'   retroglyph:::retro_path_color_width(pixel_matrix, "#ff0000")
#'   # Returns approximately 3
retro_path_color_width <- function(pixel_matrix, color) {
  total_pixels <- sum(pixel_matrix == color)
  # Skeletonize this series to get path length.
  # ImageMagick morphology treats white as foreground.
  binary_matrix <- matrix(
    "#000000",
    nrow = nrow(pixel_matrix),
    ncol = ncol(pixel_matrix)
  )
  binary_matrix[pixel_matrix == color] <- "#ffffff"
  binary_image <- magick::image_read(binary_matrix)
  skeleton_image <- magick::image_morphology(
    binary_image,
    "Thinning",
    "Skeleton",
    iterations = -1
  )
  skeleton_raster <- as.matrix(
    magick::image_raster(skeleton_image, tidy = FALSE)
  )
  skeleton_pixels <- sum(skeleton_raster != "#000000ff")
  # Line width = area / path length
  if (skeleton_pixels == 0L) {
    return(NA_real_) # nocov
  }
  total_pixels / skeleton_pixels
}
