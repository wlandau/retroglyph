#' @title Extract the pixel matrix from an image
#' @keywords internal
#' @noRd
#' @description Reads an image into a character matrix of 7-character
#'   lowercase hex RGB colors.
#' @param image A magick image object.
#' @return Character matrix (height x width) of hex colors.
#' @examples
#'   pixel_matrix <- matrix(
#'     c("#ffffff", "#ffffff", "#000000", "#ffffff"),
#'     nrow = 2
#'   )
#'   image <- magick::image_read(pixel_matrix)
#'   print(retroglyph:::retro_components_pixel_matrix(image))
retro_components_pixel_matrix <- function(image) {
  raster <- magick::image_raster(image, tidy = FALSE)
  pixel_matrix <- as.matrix(raster)
  pixel_matrix[] <- retro_color_rgb(pixel_matrix)
  pixel_matrix
}

#' @title Classify pixels as foreground or background via 2-color quantization
#' @keywords internal
#' @noRd
#' @description Quantizes the image to exactly 2 colors (no dithering).
#'   The more frequent color is background; everything else is foreground.
#'   This collapses near-background colors (light grays, faint grid lines)
#'   into background, producing a clean binary mask.
#' @details Drops the alpha channel before quantizing, via
#'   [retro_color_opaque()], which explains why. Without it, some
#'   `ImageMagick` builds return a single `"transparent"` color instead of
#'   the two colors asked for, and every pixel then compares equal to the
#'   detected background, yielding an all-`FALSE` mask.
#' @param image A magick image object.
#' @return Logical matrix (height x width), `TRUE` for foreground.
#' @examples
#'   pixel_matrix <- matrix(
#'     c("#ffffff", "#ffffff", "#000000", "#ffffff"),
#'     nrow = 2
#'   )
#'   image <- magick::image_read(pixel_matrix)
#'   print(retroglyph:::retro_components_foreground_mask(image))
retro_components_foreground_mask <- function(image) {
  quantized <- retro_components_quantize(image)
  raster <- magick::image_raster(quantized, tidy = FALSE)
  pixel_matrix <- as.matrix(raster)
  pixel_matrix[] <- retro_color_rgb(pixel_matrix)
  color_counts <- table(as.vector(pixel_matrix))
  background <- names(color_counts)[which.max(color_counts)]
  pixel_matrix != background
}

#' @title Quantize an image to 2 colors for foreground/background split
#' @keywords internal
#' @noRd
#' @description Drops any alpha channel, then quantizes to exactly 2
#'   colors with dithering disabled.
#' @details Every 2-color quantization in the package goes through this
#'   function, so the alpha-channel precaution in [retro_color_opaque()]
#'   is applied in exactly one place rather than repeated at each call
#'   site.
#' @param image A magick image object.
#' @return A magick image object with at most 2 colors and no alpha
#'   channel.
#' @examples
#'   image <- magick::image_read(matrix(c("#ffffff", "#000000"), nrow = 1))
#'   print(retroglyph:::retro_components_quantize(image))
retro_components_quantize <- function(image) {
  retro_color_opaque(image) |>
    magick::image_quantize(max = 2L, dither = FALSE)
}

#' @title Determine background color from a foreground mask
#' @keywords internal
#' @noRd
#' @description Returns the most common color among background pixels
#'   (those where the foreground mask is `FALSE`).
#' @param foreground_mask Logical matrix (height x width).
#' @param pixel_matrix Character matrix (height x width) of hex colors.
#' @return Character scalar, the background hex color.
#' @examples
#'   foreground_mask <- matrix(c(FALSE, FALSE, TRUE, FALSE), nrow = 2)
#'   pixel_matrix <- matrix(
#'     c("#ffffff", "#ffffff", "#000000", "#ffffff"),
#'     nrow = 2
#'   )
#'   print(retroglyph:::retro_components_background_color(
#'     foreground_mask,
#'     pixel_matrix
#'   ))
retro_components_background_color <- function(foreground_mask, pixel_matrix) {
  background_colors <- pixel_matrix[!foreground_mask]
  color_counts <- table(background_colors)
  names(color_counts)[which.max(color_counts)]
}

#' @title Build a 4-connected adjacency graph from foreground pixels
#' @keywords internal
#' @noRd
#' @description Creates an unweighted igraph with one vertex per
#'   foreground pixel and undirected edges between all 4-adjacent
#'   foreground neighbors (up, down, left, right).
#' @param foreground_mask Logical matrix (height x width).
#' @param foreground_indices Integer vector of linear indices
#'   (from `which(foreground_mask)`).
#' @return An igraph object (undirected, unweighted).
#' @examples
#'   # 2x3 mask with 3 horizontally adjacent foreground pixels on row 1.
#'   foreground_mask <- matrix(FALSE, nrow = 2, ncol = 3)
#'   foreground_mask[1, ] <- TRUE
#'   foreground_indices <- which(foreground_mask)
#'   graph <- retroglyph:::retro_components_adjacency_graph(
#'     foreground_mask,
#'     foreground_indices
#'   )
#'   print(igraph::vcount(graph))
#'   print(igraph::ecount(graph))
retro_components_adjacency_graph <- function(
  foreground_mask,
  foreground_indices
) {
  height <- nrow(foreground_mask)
  vertex_count <- length(foreground_indices)
  vertex_lookup <- integer(length(foreground_mask))
  vertex_lookup[foreground_indices] <- seq_len(vertex_count)
  rows <- ((foreground_indices - 1L) %% height) + 1L
  cols <- ((foreground_indices - 1L) %/% height) + 1L
  # For an undirected graph, checking right and down from each pixel
  # generates every unique 4-connected edge exactly once (each pair is
  # found from whichever pixel is above or to the left).
  right <- foreground_indices + as.integer(height)
  right_valid <- cols < ncol(foreground_mask) &
    foreground_mask[ifelse(cols < ncol(foreground_mask), right, 1L)]
  # Down neighbor: next row, same column (linear index + 1)
  down <- foreground_indices + 1L
  down_valid <- rows < height &
    foreground_mask[ifelse(rows < height, down, 1L)]
  # Combine (undirected graph, so right+down covers all unique pairs)
  from_vertices <- c(
    seq_len(vertex_count)[right_valid],
    seq_len(vertex_count)[down_valid]
  )
  to_vertices <- c(
    vertex_lookup[right[right_valid]],
    vertex_lookup[down[down_valid]]
  )
  graph <- igraph::make_empty_graph(n = vertex_count, directed = FALSE)
  if (length(from_vertices) > 0L) {
    graph <- igraph::add_edges(
      graph,
      as.vector(rbind(from_vertices, to_vertices))
    )
  }
  graph
}

#' @title Label 4-connected components in a foreground mask
#' @keywords internal
#' @noRd
#' @description Identifies 4-connected components of foreground pixels.
#'   Shared groundwork for any filter that keeps or removes whole
#'   components, whether by span ([retro_image_clean()]) or by adjacency
#'   to a known feature like an axis line ([retro_do_label()]).
#' @param foreground_mask Logical matrix (height x width).
#' @return A list with `foreground_indices` (integer vector of linear
#'   indices, from `which(foreground_mask)`) and `membership` (integer
#'   vector of component IDs, same length and order as
#'   `foreground_indices`).
#' @examples
#'   # 3x3 mask: pixels at (1,1) and (1,2) are 4-adjacent (one component);
#'   # the pixel at (3,3) is isolated (a separate component).
#'   foreground_mask <- matrix(FALSE, nrow = 3, ncol = 3)
#'   foreground_mask[1, 1:2] <- TRUE
#'   foreground_mask[3, 3] <- TRUE
#'   result <- retroglyph:::retro_components_label(foreground_mask)
#'   print(result$foreground_indices)
#'   print(result$membership)
#'   # The two adjacent pixels (indices 1 and 4) share one membership
#'   # label; the isolated pixel (index 9) gets a different label.
retro_components_label <- function(foreground_mask) {
  foreground_indices <- which(foreground_mask)
  if (length(foreground_indices) == 0L) {
    return(list(
      foreground_indices = foreground_indices,
      membership = integer(0L)
    ))
  }
  graph <- retro_components_adjacency_graph(foreground_mask, foreground_indices)
  membership <- igraph::components(graph)$membership
  list(foreground_indices = foreground_indices, membership = membership)
}
