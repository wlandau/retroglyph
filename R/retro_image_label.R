#' @title Draw set-of-mark annotations on an image
#' @keywords internal
#' @noRd
#' @description Draw red bounding boxes and sequential letter labels on an
#'   image and write the result. Each detection gets a unique letter label
#'   drawn in white text on a red background badge next to its bounding
#'   box. Reads labels from `findings$label` rather than generating them -
#'   see [retro_data_ocr()], which assigns them.
#' @param input Character scalar, path to the image file to annotate.
#' @param output Character scalar, path to write the annotated image.
#' @param findings Tibble with columns `label`, `x1`, `x2`, `y1`, `y2`
#'   (e.g. from [retro_data_ocr()]).
#' @return `NULL` (invisibly). Called for its side effect of writing an
#'   image file.
#' @examples
#'   input <- system.file("simulation.png", package = "retroglyph")
#'   gray_input <- tempfile(fileext = ".png")
#'   retroglyph:::retro_image_grayscale(input, gray_input)
#'   findings <- retroglyph:::retro_data_ocr(gray_input, magnify = 4)
#'   output <- tempfile(fileext = ".png")
#'   retroglyph:::retro_image_label(gray_input, output, findings)
#'   if (interactive()) {
#'     browseURL(output)
#'   }
retro_image_label <- function(input, output, findings) {
  stopifnot(
    "Source image is missing. Please provide a source image." = is.character(
      input
    ) &&
      length(input) == 1L,
    "input must exist" = file.exists(input),
    "output must be a single string" = is.character(output) &&
      length(output) == 1L,
    "findings must be a data frame" = is.data.frame(findings)
  )
  image <- magick::image_read(input)
  # Draw annotations using magick::image_draw (opens a base R graphics device)
  image <- magick::image_draw(image)
  # image_draw() sets up a flipped y-axis (par("usr") has ymin > ymax, to
  # match raster row order), so strheight() returns a negative height.
  # Take the absolute value or downstream cex computations go negative.
  base_char_height <- abs(strheight("M", units = "user"))
  # Use the median detected text height only to size the border - a single
  # uniform border thickness across all labels, robust to any one box being
  # unusually small (e.g. an axis tick label) or large.
  median_height <- stats::median(findings$y2 - findings$y1 + 1L)
  for (i in seq_len(nrow(findings))) {
    retro_label_draw_annotation(
      label = findings$label[i],
      x1 = findings$x1[i],
      x2 = findings$x2[i],
      y1 = findings$y1[i],
      y2 = findings$y2[i],
      base_char_height = base_char_height,
      median_height = median_height
    )
  }
  dev.off()
  magick::image_write(image, path = output)
  invisible(NULL)
}

#' @title Draw a single set-of-mark annotation
#' @keywords internal
#' @noRd
#' @description Draw a red bounding box and a labeled badge for one OCR
#'   detection. Must be called within an active `magick::image_draw()`
#'   graphics device.
#' @param label Character scalar, the label text to draw (e.g. "A", "AB").
#' @param x1 Integer scalar, left edge of the bounding box (1-based).
#' @param x2 Integer scalar, right edge of the bounding box (1-based).
#' @param y1 Integer scalar, top edge of the bounding box (1-based).
#' @param y2 Integer scalar, bottom edge of the bounding box (1-based).
#' @param base_char_height Numeric scalar, the height of "M" in user units
#'   from `strheight()`, used to scale text.
#' @param median_height Numeric scalar, the median bounding box height
#'   across all findings, used to set a uniform border thickness so it
#'   reads clearly regardless of how small or large any one detected box
#'   is.
#' @return Called for its side effect (drawing on the active device).
#' @examples
#'   input <- system.file("simulation.png", package = "retroglyph")
#'   image <- magick::image_read(input)
#'   image <- magick::image_draw(image)
#'   base_char_height <- abs(strheight("M", units = "user"))
#'   retroglyph:::retro_label_draw_annotation(
#'     label = "A",
#'     x1 = 10L,
#'     x2 = 50L,
#'     y1 = 10L,
#'     y2 = 30L,
#'     base_char_height = base_char_height,
#'     median_height = 20
#'   )
#'   dev.off()
#'   output <- tempfile(fileext = ".png")
#'   magick::image_write(image, path = output)
#'   print(file.exists(output))
retro_label_draw_annotation <- function(
  label,
  x1,
  x2,
  y1,
  y2,
  base_char_height,
  median_height
) {
  # Border thickness scales with the median detected text height so it
  # reads clearly on both small and very large source images.
  border_lwd <- max(2, median_height * 0.15)
  # Expand the box outward by half the border width (plus a 1px buffer) so
  # the border stroke sits outside the detected text instead of covering
  # it.
  box_padding <- ceiling(border_lwd / 2) + 1L
  box_x1 <- x1 - box_padding
  box_x2 <- x2 + box_padding
  box_y1 <- y1 - box_padding
  box_y2 <- y2 + box_padding
  # Draw red bounding box (device coords are 0-based)
  rect(
    xleft = box_x1,
    ybottom = box_y2,
    xright = box_x2,
    ytop = box_y1,
    border = "#FF0000",
    lwd = border_lwd
  )
  # Draw label badge: red background rectangle with white text
  # Badge is adjoined to the left side of the bounding box and matches its
  # height, so the label font size lands comparably to the actual text the
  # bounding box encloses. Padding ensures text does not touch badge edges.
  padding <- 2L
  badge_height <- box_y2 - box_y1
  badge_width <- badge_height * nchar(label) * 0.7 + 2L * padding
  # Position badge to the left of the bounding box, vertically centered
  box_mid_y <- (box_y1 + box_y2) / 2
  badge_y1 <- box_mid_y - badge_height / 2
  badge_y2 <- box_mid_y + badge_height / 2
  badge_x2 <- box_x1 - 2L
  badge_x1 <- badge_x2 - badge_width
  # If badge would go off the left edge, place it to the right instead
  if (badge_x1 < 0) {
    badge_x1 <- box_x2 + 2L
    badge_x2 <- badge_x1 + badge_width
  }
  # Filled red background for label
  rect(
    xleft = badge_x1,
    ybottom = badge_y2,
    xright = badge_x2,
    ytop = badge_y1,
    col = "#FF0000",
    border = NA
  )
  # White text label scaled to badge interior (excluding padding)
  text_height <- badge_height - 2L * padding
  text(
    x = (badge_x1 + badge_x2) / 2,
    y = (badge_y1 + badge_y2) / 2,
    labels = label,
    col = "#FFFFFF",
    cex = text_height / base_char_height * 0.7,
    font = 2
  )
}
