#' @title Label the numbers in an image with set-of-mark annotations
#' @keywords internal
#' @noRd
#' @description Run OCR on a cleaned-up copy of the source image with the
#'   axes, tick marks, and gridlines stripped out, then draw red bounding
#'   boxes and sequential letter labels for each detection onto a plain
#'   grayscale rendering of the source image and return a tibble of
#'   findings. The annotated image uses set-of-mark prompting: each
#'   detection gets a unique letter label (A, B, C, ..., Z, AA, AB, ...)
#'   drawn in white text on a red background badge next to its bounding box.
#'
#'   Detection and presentation are deliberately split. Axis lines and
#'   gridlines wreck OCR accuracy, so they are removed before Tesseract
#'   runs; but the model reading the annotated output still needs to see
#'   the plot whole, so `output` keeps them.
#'
#'   OCR is restricted to numeric characters (`0123456789.-`) to detect
#'   axis tick values and risk table counts. Non-numeric text (titles,
#'   axis labels, series names) is not detected - those are
#'   identified by the model visually from context.
#' @details Three steps, each an exported function in its own right:
#'
#'   1. [retro_image_grayscale()] converts `input` to a plain grayscale
#'      rendering, preserving anti-aliasing and gray gradients for
#'      legibility. This is both what OCR reads and what the annotated
#'      `output` is drawn on.
#'   2. [retro_data_ocr()] detects numeric text on that grayscale copy and
#'      returns a tibble of findings with sequential letter labels already
#'      assigned. Everything needed to keep axis lines, gridlines, and
#'      curves out of Tesseract's way - quantizing to black and white,
#'      building a component mask, dilating it - happens inside this call
#'      and never leaves it.
#'   3. [retro_image_label()] draws the red bounding boxes and letter
#'      badges from those findings onto the grayscale copy and writes
#'      `output`. Skipped when there are no findings, in which case the
#'      grayscale copy is written to `output` unchanged.
#'
#'   Coordinates follow PNG conventions: (1, 1) is the top-left corner,
#'   x increases left to right, y increases top to bottom.
#' @return A tibble with one row per OCR detection. Columns:
#'   * `label` - Character, sequential letter label (A, B, ..., Z, AA, AB, ...).
#'   * `word` - Character, the detected text. Any hyphens after the first
#'     character are removed (interior hyphens are OCR artifacts, not part
#'     of a number); a leading minus sign is preserved.
#'   * `confidence` - Numeric, Tesseract confidence score (0-100).
#'   * `x` - Numeric, pixel x-coordinate of bounding box centroid.
#'   * `y` - Numeric, pixel y-coordinate of bounding box centroid.
#'   * `x1` - Integer, left edge of bounding box.
#'   * `x2` - Integer, right edge of bounding box.
#'   * `y1` - Integer, top edge of bounding box.
#'   * `y2` - Integer, bottom edge of bounding box.
#' @param input Character scalar, path to the source image file.
#' @param output Character scalar, path where the annotated grayscale image
#'   with set-of-mark annotations will be written. This is the plain
#'   grayscale rendering of `input`, axes and gridlines intact - the
#'   cleaned-up copy used for detection is not what gets written here.
#' @param confidence Numeric scalar, minimum Tesseract confidence score
#'   (0-100) for a detected word to be included.
#' @param magnify Numeric scalar, factor by which to upscale the image
#'   before OCR. Higher values improve detection of small text.
#' @param span_threshold Numeric scalar in `(0, 1]`. A connected foreground
#'   component is erased before OCR if its bounding-box height is at least
#'   `span_threshold * image height`, or its bounding-box width is at
#'   least `span_threshold * image width`. Lower it if
#'   axis lines or curves are surviving into OCR; raise it if real tick
#'   labels are being erased.
#' @param dilate Non-negative integer scalar, pixels to grow the
#'   oversized-component mask by before subtracting it from the allow
#'   list. Anti-aliased fringe dark enough to quantize to foreground forms
#'   its own small components alongside an axis line, which neither the
#'   allow list nor the span filter removes; growing the line's mask
#'   sweeps them up. Raise it if fringe still shows
#'   through; set it to 0 to disable.
#' @examples
#'   input <- system.file("simulation.png", package = "retroglyph")
#'   output <- tempfile(fileext = ".png")
#'   retroglyph:::retro_do_label(input, output, magnify = 4)
#'   if (interactive()) {
#'     browseURL(output)
#'   }
retro_do_label <- function(
  input,
  output,
  confidence = 50,
  magnify = 4,
  span_threshold = 0.15,
  dilate = 2
) {
  stopifnot(
    "output must be a single string" = is.character(output) &&
      length(output) == 1L
  )
  gray_input <- tempfile(fileext = ".png")
  on.exit(unlink(gray_input), add = TRUE)
  retro_image_grayscale(input = input, output = gray_input)
  findings <- retro_data_ocr(
    input = gray_input,
    confidence = confidence,
    magnify = magnify,
    span_threshold = span_threshold,
    dilate = dilate
  )
  # Annotate the uncleaned grayscale image, not the cleaned copy OCR read.
  # The cleaning exists to keep axes and gridlines out of Tesseract's way;
  # the model still wants to see the plot intact to reason about it.
  if (nrow(findings) == 0L) {
    file.copy(gray_input, output, overwrite = TRUE)
    return(findings)
  }
  retro_image_label(gray_input, output, findings)
  findings
}
