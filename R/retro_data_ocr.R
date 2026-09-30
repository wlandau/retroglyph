#' @title Detect numeric text in a grayscale image via OCR
#' @keywords internal
#' @noRd
#' @description Run OCR on a cleaned-up copy of a grayscale image with the
#'   axes, tick marks, and gridlines stripped out, and return a tibble of
#'   findings with sequential letter labels assigned.
#'
#'   Tesseract's character whitelist option is unreliable under the LSTM
#'   engine (see [retro_ocr_detect()]), so findings are filtered to numeric
#'   text after OCR runs, rather than restricting the engine itself. This
#'   still only detects axis tick values and risk table counts;
#'   non-numeric text (titles, axis labels, series names) is not detected -
#'   those are identified by the model visually from context.
#' @details The processing pipeline:
#'
#'   1. Quantize `input` to strict black and white (see
#'      [retro_ocr_quantize()]) - a decisive 2-color split so a thin line's
#'      connected component stays contiguous instead of fragmenting at
#'      faint anti-aliased edge pixels. This quantized copy is used only to
#'      build the component mask below; it is never returned or written
#'      anywhere durable.
#'   2. Treat that quantized copy's foreground as an allow list of pixels
#'      to keep. Light gridlines and anti-aliased halos quantize to
#'      background, so they drop out here even though they are visibly
#'      non-background in `input`.
#'   3. Label 4-connected components from the quantized copy and subtract
#'      from the allow list every component whose bounding-box height or
#'      width is at least `span_threshold` of the image's height or width,
#'      grown by `dilate` pixels (see [retro_ocr_dilate()]). Axis lines,
#'      plotted curves, and solid gridlines reliably span a large fraction
#'      of the image; individual digit glyphs do not, so this needs no
#'      prior knowledge of where the panel or axes actually are. Pixels
#'      outside the resulting allow list are set to the background color
#'      in `input` itself - a copy of it, never `input` in place. See
#'      [retro_ocr_clean_image()].
#'   4. Run Tesseract OCR on the (optionally magnified) cleaned copy under
#'      several page segmentation modes, keep only findings that are
#'      numeric text from each pass, and pool the passes into one set of
#'      findings. See [retro_ocr_detect()].
#'   5. Assign sequential letter labels (A, B, C, ..., Z, AA, AB, ...) to
#'      the detections, in reading order (top to bottom, then left to
#'      right; see [retro_ocr_pool_findings()]) rather than Tesseract's
#'      own confidence-ranked order, which is not reproducible across
#'      runs. See [retro_ocr_letters()].
#'
#'   Coordinates follow PNG conventions: (1, 1) is the top-left corner,
#'   x increases left to right, y increases top to bottom.
#' @return A tibble with one row per OCR detection. Columns:
#'   * `label` - Character, sequential letter label (A, B, ..., Z, AA, AB, ...).
#'   * `word` - Character, the detected text. Hyphens are stripped (they
#'     are OCR artifacts, not part of a number); only positive numbers are
#'     kept.
#'   * `confidence` - Numeric, Tesseract confidence score (0-100).
#'   * `x` - Numeric, pixel x-coordinate of bounding box centroid.
#'   * `y` - Numeric, pixel y-coordinate of bounding box centroid.
#'   * `x1` - Integer, left edge of bounding box.
#'   * `x2` - Integer, right edge of bounding box.
#'   * `y1` - Integer, top edge of bounding box.
#'   * `y2` - Integer, bottom edge of bounding box.
#' @param input Character scalar, path to a grayscale image file (from
#'   [retro_image_grayscale()]).
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
#'   gray_input <- tempfile(fileext = ".png")
#'   retroglyph:::retro_image_grayscale(input, gray_input)
#'   findings <- retroglyph:::retro_data_ocr(gray_input, magnify = 4)
#'   print(names(findings))
#'   print(nrow(findings))
retro_data_ocr <- function(
  input,
  confidence = 50,
  magnify = 4,
  span_threshold = 0.15,
  dilate = 2
) {
  stopifnot(
    "input must be a single string" = is.character(input) &&
      length(input) == 1L,
    "input must exist" = file.exists(input),
    "confidence must be a single number" = is.numeric(confidence) &&
      length(confidence) == 1L,
    "confidence must be in [0, 100]" = confidence >= 0 && confidence <= 100,
    "magnify must be a single number" = is.numeric(magnify) &&
      length(magnify) == 1L,
    "magnify must be >= 1" = magnify >= 1,
    "span_threshold must be a single number" = is.numeric(span_threshold) &&
      length(span_threshold) == 1L,
    "span_threshold must be in (0, 1]" = span_threshold > 0 &&
      span_threshold <= 1,
    "dilate must be a single number" = is.numeric(dilate) &&
      length(dilate) == 1L &&
      !is.na(dilate),
    "dilate must be a non-negative integer" = dilate >= 0 &&
      dilate == as.integer(dilate)
  )
  binary_input <- retro_ocr_quantize(input)
  on.exit(unlink(binary_input), add = TRUE)
  ocr_input <- retro_ocr_clean_image(
    gray_input = input,
    binary_input = binary_input,
    span_threshold = span_threshold,
    dilate = dilate
  )
  on.exit(unlink(ocr_input), add = TRUE)
  findings <- retro_ocr_detect(ocr_input, confidence, magnify)
  findings$label <- retro_ocr_letters(nrow(findings))
  findings[, c("label", "word", "confidence", "x", "y", "x1", "x2", "y1", "y2")]
}

#' @title Quantize an image to strict black and white
#' @keywords internal
#' @noRd
#' @description Quantizes the image to exactly 2 colors (no dithering),
#'   collapsing anti-aliased edges into a clean binary foreground/
#'   background split. Used only to build the connected-component mask in
#'   [retro_ocr_clean_image()] - a decisive black/white split keeps a
#'   thin line's component contiguous, rather than fragmenting it at
#'   faint anti-aliased edge pixels the way a grayscale or full-color
#'   image would. The image actually cleaned and OCR'd is always the
#'   grayscale copy passed to [retro_data_ocr()], not this one.
#' @param input Character scalar, path to an image file.
#' @return Character scalar, path to a temporary quantized PNG file.
#' @examples
#'   input <- system.file("simulation.png", package = "retroglyph")
#'   quantized_path <- retroglyph:::retro_ocr_quantize(input)
#'   quantized_image <- magick::image_read(quantized_path)
#'   colors <- retroglyph:::retro_components_pixel_matrix(quantized_image)
#'   # image_quantize(max = 2, dither = FALSE) guarantees at most 2 colors.
#'   print(length(unique(as.vector(colors))) <= 2L)
retro_ocr_quantize <- function(input) {
  image <- magick::image_read(input) |>
    magick::image_quantize(max = 2L, dither = FALSE)
  output <- tempfile(fileext = ".png")
  magick::image_write(image, path = output)
  output
}

#' @title Keep only quantized-foreground pixels that are not oversized
#' @keywords internal
#' @noRd
#' @description Builds an allow list of grayscale pixels to keep and sets
#'   everything else to the background color. Two steps:
#'
#'   1. Start from the foreground mask of a strictly quantized (black and
#'      white) copy of the image. That mask is the allow list: a pixel
#'      survives only if it quantized to foreground. Light gridlines and
#'      anti-aliased halos quantize to *background*, so they are dropped
#'      here even though they are visibly non-background in grayscale -
#'      which is exactly why they used to show through into the annotated
#'      output.
#'   2. Subtract from that allow list every 4-connected component whose
#'      bounding-box height or width is at least `span_threshold` of the
#'      image's height or width, grown by `dilate` pixels. Axis lines,
#'      plotted curves, and solid gridlines reliably span a large fraction
#'      of the image regardless of orientation; individual digit glyphs
#'      (even multi-digit labels) do not. This needs no prior knowledge of
#'      where the panel or axes are.
#'
#'   Component labeling runs on the quantized copy (a decisive 2-color
#'   split keeps a thin line's component contiguous rather than
#'   fragmenting it at faint edge pixels), but the pixels actually kept or
#'   erased are the grayscale copy's, since that - not the quantized copy
#'   - is what OCR runs on.
#' @param gray_input Character scalar, path to a grayscale copy of the
#'   source image. Not modified; erasure happens on a copy.
#' @param binary_input Character scalar, path to a strictly quantized
#'   (2-color) copy of the same source image (from
#'   [retro_ocr_quantize()]), used to build the allow list and to label
#'   components.
#' @param span_threshold Numeric scalar in `(0, 1]`.
#' @param dilate Integer scalar, pixels to grow the oversized-component
#'   mask by before subtracting it from the allow list. See
#'   [retro_ocr_dilate()].
#' @return Character scalar, path to a temporary grayscale PNG holding
#'   only the allowed pixels. Caller is responsible for deleting it.
#' @examples
#'   input <- system.file("simulation.png", package = "retroglyph")
#'   gray_input <- tempfile(fileext = ".png")
#'   retroglyph:::retro_image_grayscale(input, gray_input)
#'   binary_input <- retroglyph:::retro_ocr_quantize(input)
#'   cleaned <- retroglyph:::retro_ocr_clean_image(
#'     gray_input = gray_input,
#'     binary_input = binary_input,
#'     span_threshold = 0.15,
#'     dilate = 2
#'   )
#'   print(file.exists(cleaned))
#'   # Erasure only recolors pixels to background; dimensions are unchanged.
#'   before <- magick::image_info(magick::image_read(gray_input))
#'   after <- magick::image_info(magick::image_read(cleaned))
#'   print(identical(
#'     c(before$width, before$height),
#'     c(after$width, after$height)
#'   ))
retro_ocr_clean_image <- function(
  gray_input,
  binary_input,
  span_threshold,
  dilate
) {
  gray_image <- magick::image_read(gray_input)
  pixel_matrix <- retro_components_pixel_matrix(gray_image)
  binary_image <- magick::image_read(binary_input)
  foreground_mask <- retro_components_foreground_mask(binary_image)
  labeled <- retro_components_label(foreground_mask)
  output <- tempfile(fileext = ".png")
  if (length(labeled$foreground_indices) == 0L) {
    magick::image_write(gray_image, path = output)
    return(output)
  }
  height <- nrow(foreground_mask)
  width <- ncol(foreground_mask)
  rows <- ((labeled$foreground_indices - 1L) %% height) + 1L
  cols <- ((labeled$foreground_indices - 1L) %/% height) + 1L
  oversized_ids <- retro_ocr_oversized_components(
    membership = labeled$membership,
    rows = rows,
    cols = cols,
    max_height = span_threshold * height,
    max_width = span_threshold * width
  )
  erase_mask <- matrix(FALSE, nrow = height, ncol = width)
  erase_mask[labeled$foreground_indices[
    labeled$membership %in% oversized_ids
  ]] <- TRUE
  background <- retro_components_background_color(
    foreground_mask,
    pixel_matrix
  )
  # The same helper with the mask inverted returns the most common color
  # among foreground rather than background pixels: the ink color.
  foreground <- retro_components_background_color(
    !foreground_mask,
    pixel_matrix
  )
  erase_mask <- retro_ocr_dilate(
    mask = erase_mask,
    radius = dilate,
    foreground = foreground,
    background = background
  )
  # The quantized foreground is the allow list; the oversized components
  # are subtracted from it. Everything outside the result goes to
  # background - including the light gridlines and anti-aliased halos that
  # quantized to background but are still visibly non-background in
  # grayscale.
  keep_mask <- foreground_mask & !erase_mask
  pixel_matrix[!keep_mask] <- background
  magick::image_read(pixel_matrix) |>
    magick::image_write(path = output)
  output
}

#' @title Dilate an erase mask by a pixel radius
#' @keywords internal
#' @noRd
#' @description Grows a mask outward by `radius` pixels in every
#'   direction, diagonals included, via a morphological dilation with a
#'   square `(2 * radius + 1)`-wide structuring element.
#'
#'   This exists because of anti-aliasing. Fringe pixels around a line
#'   that are light enough to quantize to background are already handled
#'   by the allow list in [retro_ocr_clean_image()], but fringe dark
#'   enough to quantize to foreground is not: it forms its own small
#'   components beside the line, too small for the span filter to catch
#'   and 4-connected to nothing that is. Fattening the line's mask by a
#'   pixel or two reaches them.
#' @param mask Logical matrix (height x width), `TRUE` for pixels to grow.
#' @param radius Integer scalar, number of pixels to grow by. Values
#'   below 1 return `mask` unchanged.
#' @param foreground Character scalar, hex color standing in for `TRUE`
#'   while the mask round-trips through ImageMagick.
#' @param background Character scalar, hex color standing in for `FALSE`.
#' @return Logical matrix, same dimensions as `mask`.
#' @examples
#'   mask <- matrix(FALSE, nrow = 5, ncol = 5)
#'   mask[3, 3] <- TRUE
#'   grown <- retroglyph:::retro_ocr_dilate(
#'     mask = mask,
#'     radius = 1L,
#'     foreground = "#000000",
#'     background = "#ffffff"
#'   )
#'   print(grown)
#'   # The single TRUE pixel grows into a solid 3x3 block (rows 2-4,
#'   # columns 2-4). Dark foreground on a lighter background is grown via
#'   # ImageMagick's "Erode" (minimum) operator, not "Dilate" - see
#'   # @description above.
retro_ocr_dilate <- function(mask, radius, foreground, background) {
  radius <- as.integer(radius)
  if (radius < 1L) {
    return(mask)
  }
  encoded <- matrix(background, nrow = nrow(mask), ncol = ncol(mask))
  encoded[mask] <- foreground
  foreground_brightness <- retro_ocr_brightness(foreground)
  background_brightness <- retro_ocr_brightness(background)
  # ImageMagick morphology grows whichever of the two regions is brighter,
  # so on the usual dark-ink-on-light-paper plot it is "Erode", not
  # "Dilate", that grows the masked pixels.
  method <- if (foreground_brightness >= background_brightness) {
    "Dilate"
  } else {
    "Erode"
  }
  grown <- magick::image_read(encoded) |>
    magick::image_morphology(
      method = method,
      kernel = paste0("Square:", radius)
    )
  raster <- as.matrix(magick::image_raster(grown, tidy = FALSE))
  # Morphology leaves faint interpolated shades along edges, so decide each
  # output pixel by which of the two encoding colors it is nearer to
  # rather than testing for an exact match.
  colors <- unique(as.vector(raster))
  brightness <- retro_ocr_brightness(colors)
  is_foreground <- abs(brightness - foreground_brightness) <=
    abs(brightness - background_brightness)
  names(is_foreground) <- colors
  matrix(
    is_foreground[raster],
    nrow = nrow(raster),
    ncol = ncol(raster)
  )
}

#' @title Mean RGB brightness of hex colors
#' @keywords internal
#' @noRd
#' @description Averages the red, green, and blue channels of each hex
#'   color into a single brightness scalar. Used to decide which of two
#'   encoding colors ImageMagick morphology will actually grow (see
#'   [retro_ocr_dilate()]) and to tell foreground from background colors
#'   apart after a mask round-trips through it.
#' @param color Character vector of hex colors (6- or 8-character).
#' @return Numeric vector of mean channel values, 0-255.
#' @examples
#'   retroglyph:::retro_ocr_brightness(c("#000000", "#ffffff", "#808080"))
#'   # 0, 255, 128 - the mean of the R, G, and B channels for each color
retro_ocr_brightness <- function(color) {
  colMeans(grDevices::col2rgb(retro_color_rgb(color)))
}

#' @title Identify components that are too large in either dimension
#' @keywords internal
#' @noRd
#' @description Given component membership and per-pixel row/column
#'   positions, returns the IDs of components whose row span or column
#'   span meets or exceeds the corresponding maximum.
#' @param membership Integer vector of component IDs (one per vertex).
#' @param rows Integer vector of row positions (one per vertex).
#' @param cols Integer vector of column positions (one per vertex).
#' @param max_height Numeric scalar, row span at or above which a
#'   component is considered oversized.
#' @param max_width Numeric scalar, column span at or above which a
#'   component is considered oversized.
#' @return Integer vector of component IDs to erase.
#' @examples
#'   # Component 1 spans all 10 columns of a 10-wide grid (an axis line);
#'   # component 2 is a compact 2x2 block (e.g. a digit glyph).
#'   membership <- c(rep(1L, 10L), rep(2L, 4L))
#'   rows <- c(rep(5L, 10L), c(8L, 8L, 9L, 9L))
#'   cols <- c(1:10, c(2L, 3L, 2L, 3L))
#'   retroglyph:::retro_ocr_oversized_components(
#'     membership = membership,
#'     rows = rows,
#'     cols = cols,
#'     max_height = 5,
#'     max_width = 5
#'   )
#'   # 1L - only component 1's column span (10) meets max_width (5);
#'   # component 2's row and column spans (2 each) meet neither threshold.
retro_ocr_oversized_components <- function(
  membership,
  rows,
  cols,
  max_height,
  max_width
) {
  component_ids <- unique(membership)
  is_oversized <- vapply(
    component_ids,
    function(component_id) {
      member <- membership == component_id
      row_span <- max(rows[member]) - min(rows[member]) + 1L
      col_span <- max(cols[member]) - min(cols[member]) + 1L
      row_span >= max_height || col_span >= max_width
    },
    logical(1L)
  )
  component_ids[is_oversized]
}

#' @title Run OCR under several segmentation modes and pool the findings
#' @keywords internal
#' @noRd
#' @description Run Tesseract OCR to detect axis tick values and risk
#'   table numbers, once per mode in `segmentation_modes`, filter each
#'   pass's results to numeric text, and pool the passes with
#'   [retro_ocr_pool_findings()]. No single page segmentation mode
#'   reliably finds every tick label and risk table count, so running
#'   several and pooling catches findings that any one mode alone would
#'   miss. Tesseract's `tessedit_char_whitelist` engine option does not
#'   reliably restrict characters under the LSTM engine (c.f.
#'   https://github.com/tesseract-ocr/tesseract/issues/751), so the
#'   numeric filtering happens on the OCR output in R instead of at the
#'   engine. Optionally magnifies the image before detection and rescales
#'   coordinates back.
#' @param input Character scalar, path to an image file.
#' @param confidence Numeric scalar, minimum confidence threshold.
#' @param magnify Numeric scalar, upscale factor.
#' @param segmentation_modes Integer vector, Tesseract
#'   `tessedit_pageseg_mode` values to run in turn.
#' @return A tibble of numeric findings only, with columns `word`,
#'   `confidence`, `x`, `x1`, `x2`, `y`, `y1`, `y2`.
#' @examples
#'   input <- system.file("simulation.png", package = "retroglyph")
#'   gray_input <- tempfile(fileext = ".png")
#'   retroglyph:::retro_image_grayscale(input, gray_input)
#'   findings <- retroglyph:::retro_ocr_detect(
#'     input = gray_input,
#'     confidence = 50,
#'     magnify = 4
#'   )
#'   print(nrow(findings))
#'   print(head(findings))
retro_ocr_detect <- function(
  input,
  confidence,
  magnify,
  segmentation_modes = c(4L, 7L, 11L)
) {
  ocr_input <- input
  image <- magick::image_read(input)
  if (magnify > 1) {
    image <- magick::image_scale(image, paste0(magnify * 100, "%"))
    ocr_input <- tempfile(fileext = ".png")
    on.exit(unlink(ocr_input), add = TRUE)
    magick::image_write(image, ocr_input)
  }
  # tessedit_char_whitelist does not reliably restrict characters under the
  # LSTM engine (c.f. https://github.com/tesseract-ocr/tesseract/issues/4407),
  # so non-numeric findings are filtered out of the OCR output below instead.
  findings_by_mode <- lapply(segmentation_modes, function(mode) {
    engine <- tesseract::tesseract(
      "eng",
      options = list(tessedit_pageseg_mode = as.character(mode))
    )
    result <- tesseract::ocr_data(ocr_input, engine = engine)
    result <- result[result$confidence >= confidence, , drop = FALSE]
    if (nrow(result) == 0L) {
      return(tibble::tibble(
        word = character(0L),
        confidence = numeric(0L),
        x = numeric(0L),
        x1 = integer(0L),
        x2 = integer(0L),
        y = numeric(0L),
        y1 = integer(0L),
        y2 = integer(0L)
      ))
    }
    bounding_box <- strsplit(result$bbox, ",", fixed = TRUE)
    x1 <- vapply(bounding_box, function(b) as.integer(b[1L]), integer(1L)) +
      1L
    y1 <- vapply(bounding_box, function(b) as.integer(b[2L]), integer(1L)) +
      1L
    x2 <- vapply(bounding_box, function(b) as.integer(b[3L]), integer(1L)) +
      1L
    y2 <- vapply(bounding_box, function(b) as.integer(b[4L]), integer(1L)) +
      1L
    if (magnify > 1) {
      x1 <- as.integer(round(x1 / magnify))
      x2 <- as.integer(round(x2 / magnify))
      y1 <- as.integer(round(y1 / magnify))
      y2 <- as.integer(round(y2 / magnify))
    }
    # Strip hyphens entirely - they are OCR artifacts (e.g. tick marks read
    # as "-"), never part of a number. Only positive tick values and risk
    # table counts are expected, so negative numbers are not a case worth
    # handling.
    result$word <- gsub("-", "", result$word, fixed = TRUE)
    findings <- tibble::tibble(
      word = result$word,
      confidence = result$confidence,
      x = (x1 + x2) / 2,
      x1 = x1,
      x2 = x2,
      y = (y1 + y2) / 2,
      y1 = y1,
      y2 = y2
    )
    findings[
      retro_ocr_is_positive_numeric_word(findings$word),
      ,
      drop = FALSE
    ]
  })
  retro_ocr_pool_findings(findings_by_mode)
}

#' @title Test whether a word is a positive number
#' @keywords internal
#' @noRd
#' @description Tesseract's `tessedit_char_whitelist` engine option does
#'   not reliably restrict characters under the LSTM engine (see
#'   [retro_ocr_detect()]), so OCR findings are filtered to numeric text
#'   after OCR runs instead of at the engine. Only positive tick values
#'   and risk table counts are expected, so negative numbers are not a
#'   case worth handling.
#' @param word Character vector of OCR word text.
#' @return Logical vector, `TRUE` where the corresponding `word` is a
#'   positive integer or decimal number.
#' @examples
#'   retroglyph:::retro_ocr_is_positive_numeric_word(
#'     c("12", "3.5", "n=10", "-4", "")
#'   )
#'   # TRUE TRUE FALSE FALSE FALSE
retro_ocr_is_positive_numeric_word <- function(word) {
  grepl("^[0-9]*\\.?[0-9]+$", word)
}

#' @title Pool OCR findings from multiple segmentation-mode passes
#' @keywords internal
#' @noRd
#' @description Combines the findings tibbles from separate Tesseract
#'   passes (one per page segmentation mode; see [retro_ocr_detect()])
#'   into a single tibble, resolving cases where more than one pass
#'   detected the same word. Two findings are treated as the same
#'   detection if their bounding boxes overlap by a positive area -
#'   boxes that merely touch at a shared edge or corner pixel do not
#'   count, since that is rounding noise from [retro_ocr_detect()]'s
#'   `magnify` descaling rather than a real collision. Among overlapping
#'   findings, the one with the higher confidence is kept and the rest
#'   are dropped entirely (no box merging). Ties are broken in favor of
#'   whichever tibble appears earliest in `findings_list`.
#'
#'   The returned rows are sorted in reading order (top to bottom, then
#'   left to right), not by confidence. Confidence is only used to pick
#'   a winner among overlapping candidates above; ordering the final
#'   rows by it would make [retro_ocr_letters()]'s letter assignment
#'   depend on Tesseract's confidence scores, which are not bit-for-bit
#'   reproducible across runs (the LSTM engine is multi-threaded) - two
#'   close scores can flip order between runs and silently reassign a
#'   tick's letter.
#' @param findings_list List of tibbles, each with columns `word`,
#'   `confidence`, `x`, `x1`, `x2`, `y`, `y1`, `y2` (the shape
#'   [retro_ocr_detect()] produces for one segmentation mode).
#' @return A tibble with the same columns, one row per pooled finding,
#'   sorted in reading order.
#' @examples
#'   mode_a <- tibble::tibble(
#'     word = "12", confidence = 80,
#'     x = 3, x1 = 1, x2 = 5, y = 3, y1 = 1, y2 = 5
#'   )
#'   mode_b <- tibble::tibble(
#'     word = "12", confidence = 90,
#'     x = 4, x1 = 2, x2 = 6, y = 3, y1 = 1, y2 = 5
#'   )
#'   pooled <- retroglyph:::retro_ocr_pool_findings(list(mode_a, mode_b))
#'   print(nrow(pooled))
#'   # 1 - the two findings' bounding boxes overlap, so only the
#'   # higher-confidence one (mode_b, confidence 90) survives.
#'   print(pooled$confidence)
#'   # 90
retro_ocr_pool_findings <- function(findings_list) {
  combined <- dplyr::bind_rows(findings_list)
  if (nrow(combined) == 0L) {
    return(combined)
  }
  candidate_order <- order(-combined$confidence, seq_len(nrow(combined)))
  kept_index <- integer(0L)
  for (candidate in candidate_order) {
    overlaps_kept <- combined$x1[candidate] < combined$x2[kept_index] &
      combined$x2[candidate] > combined$x1[kept_index] &
      combined$y1[candidate] < combined$y2[kept_index] &
      combined$y2[candidate] > combined$y1[kept_index]
    if (!any(overlaps_kept)) {
      kept_index <- c(kept_index, candidate)
    }
  }
  reading_order <- retro_ocr_reading_order(
    y1 = combined$y1[kept_index],
    y2 = combined$y2[kept_index],
    x1 = combined$x1[kept_index]
  )
  combined[kept_index[reading_order], , drop = FALSE]
}

#' @title Order findings in reading order, tolerant of row jitter
#' @keywords internal
#' @noRd
#' @description Clusters findings into rows and orders them top to
#'   bottom, then left to right within a row. Sorting on raw `y1` alone
#'   is not robust: two findings on the same visual row (e.g. adjacent
#'   x-axis tick labels) can have `y1` values that differ by a pixel or
#'   two of Tesseract detection jitter, which is enough to flip their
#'   relative order across runs even though [retro_ocr_pool_findings()]
#'   already sorts in reading order - silently swapping which letter
#'   lands on which tick. Rows are instead built by sorting on `y1` and
#'   starting a new row only when the gap to the previous finding
#'   exceeds half the median bounding-box height, so same-row findings
#'   stay grouped regardless of that jitter and are then ordered by
#'   `x1` within the row.
#' @param y1 Integer vector, top edge of each finding's bounding box.
#' @param y2 Integer vector, bottom edge of each finding's bounding box.
#' @param x1 Integer vector, left edge of each finding's bounding box.
#' @return Integer vector, a permutation of `seq_along(y1)` giving
#'   reading order.
#' @examples
#'   # "20" and "24" are the same row (y1 differs by jitter only); "5" is
#'   # a lower row. Reading order should be "20", "24", "5" even though
#'   # "24"'s y1 (11) is briefly higher than "20"'s (9).
#'   order <- retroglyph:::retro_ocr_reading_order(
#'     y1 = c(9, 11, 40),
#'     y2 = c(19, 21, 50),
#'     x1 = c(50, 10, 30)
#'   )
#'   print(order)
#'   # 2 1 3
retro_ocr_reading_order <- function(y1, y2, x1) {
  n <- length(y1)
  if (n <= 1L) {
    return(seq_len(n))
  }
  tolerance <- max(1, stats::median(y2 - y1 + 1L) / 2)
  y_order <- order(y1)
  row_id <- integer(n)
  row_id[y_order[1L]] <- 1L
  for (i in seq(2L, n)) {
    current <- y_order[i]
    previous <- y_order[i - 1L]
    if (y1[current] - y1[previous] > tolerance) {
      row_id[current] <- row_id[previous] + 1L
    } else {
      row_id[current] <- row_id[previous]
    }
  }
  order(row_id, x1)
}

#' @title Generate sequential letter labels
#' @keywords internal
#' @noRd
#' @description Generate a sequence of labels: A, B, ..., Z, AA, AB, ...
#'   Uses a bijective base-25 numbering system with uppercase letters,
#'   excluding Q (see `retro_ocr_alphabet` below).
#' @param n Integer scalar, number of labels to generate.
#' @return Character vector of length `n`.
#' @examples
#'   retroglyph:::retro_ocr_letters(5)
#'   # "A" "B" "C" "D" "E"
#'   retroglyph:::retro_ocr_letters(0)
#'   # character(0)

# Q is excluded from the labeling alphabet: in the annotated screenshots
# passed to the model, a solo capital Q is easily misread as an O, and
# that misread silently maps a finding to the wrong table row.
retro_ocr_alphabet <- LETTERS[LETTERS != "Q"]

retro_ocr_letters <- function(n) {
  if (n == 0L) {
    return(character(0L))
  }
  base <- length(retro_ocr_alphabet)
  vapply(
    seq_len(n),
    function(i) {
      label <- ""
      remaining <- i
      while (remaining > 0L) {
        remaining <- remaining - 1L
        label <- paste0(retro_ocr_alphabet[remaining %% base + 1L], label)
        remaining <- remaining %/% base
      }
      label
    },
    character(1L)
  )
}
