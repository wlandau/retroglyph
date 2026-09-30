#' @title Number labeling tool constructor
#' @keywords internal
#' @noRd
#' @description Create an `ellmer` tool that labels the numbers in the
#'   source image, so the model can name an axis tick and have the pixels
#'   that tick was measured at come along with the name.
#' @details The returned tool, when invoked by the model, calls
#'   [retro_do_label()] on the source image stored in `state$image_source`,
#'   with `confidence` and `magnify` fixed at 50 and 4. Those settings are
#'   not exposed as tool arguments: the tool takes no arguments at all,
#'   because a repeat call with the same fixed settings on the same image
#'   returns an identical result, so there is never a reason to call this
#'   tool more than once. The tool writes the annotated image to
#'   `state$image_label` and the findings tibble to `state$data_label`.
#'
#'   The result carries one image and nothing else: a grayscale rendering
#'   of the source annotated with set-of-mark labels (red bounding boxes
#'   and sequential letter labels). The model already saw the image's
#'   colors from [retro_tool_quantize()] (the preceding step); this image
#'   is deliberately grayscale so the red set-of-mark annotations stand
#'   out clearly. The OCR findings are kept in `state$data_label` for
#'   later tools to consume programmatically; the model itself never sees
#'   them as data and must instead read each labeled number by eye off
#'   the returned image.
#'
#'   This is the second tool in the workflow (after quantize), so it
#'   requires `state$image_quantized` to be non-`NULL`. It runs on the
#'   full, uncropped source image - detections may include numbers inside
#'   the plotting panel as well as axis ticks and risk table counts. The
#'   model picks two labeled ticks on each axis from the annotated
#'   output, and [retro_tool_distill()] resolves those letter labels back
#'   to pixel positions to calibrate both axes.
#'
#'   Detection runs through OCR, restricted to numeric characters
#'   (`0123456789.-`).
#' @return An `ellmer` `ToolDef` object.
#' @param state An environment or Shiny `reactiveValues` list where
#'   `state$image_source` holds the path to the source image, and
#'   `state$image_label` / `state$data_label` will hold the results.
#' @examples
#'   state <- new.env(parent = emptyenv())
#'   tool <- retroglyph:::retro_tool_label(state)
retro_tool_label <- function(state) {
  ellmer::tool(
    fun = function() {
      if (is.null(shiny::isolate(state$image_quantized))) {
        stop("Call the quantize tool before label.")
      }
      if (!is.null(shiny::isolate(state$data_label))) {
        stop(
          "The label tool has already been called once. ",
          "It is pointless and wasteful to call it again, ",
          "because it always returns the same result."
        )
      }
      state$image_label <- tempfile(fileext = ".png")
      state$data_label <- retro_do_label(
        input = shiny::isolate(state$image_source),
        output = shiny::isolate(state$image_label),
        confidence = 50,
        magnify = 4
      )
      ellmer::ContentToolResult(
        value = list(
          ellmer::content_image_file(shiny::isolate(state$image_label))
        ),
        extra = list(
          display = list(
            title = "Labeled image",
            html = as.character(
              htmltools::tags$img(
                src = base64enc::dataURI(
                  file = shiny::isolate(state$image_label),
                  mime = "image/png"
                ),
                style = "max-width: 100%; height: auto;"
              )
            ),
            open = FALSE,
            full_screen = TRUE,
            show_request = FALSE
          )
        )
      )
    },
    name = "label",
    description = paste(
      "Label the numbers in the source image. Call this tool SECOND,",
      "right after quantize, and EXACTLY ONCE for the whole conversation.",
      "It will throw an error if called again, so don't do this.",
      "Repeat calls are pointless and wasteful",
      "because the tool always returns the same result.",
      "",
      "The tool returns ONE image: the source image annotated with red",
      "bounding boxes and sequential letter labels (A, B, C, ...) around",
      "each number OCR detected (restricted to digits, decimal points,",
      "and minus signs: 0123456789.-).",
      "The tool returns no other data. Read each labeled number's value",
      "by eye directly off this image, using its bounding box and letter",
      "label to identify which number it is."
    )
  )
}
