#' @title Quantization tool constructor
#' @keywords internal
#' @noRd
#' @description Create an `ellmer` tool that reduces the source image to at
#'   most 256 colors so the model can read colors directly off a simplified
#'   image instead of the full-color original. This is the first tool in the
#'   workflow: it must be called before label, distill, or data.
#' @details The returned tool, when invoked by the model, calls
#'   [retro_image_quantize()] on the source image stored in
#'   `state$image_source`, with `n_colors` fixed at 256. The tool takes no
#'   arguments because there is never a reason to change `n_colors` or to
#'   call the tool a second time on the same image (the result is always
#'   identical). The tool writes the quantized image path to
#'   `state$image_quantized`.
#'
#'   The quantized image is what [retro_tool_distill()] later operates on:
#'   because the model reads colors for `distill`'s
#'   `background`/`garbage`/`series` arguments by eye from this image,
#'   distillation must run on the same quantized image so that
#'   [retro_image_classify()]'s nearest-color mapping matches what the model
#'   saw.
#' @return An `ellmer` `ToolDef` object.
#' @param state An environment or Shiny `reactiveValues` list where
#'   `state$image_source` holds the path to the source image, and
#'   `state$image_quantized` will hold the result.
#' @examples
#'   state <- new.env(parent = emptyenv())
#'   tool <- retroglyph:::retro_tool_quantize(state)
retro_tool_quantize <- function(state) {
  ellmer::tool(
    fun = function() {
      if (!is.null(shiny::isolate(state$image_quantized))) {
        stop(
          "The quantize tool has already been called once. ",
          "It is pointless and wasteful to call it again, ",
          "because it always returns the same result."
        )
      }
      state$image_quantized <- tempfile(fileext = ".png")
      retro_image_quantize(
        input = shiny::isolate(state$image_source),
        output = shiny::isolate(state$image_quantized),
        n_colors = 256L
      )
      ellmer::ContentToolResult(
        value = list(
          ellmer::content_image_file(shiny::isolate(state$image_quantized))
        ),
        extra = list(
          display = list(
            title = "Quantized image",
            html = as.character(
              htmltools::tags$img(
                src = base64enc::dataURI(
                  file = shiny::isolate(state$image_quantized),
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
    name = "quantize",
    description = paste(
      "Reduce the source image to at most 256 colors and return the",
      "quantized image. Call this tool FIRST and EXACTLY ONCE for the",
      "whole conversation. It will throw an error if called again, so",
      "don't do this. Repeat calls are pointless and wasteful because the",
      "tool always returns the same result.",
      "",
      "The tool takes no arguments.",
      "",
      "Read colors for the distill tool's background, garbage, and series",
      "arguments by eye directly off this quantized image. Quantization",
      "simplifies the image down to a small number of flat colors, which",
      "makes reading colors by eye easier and more reliable than reading",
      "them off the full-color source image."
    )
  )
}
