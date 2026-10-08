#' @title Distillation tool constructor
#' @keywords internal
#' @noRd
#' @description Create an `ellmer` tool that distills the quantized image
#'   down to its Kaplan-Meier curves on a flat background: isolate the
#'   plotting panel, classify the curves to one color each, and clean off
#'   the legend keys and stray marks that classification leaves behind.
#' @details The returned tool, when invoked by the model, calls
#'   [retro_do_distill()] on the quantized image in
#'   `state$image_quantized` (from [retro_tool_quantize()]), using the
#'   labeled ticks in `state$data_label` (from [retro_tool_label()]) to
#'   calibrate the axes. It writes `state$image_clean` and, for later
#'   diagnosis, the intermediate `state$image_panel` and
#'   `state$image_classified`. It also populates `state$data_panel`,
#'   `state$data_x`, `state$data_y`, `state$data_background`, and
#'   `state$data_legend`.
#'
#'   The tool returns only the cleaned image to the model; the legend
#'   (each series and its color) is kept in `state$data_legend` for later
#'   tools to consume, not sent back to the model. Inspect the returned
#'   image itself to check whether a series was classified or cleaned
#'   away entirely.
#'
#'   `state$data_legend`'s row order matches `names` exactly.
#'   `state$data_legend` carries a logical `reference` column marking the
#'   reference series identified by the model's `reference` argument. Every
#'   `state$data_*` field populated later by [retro_tool_data()] uses the
#'   same series labels as `state$data_legend$series`.
#' @return An `ellmer` `ToolDef` object.
#' @param state An environment or Shiny `reactiveValues` list where
#'   `state$image_quantized` holds the path to the quantized image (from
#'   [retro_tool_quantize()]) and `state$data_label` holds the
#'   labeled-number findings (from [retro_tool_label()]). The results
#'   listed in the details section are written back into it.
#' @examples
#'   state <- new.env(parent = emptyenv())
#'   tool <- retroglyph:::retro_tool_distill(state)
retro_tool_distill <- function(state) {
  ellmer::tool(
    fun = function(
      x_label,
      x_value,
      y_label,
      y_value,
      background,
      garbage = NULL,
      series,
      names,
      reference
    ) {
      if (is.null(shiny::isolate(state$image_quantized))) {
        stop("Call the quantize tool before distill.")
      }
      if (is.null(shiny::isolate(state$data_label))) {
        stop("Call the label tool before distill.")
      }
      result <- retro_do_distill(
        input = shiny::isolate(state$image_quantized),
        output_clean = tempfile(fileext = ".png"),
        data_label = shiny::isolate(state$data_label),
        x_label = x_label,
        x_value = x_value,
        y_label = y_label,
        y_value = y_value,
        background = background,
        garbage = garbage,
        series = series,
        series_names = names,
        reference = reference
      )
      state$image_clean <- result$image_clean
      state$image_panel <- result$image_panel
      state$image_classified <- result$image_classified
      state$data_panel <- result$data_panel
      state$data_x <- result$x_axis
      state$data_y <- result$y_axis
      state$data_background <- result$background
      state$data_legend <- result$legend
      ellmer::ContentToolResult(
        value = list(
          ellmer::content_image_file(shiny::isolate(state$image_clean))
        ),
        extra = list(
          display = list(
            title = "Distilled image",
            html = as.character(
              htmltools::tags$img(
                src = base64enc::dataURI(
                  file = shiny::isolate(state$image_clean),
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
    name = "distill",
    description = paste(
      "This tool distills the source image down to a flat background and",
      "the Kaplan-Meier pixel channels.",
      "Sequential steps:",
      " - Locate the true x/y axes using the tick labels/values you supply.",
      " - Delete the axes and everything outside the plotting panel.",
      " - Map every remaining pixel to a color palette you supply",
      "   so background has a constant color, garbage colors become background,",
      "   and each KM curve becomes one distinct unique flat color.",
      " - Remove leftover legend keys and stray marks (small connnected shapes).",
      " - Create a legend that maps each KM curve's color to the name you supply for it."
    ),
    arguments = list(
      x_label = ellmer::type_array(
        paste(
          "Capital letter labels of the red bounding boxes of the x axis labels.",
          "These red labels and adjoining red bounding boxes are on the",
          "annotated image from the label tool.",
          "You must choose 2 and only 2 labels, they must be side-by-side,",
          "and they must not repeat any label used in y_label.",
          "When you have more than 2 labels to choose from,",
          "choose ones that are far from where the x and y axes cross,",
          "and choose ones that you can read clearly and unambiguously from the image",
          "with high confidence."
        ),
        items = ellmer::type_string()
      ),
      x_value = ellmer::type_array(
        paste(
          "Numeric x-axis tick values corresponding to the labels in x_label.",
          "Each value is the number printed in the bounding box.",
          "Decimal points can be hard to detect, so if you are unsure if one is present,",
          "use the fact that values increase linearly from a nonnegative start.",
          "X values are time coordinate: always >= 0, increasing left to right."
        ),
        items = ellmer::type_number()
      ),
      y_label = ellmer::type_array(
        paste(
          "Capital letter labels of the red bounding boxes of the y axis labels.",
          "These red labels and adjoining red bounding boxes are on the",
          "annotated image from the label tool.",
          "You must choose 2 and only 2 labels, they must be stacked vertically,",
          "and they must not repeat any label used in x_label.",
          "When you have more than 2 labels to choose from,",
          "choose ones that are far from where the x and y axes cross,",
          "and choose ones that you can read clearly and unambiguously from the image",
          "with high confidence."
        ),
        items = ellmer::type_string()
      ),
      y_value = ellmer::type_array(
        paste(
          "Numeric y-axis tick values corresponding to the labels in y_label.",
          "Each value is the number printed in the bounding box.",
          "Decimal points can be hard to detect, so if you are unsure if one is present,",
          "use the fact that values increase linearly from a nonnegative start.",
          "Y values are a survival measure: always >= 0, increasing bottom to top,",
          "and either a 0-1 probability or a 0-100 percentage - never, say,",
          "105 or 305."
        ),
        items = ellmer::type_number()
      ),
      background = ellmer::type_string(
        paste(
          "Hex color for the background (e.g. '#FFFFFF').",
          "Identify this color by eye from the quantized image returned",
          "by the quantize tool.",
          "This is the most dominant color in the image,",
          "typically white or near-white."
        )
      ),
      garbage = ellmer::type_array(
        paste(
          "Array of hex color strings for non-data elements such as",
          "confidence bands, shaded regions, grid lines, or annotations.",
          "Identify these colors by eye from the quantized image returned",
          "by the quantize tool.",
          "Pixels that map to these colors will be set to background",
          "in the final image. Leave this empty (the default) unless",
          "you see confidence bands, shading, grid lines, or annotations that",
          "are a different color from the curves. Do NOT include colors",
          "here if the image contains only meaningful data curves because",
          "adding unnecessary garbage colors can interfere with the",
          "classification."
        ),
        items = ellmer::type_string(),
        required = FALSE
      ),
      series = ellmer::type_array(
        paste(
          "Array of hex color strings, one per Kaplan-Meier curve.",
          "Read the exact values from the quantize tool's color table.",
          "Each color should be the dominant hue for that curve.",
          "Colors must be clearly distinct from one another.",
          "If you cannot identify a single unambiguous color per curve",
          "(e.g. due to transparency, blending, or overlap artifacts),",
          "the image is not suitable for retroglyph - stop and tell the user."
        ),
        items = ellmer::type_string()
      ),
      names = ellmer::type_array(
        paste(
          "Array of human-readable names, one per entry in series, in the",
          "same order. Each name should identify the group that the curve",
          "represents, as determined from the image legend or surrounding",
          "context.",
          "",
          "Order is never re-sorted for you. Put the names in the risk",
          "table's reading order (top-to-bottom, or left-to-right if the",
          "risk table is arranged horizontally), or the legend's order if",
          "the figure has no risk table.",
          "The reference series is identified separately by the",
          "'reference' argument; it does NOT have to be first in this array.",
          "",
          "For example, c('Drug 10mg', 'Placebo', 'Drug 20mg') is fine",
          "if that is the risk table's (or legend's) row order."
        ),
        items = ellmer::type_string()
      ),
      reference = ellmer::type_string(
        paste(
          "The name of the reference series (e.g. 'Placebo').",
          "Must be one of the entries in the 'names' array.",
          "This series is used as the denominator in hazard ratio",
          "calculations. In a clinical trial with multiple study arms,",
          "this is typically where the control arm goes.",
          "It does NOT need to be the first element of",
          "names; put it wherever it falls in the risk table's (or",
          "legend's) reading",
          "order. If the image has no obvious reference group, pick",
          "whichever series should serve as the denominator - the choice",
          "is positional, not a clinical judgment."
        )
      )
    )
  )
}
