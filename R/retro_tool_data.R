#' @title Data reading tool constructor
#' @keywords internal
#' @noRd
#' @description Create an `ellmer` tool that reads the data off the
#'   distilled image: recover the pixels each curve lost to occlusion,
#'   scale the curve pixels against the calibrated axes, and reconstruct
#'   individual patient survival data from the model's reading of the
#'   risk table. Every image must have a risk table.
#' @details The returned tool, when invoked by the model, calls
#'   [retro_do_data()] on the cleaned image in `state$image_clean`
#'   (from [retro_tool_distill()]) and stores the results in
#'   `state$data_path`, `state$data_risk`, `state$data_events`,
#'   `state$data_scaled`, and `state$data_survival`. The axis calibration
#'   and the color/series legend were already recorded by
#'   [retro_tool_distill()]; this tool takes only the risk table and the
#'   optional per-series total events from the model.
#'
#'   Both are read from the original unmodified source image, using the
#'   model's visual and color reasoning. Neither uses the set-of-mark
#'   letter labels at all, so there is no `risk_label` argument. Series
#'   names are validated against `state$data_legend`.
#'
#'   Risk tables report the numbers of patients still at risk (still
#'   under observation, i.e. neither censored nor a case yet) at each
#'   time point in `risk_patients` — this is the quantity
#'   [retro_data_survival()] reconstructs from. Because patients can only
#'   leave a risk set, these counts must be non-increasing over time
#'   within a series; a value that goes back up almost always means a
#'   digit from a different row (events, deaths, censored) was read
#'   into the wrong place.
#'
#'   `events_total`/`events_series` are separate and optional: one total
#'   events count per series, over the whole follow-up period, which
#'   sharpens the censoring estimate in the final interval. The argument
#'   descriptions carry the reliability ordering the model is asked to
#'   respect — a number the user stated outranks one read off the figure,
#'   which outranks a count of censoring tick marks (which the model is
#'   told never to do). Coverage may be partial, so a series with no stated
#'   total is simply left out; see [retro_data_events()].
#'
#'   The tool returns only a short confirmation string to the model; the
#'   risk table it recorded and the total events table (when one was
#'   supplied) are kept in `state$data_risk` and `state$data_events` for
#'   later use, not sent back to the model.
#'
#'   The `series` column of `state$data_risk`, `state$data_events`,
#'   `state$data_scaled`, and `state$data_survival` (if produced) is a
#'   character vector with the same values
#'   as `state$data_legend$series` - [retro_data_risk()] and
#'   [retro_data_events()] build their
#'   `series` column from `state$data_legend$series`'s values, and
#'   [retro_data_scale()] and [retro_data_survival()] each inherit their
#'   `series` column directly from their inputs, so every object agrees on
#'   which series exist and which one is the reference (marked by the
#'   `reference` column in `state$data_legend`).
#' @return An `ellmer` `ToolDef` object.
#' @param state An environment or Shiny `reactiveValues` list where
#'   `state$image_clean` holds the path to the cleaned image,
#'   `state$data_label` holds the labeled-number findings (from
#'   [retro_tool_label()]), and `state$data_legend`, `state$data_x`,
#'   `state$data_y` hold the legend and axis calibration (from
#'   [retro_tool_distill()]). `state$data_path`, `state$data_risk`,
#'   `state$data_events`, `state$data_scaled`, and `state$data_survival`
#'   will hold the results. `state$data_events` is `NULL` when no series
#'   reported a total events count, so unlike the others it is not a
#'   usable signal that this tool has run.
#' @examples
#'   state <- new.env(parent = emptyenv())
#'   tool <- retroglyph:::retro_tool_data(state)
retro_tool_data <- function(state) {
  ellmer::tool(
    fun = function(
      risk_patients,
      risk_series,
      risk_x,
      events_total = NULL,
      events_series = NULL
    ) {
      if (is.null(shiny::isolate(state$image_clean))) {
        stop("Call the distill tool before the data tool.")
      }
      result <- retro_do_data(
        input = shiny::isolate(state$image_clean),
        data_label = shiny::isolate(state$data_label),
        legend = shiny::isolate(state$data_legend),
        x_axis = shiny::isolate(state$data_x),
        y_axis = shiny::isolate(state$data_y),
        risk_patients = risk_patients,
        risk_series = risk_series,
        risk_x = risk_x,
        events_total = events_total,
        events_series = events_series
      )
      state$data_path <- result$path
      state$data_risk <- result$risk
      state$data_events <- result$events
      state$data_scaled <- result$scaled
      state$data_survival <- result$survival
      image_survival <- shiny::isolate(retro_data_preview(state))
      ellmer::ContentToolResult(
        value = "Reconstructed the data.",
        extra = list(
          display = list(
            title = "Re-plotted reconstructed survival data",
            html = as.character(
              htmltools::tags$img(
                src = base64enc::dataURI(
                  file = image_survival,
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
    name = "data",
    description = paste(
      "Read the data off the distilled image and record the risk table.",
      "Call this tool AFTER the distill tool.",
      "",
      "Read the risk table from the ORIGINAL, unmodified source image",
      "(the first image you were shown), using your own visual and color",
      "reasoning, never the set-of-mark letter labels - match each row to",
      "a series by color and position, and read each column's time point",
      "off the image."
    ),
    arguments = list(
      risk_patients = ellmer::type_array(
        paste(
          "Numeric risk table values: the number of patients STILL AT",
          "RISK (still under observation) at that time point - never an",
          "events, deaths, or censored count. Must be non-increasing over",
          "time within each series. Required: every image must have a risk",
          "table; supply together with risk_series and risk_x."
        ),
        items = ellmer::type_integer(),
        required = TRUE
      ),
      risk_series = ellmer::type_array(
        paste(
          "Series name (from the distill tool) for each risk table",
          "entry, same length and order as risk_patients. Required",
          "(non-empty) for every entry.",
          "Series names must exactly match the ones from the",
          "distill tool's legend."
        ),
        items = ellmer::type_string(),
        required = TRUE
      ),
      risk_x = ellmer::type_array(
        paste(
          "The x-axis (time) coordinate of each risk table entry, same",
          "length and order as risk_patients, read from the risk table's",
          "time-point columns. Need not fall within the two x-axis",
          "calibration ticks distill used - use whichever values the",
          "risk table's columns actually align with. Required."
        ),
        items = ellmer::type_number(),
        required = TRUE
      ),
      events_total = ellmer::type_array(
        paste(
          "Optional. ONE number per KM curve: the TOTAL number of",
          "events (deaths, progressions, etc.) that curve accumulated over",
          "the WHOLE follow-up period.",
          "Some images state this information directly.",
          "In other cases, the user may volunteer values.",
          "If this information is not available, it is always safe to omit."
        ),
        items = ellmer::type_integer(),
        required = FALSE
      ),
      events_series = ellmer::type_array(
        paste(
          "Optional. The KM curve name (from the distill tool) each",
          "events_total entry belongs to, same length and order as",
          "events_total. Required (non-empty) for every entry whenever",
          "events_total is supplied, and omitted whenever it is not.",
          "At most ONE entry per KM curve. A subset of the curves is fine: name",
          "only the curves you have a stated total for, and leave the rest",
          "out entirely."
        ),
        items = ellmer::type_string(),
        required = FALSE
      )
    )
  )
}

#' @title Render the reconstructed survival data with axes, for display only
#' @keywords internal
#' @noRd
#' @description Re-plots `state$data_survival` with
#'   [retro_image_layer_survival()], then draws the axes and tick labels
#'   back on with [retro_image_ruler()], so [retro_tool_data()]'s tool
#'   card shows a labeled figure instead of a bare curve on a flat
#'   background. Combines the two calls in one place because both are
#'   display-only plumbing for the tool card: this image is never sent
#'   back to the model, unlike the images the label and distill tools
#'   return.
#' @return Character scalar, path to the rendered PNG.
#' @param state An environment or Shiny `reactiveValues` list with
#'   `state$data_survival`, `state$data_legend`, `state$data_background`,
#'   `state$data_x`, `state$data_y`, `state$data_panel`, `state$data_path`,
#'   `state$data_scaled`, and `state$data_label` already populated - true
#'   by the time [retro_tool_data()]'s tool function reaches this call.
#' @examples
#'   state <- new.env(parent = emptyenv())
#'   state$data_survival <- tibble::tibble(
#'     series = rep("Placebo", 6),
#'     time = c(1, 2, 3, 4, 5, 6),
#'     status = c(1, 0, 1, 0, 1, 0)
#'   )
#'   state$data_legend <- tibble::tibble(series = "Placebo", color = "#dc3030")
#'   state$data_background <- "#ffffff"
#'   state$data_x <- tibble::tibble(
#'     label = c("A", "B"), value = c(0, 10),
#'     x = c(5, 55), y = c(95, 95),
#'     x1 = c(3L, 53L), x2 = c(7L, 57L), y1 = c(93L, 93L), y2 = c(97L, 97L)
#'   )
#'   state$data_y <- tibble::tibble(
#'     label = c("C", "D"), value = c(0, 1),
#'     x = c(3, 3), y = c(95, 5),
#'     x1 = c(1L, 1L), x2 = c(5L, 5L), y1 = c(93L, 3L), y2 = c(97L, 7L)
#'   )
#'   state$data_panel <- tibble::tibble(
#'     x1 = 5L, x2 = 55L, y1 = 5L, y2 = 95L, pad_x1 = 2L, pad_y2 = -2L
#'   )
#'   state$data_path <- tibble::tibble(
#'     x = 5L, y = 5L, color = "#dc3030", line_width = 2L,
#'     width = 60L, height = 100L
#'   )
#'   state$data_scaled <- tibble::tibble(
#'     series = "Placebo", max_y = 1, increasing = FALSE
#'   )
#'   state$data_label <- tibble::tibble(
#'     label = c("A", "B", "C", "D"),
#'     word = c("0", "10", "0", "1"),
#'     x = c(5, 55, 3, 3),
#'     x1 = c(3L, 53L, 1L, 1L), x2 = c(7L, 57L, 5L, 5L),
#'     y = c(95, 95, 95, 5),
#'     y1 = c(93L, 93L, 93L, 3L), y2 = c(97L, 97L, 97L, 7L)
#'   )
#'   output <- retroglyph:::retro_data_preview(state)
#'   if (interactive()) {
#'     browseURL(output)
#'   }
retro_data_preview <- function(state) {
  legend <- state$data_legend
  layered <- tempfile(fileext = ".png")
  on.exit(unlink(layered))
  output <- tempfile(fileext = ".png")
  retro_image_layer_survival(
    data = state$data_survival,
    output = layered,
    layers = legend$color,
    background = state$data_background,
    x_axis = state$data_x,
    y_axis = state$data_y,
    legend = legend,
    width = state$data_path$width[1L],
    height = state$data_path$height[1L],
    line_width = state$data_path$line_width[1L],
    max_y = state$data_scaled$max_y[1L],
    increasing = state$data_scaled$increasing[1L]
  )
  retro_image_ruler(
    input = layered,
    output = output,
    data_panel = state$data_panel,
    data_label = state$data_label,
    x_axis = state$data_x,
    y_axis = state$data_y
  )
  output
}
