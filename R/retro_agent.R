#' @title `retroglyph` agent constructor.
#' @export
#' @family agents
#' @description Create a `retroglyph` agent object.
#' @return A `retro_agent` `R6` object.
#' @param chat An `ellmer` chat object with no registered tools.
#'   It is best to use a pragmatic low-cost model (e.g. `"sonnet"`)
#'   for the chat, since sending images to a frontier model
#'   can be expensive.
#' @param state An empty environment or empty Shiny `reactiveValues` list to
#'   hold agent state.
#'   The state must be completely empty when passed to `retro_agent()`,
#'   and it should be reserved for the agent's use only.
#' @examples
#'   if (nzchar(Sys.getenv("ANTHROPIC_API_KEY"))) {
#'     chat <- ellmer::chat_anthropic()
#'     agent <- retro_agent(chat)
#'     agent$chat$chat("ping")
#'   }
retro_agent <- function(
  chat,
  state = new.env(parent = emptyenv())
) {
  stopifnot(
    "chat must be an ellmer chat object" = inherits(chat, "Chat"),
    "chat must have no registered tools" = length(chat$get_tools()) == 0L,
    "state must be an environment or reactiveValues" = is.environment(state) ||
      inherits(state, "reactivevalues")
  )
  if (!is.null(chat$get_system_prompt())) {
    warning(
      "Overriding existing system prompt with the retroglyph prompt.",
      call. = FALSE
    )
  }
  prompt <- system.file(
    "prompts",
    "system.md",
    package = "retroglyph",
    mustWork = TRUE
  )
  chat$set_system_prompt(readLines(prompt, warn = FALSE))
  chat$register_tool(retro_tool_quantize(state))
  chat$register_tool(retro_tool_label(state))
  chat$register_tool(retro_tool_distill(state))
  chat$register_tool(retro_tool_data(state))
  retro_agent_class$new(chat = chat, state = state)
}

#' @title retro_agent R6 class
#' @description R6 class for a `retroglyph` agent with an `ellmer`
#'   chat and associated state.
retro_agent_class <- R6::R6Class(
  classname = "retro_agent",
  public = list(
    #' @field chat The inner `ellmer` chat object.
    chat = NULL,
    #' @field state Environment or Shiny `reactiveValues` list
    #'   holding the state of the agent.
    state = NULL,
    #' @description Create a new `retro_agent` object.
    #' @param chat An `ellmer` chat object.
    #' @param state An environment for agent state.
    initialize = function(chat, state) {
      self$chat <- chat
      self$state <- state
    },
    #' @description Register a source image for reconstruction: normalize it
    #'   to an opaque PNG, store it in `state$image_source`, and optionally
    #'   clear the chat's turn history. This is the half of reconstruction
    #'   that needs no model call, so a Shiny app can call it as soon as the
    #'   user uploads a file - before the chat loop (driven by `shinychat`'s
    #'   or `ellmer`'s own round trip to the model, triggered separately by
    #'   the user typing into the chat) ever starts. [retro_agent_class]
    #'   $reconstruct() calls this method itself, so most callers never need
    #'   to call it directly. Resets `state$image_quantized` and
    #'   `state$data_label` to `NULL` so the four-tool workflow
    #'   (quantize → label → distill → data) starts fresh.
    #' @param file Character scalar, path to the source image file.
    #'   Only PNG, SVG, and JPEG files are supported.
    #' @param clear Logical scalar. If `TRUE`, wipe the
    #'   chat's turn history, so the next chat turn begins with a fresh
    #'   conversation and no trace of prior images or turns. The system
    #'   prompt and registered tools are untouched. Set to `FALSE` to keep
    #'   the prior conversation turns, e.g. to reconstruct a new image
    #'   within an ongoing conversation.
    #' @return `NULL`, invisibly.
    register = function(file, clear = TRUE) {
      stopifnot(
        "file must be a single character string" = is.character(file) &&
          length(file) == 1L,
        "file does not exist" = file.exists(file),
        "file must be PNG, SVG, or JPEG" = tolower(tools::file_ext(file)) %in%
          c("png", "svg", "jpg", "jpeg"),
        "clear must be a single non-missing logical" = is.logical(clear) &&
          length(clear) == 1L &&
          !is.na(clear)
      )
      if (clear) {
        self$chat$set_turns(list())
      }
      self$state$image_source <- tempfile(fileext = ".png")
      self$state$image_quantized <- NULL
      self$state$data_label <- NULL
      # Every source format, PNG included, goes through the same entry point,
      # so the whole downstream pipeline is guaranteed an opaque PNG raster.
      retro_image_png(input = file, output = self$state$image_source)
      invisible(NULL)
    },
    #' @description Run the full reconstruction workflow on a source image:
    #'   register the file (see [retro_agent_class]$register()), then chat
    #'   with the model to view the image, label the numbers, distill the
    #'   image down to its curves, and read the data off them.
    #' @param file Character scalar, path to the source image file.
    #'   Only PNG, SVG, and JPEG files are supported.
    #' @param prompt Character scalar, a user prompt to send along
    #'   with the image. Contains optional user-provided
    #'   instructions or context. The most useful thing to put here is
    #'   each arm's total number of events, if the publication reports it
    #'   in the text rather than in the figure: the model is instructed to
    #'   trust a number you state over anything it reads off the image,
    #'   and a total events count sharpens the estimated censoring in each
    #'   arm's final interval (see [retro_agent_class]$events_table()).
    #' @param clear Logical scalar. If `TRUE`, wipe the
    #'   chat's turn history before reconstructing, so this call begins
    #'   with a fresh conversation and no trace of prior images or
    #'   turns. The system prompt and registered tools are untouched.
    #'   Set to `FALSE` to keep the prior conversation turns, e.g. to
    #'   ask follow-up questions about an already-reconstructed image.
    #' @param timeout Numeric scalar, the number of seconds of wall
    #'   clock time to allow the whole conversation before giving up.
    #'   The limit exists so a model that
    #'   loses its way cannot keep taking turns and spending tokens
    #'   indefinitely. Set to `Inf` to let the conversation run as long
    #'   as it likes. `ellmer` has no equivalent setting:
    #'   `getOption("ellmer_timeout_s")` bounds a single HTTP request,
    #'   not the whole tool-calling loop.
    #' @return `NULL`, invisibly.
    reconstruct = function(file, prompt = "", clear = TRUE, timeout = 600) {
      stopifnot(
        "prompt must be a single character string" = is.character(prompt) &&
          length(prompt) == 1L,
        "timeout must be a single positive number" = is.numeric(timeout) &&
          length(timeout) == 1L &&
          !is.na(timeout) &&
          timeout > 0
      )
      self$register(file, clear = clear)
      # A blown deadline surfaces as a TimeoutException between turns and as
      # an interrupt if it lands inside the HTTP request curl is waiting on.
      deadline <- proc.time()[["elapsed"]] + timeout
      stopped <- function(condition) {
        if (proc.time()[["elapsed"]] < deadline) {
          stop("reconstruct() was interrupted.", call. = FALSE)
        }
        stop(
          "reconstruct() timed out after ",
          timeout,
          " seconds. Whatever the agent finished before the deadline is ",
          "still in the agent's state. Raise the timeout argument to give ",
          "the model more time.",
          call. = FALSE
        )
      }
      tryCatch(
        R.utils::withTimeout(
          self$chat$chat(
            paste(
              "Reconstruct the data from the registered Kaplan-Meier plot.",
              "Follow the workflow stated in the system prompt."
            ),
            prompt
          ),
          # Only wall clock time counts. The tools do heavy image work, and a
          # CPU limit would cut them off long before the conversation is long.
          cpu = Inf,
          elapsed = timeout,
          onTimeout = "error"
        ),
        TimeoutException = stopped,
        interrupt = stopped
      )
      invisible(NULL)
    },
    #' @description Access the color/series legend produced by the
    #'   distill tool.
    #' @param friendly Logical scalar. If `TRUE`, the
    #'   `color` column holds the nearest friendly `grDevices::colors()`
    #'   name (e.g. `"firebrick"`) instead of the raw hex code, via the
    #'   same lookup used elsewhere in the package
    #'   (`grDevices::colors()`). Set to `FALSE` to keep the raw hex
    #'   codes.
    #' @return A tibble with columns `series`, `color`, and `reference`
    #'   (from `state$data_legend`), or `NULL` if the distill tool has not
    #'   run yet.
    legend = function(friendly = TRUE) {
      stopifnot(
        "friendly must be a single non-missing logical" = is.logical(
          friendly
        ) &&
          length(friendly) == 1L &&
          !is.na(friendly)
      )
      legend <- self$state$data_legend
      if (friendly && !is.null(legend)) {
        legend$color <- retro_color_name(legend$color)
      }
      legend
    },
    #' @description Access the risk table read by the data tool, in one of
    #'   three layouts.
    #' @param mode Character scalar, one of `"transposed"`,
    #'   `"wide"`, or `"long"`. `"long"` is the layout `state$data_risk`
    #'   already uses: one row per series/time entry, columns `series`,
    #'   `x`, `patients`. `"wide"` pivots that into one row per unique `x`
    #'   and one column per series, holding `patients` (`NA` where a
    #'   series has no entry at that `x`). `"transposed"` transposes the
    #'   wide layout again into one row per series and one column per time
    #'   point, the layout most published risk tables use directly beneath
    #'   their Kaplan-Meier figure.
    #' @return A tibble in the layout named by `mode` (see [retro_table_wide()]
    #'   and [retro_table_transpose()] for the `"wide"` and `"transposed"`
    #'   shapes), or `NULL` if the data tool has not run yet.
    risk_table = function(mode = "transposed") {
      stopifnot(
        "mode must be a single string, one of \"transposed\", \"wide\", or \"long\"" = is.character(
          mode
        ) &&
          length(mode) == 1L &&
          !is.na(mode) &&
          mode %in% c("transposed", "wide", "long")
      )
      risk_table <- self$state$data_risk
      if (is.null(risk_table)) {
        return(NULL)
      }
      if (mode == "long") {
        return(risk_table)
      }
      wide_table <- retro_table_wide(risk_table)
      if (mode == "wide") {
        return(wide_table)
      }
      retro_table_transpose(wide_table)
    },
    #' @description Access the per-arm total events counts read by the
    #'   data tool. Optional throughout: unlike the risk table, total
    #'   events is not required, and coverage may be partial, so a `NULL`
    #'   return does not mean the data tool failed to run. Worth
    #'   reviewing when it is present - the model reads the number off the
    #'   source image (or takes it from your own prompt), and it changes
    #'   the reconstruction by sharpening the estimated censoring in each
    #'   arm's final interval.
    #' @return A tibble with columns `series` and `events`, at most one
    #'   row per series (from `state$data_events`, the return value of
    #'   [retro_data_events()]), or `NULL` if the data tool has not run
    #'   yet or no arm reported a total.
    events_table = function() {
      events_table <- self$state$data_events
      if (is.null(events_table)) {
        return(NULL)
      }
      events_table
    },
    #' @description Access the reconstructed survival data. This is an
    #'   interpreted survival reconstruction: individual patient
    #'   time-to-event and censoring status, inferred from the digitized,
    #'   scaled curve data produced internally by the data tool together
    #'   with the risk table counts via the Guyot et al. (2012) algorithm (see
    #'   [retro_data_survival()]).
    #' @return A tibble with columns `series`, `time`, and `status`
    #'   (from `state$data_survival`, the return value of
    #'   [retro_data_survival()]), or `NULL` if the data tool has not run
    #'   yet.
    data = function() {
      self$state$data_survival
    },
    #' @description Fit a proportional hazards model to the reconstructed
    #'   individual patient survival data ([retro_agent_class]$data()) and
    #'   report the hazard ratio of every other series relative to a
    #'   chosen reference series (see
    #'   [retro_survival_hazard_ratios()]).
    #' @param reference Character scalar, the series to treat as the
    #'   reference (denominator) level.
    #' @param confidence Numeric scalar strictly between 0 and 1, the
    #'   confidence level for the `lower`/`upper` interval.
    #' @return A tibble with columns `series`, `estimate`, `lower`,
    #'   `upper`, and `p_value` (see [retro_survival_hazard_ratios()]), one
    #'   row per series other than `reference`.
    hazard_ratios = function(
      reference = {
        leg <- self$legend(friendly = FALSE)
        leg$series[leg$reference]
      },
      confidence = 0.95
    ) {
      if (is.null(self$state$data_survival)) {
        stop(
          "state$data_survival missing - call reconstruct() first (again, ",
          "if it errored).",
          call. = FALSE
        )
      }
      retro_survival_hazard_ratios(
        self$state$data_survival,
        reference = reference,
        confidence = confidence
      )
    },
    #' @description Compute Kaplan-Meier survival quantiles (e.g. median
    #'   survival) for each series in the reconstructed individual patient
    #'   survival data ([retro_agent_class]$data()), at the probabilities
    #'   requested.
    #'   The digitized, scaled curve data produced internally by the data
    #'   tool is deliberately never used for this, even as a fallback: it makes
    #'   no statistical claims, and reconstruction does not record
    #'   whether its y-axis was survival or cumulative incidence, so a
    #'   quantile read off it would not be statistically defensible.
    #' @param probabilities Numeric vector of probabilities between 0 and
    #'   1, interpreted according to `type`: as target survival
    #'   probabilities or as target cumulative incidence
    #'   probabilities. Order does not
    #'   matter, and duplicates (after converting to a common scale) are
    #'   dropped - the returned tibble always reports both `survival` and
    #'   `incidence` for each requested probability, sorted
    #'   chronologically. A series with a low overall event rate may
    #'   never reach a given probability within the observed follow-up -
    #'   see `time` below.
    #' @param type Character string, either `"survival"` or
    #'   `"incidence"`. Controls how `probabilities` is interpreted:
    #'   `"survival"` treats each value as a target survival probability
    #'   (fraction still alive); `"incidence"` treats each value as a
    #'   target cumulative incidence probability (fraction with the event),
    #'   i.e. `1 - survival`.
    #' @param confidence Numeric scalar strictly between 0 and 1, the
    #'   confidence level for the `time_lower`/`time_upper` interval.
    #' @return A tibble with columns `series`, `survival`, `incidence`
    #'   (`1 - survival`), `time`, `time_lower`, and `time_upper`, one row
    #'   per series/probability combination, sorted chronologically
    #'   (increasing `time`, i.e. decreasing `survival`) within each
    #'   series.
    #'   `time` (and `time_lower`/`time_upper`) is `NA` for any probability
    #'   the reconstructed survival curve never reaches.
    quantiles = function(
      probabilities = c(0.25, 0.5, 0.75),
      type = c("survival", "incidence"),
      confidence = 0.95
    ) {
      type <- match.arg(type)
      if (is.null(self$state$data_survival)) {
        stop(
          "state$data_survival missing - call reconstruct() first (again, ",
          "if it errored).",
          call. = FALSE
        )
      }
      retro_survival_quantiles(
        self$state$data_survival,
        probabilities = if (type == "incidence") {
          1 - probabilities
        } else {
          probabilities
        },
        confidence = confidence
      )
    },
    #' @description Compute Kaplan-Meier survival probabilities (e.g.
    #'   12-month survival) for each series in the reconstructed individual
    #'   patient survival data ([retro_agent_class]$data()), at the times
    #'   requested. This is the inverse of [retro_agent_class]$quantiles():
    #'   instead of asking "at what time is a target survival probability
    #'   reached", it asks "what is the survival probability at a target
    #'   time".
    #'   The digitized, scaled curve data produced internally by the data
    #'   tool is deliberately never used for this, even as a fallback: it makes
    #'   no statistical claims, and reconstruction does not record
    #'   whether its y-axis was survival or cumulative incidence, so a
    #'   probability read off it would not be statistically defensible.
    #' @param quantiles Numeric vector of non-negative time points, e.g.
    #'   `c(6, 12, 24)` for the survival probabilities at 6, 12, and 24
    #'   months. Named `quantiles` (not `time`) to mirror
    #'   [retro_agent_class]$quantiles()'s `probabilities` argument -
    #'   despite the name, these are time points, not probabilities. Order
    #'   does not matter, and duplicates are dropped - the returned tibble
    #'   is always sorted chronologically. A requested time beyond a
    #'   series' observed follow-up still returns the Kaplan-Meier estimate
    #'   at that time, extended flat from the last observation.
    #' @param confidence Numeric scalar strictly between 0 and 1, the
    #'   confidence level for the `survival_lower`/`survival_upper`
    #'   interval.
    #' @return A tibble with columns `series`, `time`, `survival`,
    #'   `survival_lower`, `survival_upper`, `incidence` (`1 - survival`),
    #'   `incidence_lower` (`1 - survival_lower`), and `incidence_upper`
    #'   (`1 - survival_upper`), one row per series/time combination,
    #'   sorted chronologically (increasing `time`) within each series.
    probabilities = function(
      quantiles,
      confidence = 0.95
    ) {
      if (is.null(self$state$data_survival)) {
        stop(
          "state$data_survival missing - call reconstruct() first (again, ",
          "if it errored).",
          call. = FALSE
        )
      }
      retro_survival_probabilities(
        self$state$data_survival,
        quantiles = quantiles,
        confidence = confidence
      )
    },
    #' @description Count the number of patients and events per series in the
    #'   reconstructed individual patient survival data
    #'   ([retro_agent_class]$data()). One row per legend row, in legend
    #'   order, plus a final total row summing across all series (see
    #'   [retro_survival_counts()]).
    #' @return A tibble with columns `series`, `patients`, and `events` (see
    #'   [retro_survival_counts()]), one row per legend row in legend order,
    #'   plus a final `"total"` row.
    counts = function() {
      if (is.null(self$state$data_survival)) {
        stop(
          "state$data_survival missing - call reconstruct() first (again, ",
          "if it errored).",
          call. = FALSE
        )
      }
      retro_survival_counts(
        self$state$data_survival,
        self$state$data_legend
      )
    },
    #' @description Visually compare the source image against a freshly
    #'   generated impression, with axes drawn back in and one series
    #'   brought to the front. Two sources are available for the impression
    #'   (see the `data` argument): the reconstructed survival data
    #'   (`state$data_survival`) refit to a Kaplan-Meier curve per series,
    #'   which validates the thing the package actually exists to produce
    #'   rather than the raw digitized pixels; or the raw digitized trace
    #'   (`state$data_scaled`) before reconstruction, which isolates
    #'   whether a disagreement traces back to retroglyph's own
    #'   digitization or to `IPDfromKM`'s reconstruction. Each series is
    #'   drawn out to its own last reconstructed observation, so arms with
    #'   shorter follow-up end earlier in the impression than arms with
    #'   longer follow-up, exactly as they do in the source figure. Assumes
    #'   `reconstruct()` has already completed successfully.
    #' @param front Integer scalar, the row of the legend (1 to
    #'   `nrow(state$data_legend)`) whose series is drawn on top in the
    #'   impression; the remaining series are layered beneath it in
    #'   legend order.
    #' @param data Character scalar, either `"survival"` to
    #'   render the reconstructed survival data (`state$data_survival`)
    #'   refit to a Kaplan-Meier curve (see [retro_image_layer_survival()]),
    #'   or `"trace"` to render the raw digitized trace (`state$data_scaled`)
    #'   with no refit (see [retro_image_layer_trace()]) - useful for
    #'   telling apart a retroglyph digitization problem from an
    #'   `IPDfromKM` reconstruction problem.
    #' @return An HTML widget (from `diffviewer::visual_diff()`)
    #'   comparing `state$image_source` (old, ground truth) to the
    #'   generated impression (new).
    compare = function(front = 1L, data = "survival") {
      stopifnot(
        "state$image_source missing - call reconstruct() first (again, if it errored)." = !is.null(
          self$state$image_source
        ),
        "front must be a single non-missing integer" = is.numeric(front) &&
          length(front) == 1L &&
          !is.na(front),
        "data must be a single string, either \"survival\" or \"trace\"" = is.character(
          data
        ) &&
          length(data) == 1L &&
          !is.na(data) &&
          data %in% c("survival", "trace")
      )
      required <- c(
        "data_legend",
        "data_background",
        "data_path",
        "data_panel",
        "data_label",
        "data_x",
        "data_y",
        "data_scaled",
        "data_survival",
        "data_risk"
      )
      missing <- required[
        vapply(
          required,
          function(field) is.null(self$state[[field]]),
          logical(1L)
        )
      ]
      if (length(missing) > 0L) {
        stop(
          "state$",
          paste(missing, collapse = ", state$"),
          " missing - call reconstruct() first (again, if it errored).",
          call. = FALSE
        )
      }
      legend <- self$state$data_legend
      stopifnot(
        "front must be between 1 and nrow(legend)" = front >= 1 &&
          front <= nrow(legend)
      )
      layered <- tempfile(fileext = ".png")
      on.exit(unlink(layered))
      impression <- tempfile(fileext = ".png")
      on.exit(unlink(impression), add = TRUE)
      if (data == "survival") {
        layer <- retro_image_layer_survival
        data <- self$state$data_survival
      } else {
        layer <- retro_image_layer_trace
        data <- self$state$data_scaled
      }
      layer(
        data = data,
        output = layered,
        layers = c(legend$color[front], legend$color[-front]),
        background = self$state$data_background,
        x_axis = self$state$data_x,
        y_axis = self$state$data_y,
        legend = legend,
        width = self$state$data_path$width[1L],
        height = self$state$data_path$height[1L],
        line_width = self$state$data_path$line_width[1L],
        max_y = self$state$data_scaled$max_y[1L],
        increasing = self$state$data_scaled$increasing[1L]
      )
      retro_image_ruler(
        input = layered,
        output = impression,
        data_panel = self$state$data_panel,
        data_label = self$state$data_label,
        x_axis = self$state$data_x,
        y_axis = self$state$data_y
      )
      diffviewer::visual_diff(
        file_old = self$state$image_source,
        file_new = impression
      )
    },
    #' @description Save the reconstruction to disk under the `output`
    #'   directory (created if it doesn't already exist), split into two
    #'   subdirectories:
    #'   * `compare/`: one self-contained HTML comparison widget per legend
    #'     row, each bringing that row's series to the front (see
    #'     [retro_agent_class]$compare()). Files are named
    #'     `<series>_<color-name>.html`, where `<color-name>` is the
    #'     nearest built-in R color name (from `grDevices::colors()`) to
    #'     the series' hex color.
    #'   * `data/`: `risk_table.csv` (see [retro_agent_class]$risk_table()),
    #'     `events_table.csv` (see [retro_agent_class]$events_table()),
    #'     `data.csv` (see [retro_agent_class]$data()), `counts.csv`
    #'     (see [retro_agent_class]$counts()), `quantiles.csv`
    #'     (see [retro_agent_class]$quantiles()), `probabilities.csv`
    #'     (see [retro_agent_class]$probabilities()), and `hazard_ratios.csv`
    #'     (see [retro_agent_class]$hazard_ratios()) whenever those are
    #'     non-`NULL`. `events_table.csv` is absent whenever no arm
    #'     reported a total events count, which is common and not an
    #'     error. `probabilities.csv` is absent unless `quantiles` (below)
    #'     is supplied.
    #'   Assumes `reconstruct()` has already completed successfully.
    #' @param output Character scalar, path to a directory to write the
    #'   `compare/` and `data/` subdirectories into.
    #'   `export()` deletes `output` before writing to it, so be careful
    #'   about the choice of output directory.
    #' @param data Character scalar, either `"survival"` or
    #'   `"trace"`, forwarded to [retro_agent_class]$compare() for every
    #'   legend row - see its `data` argument.
    #' @param probabilities Numeric vector, forwarded to
    #'   [retro_agent_class]$quantiles() for `quantiles.csv`.
    #' @param type Character string, forwarded to
    #'   [retro_agent_class]$quantiles() for `quantiles.csv`.
    #' @param quantiles Numeric vector, forwarded to
    #'   [retro_agent_class]$probabilities() for `probabilities.csv`.
    #'   Defaults to `NULL`, which skips `probabilities.csv` - there is no
    #'   dataset-agnostic default set of time points.
    #' @param confidence Numeric scalar, forwarded to both
    #'   [retro_agent_class]$quantiles() (for `quantiles.csv`) and
    #'   [retro_agent_class]$probabilities() (for `probabilities.csv`).
    #' @return `NULL`, invisibly.
    export = function(
      output,
      data = "survival",
      probabilities = c(0.25, 0.5, 0.75),
      type = c("survival", "incidence"),
      quantiles = NULL,
      confidence = 0.95
    ) {
      type <- match.arg(type)
      stopifnot(
        "output must be a single non-empty string" = is.character(output) &&
          length(output) == 1L &&
          !is.na(output) &&
          nzchar(output),
        "state$data_legend missing - call reconstruct() first (again, if it errored)." = !is.null(
          self$state$data_legend
        ),
        "state$data_scaled missing - call reconstruct() first (again, if it errored)." = !is.null(
          self$state$data_scaled
        )
      )
      unlink(output, recursive = TRUE, force = TRUE)
      compare_directory <- file.path(output, "compare")
      data_directory <- file.path(output, "data")
      dir.create(compare_directory, recursive = TRUE)
      dir.create(data_directory, recursive = TRUE)
      legend <- self$state$data_legend
      for (index in seq_len(nrow(legend))) {
        slug <- paste0(
          retro_slug(legend$series[index]),
          "_",
          retro_slug(retro_color_name(legend$color[index]))
        )
        file <- file.path(compare_directory, paste0(slug, ".html"))
        htmlwidgets::saveWidget(
          self$compare(index, data = data),
          file,
          selfcontained = TRUE
        )
        # saveWidget() means to remove its own intermediate "_files"
        # directory, but its cleanup path is relative to the working
        # directory rather than to `file`, so it silently fails to
        # delete the directory whenever the two differ. Remove it here.
        unlink(
          file.path(compare_directory, paste0(slug, "_files")),
          recursive = TRUE
        )
      }
      risk_table <- self$risk_table(mode = "long")
      if (!is.null(risk_table)) {
        utils::write.csv(
          risk_table,
          file.path(data_directory, "risk_table.csv"),
          row.names = FALSE
        )
      }
      events_table <- self$events_table()
      if (!is.null(events_table)) {
        utils::write.csv(
          events_table,
          file.path(data_directory, "events_table.csv"),
          row.names = FALSE
        )
      }
      data <- self$data()
      if (!is.null(data)) {
        utils::write.csv(
          data,
          file.path(data_directory, "data.csv"),
          row.names = FALSE
        )
        utils::write.csv(
          self$counts(),
          file.path(data_directory, "counts.csv"),
          row.names = FALSE
        )
        utils::write.csv(
          self$quantiles(
            probabilities = probabilities,
            type = type,
            confidence = confidence
          ),
          file.path(data_directory, "quantiles.csv"),
          row.names = FALSE
        )
        if (!is.null(quantiles)) {
          utils::write.csv(
            self$probabilities(
              quantiles = quantiles,
              confidence = confidence
            ),
            file.path(data_directory, "probabilities.csv"),
            row.names = FALSE
          )
        }
        utils::write.csv(
          self$hazard_ratios(),
          file.path(data_directory, "hazard_ratios.csv"),
          row.names = FALSE
        )
      }
      invisible(NULL)
    }
  )
)
