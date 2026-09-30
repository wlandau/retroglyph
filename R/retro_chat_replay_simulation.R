#' @title Replay chat constructor
#' @export
#' @family chats
#' @description Create an `ellmer` chat object that replays a fixed,
#'   real tool-call transcript instead of talking to a model. Useful
#'   for exercising [retro_agent()]/[retro_app()] end to end,
#'   deterministically and without a network call.
#' @details The returned chat is only good for one image: the
#'   `inst/simulation.png` example shipped with the package. Its
#'   `chat()`/`stream_async()` methods replay the exact tool-call
#'   sequence a live model once took to reconstruct that specific
#'   image - quantize, label, distill, then data, each called with the
#'   arguments that model read off `inst/simulation.png` - then return
#'   a canned summary of the result. `chat()` returns that summary
#'   directly, and `stream_async()` returns a promise of it, so
#'   [retro_app()] streams the replay the same way it streams a real
#'   model's response. Because the replayed calls run
#'   the real tools [retro_agent()] registers (not canned tool
#'   results), the state ends up populated exactly as it would from a
#'   real reconstruction of `inst/simulation.png`, but any other image
#'   will not track this chat's hard-coded axis calibration and risk
#'   table, so results are only sensible for `inst/simulation.png`.
#' @return An `ellmer` chat object.
#' @examples
#'   if (identical(Sys.getenv("RETROGLYPH_EXAMPLES"), "true")) {
#'     chat <- retro_chat_replay_simulation()
#'     agent <- retro_agent(chat)
#'     agent$register(system.file("simulation.png", package = "retroglyph"))
#'     agent$chat$chat("Reconstruct the data from the registered plot.")
#'     agent$data()
#'     if (interactive()) {
#'       agent$compare()
#'     }
#'     # Run the Shiny app against the replay chat instead of a real model:
#'     shiny::runApp(retro_chat_replay_simulation())
#'   }
retro_chat_replay_simulation <- function() {
  provider <- retro_chat_mock()$get_provider()
  retro_chat_replay_simulation_class$new(provider = provider)
}

retro_chat_replay_simulation_script <- list(
  distill = list(
    x_label = c("L", "M"),
    x_value = c(30, 36),
    y_label = c("B", "C"),
    y_value = c(75, 50),
    background = "#FFFFFF",
    series = c("#E8743B", "#1B1B3A"),
    names = c("Treatment", "Placebo"),
    reference = "Placebo"
  ),
  data = list(
    risk_patients = c(
      350L,
      298L,
      247L,
      202L,
      168L,
      145L,
      134L,
      350L,
      295L,
      247L,
      202L,
      167L,
      145L,
      126L
    ),
    risk_series = c(rep("Treatment", 7L), rep("Placebo", 7L)),
    risk_x = rep(c(0, 6, 12, 18, 24, 30, 36), 2L)
  ),
  answer = "Done."
)

retro_chat_replay_simulation_class <- R6::R6Class(
  classname = "Chat",
  inherit = ellmer::Chat,
  public = list(
    replayed = FALSE,
    initialize = function(provider) {
      super$initialize(provider = provider)
    },
    # Run the tool-call sequence exactly once against whichever image
    # and tools are currently registered, then return the final
    # assistant answer. Later calls skip straight to the answer
    # instead of re-running tools that error on a second call
    # (quantize, label) with a stale image already registered.
    replay = function() {
      if (self$replayed) {
        return(retro_chat_replay_simulation_script$answer)
      }
      tools <- self$get_tools()
      do.call(tools[["quantize"]], list())
      do.call(tools[["label"]], list())
      do.call(tools[["distill"]], retro_chat_replay_simulation_script$distill)
      do.call(tools[["data"]], retro_chat_replay_simulation_script$data)
      self$replayed <- TRUE
      retro_chat_replay_simulation_script$answer
    },
    chat = function(..., echo = NULL) {
      self$replay()
    },
    # shinychat's stop button only shows while a stream is in flight, and a
    # finished string is not a stream: chat_append() renders it as an
    # already-complete message, so the button never appears. Returning a
    # promise makes this a stream. chat_append() opens the stream (which
    # brings up the stop button) and then waits, so the button is on screen
    # for the whole time the replayed tools take.
    stream_async = function(
      ...,
      type = NULL,
      tool_mode = c("concurrent", "sequential"),
      stream = c("text", "content"),
      controller = NULL
    ) {
      promises::then(
        retro_chat_replay_simulation_pause(),
        function(value) self$replay()
      )
    }
  )
)

# Hand control back to the event loop for a moment. Without the delay, the
# replayed tools could start before Shiny flushes the message that opens the
# stream, and R is busy in those tools until the whole turn is done.
retro_chat_replay_simulation_pause <- function(seconds = 0.1) {
  promises::promise(
    function(resolve, reject) {
      later::later(function() resolve(TRUE), delay = seconds)
    }
  )
}
