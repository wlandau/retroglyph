#' @title Launch a `retroglyph` Shiny app.
#' @export
#' @family apps
#' @description Create and return a Shiny app that wraps a
#'   [retro_agent()] in a browser-based UI for uploading Kaplan-Meier
#'   images, chatting with the model, and reviewing the reconstructed
#'   image, risk table, hazard ratios, and survival quantiles/probabilities.
#' @details `chat` is cloned once per Shiny session (via `chat$clone(deep = TRUE)`),
#'   so the same `chat` object can be reused across multiple concurrent app
#'   sessions without one session's conversation or registered tools leaking
#'   into another's.
#' @return A Shiny app object from `shiny::shinyApp()`.
#' @param chat An `ellmer` chat object with no registered tools, as in
#'   [retro_agent()]. Cloned once per session.
#' @param theme A `bslib` theme object from `bslib::bs_theme()`, passed to
#'   `bslib::page_sidebar()`. The default is the Bootstrap 5 `"flatly"`
#'   preset with a dark navy primary color. The app puts a light/dark mode
#'   toggle (`bslib::input_dark_mode()`) in the top right of the main panel
#'   and starts in light mode. Bootstrap 5 color modes mean a single theme
#'   covers both modes.
#' @param authentication_ui `NULL`, or a Shiny tag/taglist to insert into
#'   the app's UI, outside the main navigation panels. Use this to layer
#'   on authentication UI (e.g. an SSO login gate) without `retroglyph`
#'   itself depending on any authentication package.
#' @param authentication_server `NULL`, or a function with arguments
#'   `input`, `output`, `session`, called first inside the app's server
#'   function. Use this together with `authentication_ui` to layer on
#'   server-side authentication logic.
#' @param onStart See `shiny::shinyApp()`.
#' @param options See `shiny::shinyApp()`.
#' @examples
#'   if (identical(Sys.getenv("RETROGLYPH_EXAMPLES"), "true")) {
#'     retro_app(retro_chat_replay_simulation())
#'  }
retro_app <- function(
  chat,
  theme = bslib::bs_theme(version = 5, preset = "flatly", primary = "#2C3E50"),
  authentication_ui = NULL,
  authentication_server = NULL,
  onStart = NULL,
  options = list()
) {
  stopifnot(
    "chat must be an ellmer chat object" = inherits(chat, "Chat"),
    "chat must have no registered tools" = length(chat$get_tools()) == 0L,
    "theme must be a bslib theme from bs_theme()" = inherits(theme, "bs_theme")
  )
  shiny::addResourcePath(
    "images",
    system.file(package = "retroglyph")
  )
  shiny::shinyApp(
    ui = retro_app_ui(theme = theme, authentication_ui = authentication_ui),
    server = retro_app_server(
      chat = chat,
      authentication_server = authentication_server
    ),
    onStart = onStart,
    options = options
  )
}

retro_app_ui <- function(theme, authentication_ui) {
  title <- paste(
    "retroglyph: agentic Kaplan-Meier reconstruction (version",
    paste0(as.character(utils::packageVersion("retroglyph")), ")")
  )
  bslib::page_navbar(
    title = title,
    id = "main_tabs",
    fillable = FALSE,
    theme = theme,
    header = shiny::tagList(
      retro_app_preload_shinychat(),
      retro_app_greeting_style(),
      authentication_ui
    ),
    sidebar = bslib::sidebar(
      title = "Image upload and chat window",
      width = "30vw",
      shiny::fileInput(
        "image_file",
        "Step 1: upload a Kaplan-Meier image.",
        accept = c(".png", ".jpg", ".jpeg", ".svg")
      ),
      shinychat::chat_ui(
        "chat",
        height = "100%",
        greeting = retro_app_greeting(),
        enable_cancel = TRUE
      ),
      style = "height: 100%;"
    ),
    bslib::nav_panel(
      "About",
      bslib::card(
        full_screen = TRUE,
        bslib::card_body(
          shiny::includeMarkdown(
            system.file("app", "about.md", package = "retroglyph")
          )
        )
      )
    ),
    bslib::nav_panel(
      "Image review",
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Original (old) vs reconstructed (new) image"),
        bslib::layout_columns(
          shiny::uiOutput("ui_front"),
          shiny::uiOutput("ui_data")
        ),
        diffviewer::visual_diff_output(
          "diff",
          width = "100%",
          height = "auto"
        )
      )
    ),
    bslib::nav_panel(
      "Text review",
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Original image"),
        shiny::imageOutput("original_image", height = "400px")
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Reconstructed risk table"),
        shiny::tableOutput("risk_table")
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Reconstructed total event counts (optional)"),
        shiny::tableOutput("events_table")
      )
    ),
    bslib::nav_panel(
      "Statistical review",
      bslib::layout_columns(
        col_widths = c(6, 6, 6, 6),
        bslib::card(
          full_screen = TRUE,
          bslib::card_header("Total counts"),
          shiny::tableOutput("counts_table")
        ),
        bslib::card(
          full_screen = TRUE,
          bslib::card_header("Hazard ratios"),
          shiny::tableOutput("hazard_ratio_table")
        ),
        bslib::card(
          full_screen = TRUE,
          bslib::card_header("Survival quantiles"),
          shiny::selectizeInput(
            "select_probabilities",
            label = "Cumulative event probabilities",
            choices = seq(0.05, 0.95, by = 0.05),
            selected = c(0.25, 0.5, 0.75),
            multiple = TRUE,
            options = list(create = TRUE, plugins = list("remove_button"))
          ),
          shiny::tableOutput("quantiles_table")
        ),
        bslib::card(
          full_screen = TRUE,
          bslib::card_header("Survival probabilities"),
          shiny::selectizeInput(
            "select_quantiles",
            label = "Time points",
            choices = character(0),
            multiple = TRUE,
            options = list(create = TRUE, plugins = list("remove_button"))
          ),
          shiny::tableOutput("probabilities_table")
        )
      )
    ),
    bslib::nav_panel(
      "Data download",
      bslib::card(
        bslib::card_header("Before you download"),
        bslib::card_body(
          shiny::includeMarkdown(
            system.file("app", "download.md", package = "retroglyph")
          )
        )
      ),
      bslib::card(
        bslib::card_header("Download reconstructed data"),
        bslib::card_body(
          shiny::includeMarkdown(
            system.file("app", "data.md", package = "retroglyph")
          ),
          shiny::uiOutput("download_ui")
        )
      )
    ),
    # Pushed to the right edge of the navbar. Bootstrap 5 color modes do
    # the rest: the toggle flips data-bs-theme, and the theme supplies both
    # modes, so there is no second theme to maintain. mode = "light" starts
    # every session in light mode rather than following the browser.
    bslib::nav_spacer(),
    bslib::nav_item(bslib::input_dark_mode(mode = "light"))
  )
}

# shinychat's greeting (including the "suggestion" span in
# retro_app_greeting()) does not render until shinychat's own JS bundle loads
# and hydrates. That <script> tag sits last in <head>, after several other
# packages' (diffviewer, selectize) blocking <script> tags, so the browser
# does not even start fetching it until those finish. A preload hint lets the
# browser fetch it in parallel with everything ahead of it instead.
retro_app_preload_shinychat <- function() {
  version <- as.character(utils::packageVersion("shinychat"))
  shiny::tags$head(
    shiny::tags$link(
      rel = "preload",
      as = "script",
      crossorigin = "anonymous",
      href = paste0("shinychat-", version, "/shinychat.js")
    ),
    shiny::tags$link(
      rel = "preload",
      as = "style",
      href = paste0("shinychat-", version, "/shinychat.css")
    )
  )
}

# shinychat centers the greeting vertically (margin-block: auto) while the
# chat has no messages yet. !important beats that rule without having to
# restate its selector, whose ">" combinators tags$style() would escape.
retro_app_greeting_style <- function() {
  shiny::tags$head(
    shiny::tags$style(
      htmltools::HTML(
        ".shiny-chat-greeting {margin-block: 0 !important; padding: 0 !important;}"
      )
    )
  )
}

retro_app_greeting <- function() {
  paste(
    "Step 2: prompt the AI to reconstruct the data.",
    "Click the suggestion below, or write your own prompt.",
    "",
    paste0(
      "* <span class=\"suggestion\">",
      "Reconstruct the data from the uploaded image.",
      "</span>"
    ),
    sep = "\n"
  ) |>
    shinychat::chat_greeting(persistent = TRUE)
}

# Run one chat turn and stream the response into the chat window. Called
# from an ExtendedTask, so `controller` stays reachable from the session
# while the stream is still going.
retro_app_turn <- function(chat, user_input, controller) {
  # shinychat >= 0.5.0 supplies a list of content items (text and any
  # attachments), which ellmer expects as individual arguments.
  stream <- do.call(
    chat$stream_async,
    c(
      as.list(user_input),
      list(stream = "content", controller = controller)
    )
  )
  shinychat::chat_append("chat", stream)
}

retro_app_server <- function(chat, authentication_server) {
  # nocov start
  function(input, output, session) {
    if (!is.null(authentication_server)) {
      authentication_server(input, output, session) #nocov
    }

    state <- shiny::reactiveValues()
    delayedAssign(
      x = "agent",
      value = retro_agent(chat = chat$clone(deep = TRUE), state = state)
    )

    shiny::observeEvent(input$image_file, {
      extension <- tools::file_ext(input$image_file$name)
      path <- paste0(input$image_file$datapath, ".", extension)
      file.copy(input$image_file$datapath, path)
      agent$register(path)
    })

    output$ui_data <- shiny::renderUI({
      shiny::req(state$data_legend)
      shiny::radioButtons(
        "select_data",
        label = "Data for the reconstructed (new) image",
        choices = c(
          "Reconstructed survival data",
          "Reconstructed censoring times",
          "Recaptured pixel trace"
        ),
        selected = "Reconstructed survival data"
      )
    })

    output$ui_front <- shiny::renderUI({
      legend <- state$data_legend
      shiny::req(legend)
      shiny::radioButtons(
        "select_front",
        label = "Curve in foreground",
        choices = stats::setNames(
          seq_along(legend$series),
          as.character(legend$series)
        ),
        selected = 1L
      )
    })

    controller <- ellmer::stream_controller()

    # ExtendedTask unblocks this session while the stream is in flight, so
    # the stop button of chat_ui(enable_cancel = TRUE) shows up and the
    # chat_cancel observer below can actually run controller$cancel()
    # before the response finishes:
    # https://shiny.posit.co/r/reference/shiny/latest/extendedtask
    task <- shiny::ExtendedTask$new(function(user_input, controller) {
      retro_app_turn(
        chat = agent$chat,
        user_input = user_input,
        controller = controller
      )
    })

    shiny::observeEvent(input$chat_user_input, {
      task$invoke(input$chat_user_input, controller)
    })

    shiny::observeEvent(input$chat_cancel, {
      controller$cancel()
    })

    output$diff <- diffviewer::visual_diff_render({
      shiny::req(
        state$image_source,
        state$data_survival,
        input$select_front,
        input$select_data
      )
      agent$compare(
        front = as.integer(input$select_front),
        data = switch(
          input$select_data,
          "Reconstructed survival data" = "survival",
          "Reconstructed censoring times" = "censoring",
          "Recaptured pixel trace" = "trace"
        )
      )
    })

    output$original_image <- shiny::renderImage(
      {
        shiny::req(state$image_source)
        list(
          src = state$image_source,
          contentType = "image/png",
          alt = "Original image",
          height = "100%",
          style = "object-fit: contain;"
        )
      },
      deleteFile = FALSE
    )

    output$risk_table <- shiny::renderTable({
      shiny::req(state$data_risk)
      agent$risk_table(mode = "transposed")
    })

    output$events_table <- shiny::renderTable({
      shiny::req(state$data_events)
      agent$events_table()
    })

    output$counts_table <- shiny::renderTable({
      shiny::req(state$data_survival)
      agent$counts()
    })

    output$hazard_ratio_table <- shiny::renderTable({
      shiny::req(state$data_survival)
      agent$hazard_ratios()
    })

    shiny::observeEvent(
      state$data_survival,
      {
        example_times <- round(
          stats::quantile(state$data_survival$time, probs = c(0.5, 0.75)),
          digits = 1
        )
        shiny::updateSelectizeInput(
          session,
          "select_quantiles",
          choices = as.character(example_times),
          selected = as.character(example_times)
        )
      },
      once = TRUE
    )

    output$quantiles_table <- shiny::renderTable({
      shiny::req(state$data_survival, input$select_probabilities)
      agent$quantiles(
        probabilities = as.numeric(input$select_probabilities),
        type = "survival"
      )
    })

    output$probabilities_table <- shiny::renderTable({
      shiny::req(state$data_survival, input$select_quantiles)
      agent$probabilities(quantiles = as.numeric(input$select_quantiles))
    })

    output$download_ui <- shiny::renderUI({
      if (is.null(state$data_survival)) {
        shiny::tags$p(
          class = "text-muted",
          "(No reconstructed data available yet.)"
        )
      } else {
        shiny::downloadButton("download_data", "Download CSV")
      }
    })

    output$download_data <- shiny::downloadHandler(
      filename = function() {
        "reconstructed_survival_data.csv"
      },
      content = function(file) {
        utils::write.csv(agent$data(), file, row.names = FALSE)
      }
    )

    shiny::outputOptions(output, "download_ui", suspendWhenHidden = FALSE)
    shiny::outputOptions(output, "download_data", suspendWhenHidden = FALSE)
  }
  # nocov end
}
