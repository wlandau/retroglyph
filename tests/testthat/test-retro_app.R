library(shinytest2)

test_that("retro_app() reconstructs simulation.png via replay chat", {
  skip_on_cran()
  # shinytest2 statically inspects the server function for globals and warns
  # that it cannot locate `agent`, which is created via delayedAssign(). The
  # app works fine; muffle that one warning so the test output stays clean.
  app <- withCallingHandlers(
    AppDriver$new(
      retro_app(retro_chat_replay_simulation()),
      name = "retro_app",
      # The replay runs the real image-processing tools (quantize, label,
      # distill, data), which keeps R busy far longer than the 4-second
      # shinytest2 default. Every wait_for_idle() below inherits this.
      timeout = 120000
    ),
    warning = function(w) {
      if (grepl("Failed to locate globals", conditionMessage(w))) {
        invokeRestart("muffleWarning")
      }
    }
  )
  app$wait_for_idle()

  # Step 1: upload the simulation image
  app$upload_file(
    image_file = system.file("simulation.png", package = "retroglyph")
  )

  # Step 2: enter "do" in the shinychat interface.
  app$set_inputs(
    chat_user_input = "do",
    allow_no_input_binding_ = TRUE,
    priority_ = "event"
  )
  # The turn runs in a shiny::ExtendedTask, which deliberately leaves the
  # session unblocked while the stream is in flight, so wait_for_idle()
  # returns long before the replayed tools finish. Poll the chat window in
  # the browser until the assistant message actually lands.
  app$wait_for_js(
    paste0(
      "document.querySelector('.shiny-chat-messages-content')",
      ".textContent.includes('Done.')"
    ),
    timeout = 120000
  )
  app$wait_for_idle()

  # The replay chat returns "Done." as the assistant message
  chat_html <- app$get_html(".shiny-chat-messages-content")
  expect_match(chat_html, "Done\\.")

  # Each tab's outputs use suspendWhenHidden = TRUE by default,
  # so they only compute once their tab is shown.
  expect_table_populated <- function(html) {
    rows <- lengths(regmatches(html, gregexpr("<tr", html)))
    expect_gt(rows, 1) # header row + at least one data row
  }

  # Text review: risk table (agent$risk_table)
  app$set_inputs(main_tabs = "Text review")
  app$wait_for_idle()
  risk_table <- app$get_value(output = "risk_table")
  expect_table_populated(risk_table)
  expect_match(risk_table, "Treatment")
  expect_match(risk_table, "Placebo")

  # Statistical review: counts, quantiles, probabilities
  app$set_inputs(main_tabs = "Statistical review")
  app$wait_for_idle()

  counts_table <- app$get_value(output = "counts_table")
  expect_table_populated(counts_table)
  expect_match(counts_table, "patients")
  expect_match(counts_table, "events")

  quantiles_table <- app$get_value(output = "quantiles_table")
  expect_table_populated(quantiles_table)
  expect_match(quantiles_table, "time_lower")

  probabilities_table <- app$get_value(output = "probabilities_table")
  expect_table_populated(probabilities_table)
  expect_match(probabilities_table, "survival_lower")

  # Data download: verify the downloaded CSV (agent$data)
  app$set_inputs(main_tabs = "Data download")
  app$wait_for_idle()
  expect_match(app$get_html("#download_ui"), "download_data")

  download_path <- tempfile()
  app$get_download("download_data", filename = download_path)
  downloaded <- read.csv(download_path)
  expect_gt(nrow(downloaded), 0)
  expect_setequal(names(downloaded), c("series", "time", "status"))
  for (field in c("series", "time", "status")) {
    expect_false(anyNA(downloaded[[field]]))
  }
  expect_setequal(unique(downloaded$series), c("Treatment", "Placebo"))
  expect_true(all(downloaded$time >= 0))
  expect_true(all(downloaded$status %in% c(0, 1)))
})
