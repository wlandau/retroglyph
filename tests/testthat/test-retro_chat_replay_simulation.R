test_that("retro_chat_replay_simulation() reconstructs simulation.png", {
  skip_on_cran()
  chat <- retro_chat_replay_simulation()
  agent <- retro_agent(chat)
  agent$register(system.file("simulation.png", package = "retroglyph"))
  agent$chat$chat("Reconstruct the data from the registered plot.")
  data <- agent$data()
  expect_s3_class(data, "data.frame")
  expect_setequal(colnames(data), c("series", "time", "status"))
  expect_setequal(unique(data$series), c("Treatment", "Placebo"))
  expect_true(all(data$time >= 0))
  expect_true(all(data$status %in% c(0, 1)))
  expect_identical(
    agent$chat$chat("Reconstruct the data from the registered plot."),
    "Done."
  )
})

test_that("retro_chat_replay_simulation()$stream_async() replays the script", {
  skip_on_cran()
  chat <- retro_chat_replay_simulation()
  agent <- retro_agent(chat)
  agent$register(system.file("simulation.png", package = "retroglyph"))
  # stream_async() returns a promise, so the app can stream the replay the
  # same way it streams a real model's response.
  answer <- NULL
  promises::then(agent$chat$stream_async(), function(value) answer <<- value)
  while (is.null(answer)) {
    later::run_now(timeoutSecs = 0.1)
  }
  expect_identical(answer, "Done.")
  data <- agent$data()
  expect_s3_class(data, "data.frame")
})
