test_that("retro_agent() returns correct class", {
  chat <- retro_chat_mock()
  agent <- retro_agent(chat)
  expect_s3_class(agent, "retro_agent")
})

test_that("retro_agent() sets system prompt", {
  chat <- retro_chat_mock()
  agent <- retro_agent(chat)
  prompt <- agent$chat$get_system_prompt()
  expect_true(nzchar(prompt))
  expect_true(grepl("reconstruction", prompt))
})

test_that("retro_agent() stores state", {
  chat <- retro_chat_mock()
  state <- new.env(parent = emptyenv())
  agent <- retro_agent(chat, state = state)
  expect_identical(agent$state, state)
})

test_that("retro_agent() warns on existing system prompt", {
  chat <- retro_chat_mock()
  chat$set_system_prompt("existing prompt")
  expect_warning(retro_agent(chat), "Overriding")
})

test_that("retro_agent() rejects chat with tools", {
  chat <- retro_chat_mock()
  chat$register_tool(ellmer::tool(
    fun = function() "hi",
    name = "dummy",
    description = "dummy tool"
  ))
  expect_error(retro_agent(chat), "no registered tools")
})

test_that("retro_agent() rejects invalid state type", {
  chat <- retro_chat_mock()
  expect_error(retro_agent(chat, state = list()), "environment or reactive")
})

test_that("retro_agent$register() validates file", {
  chat <- retro_chat_mock()
  agent <- retro_agent(chat)
  expect_error(agent$register(123), "character string")
  expect_error(agent$register("nonexistent.png"), "does not exist")
  file <- tempfile(fileext = ".bmp")
  file.create(file)
  on.exit(unlink(file))
  expect_error(agent$register(file), "PNG, SVG, or JPEG")
})

test_that("retro_agent$register() validates clear", {
  chat <- retro_chat_mock()
  agent <- retro_agent(chat)
  file <- system.file("simulation.png", package = "retroglyph")
  expect_error(agent$register(file, clear = "yes"), "clear must be")
  expect_error(agent$register(file, clear = NA), "clear must be")
  expect_error(agent$register(file, clear = c(TRUE, FALSE)), "clear must be")
})

test_that("retro_agent$register() returns invisibly and stores the source", {
  chat <- retro_chat_mock()
  agent <- retro_agent(chat)
  file <- system.file("simulation.png", package = "retroglyph")
  result <- withVisible(agent$register(file))
  expect_null(result$value)
  expect_false(result$visible)
  expect_true(file.exists(agent$state$image_source))
  # PNG sources pass through byte-for-byte.
  expect_equal(
    tools::md5sum(file),
    tools::md5sum(agent$state$image_source),
    ignore_attr = TRUE
  )
})

test_that("retro_agent$register() resets image_quantized and data_label", {
  chat <- retro_chat_mock()
  agent <- retro_agent(chat)
  agent$state$image_quantized <- "some_path.png"
  agent$state$data_label <- tibble::tibble(label = "A")
  file <- system.file("simulation.png", package = "retroglyph")
  agent$register(file)
  expect_null(agent$state$image_quantized)
  expect_null(agent$state$data_label)
})

test_that("retro_agent$register() normalizes SVG and JPEG to PNG", {
  svg <- paste0(
    "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"40\" height=\"30\">",
    "<rect width=\"40\" height=\"30\" fill=\"#3030a0\"/></svg>"
  )
  svg_file <- tempfile(fileext = ".svg")
  on.exit(unlink(svg_file))
  writeLines(svg, svg_file)
  jpg_file <- system.file("images", "building.jpg", package = "magick")
  for (file in c(svg_file, jpg_file)) {
    agent <- retro_agent(retro_chat_mock())
    agent$register(file)
    expect_match(agent$state$image_source, "[.]png$")
    info <- magick::image_info(magick::image_read(agent$state$image_source))
    expect_equal(info$format, "PNG")
  }
})

test_that("retro_agent$register() clears chat turns by default", {
  chat <- retro_chat_mock()
  agent <- retro_agent(chat)
  calls <- list()
  agent$chat <- list(
    set_turns = function(value) calls[[length(calls) + 1L]] <<- value
  )
  file <- system.file("simulation.png", package = "retroglyph")
  agent$register(file)
  expect_length(calls, 1L)
  expect_identical(calls[[1L]], list())
})

test_that("retro_agent$register(clear = FALSE) keeps chat turns", {
  chat <- retro_chat_mock()
  agent <- retro_agent(chat)
  set_turns_calls <- 0L
  agent$chat <- list(
    set_turns = function(value) set_turns_calls <<- set_turns_calls + 1L
  )
  file <- system.file("simulation.png", package = "retroglyph")
  agent$register(file, clear = FALSE)
  expect_identical(set_turns_calls, 0L)
})

test_that("retro_agent$reconstruct() validates inputs", {
  chat <- retro_chat_mock()
  agent <- retro_agent(chat)
  expect_error(agent$reconstruct(123), "character string")
  expect_error(agent$reconstruct("nonexistent.png"), "does not exist")
  file <- tempfile(fileext = ".bmp")
  file.create(file)
  on.exit(unlink(file))
  expect_error(agent$reconstruct(file), "PNG, SVG, or JPEG")
})

test_that("retro_agent$reconstruct() copies source and returns invisibly", {
  chat <- retro_chat_mock()
  agent <- retro_agent(chat)
  # Stub the chat method so it doesn't call the provider.
  agent$chat <- list(
    chat = function(...) invisible(NULL),
    set_turns = function(value) invisible(NULL)
  )
  local_mocked_bindings(
    tesseract = function(...) NULL,
    ocr_data = function(...) {
      tibble::tibble(
        word = character(0),
        confidence = numeric(0),
        bbox = character(0)
      )
    },
    .package = "tesseract"
  )
  file <- system.file("simulation.png", package = "retroglyph")
  result <- agent$reconstruct(file)
  expect_null(result)
  expect_true(file.exists(agent$state$image_source))
  # PNG sources pass through byte-for-byte.
  expect_equal(
    tools::md5sum(file),
    tools::md5sum(agent$state$image_source),
    ignore_attr = TRUE
  )
})

test_that("retro_agent$reconstruct() clears chat turns by default", {
  chat <- retro_chat_mock()
  agent <- retro_agent(chat)
  calls <- list()
  agent$chat <- list(
    chat = function(...) invisible(NULL),
    set_turns = function(value) calls[[length(calls) + 1L]] <<- value
  )
  local_mocked_bindings(
    tesseract = function(...) NULL,
    ocr_data = function(...) {
      tibble::tibble(
        word = character(0),
        confidence = numeric(0),
        bbox = character(0)
      )
    },
    .package = "tesseract"
  )
  file <- system.file("simulation.png", package = "retroglyph")
  agent$reconstruct(file)
  expect_length(calls, 1L)
  expect_identical(calls[[1L]], list())
})

test_that("retro_agent$reconstruct(clear = FALSE) keeps chat turns", {
  chat <- retro_chat_mock()
  agent <- retro_agent(chat)
  set_turns_calls <- 0L
  agent$chat <- list(
    chat = function(...) invisible(NULL),
    set_turns = function(value) set_turns_calls <<- set_turns_calls + 1L
  )
  local_mocked_bindings(
    tesseract = function(...) NULL,
    ocr_data = function(...) {
      tibble::tibble(
        word = character(0),
        confidence = numeric(0),
        bbox = character(0)
      )
    },
    .package = "tesseract"
  )
  file <- system.file("simulation.png", package = "retroglyph")
  agent$reconstruct(file, clear = FALSE)
  expect_identical(set_turns_calls, 0L)
})

test_that("retro_agent$reconstruct() validates clear", {
  chat <- retro_chat_mock()
  agent <- retro_agent(chat)
  file <- system.file("simulation.png", package = "retroglyph")
  expect_error(agent$reconstruct(file, clear = "yes"), "clear must be")
  expect_error(agent$reconstruct(file, clear = NA), "clear must be")
  expect_error(agent$reconstruct(file, clear = c(TRUE, FALSE)), "clear must be")
})

test_that("retro_agent$reconstruct() validates timeout", {
  chat <- retro_chat_mock()
  agent <- retro_agent(chat)
  file <- system.file("simulation.png", package = "retroglyph")
  expect_error(agent$reconstruct(file, timeout = "soon"), "timeout must be")
  expect_error(agent$reconstruct(file, timeout = NA), "timeout must be")
  expect_error(agent$reconstruct(file, timeout = c(1, 2)), "timeout must be")
  expect_error(agent$reconstruct(file, timeout = 0), "timeout must be")
  expect_error(agent$reconstruct(file, timeout = -1), "timeout must be")
})

test_that("retro_agent$reconstruct() stops a conversation that runs long", {
  chat <- retro_chat_mock()
  agent <- retro_agent(chat)
  # Stand in for a model that keeps taking turns and never converges.
  agent$chat <- list(
    chat = function(...) {
      repeat {
        runif(1e5)
      }
    },
    set_turns = function(value) invisible(NULL)
  )
  local_mocked_bindings(
    tesseract = function(...) NULL,
    ocr_data = function(...) {
      tibble::tibble(
        word = character(0),
        confidence = numeric(0),
        bbox = character(0)
      )
    },
    .package = "tesseract"
  )
  file <- system.file("simulation.png", package = "retroglyph")
  start <- proc.time()[["elapsed"]]
  expect_error(agent$reconstruct(file, timeout = 1), "timed out after 1 second")
  expect_lt(proc.time()[["elapsed"]] - start, 30)
})

test_that("retro_agent$reconstruct() reports a real interrupt as such", {
  chat <- retro_chat_mock()
  agent <- retro_agent(chat)
  # An interrupt well inside the deadline is the user's, not the timeout's.
  agent$chat <- list(
    chat = function(...) {
      condition <- structure(
        class = c("interrupt", "condition"),
        list(message = "", call = NULL)
      )
      signalCondition(condition)
      invisible(NULL)
    },
    set_turns = function(value) invisible(NULL)
  )
  local_mocked_bindings(
    tesseract = function(...) NULL,
    ocr_data = function(...) {
      tibble::tibble(
        word = character(0),
        confidence = numeric(0),
        bbox = character(0)
      )
    },
    .package = "tesseract"
  )
  file <- system.file("simulation.png", package = "retroglyph")
  expect_error(agent$reconstruct(file, timeout = 600), "was interrupted")
})

test_that("retro_agent$reconstruct() lifts the time limit when it returns", {
  chat <- retro_chat_mock()
  agent <- retro_agent(chat)
  agent$chat <- list(
    chat = function(...) invisible(NULL),
    set_turns = function(value) invisible(NULL)
  )
  local_mocked_bindings(
    tesseract = function(...) NULL,
    ocr_data = function(...) {
      tibble::tibble(
        word = character(0),
        confidence = numeric(0),
        bbox = character(0)
      )
    },
    .package = "tesseract"
  )
  file <- system.file("simulation.png", package = "retroglyph")
  expect_null(agent$reconstruct(file, timeout = 1))
  # The limit must not outlive the call it was set for, so work that runs
  # longer than the timeout afterwards should finish without complaint.
  start <- proc.time()[["elapsed"]]
  while (proc.time()[["elapsed"]] - start < 2) {
    runif(1e5)
  }
  expect_true(TRUE)
})

test_that("retro_agent$legend() is NULL before quantization", {
  chat <- retro_chat_mock()
  agent <- retro_agent(chat)
  expect_null(agent$legend())
})

test_that("retro_agent$legend() accesses state$data_legend", {
  chat <- retro_chat_mock()
  agent <- retro_agent(chat)
  agent$state$data_legend <- tibble::tibble(
    color = "#dc3030",
    series = "Placebo"
  )
  expect_equal(agent$legend(friendly = FALSE), agent$state$data_legend)
})

test_that("retro_agent$legend() converts colors to friendly names by default", {
  chat <- retro_chat_mock()
  agent <- retro_agent(chat)
  agent$state$data_legend <- tibble::tibble(
    color = "#dc3030",
    series = "Placebo"
  )
  result <- agent$legend()
  expect_equal(result$color, retro_color_name("#dc3030"))
  expect_equal(result$series, "Placebo")
})

test_that("retro_agent$legend(friendly = FALSE) keeps raw hex colors", {
  chat <- retro_chat_mock()
  agent <- retro_agent(chat)
  agent$state$data_legend <- tibble::tibble(
    color = "#dc3030",
    series = "Placebo"
  )
  result <- agent$legend(friendly = FALSE)
  expect_equal(result$color, "#dc3030")
})

test_that("retro_agent$legend() rejects a non-scalar-logical friendly", {
  chat <- retro_chat_mock()
  agent <- retro_agent(chat)
  agent$state$data_legend <- tibble::tibble(
    color = "#dc3030",
    series = "Placebo"
  )
  expect_error(
    agent$legend(friendly = "yes"),
    "friendly must be a single non-missing logical"
  )
  expect_error(
    agent$legend(friendly = c(TRUE, FALSE)),
    "friendly must be a single non-missing logical"
  )
  expect_error(
    agent$legend(friendly = NA),
    "friendly must be a single non-missing logical"
  )
})

test_that("retro_agent$legend(friendly = FALSE) is NULL before quantization", {
  chat <- retro_chat_mock()
  agent <- retro_agent(chat)
  expect_null(agent$legend(friendly = FALSE))
})

test_that("retro_agent$risk_table() is NULL before the data tool runs", {
  chat <- retro_chat_mock()
  agent <- retro_agent(chat)
  expect_null(agent$risk_table())
})

test_that("retro_agent$risk_table(mode = \"long\") accesses state$data_risk", {
  chat <- retro_chat_mock()
  agent <- retro_agent(chat)
  agent$state$data_risk <- tibble::tibble(
    patients = c(100, 95),
    series = c("Placebo", "Drug 10mg"),
    x = c(0, 0)
  )
  result <- agent$risk_table(mode = "long")
  expect_named(result, c("patients", "series", "x"))
  expect_equal(result$series, c("Placebo", "Drug 10mg"))
  expect_equal(result$x, c(0, 0))
  expect_equal(result$patients, c(100, 95))
})

test_that("retro_agent$risk_table(mode = \"wide\") pivots to one row per time", {
  chat <- retro_chat_mock()
  agent <- retro_agent(chat)
  agent$state$data_risk <- tibble::tibble(
    series = c("Placebo", "Placebo", "Drug 10mg", "Drug 10mg"),
    x = c(0, 6, 0, 6),
    patients = c(100, 80, 95, 90)
  )
  result <- agent$risk_table(mode = "wide")
  expect_named(result, c("x", "Placebo", "Drug 10mg"))
  expect_equal(result$x, c(0, 6))
  expect_equal(result$Placebo, c(100, 80))
  expect_equal(result$`Drug 10mg`, c(95, 90))
})

test_that("retro_agent$risk_table(mode = \"transposed\") is the default", {
  chat <- retro_chat_mock()
  agent <- retro_agent(chat)
  agent$state$data_risk <- tibble::tibble(
    series = c("Placebo", "Placebo", "Drug 10mg", "Drug 10mg"),
    x = c(0, 6, 0, 6),
    patients = c(100, 80, 95, 90)
  )
  expect_equal(agent$risk_table(), agent$risk_table(mode = "transposed"))
  result <- agent$risk_table(mode = "transposed")
  expect_named(result, c("series", "0", "6"))
  expect_equal(result$series, c("Placebo", "Drug 10mg"))
  expect_equal(result$`0`, c(100, 95))
  expect_equal(result$`6`, c(80, 90))
})

test_that("retro_agent$risk_table() rejects an invalid mode", {
  chat <- retro_chat_mock()
  agent <- retro_agent(chat)
  agent$state$data_risk <- tibble::tibble(
    series = "Placebo",
    x = 0,
    patients = 100
  )
  expect_error(
    agent$risk_table(mode = "wideeee"),
    "mode must be a single string"
  )
  expect_error(
    agent$risk_table(mode = c("wide", "long")),
    "mode must be a single string"
  )
  expect_error(agent$risk_table(mode = NA), "mode must be a single string")
})

test_that("retro_agent$events_table() is NULL before the data tool runs", {
  chat <- retro_chat_mock()
  agent <- retro_agent(chat)
  expect_null(agent$events_table())
})

test_that("retro_agent$events_table() accesses state$data_events", {
  chat <- retro_chat_mock()
  agent <- retro_agent(chat)
  agent$state$data_events <- tibble::tibble(
    series = c("Placebo", "Drug 10mg"),
    events = c(120, 118)
  )
  result <- agent$events_table()
  expect_named(result, c("series", "events"))
  expect_equal(result$series, c("Placebo", "Drug 10mg"))
  expect_equal(result$events, c(120, 118))
})

test_that("retro_agent$data() is NULL when no risk table was supplied", {
  chat <- retro_chat_mock()
  agent <- retro_agent(chat)
  expect_null(agent$data())
})

test_that("retro_agent$data() accesses state$data_survival", {
  chat <- retro_chat_mock()
  agent <- retro_agent(chat)
  agent$state$data_survival <- tibble::tibble(
    time = 1,
    status = 1L,
    series = "Placebo"
  )
  expect_equal(agent$data(), agent$state$data_survival)
})

survival_agent <- function() {
  agent <- retro_agent(retro_chat_mock())
  agent$state$data_legend <- tibble::tibble(
    series = c("Placebo", "Drug"),
    color = c("#dc3030", "#3030a0"),
    reference = c(TRUE, FALSE)
  )
  agent$state$data_survival <- tibble::tibble(
    series = rep(c("Placebo", "Drug"), each = 5),
    time = c(1, 2, 3, 4, 5, 2, 4, 6, 8, 10),
    status = c(1, 1, 1, 0, 1, 1, 1, 0, 1, 1)
  )
  agent
}

test_that("retro_agent$hazard_ratios() errors when data_survival is missing", {
  chat <- retro_chat_mock()
  agent <- retro_agent(chat)
  expect_error(agent$hazard_ratios(), "state\\$data_survival missing")
})

test_that("retro_agent$hazard_ratios() computes hazard ratios via state$data_survival", {
  agent <- survival_agent()
  result <- agent$hazard_ratios()
  ref_data <- agent$state$data_survival
  ref_data$series <- factor(ref_data$series, levels = c("Placebo", "Drug"))
  fit <- survival::coxph(
    survival::Surv(time, status) ~ series,
    data = ref_data
  )
  reference <- summary(fit, conf.int = 0.95)
  expect_equal(result$series, "Drug")
  expect_equal(result$estimate, unname(reference$conf.int[, "exp(coef)"]))
  expect_equal(result$lower, unname(reference$conf.int[, 3L]))
  expect_equal(result$upper, unname(reference$conf.int[, 4L]))
  expect_equal(result$p_value, unname(reference$coefficients[, "Pr(>|z|)"]))
})

test_that("retro_agent$hazard_ratios() defaults reference to the legend's reference series", {
  agent <- survival_agent()
  expect_equal(
    agent$hazard_ratios(),
    agent$hazard_ratios(reference = "Placebo")
  )
})

test_that("retro_agent$hazard_ratios() respects an explicit reference argument", {
  agent <- survival_agent()
  result <- agent$hazard_ratios(reference = "Drug")
  expect_equal(result$series, "Placebo")
})

test_that("retro_agent$hazard_ratios() respects a confidence argument", {
  agent <- survival_agent()
  default <- agent$hazard_ratios()
  wider <- agent$hazard_ratios(confidence = 0.5)
  expect_true(wider$lower > default$lower)
  expect_true(wider$upper < default$upper)
})

test_that("retro_agent$hazard_ratios() errors on an invalid reference", {
  agent <- survival_agent()
  expect_error(
    agent$hazard_ratios(reference = "Nonexistent"),
    "reference must be one of unique"
  )
})

test_that("retro_agent$hazard_ratios() errors when no series exists besides reference", {
  agent <- survival_agent()
  agent$state$data_survival <- agent$state$data_survival[
    agent$state$data_survival$series == "Placebo",
  ]
  expect_error(
    agent$hazard_ratios(reference = "Placebo"),
    "at least one series besides reference"
  )
})

test_that("retro_agent$quantiles() errors when data_survival is missing", {
  chat <- retro_chat_mock()
  agent <- retro_agent(chat)
  expect_error(agent$quantiles(c(0.5)), "state\\$data_survival missing")
})

test_that("retro_agent$quantiles() defaults probabilities to c(0.75, 0.5, 0.25), type to \"survival\", and confidence to 0.95", {
  agent <- survival_agent()
  expect_equal(
    agent$quantiles(),
    agent$quantiles(c(0.75, 0.5, 0.25), type = "survival", confidence = 0.95)
  )
})

test_that("retro_agent$quantiles() groups by series in legend order and sorts chronologically", {
  agent <- survival_agent()
  result <- agent$quantiles(c(0.25, 0.75, 0.5))
  expect_named(
    result,
    c("series", "survival", "incidence", "time", "time_lower", "time_upper")
  )
  expect_equal(
    result$series,
    rep(c("Placebo", "Drug"), each = 3)
  )
  expect_equal(result$survival, rep(c(0.75, 0.5, 0.25), 2))
  expect_equal(result$incidence, rep(c(0.25, 0.5, 0.75), 2))
  expect_equal(result$time, c(2, 3, 5, 4, 8, 10))
})

test_that("retro_agent$quantiles() rejects an invalid type", {
  agent <- survival_agent()
  expect_error(
    agent$quantiles(type = "nonsense"),
    "'arg' should be one of"
  )
})

test_that("retro_agent$quantiles(type = \"incidence\") is the complement of the survival call", {
  agent <- survival_agent()
  expect_equal(
    agent$quantiles(c(0.25, 0.5, 0.75), type = "incidence"),
    agent$quantiles(c(0.75, 0.5, 0.25), type = "survival")
  )
})

test_that("retro_agent$quantiles() respects a confidence argument", {
  agent <- survival_agent()
  result <- agent$quantiles(c(0.5), confidence = 0.90)
  placebo <- agent$state$data_survival[
    agent$state$data_survival$series == "Placebo",
  ]
  fit <- survival::survfit(
    survival::Surv(time, status) ~ 1,
    data = placebo,
    conf.int = 0.90
  )
  reference <- stats::quantile(fit, probs = 1 - c(0.5), conf.int = TRUE)
  placebo_result <- result[result$series == "Placebo", ]
  expect_equal(placebo_result$time_lower, unname(reference$lower))
  expect_equal(placebo_result$time_upper, unname(reference$upper))
})

test_that("retro_agent$quantiles() returns NA for a quantile not reached", {
  agent <- survival_agent()
  agent$state$data_survival <- tibble::tibble(
    series = "Placebo",
    time = c(10, 20, 30),
    status = c(0, 0, 0)
  )
  result <- agent$quantiles(c(0.5))
  expect_true(is.na(result$time))
  expect_true(is.na(result$time_lower))
  expect_true(is.na(result$time_upper))
})

test_that("retro_agent$probabilities() errors when data_survival is missing", {
  chat <- retro_chat_mock()
  agent <- retro_agent(chat)
  expect_error(agent$probabilities(c(2)), "state\\$data_survival missing")
})

test_that("retro_agent$probabilities() groups by series in legend order and sorts chronologically", {
  agent <- survival_agent()
  result <- agent$probabilities(c(6, 2, 4))
  expect_named(
    result,
    c(
      "series",
      "time",
      "survival",
      "survival_lower",
      "survival_upper",
      "incidence",
      "incidence_lower",
      "incidence_upper"
    )
  )
  expect_equal(
    result$series,
    rep(c("Placebo", "Drug"), each = 3)
  )
  expect_equal(result$time, rep(c(2, 4, 6), 2))
})

test_that("retro_agent$probabilities() respects a confidence argument", {
  agent <- survival_agent()
  result <- agent$probabilities(c(2, 4), confidence = 0.90)
  placebo <- agent$state$data_survival[
    agent$state$data_survival$series == "Placebo",
  ]
  fit <- survival::survfit(
    survival::Surv(time, status) ~ 1,
    data = placebo,
    conf.int = 0.90
  )
  reference <- summary(fit, times = c(2, 4), extend = TRUE)
  placebo_result <- result[result$series == "Placebo", ]
  expect_equal(placebo_result$survival, as.numeric(reference$surv))
  expect_equal(placebo_result$survival_lower, as.numeric(reference$lower))
  expect_equal(placebo_result$survival_upper, as.numeric(reference$upper))
  expect_equal(
    placebo_result$incidence_lower,
    1 - as.numeric(reference$lower)
  )
  expect_equal(
    placebo_result$incidence_upper,
    1 - as.numeric(reference$upper)
  )
})

test_that("retro_agent$probabilities() extends flat past the last observed time", {
  agent <- survival_agent()
  agent$state$data_survival <- tibble::tibble(
    series = "A",
    time = c(10, 20, 30),
    status = c(1, 1, 0)
  )
  result <- agent$probabilities(c(30, 100))
  expect_false(anyNA(result$survival))
  expect_equal(
    result$survival[result$time == 100],
    result$survival[result$time == 30]
  )
})

test_that("retro_agent$counts() errors when data_survival is missing", {
  chat <- retro_chat_mock()
  agent <- retro_agent(chat)
  expect_error(agent$counts(), "state\\$data_survival missing")
})

test_that("retro_agent$counts() counts patients and events per series in legend order", {
  agent <- survival_agent()
  result <- agent$counts()
  expect_named(result, c("series", "patients", "events"))
  expect_equal(result$series, c("Placebo", "Drug", "total"))
  expect_equal(result$patients, c(5L, 5L, 10L))
  expect_equal(result$events, c(4L, 4L, 8L))
})

test_that("retro_agent$reconstruct() normalizes SVG and JPEG to PNG", {
  # Stub the chat method so it doesn't call the provider.
  stub_chat <- list(
    chat = function(...) invisible(NULL),
    set_turns = function(value) invisible(NULL)
  )
  svg <- paste0(
    "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"40\" height=\"30\">",
    "<rect width=\"40\" height=\"30\" fill=\"#3030a0\"/></svg>"
  )
  svg_file <- tempfile(fileext = ".svg")
  on.exit(unlink(svg_file))
  writeLines(svg, svg_file)
  jpg_file <- system.file("images", "building.jpg", package = "magick")
  local_mocked_bindings(
    tesseract = function(...) NULL,
    ocr_data = function(...) {
      tibble::tibble(
        word = character(0),
        confidence = numeric(0),
        bbox = character(0)
      )
    },
    .package = "tesseract"
  )
  for (file in c(svg_file, jpg_file)) {
    agent <- retro_agent(retro_chat_mock())
    agent$chat <- stub_chat
    agent$reconstruct(file)
    expect_match(agent$state$image_source, "[.]png$")
    info <- magick::image_info(magick::image_read(agent$state$image_source))
    expect_equal(info$format, "PNG")
  }
})

# Helper: populate an agent's state as if reconstruct() had already run.
reconstructed_agent <- function() {
  pixel_matrix <- matrix("#ffffff", nrow = 10L, ncol = 20L)
  pixel_matrix[5L, 2L:8L] <- "#ff0000"
  pixel_matrix[6L, 10L:18L] <- "#0000ff"
  agent <- retro_agent(retro_chat_mock())
  agent$state$image_source <- system.file(
    "simulation.png",
    package = "retroglyph"
  )
  agent$state$data_path <- retro_test_path(pixel_matrix)
  agent$state$data_legend <- tibble::tibble(
    color = c("#ff0000", "#0000ff"),
    series = c("Arm A", "Arm B"),
    reference = c(TRUE, FALSE)
  )
  agent$state$data_background <- "#ffffff"
  agent$state$data_panel <- tibble::tibble(
    x1 = 1L,
    x2 = 20L,
    y1 = 1L,
    y2 = 10L,
    pad_x1 = 1L,
    pad_x2 = -1L,
    pad_y1 = 1L,
    pad_y2 = -1L
  )
  agent$state$data_label <- tibble::tibble(
    label = c("A", "B", "C", "D"),
    word = c("0", "10", "0", "1"),
    confidence = rep(90, 4L),
    x = c(2, 18, 1, 1),
    y = c(10, 10, 8, 2),
    x1 = c(1L, 17L, 0L, 0L),
    x2 = c(3L, 19L, 2L, 2L),
    y1 = c(9L, 9L, 7L, 1L),
    y2 = c(11L, 11L, 9L, 3L)
  )
  agent$state$data_x <- tibble::tibble(
    label = c("A", "B"),
    value = c(0, 10),
    x = c(2, 18)
  )
  agent$state$data_y <- tibble::tibble(
    label = c("C", "D"),
    value = c(0, 1),
    y = c(8, 2)
  )
  agent$state$data_scaled <- tibble::tibble(
    series = rep(c("Arm A", "Arm B"), each = 2L),
    x = c(0, 5, 0, 5),
    y = c(1, 0.8, 1, 0.7),
    max_y = 1,
    increasing = FALSE
  )
  agent$state$data_survival <- tibble::tibble(
    series = rep(c("Arm A", "Arm B"), each = 4L),
    time = c(2, 4, 6, 8, 3, 5, 7, 9),
    status = c(1, 0, 1, 0, 1, 0, 1, 0)
  )
  agent$state$data_risk <- tibble::tibble(
    series = c("Arm A", "Arm B"),
    x = c(8, 9)
  )
  agent
}

test_that("retro_agent$compare() rejects a bad front argument", {
  agent <- reconstructed_agent()
  bad_fronts <- list("a", c(1L, 2L), NA_integer_)
  for (bad in bad_fronts) {
    expect_error(agent$compare(bad), "front must be")
  }
})

test_that("retro_agent$compare() rejects a bad data argument", {
  agent <- reconstructed_agent()
  bad_data <- list("impression", 1, NA, c("survival", "trace"))
  for (bad in bad_data) {
    expect_error(agent$compare(data = bad), "data must be")
  }
})

test_that("retro_agent$compare() errors before reconstruct()", {
  agent <- retro_agent(retro_chat_mock())
  expect_error(agent$compare(), "call reconstruct")
})

test_that("retro_agent$compare() reports which state pieces are missing", {
  agent <- retro_agent(retro_chat_mock())
  agent$state$image_source <- system.file(
    "simulation.png",
    package = "retroglyph"
  )
  agent$state$data_legend <- tibble::tibble(color = "#ff0000", series = "Arm A")
  expect_error(agent$compare(), "data_background")
  expect_error(agent$compare(), "data_path")
})

test_that("retro_agent$compare() rejects front outside the legend range", {
  agent <- reconstructed_agent()
  expect_error(agent$compare(0L), "between 1 and nrow")
  expect_error(agent$compare(3L), "between 1 and nrow")
})

test_that("retro_agent$compare() returns a visual_diff htmlwidget", {
  agent <- reconstructed_agent()
  widget <- agent$compare()
  expect_s3_class(widget, "htmlwidget")
  expect_s3_class(widget, "visual_diff")
})

test_that("retro_agent$compare(data = \"trace\") returns a visual_diff htmlwidget", {
  agent <- reconstructed_agent()
  widget <- agent$compare(data = "trace")
  expect_s3_class(widget, "htmlwidget")
  expect_s3_class(widget, "visual_diff")
})

test_that("retro_agent$compare() embeds the source and impression images", {
  agent <- reconstructed_agent()
  widget <- agent$compare(front = 2L)
  expect_equal(widget$x$typediff, "image")
  expect_match(widget$x$old, "^data:image/")
  expect_match(widget$x$new, "^data:image/")
})

test_that("retro_agent$compare() deletes its temporary image files", {
  agent <- reconstructed_agent()
  before <- list.files(tempdir(), pattern = "[.]png$")
  agent$compare()
  after <- list.files(tempdir(), pattern = "[.]png$")
  expect_equal(sort(before), sort(after))
})

test_that("retro_agent$export() rejects a bad output argument", {
  agent <- reconstructed_agent()
  bad_outputs <- list(123, c("a", "b"), NA_character_, "")
  for (bad in bad_outputs) {
    expect_error(agent$export(bad), "output must be")
  }
})

test_that("retro_agent$export() rejects a bad data argument", {
  agent <- reconstructed_agent()
  output <- tempfile()
  on.exit(unlink(output, recursive = TRUE))
  expect_error(agent$export(output, data = "impression"), "data must be")
})

test_that("retro_agent$export(data = \"trace\") writes one HTML widget per legend row", {
  agent <- reconstructed_agent()
  output <- tempfile()
  on.exit(unlink(output, recursive = TRUE))
  agent$export(output, data = "trace")
  compare_directory <- file.path(output, "compare")
  expect_equal(
    sort(list.files(compare_directory)),
    sort(c("arm_a_red.html", "arm_b_blue.html"))
  )
})

test_that("retro_agent$export() errors before reconstruct()", {
  agent <- retro_agent(retro_chat_mock())
  output <- tempfile()
  expect_error(agent$export(output), "call reconstruct")
})

test_that("retro_agent$export() writes one named HTML widget per legend row", {
  agent <- reconstructed_agent()
  output <- tempfile()
  on.exit(unlink(output, recursive = TRUE))
  result <- withVisible(agent$export(output))
  expect_null(result$value)
  expect_false(result$visible)
  expect_true(dir.exists(output))
  # Legend: "Arm A" = #ff0000 (red), "Arm B" = #0000ff (blue).
  compare_directory <- file.path(output, "compare")
  expect_equal(
    sort(list.files(compare_directory)),
    sort(c("arm_a_red.html", "arm_b_blue.html"))
  )
  contents <- paste(
    readLines(file.path(compare_directory, "arm_a_red.html"), warn = FALSE),
    collapse = "\n"
  )
  expect_match(contents, "<!DOCTYPE html>", fixed = TRUE)
})

test_that("retro_agent$export() does not leave stray dependency directories", {
  agent <- reconstructed_agent()
  output <- tempfile()
  on.exit(unlink(output, recursive = TRUE))
  agent$export(output)
  expect_equal(
    sort(list.files(file.path(output, "compare"))),
    sort(c("arm_a_red.html", "arm_b_blue.html"))
  )
})

test_that("retro_agent$export() writes risk_table.csv when a risk table exists", {
  agent <- reconstructed_agent()
  agent$state$data_risk <- tibble::tibble(
    series = c("Arm A", "Arm B"),
    x = c(0, 0),
    patients = c(100L, 100L)
  )
  output <- tempfile()
  on.exit(unlink(output, recursive = TRUE))
  agent$export(output)
  risk_table_file <- file.path(output, "data", "risk_table.csv")
  expect_true(file.exists(risk_table_file))
  risk_table <- utils::read.csv(risk_table_file)
  expect_equal(risk_table$series, c("Arm A", "Arm B"))
  expect_equal(risk_table$patients, c(100L, 100L))
})

test_that("retro_agent$export() writes events_table.csv when total events exist", {
  agent <- reconstructed_agent()
  agent$state$data_events <- tibble::tibble(
    events = c(40, 30),
    series = c("Arm A", "Arm B")
  )
  output <- tempfile()
  on.exit(unlink(output, recursive = TRUE))
  agent$export(output)
  events_file <- file.path(output, "data", "events_table.csv")
  expect_true(file.exists(events_file))
  events_table <- utils::read.csv(events_file)
  expect_equal(events_table$series, c("Arm A", "Arm B"))
  expect_equal(events_table$events, c(40, 30))
})

test_that("retro_agent$export() omits events_table.csv when no total events exist", {
  # Total events is optional, so its absence is not an error and must not
  # block the rest of the export.
  agent <- reconstructed_agent()
  agent$state$data_events <- NULL
  output <- tempfile()
  on.exit(unlink(output, recursive = TRUE))
  agent$export(output)
  expect_false(file.exists(file.path(output, "data", "events_table.csv")))
  expect_true(file.exists(file.path(output, "data", "risk_table.csv")))
})

test_that("retro_agent$export() errors when there is no risk table", {
  # export() calls compare() for every legend row before it ever gets to
  # the risk_table.csv/data.csv writing logic, and compare() now requires
  # data_risk (to render the reconstructed survival curve), so a missing
  # risk table surfaces as an error rather than a silently skipped file.
  agent <- reconstructed_agent()
  agent$state$data_risk <- NULL
  output <- tempfile()
  on.exit(unlink(output, recursive = TRUE))
  expect_error(agent$export(output), "data_risk")
})

test_that("retro_agent$export() writes data.csv when survival data exists", {
  agent <- reconstructed_agent()
  agent$state$data_survival <- tibble::tibble(
    series = rep(c("Arm A", "Arm B"), each = 5),
    time = c(1, 2, 3, 4, 5, 2, 4, 6, 8, 10),
    status = c(1, 1, 1, 0, 1, 1, 1, 0, 1, 1)
  )
  output <- tempfile()
  on.exit(unlink(output, recursive = TRUE))
  agent$export(output)
  data_file <- file.path(output, "data", "data.csv")
  expect_true(file.exists(data_file))
  data <- utils::read.csv(data_file)
  expect_equal(data$series, rep(c("Arm A", "Arm B"), each = 5))
  expect_equal(data$time, c(1, 2, 3, 4, 5, 2, 4, 6, 8, 10))
  expect_equal(data$status, c(1, 1, 1, 0, 1, 1, 1, 0, 1, 1))
})

test_that("retro_agent$export() errors when there is no survival data", {
  # Same reasoning as the missing-risk-table case above: compare() now
  # requires data_survival to render the reconstructed curve, so export()
  # errors before it would otherwise skip writing data.csv.
  agent <- reconstructed_agent()
  agent$state$data_survival <- NULL
  output <- tempfile()
  on.exit(unlink(output, recursive = TRUE))
  expect_error(agent$export(output), "data_survival")
})

test_that("retro_agent$export() writes quantiles.csv and hazard_ratios.csv when survival data exists", {
  agent <- reconstructed_agent()
  agent$state$data_survival <- tibble::tibble(
    series = rep(c("Arm A", "Arm B"), each = 5),
    time = c(1, 2, 3, 4, 5, 2, 4, 6, 8, 10),
    status = c(1, 1, 1, 0, 1, 1, 1, 0, 1, 1)
  )
  output <- tempfile()
  on.exit(unlink(output, recursive = TRUE))
  agent$export(output)
  quantiles_file <- file.path(output, "data", "quantiles.csv")
  hazard_ratios_file <- file.path(output, "data", "hazard_ratios.csv")
  expect_true(file.exists(quantiles_file))
  expect_true(file.exists(hazard_ratios_file))
  quantiles <- utils::read.csv(quantiles_file)
  expect_named(
    quantiles,
    c("series", "survival", "incidence", "time", "time_lower", "time_upper")
  )
  hazard_ratios <- utils::read.csv(hazard_ratios_file)
  expect_named(
    hazard_ratios,
    c("series", "estimate", "lower", "upper", "p_value")
  )
  expect_equal(hazard_ratios$series, "Arm B")
})

test_that("retro_agent$export() forwards probabilities, type, and confidence to quantiles.csv", {
  agent <- reconstructed_agent()
  agent$state$data_survival <- tibble::tibble(
    series = rep(c("Arm A", "Arm B"), each = 5),
    time = c(1, 2, 3, 4, 5, 2, 4, 6, 8, 10),
    status = c(1, 1, 1, 0, 1, 1, 1, 0, 1, 1)
  )
  output <- tempfile()
  on.exit(unlink(output, recursive = TRUE))
  agent$export(
    output,
    probabilities = c(0.9, 0.6),
    type = "incidence",
    confidence = 0.5
  )
  quantiles <- utils::read.csv(file.path(output, "data", "quantiles.csv"))
  expected <- agent$quantiles(
    probabilities = c(0.9, 0.6),
    type = "incidence",
    confidence = 0.5
  )
  expect_equal(quantiles$survival, expected$survival)
  expect_equal(quantiles$time_lower, expected$time_lower)
})

test_that("retro_agent$export() omits probabilities.csv when quantiles is not supplied", {
  agent <- reconstructed_agent()
  agent$state$data_survival <- tibble::tibble(
    series = rep(c("Arm A", "Arm B"), each = 5),
    time = c(1, 2, 3, 4, 5, 2, 4, 6, 8, 10),
    status = c(1, 1, 1, 0, 1, 1, 1, 0, 1, 1)
  )
  output <- tempfile()
  on.exit(unlink(output, recursive = TRUE))
  agent$export(output)
  expect_false(file.exists(file.path(output, "data", "probabilities.csv")))
})

test_that("retro_agent$export() writes probabilities.csv when quantiles is supplied", {
  agent <- reconstructed_agent()
  agent$state$data_survival <- tibble::tibble(
    series = rep(c("Arm A", "Arm B"), each = 5),
    time = c(1, 2, 3, 4, 5, 2, 4, 6, 8, 10),
    status = c(1, 1, 1, 0, 1, 1, 1, 0, 1, 1)
  )
  output <- tempfile()
  on.exit(unlink(output, recursive = TRUE))
  agent$export(output, quantiles = c(2, 4, 6), confidence = 0.5)
  probabilities_file <- file.path(output, "data", "probabilities.csv")
  expect_true(file.exists(probabilities_file))
  probabilities <- utils::read.csv(probabilities_file)
  expect_named(
    probabilities,
    c(
      "series",
      "time",
      "survival",
      "survival_lower",
      "survival_upper",
      "incidence",
      "incidence_lower",
      "incidence_upper"
    )
  )
  expected <- agent$probabilities(quantiles = c(2, 4, 6), confidence = 0.5)
  expect_equal(probabilities$survival, expected$survival)
  expect_equal(probabilities$survival_lower, expected$survival_lower)
})

test_that("retro_agent$export() errors when there is no survival data (quantiles/hazard ratios)", {
  # Same reasoning again: compare() requires data_survival, so this never
  # reaches the point where quantiles.csv/hazard_ratios.csv would be skipped.
  agent <- reconstructed_agent()
  agent$state$data_survival <- NULL
  output <- tempfile()
  on.exit(unlink(output, recursive = TRUE))
  expect_error(agent$export(output), "data_survival")
})

test_that("retro_agent$export() writes counts.csv when survival data exists", {
  agent <- reconstructed_agent()
  agent$state$data_survival <- tibble::tibble(
    series = rep(c("Arm A", "Arm B"), each = 5),
    time = c(1, 2, 3, 4, 5, 2, 4, 6, 8, 10),
    status = c(1, 1, 1, 0, 1, 1, 1, 0, 1, 1)
  )
  output <- tempfile()
  on.exit(unlink(output, recursive = TRUE))
  agent$export(output)
  counts_file <- file.path(output, "data", "counts.csv")
  expect_true(file.exists(counts_file))
  counts <- utils::read.csv(counts_file)
  expect_named(counts, c("series", "patients", "events"))
  expect_equal(counts$series, c("Arm A", "Arm B", "total"))
  expect_equal(counts$patients, c(5, 5, 10))
  expect_equal(counts$events, c(4, 4, 8))
})
