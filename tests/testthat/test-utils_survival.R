mock_data_survival <- tibble::tibble(
  series = rep(c("Placebo", "Drug"), each = 5),
  time = c(1, 2, 3, 4, 5, 2, 4, 6, 8, 10),
  status = c(1, 1, 1, 0, 1, 1, 1, 0, 1, 1)
)

mock_data_legend <- tibble::tibble(
  series = c("Placebo", "Drug"),
  color = c("#dc3030", "#3030a0"),
  reference = c(TRUE, FALSE)
)

test_that("retro_survival_hazard_ratios() matches survival::coxph() directly", {
  result <- retro_survival_hazard_ratios(
    mock_data_survival,
    reference = "Placebo"
  )
  ref_data <- mock_data_survival
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

test_that("retro_survival_hazard_ratios() respects a custom reference", {
  result <- retro_survival_hazard_ratios(mock_data_survival, reference = "Drug")
  expect_equal(result$series, "Placebo")
  expect_equal(
    result$estimate,
    1 /
      retro_survival_hazard_ratios(
        mock_data_survival,
        reference = "Placebo"
      )$estimate
  )
})

test_that("retro_survival_hazard_ratios() respects a custom confidence level", {
  narrow <- retro_survival_hazard_ratios(
    mock_data_survival,
    reference = "Placebo",
    confidence = 0.5
  )
  wide <- retro_survival_hazard_ratios(
    mock_data_survival,
    reference = "Placebo",
    confidence = 0.95
  )
  expect_true(narrow$lower > wide$lower)
  expect_true(narrow$upper < wide$upper)
})

test_that("retro_survival_hazard_ratios() rejects an invalid reference", {
  expect_error(
    retro_survival_hazard_ratios(mock_data_survival, reference = "Nonexistent"),
    "reference must be one of unique"
  )
})

test_that("retro_survival_hazard_ratios() rejects a reference with no comparator", {
  data <- mock_data_survival[mock_data_survival$series == "Placebo", ]
  expect_error(
    retro_survival_hazard_ratios(data, reference = "Placebo"),
    "at least one series besides reference"
  )
})

test_that("retro_survival_hazard_ratios() rejects an invalid confidence", {
  expect_error(
    retro_survival_hazard_ratios(
      mock_data_survival,
      reference = "Placebo",
      confidence = 1
    ),
    "confidence must be a single number strictly between 0 and 1"
  )
  expect_error(
    retro_survival_hazard_ratios(
      mock_data_survival,
      reference = "Placebo",
      confidence = 0
    ),
    "confidence must be a single number strictly between 0 and 1"
  )
})

test_that("retro_survival_quantiles() groups by series in input order", {
  result <- retro_survival_quantiles(
    mock_data_survival,
    probabilities = c(0.5)
  )
  expect_equal(
    result$series,
    c("Placebo", "Drug")
  )
})

test_that("retro_survival_quantiles() sorts chronologically regardless of input order", {
  result <- retro_survival_quantiles(
    mock_data_survival,
    probabilities = c(0.25, 0.75, 0.5)
  )
  expect_equal(result$survival, rep(c(0.75, 0.5, 0.25), 2))
  expect_equal(result$incidence, rep(c(0.25, 0.5, 0.75), 2))
  expect_equal(result$time, c(2, 3, 5, 4, 8, 10))
})

test_that("retro_survival_quantiles() drops duplicate probabilities", {
  result <- retro_survival_quantiles(
    mock_data_survival,
    probabilities = c(0.5, 0.5, 0.25)
  )
  expect_equal(result$survival, rep(c(0.5, 0.25), 2))
})

test_that("retro_survival_quantiles() matches survival::quantile.survfit() for lower/upper", {
  result <- retro_survival_quantiles(
    mock_data_survival,
    probabilities = c(0.25, 0.5, 0.75),
    confidence = 0.90
  )
  placebo <- mock_data_survival[mock_data_survival$series == "Placebo", ]
  fit <- survival::survfit(
    survival::Surv(time, status) ~ 1,
    data = placebo,
    conf.int = 0.90
  )
  reference <- stats::quantile(
    fit,
    probs = 1 - sort(c(0.25, 0.5, 0.75), decreasing = TRUE),
    conf.int = TRUE
  )
  placebo_result <- result[result$series == "Placebo", ]
  expect_equal(placebo_result$time_lower, unname(reference$lower))
  expect_equal(placebo_result$time_upper, unname(reference$upper))
})

test_that("retro_survival_quantiles() returns NA for a quantile not reached", {
  data <- tibble::tibble(
    series = "A",
    time = c(10, 20, 30),
    status = c(0, 0, 0)
  )
  result <- retro_survival_quantiles(data, probabilities = c(0.5))
  expect_true(is.na(result$time))
  expect_true(is.na(result$time_lower))
  expect_true(is.na(result$time_upper))
})

test_that("retro_survival_quantiles() rejects invalid probabilities", {
  expect_error(
    retro_survival_quantiles(mock_data_survival, probabilities = c(-0.1)),
    "probabilities must be a non-empty numeric vector"
  )
  expect_error(
    retro_survival_quantiles(mock_data_survival, probabilities = c(1.5)),
    "probabilities must be a non-empty numeric vector"
  )
  expect_error(
    retro_survival_quantiles(mock_data_survival, probabilities = numeric(0)),
    "probabilities must be a non-empty numeric vector"
  )
})

test_that("retro_survival_quantiles() rejects an invalid confidence", {
  expect_error(
    retro_survival_quantiles(
      mock_data_survival,
      probabilities = c(0.5),
      confidence = 1
    ),
    "confidence must be a single number strictly between 0 and 1"
  )
  expect_error(
    retro_survival_quantiles(
      mock_data_survival,
      probabilities = c(0.5),
      confidence = 0
    ),
    "confidence must be a single number strictly between 0 and 1"
  )
})

test_that("retro_survival_probabilities() groups by series in input order", {
  result <- retro_survival_probabilities(
    mock_data_survival,
    quantiles = c(2)
  )
  expect_equal(
    result$series,
    c("Placebo", "Drug")
  )
})

test_that("retro_survival_probabilities() sorts chronologically regardless of input order", {
  result <- retro_survival_probabilities(
    mock_data_survival,
    quantiles = c(6, 2, 4)
  )
  expect_equal(result$time, rep(c(2, 4, 6), 2))
})

test_that("retro_survival_probabilities() drops duplicate quantiles", {
  result <- retro_survival_probabilities(
    mock_data_survival,
    quantiles = c(2, 2, 4)
  )
  expect_equal(result$time, rep(c(2, 4), 2))
})

test_that("retro_survival_probabilities() matches survival::summary.survfit() for survival/lower/upper", {
  result <- retro_survival_probabilities(
    mock_data_survival,
    quantiles = c(2, 4, 6),
    confidence = 0.90
  )
  placebo <- mock_data_survival[mock_data_survival$series == "Placebo", ]
  fit <- survival::survfit(
    survival::Surv(time, status) ~ 1,
    data = placebo,
    conf.int = 0.90
  )
  reference <- summary(fit, times = c(2, 4, 6), extend = TRUE)
  placebo_result <- result[result$series == "Placebo", ]
  expect_equal(placebo_result$survival, as.numeric(reference$surv))
  expect_equal(placebo_result$incidence, 1 - as.numeric(reference$surv))
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

test_that("retro_survival_probabilities() extends flat past the last observed time", {
  data <- tibble::tibble(
    series = "A",
    time = c(10, 20, 30),
    status = c(1, 1, 0)
  )
  result <- retro_survival_probabilities(data, quantiles = c(30, 100))
  expect_false(anyNA(result$survival))
  expect_equal(
    result$survival[result$time == 100],
    result$survival[result$time == 30]
  )
})

test_that("retro_survival_probabilities() rejects invalid quantiles", {
  expect_error(
    retro_survival_probabilities(mock_data_survival, quantiles = c(-1)),
    "quantiles must be a non-empty numeric vector"
  )
  expect_error(
    retro_survival_probabilities(mock_data_survival, quantiles = numeric(0)),
    "quantiles must be a non-empty numeric vector"
  )
  expect_error(
    retro_survival_probabilities(mock_data_survival, quantiles = NA_real_),
    "quantiles must be a non-empty numeric vector"
  )
})

test_that("retro_survival_probabilities() rejects an invalid confidence", {
  expect_error(
    retro_survival_probabilities(
      mock_data_survival,
      quantiles = c(2),
      confidence = 1
    ),
    "confidence must be a single number strictly between 0 and 1"
  )
  expect_error(
    retro_survival_probabilities(
      mock_data_survival,
      quantiles = c(2),
      confidence = 0
    ),
    "confidence must be a single number strictly between 0 and 1"
  )
})

test_that("retro_survival_counts() counts patients and events per series in legend order", {
  result <- retro_survival_counts(mock_data_survival, mock_data_legend)
  expect_named(result, c("series", "patients", "events"))
  expect_equal(result$series, c("Placebo", "Drug", "total"))
  expect_equal(result$patients, c(5L, 5L, 10L))
  expect_equal(result$events, c(4L, 4L, 8L))
})

test_that("retro_survival_counts() respects legend order, not data order", {
  reversed_legend <- tibble::tibble(
    series = c("Drug", "Placebo"),
    color = c("#3030a0", "#dc3030"),
    reference = c(FALSE, TRUE)
  )
  result <- retro_survival_counts(mock_data_survival, reversed_legend)
  expect_equal(result$series, c("Drug", "Placebo", "total"))
  expect_equal(result$patients, c(5L, 5L, 10L))
  expect_equal(result$events, c(4L, 4L, 8L))
})

test_that("retro_survival_counts() handles a legend series with no rows in data", {
  data <- tibble::tibble(
    series = c("Placebo", "Placebo", "Drug"),
    time = c(1, 2, 3),
    status = c(1, 0, 1)
  )
  legend <- tibble::tibble(
    series = c("Placebo", "Drug", "Empty"),
    color = c("#dc3030", "#3030a0", "#00ff00"),
    reference = c(TRUE, FALSE, FALSE)
  )
  result <- retro_survival_counts(data, legend)
  expect_equal(result$series, c("Placebo", "Drug", "Empty", "total"))
  expect_equal(result$patients, c(2L, 1L, 0L, 3L))
  expect_equal(result$events, c(1L, 1L, 0L, 2L))
})

test_that("retro_survival_counts() handles a zero-row data tibble", {
  data <- tibble::tibble(
    series = character(0L),
    time = numeric(0L),
    status = integer(0L)
  )
  result <- retro_survival_counts(data, mock_data_legend)
  expect_equal(result$series, c("Placebo", "Drug", "total"))
  expect_equal(result$patients, c(0L, 0L, 0L))
  expect_equal(result$events, c(0L, 0L, 0L))
})

test_that("retro_survival_counts() rejects invalid data", {
  expect_error(
    retro_survival_counts(list(), mock_data_legend),
    "data must be a tibble with columns series, time, status"
  )
  expect_error(
    retro_survival_counts(
      tibble::tibble(series = 1, time = 1, status = 1),
      mock_data_legend
    ),
    "data\\$series must be a character vector"
  )
})

test_that("retro_survival_counts() rejects an invalid legend", {
  expect_error(
    retro_survival_counts(mock_data_survival, list()),
    "legend must be a tibble with column series"
  )
  expect_error(
    retro_survival_counts(
      mock_data_survival,
      tibble::tibble(series = 1)
    ),
    "legend\\$series must be a character vector"
  )
})
