test_that("retro_table_wide() pivots to one row per time, one column per series", {
  risk_table <- tibble::tibble(
    series = c("Placebo", "Placebo", "Drug", "Drug"),
    x = c(0, 6, 0, 6),
    patients = c(100, 80, 95, 90)
  )
  result <- retro_table_wide(risk_table)
  expect_named(result, c("x", "Placebo", "Drug"))
  expect_equal(result$x, c(0, 6))
  expect_equal(result$Placebo, c(100, 80))
  expect_equal(result$Drug, c(95, 90))
})

test_that("retro_table_wide() fills NA when a series has no entry at x", {
  risk_table <- tibble::tibble(
    series = c("Placebo", "Placebo", "Drug"),
    x = c(0, 6, 0),
    patients = c(100, 80, 95)
  )
  result <- retro_table_wide(risk_table)
  expect_equal(result$x, c(0, 6))
  expect_equal(result$Placebo, c(100, 80))
  expect_equal(result$Drug, c(95, NA))
})

test_that("retro_table_wide() orders series columns by first appearance", {
  risk_table <- tibble::tibble(
    series = c("Drug", "Placebo", "Drug", "Placebo"),
    x = c(0, 0, 6, 6),
    patients = c(95, 100, 90, 80)
  )
  result <- retro_table_wide(risk_table)
  expect_named(result, c("x", "Drug", "Placebo"))
})

test_that("retro_table_wide() sorts x increasing regardless of input order", {
  risk_table <- tibble::tibble(
    series = c("Placebo", "Placebo"),
    x = c(6, 0),
    patients = c(80, 100)
  )
  result <- retro_table_wide(risk_table)
  expect_equal(result$x, c(0, 6))
  expect_equal(result$Placebo, c(100, 80))
})

test_that("retro_table_transpose() transposes to one row per series", {
  wide_table <- tibble::tibble(
    x = c(0, 6),
    Placebo = c(100, 80),
    Drug = c(95, 90)
  )
  result <- retro_table_transpose(wide_table)
  expect_named(result, c("series", "0", "6"))
  expect_equal(result$series, c("Placebo", "Drug"))
  expect_equal(result$`0`, c(100, 95))
  expect_equal(result$`6`, c(80, 90))
})

test_that("retro_table_transpose() preserves NA cells", {
  wide_table <- tibble::tibble(
    x = c(0, 6),
    Placebo = c(100, 80),
    Drug = c(95, NA)
  )
  result <- retro_table_transpose(wide_table)
  expect_equal(result$`6`, c(80, NA))
})

test_that("retro_table_wide() and retro_table_transpose() round-trip", {
  risk_table <- tibble::tibble(
    series = c("Placebo", "Placebo", "Drug", "Drug"),
    x = c(0, 6, 0, 6),
    patients = c(100, 80, 95, 90)
  )
  result <- retro_table_transpose(retro_table_wide(risk_table))
  expect_named(result, c("series", "0", "6"))
  expect_equal(result$series, c("Placebo", "Drug"))
  expect_equal(result$`0`, c(100, 95))
  expect_equal(result$`6`, c(80, 90))
})
