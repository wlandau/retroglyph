test_that("retro_slug() lowercases and replaces spaces with underscores", {
  expect_equal(retro_slug("Drug 10mg"), "drug_10mg")
  expect_equal(retro_slug("Arm A"), "arm_a")
})

test_that("retro_slug() collapses runs of non-alphanumeric characters", {
  expect_equal(retro_slug("a--b  c__d"), "a_b_c_d")
})

test_that("retro_slug() trims leading and trailing underscores", {
  expect_equal(retro_slug(" Placebo! "), "placebo")
})

test_that("retro_slug() is vectorized", {
  expect_equal(
    retro_slug(c("Drug 10mg", "Placebo")),
    c("drug_10mg", "placebo")
  )
})
