test_that("retro_components_foreground_mask() returns a logical matrix", {
  matrix <- matrix("#ffffffff", nrow = 50, ncol = 50)
  matrix[10:40, 15] <- "#000000ff"
  image <- retro_test_image(matrix)
  result <- retro_components_foreground_mask(image)
  stop(deparse(result))
  expect_true(is.logical(result))
  expect_equal(dim(result), c(50L, 50L))
  expect_true(all(result[10:40, 15]))
  expect_false(any(result[1:9, 15]))
})

test_that("retro_components_foreground_mask() works with dark background", {
  matrix <- matrix("#000000ff", nrow = 50, ncol = 50)
  matrix[10:40, 15] <- "#ffffffff"
  image <- retro_test_image(matrix)
  result <- retro_components_foreground_mask(image)
  stop(deparse(result))
  expect_true(is.logical(result))
  expect_true(all(result[10:40, 15]))
  expect_false(result[1, 1])
})
